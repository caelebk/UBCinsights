import {InsightDatasetKind, InsightError} from "../../../controller/IInsightFacade";
import {
	Logic,
	MComparatorLogic,
	MFieldRoom,
	MFieldSection,
	SFieldRoom,
	SFieldSection
} from "../../../models/QueryModels/Enums";
import {
	Comparator,
	LogicComparator,
	MComparator,
	NegationComparator,
	SComparator
} from "../../../models/QueryModels/Comparators";
import {Key, MKey, SKey} from "../../../models/QueryModels/Keys";
import Options from "../../../models/QueryModels/Options";
import {DatasetProperties, ValidComparator, ValidQuery} from "../QueryInterfaces";
import Where from "../../../models/QueryModels/Where";
import Query from "../../../models/QueryModels/Query";
import {Data} from "../../../models/DatasetModels/Data";
import Transformations from "../../../models/QueryModels/Transformations";
import parseAndValidateTransformations from "./TransformationValidator";
import parseAndValidateOptions from "./OptionsValidator";

export default function parseAndValidateQuery(query: unknown, data: Data, datasetProp: DatasetProperties): Query {
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}
	const checkQuery: ValidQuery = query as ValidQuery;
	if (!checkQuery?.OPTIONS) {
		throw new InsightError("Query is missing OPTIONS keyword");
	}
	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE keyword.");
	}
	let transformations: Transformations | undefined;
	let totalQueryKeys: number = 2;
	if (checkQuery.TRANSFORMATIONS) {
		transformations = parseAndValidateTransformations(checkQuery.TRANSFORMATIONS, datasetProp);
		totalQueryKeys = 3;
	}
	validateNumberKeys(Object.keys(checkQuery), totalQueryKeys);
	let options: Options = parseAndValidateOptions(checkQuery.OPTIONS, datasetProp, transformations?.group);
	const isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ?
		parseAndValidateComparator(checkQuery.WHERE, datasetProp) : undefined;
	let where: Where = new Where(comparator);
	if (datasetProp.datasetId === "") {
		throw new InsightError("No dataset id received.");
	}
	return new Query(where, options, datasetProp.datasetId, transformations);
}
function parseAndValidateComparator(comparator: ValidComparator, datasetProp: DatasetProperties): Comparator {
	if (!comparator) {
		throw new InsightError("comparator is undefined");
	}
	validateNumberKeys(Object.keys(comparator), 1);
	if (comparator.LT) {
		return parseAndValidateMComparator(comparator.LT, MComparatorLogic.LT, datasetProp);
	} else if (comparator.EQ) {
		return parseAndValidateMComparator(comparator.EQ, MComparatorLogic.EQ, datasetProp);
	} else if (comparator.GT) {
		return parseAndValidateMComparator(comparator.GT, MComparatorLogic.GT, datasetProp);
	} else if (comparator.IS) {
		return parseAndValidateSComparator(comparator.IS, datasetProp);
	} else if (comparator.NOT) {
		return new NegationComparator(parseAndValidateComparator(comparator.NOT, datasetProp));
	} else if (comparator.AND) {
		if (!comparator.AND?.length || comparator.AND.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.AND.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, datasetProp);
		});
		return new LogicComparator(Logic.AND, recursion as Comparator[]);
	} else if (comparator.OR) {
		if (!comparator.OR?.length || comparator.OR.length === 0) {
			throw new InsightError("OR must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.OR.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, datasetProp);
		});
		return new LogicComparator(Logic.OR, recursion as Comparator[]);
	} else {
		throw new InsightError("Query is missing a comparator");
	}
}
function parseAndValidateMComparator(mComparator: object,
									 type: MComparatorLogic,
									 datasetProp: DatasetProperties): MComparator {
	if (!mComparator) {
		throw new InsightError("MComparator was undefined");
	}
	const keys: string[] = validateNumberKeys(Object.keys(mComparator), 1) as string[];
	let keyComponents: string[] = splitAndValidateKeyComponent(keys[0]);
	let mKey: MKey | undefined = parseAndValidateMKey(keyComponents, datasetProp);
	validateDatasetID(keyComponents[0], datasetProp);
	if (!mKey) {
		throw new InsightError("Invalid MField for MKey");
	}
	const value: unknown[] = Object.values(mComparator);
	if (typeof value[0] !== "number") {
		throw new InsightError("Value of mkey must be a number");
	}
	return new MComparator(mKey, value[0], type);
}
function parseAndValidateSComparator(sComparator: object, datasetProp: DatasetProperties): SComparator {
	if (!sComparator) {
		throw new InsightError("SComparator was undefined");
	}
	const keys: string[] = validateNumberKeys(Object.keys(sComparator), 1) as string[];
	let keyComponents: string[] = splitAndValidateKeyComponent(keys[0]);
	validateDatasetID(keyComponents[0], datasetProp);
	let sKey: SKey | undefined = parseAndValidateSKey(keyComponents, datasetProp);
	if (!sKey) {
		throw new InsightError("Invalid SField for SKey");
	}
	const value: unknown[] = Object.values(sComparator);
	if (typeof value[0] !== "string") {
		throw new InsightError("Invalid type for inputstring");
	}
	const inputString: string = value[0] as string;
	const invalidAsterisk: boolean = inputString.length > 2
		&& inputString.substring(1, inputString.length - 1).indexOf("*") !== -1;
	if (invalidAsterisk) {
		throw new InsightError("Asterisks can only be the first or last character of the input string");
	}
	return new SComparator(sKey, inputString);
}
export function parseAndValidateKey(key: string, datasetProp: DatasetProperties): Key {
	const keyComponents: string[] = splitAndValidateKeyComponent(key);
	validateDatasetID(keyComponents[0], datasetProp);
	let mKey: MKey | undefined = parseAndValidateMKey(keyComponents, datasetProp);
	let sKey: SKey | undefined = parseAndValidateSKey(keyComponents, datasetProp);
	if (mKey) {
		return mKey;
	} else if (sKey) {
		return sKey;
	} else {
		throw new InsightError("Invalid Field for Key.");
	}
}

function parseAndValidateMKey(keyComponents: string[], datasetProp: DatasetProperties): MKey | undefined {
	if (datasetProp.dataKind === InsightDatasetKind.Sections) {
		if (keyComponents[1] in MFieldSection) {
			return new MKey(keyComponents[1] as MFieldSection);
		}
	} else {
		if (keyComponents[1] in MFieldRoom) {
			return new MKey(keyComponents[1] as MFieldRoom);
		}
	}
	return undefined;
}

function parseAndValidateSKey(keyComponents: string[], datasetProp: DatasetProperties): SKey | undefined {
	if (datasetProp.dataKind === InsightDatasetKind.Sections) {
		if (keyComponents[1] in SFieldSection) {
			return new SKey(keyComponents[1] as SFieldSection);
		}
	} else {
		if (keyComponents[1] in SFieldRoom) {
			return new SKey(keyComponents[1] as SFieldRoom);
		}
	}
	return undefined;
}

export function validateNumberKeys(keys: unknown[], numKeys: number): unknown[] {
	if (keys.length !== numKeys) {
		throw new InsightError("There are extra keys in query");
	}
	return keys;
}
export function splitAndValidateKeyComponent(key: string): string[] {
	const keyComponents: string[] = key.split("_");
	const numberSplit = 2;
	if (keyComponents.length !== numberSplit) {
		throw new InsightError("Key is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}
	return keyComponents;
}
export function validateDatasetID (id: string, datasetProp: DatasetProperties): void {
	if (datasetProp.datasetId === "" && datasetProp.data.has(id)) {
		datasetProp.datasetId = id;
		datasetProp.dataKind = datasetProp.data.get(id).kind;
	} else if (id.trim() === "") {
		throw new InsightError("Key: Dataset id cannot be blank/whitespace");
	} else if (!datasetProp.data.has(id)) {
		throw new InsightError("Key: Dataset cannot be found.");
	} else if (datasetProp.datasetId !== id) {
		throw new InsightError("Key: Can't reference multiple dataset ids");
	}
}
