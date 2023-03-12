import {InsightDatasetKind, InsightError} from "../../controller/IInsightFacade";
import {Logic, MComparatorLogic, MFieldSection, SFieldSection} from "../../models/QueryModels/Enums";
import {
	Comparator,
	LogicComparator,
	MComparator,
	NegationComparator,
	SComparator
} from "../../models/QueryModels/Comparators";
import {Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Options from "../../models/QueryModels/Options";
import {DatasetProperties, ValidComparator, ValidQuery} from "./QueryInterfaces";
import Where from "../../models/QueryModels/Where";
import Query from "../../models/QueryModels/Query";
import {Data} from "../../models/DatasetModels/Data";
import Transformations from "../../models/QueryModels/Transformations";
import parseAndValidateTransformations from "./TransformationValidator";
import parseAndValidateOptions from "./OptionsValidator";

export default function parseAndValidateQuery(query: unknown, data: Data): Query {
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}
	const checkQuery: ValidQuery = query as ValidQuery;
	if (!checkQuery) {
		throw new InsightError("Query passed in was undefined");
	}
	if (!checkQuery?.OPTIONS) {
		throw new InsightError("Query is missing OPTIONS keyword");
	}
	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE keyword.");
	}
	let datasetProperties: DatasetProperties = {
		data : data,
		datasetId: "",
		applyKeys : new Set<string>()
	};
	let transformations: Transformations | undefined;
	const numKeys: number = Object.keys(checkQuery).length;
	let totalQueryKeys: number = 2;
	if (checkQuery.TRANSFORMATIONS) {
		transformations = parseAndValidateTransformations(checkQuery.TRANSFORMATIONS, datasetProperties);
		totalQueryKeys = 3;
	}
	if (numKeys > totalQueryKeys) {
		throw new InsightError("Excess keys in query");
	}
	let options: Options = parseAndValidateOptions(checkQuery.OPTIONS, datasetProperties, transformations?.group);
	const isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ?
		parseAndValidateComparator(checkQuery.WHERE, datasetProperties) : undefined;
	let where: Where = new Where(comparator);
	if (datasetProperties.datasetId === "") {
		throw new InsightError("No dataset id received.");
	}
	return new Query(where, options, datasetProperties.datasetId, InsightDatasetKind.Sections, transformations);
}
function parseAndValidateComparator(comparator: ValidComparator, datasetProperties: DatasetProperties): Comparator {
	if (!comparator) {
		throw new InsightError("comparator is undefined");
	}
	if (Object.keys(comparator).length !== 1) {
		throw new InsightError("Should only have 1 comparator type instead has "
			+ Object.keys(comparator).length);
	}
	if (comparator.LT) {
		return parseAndValidateMComparator(comparator.LT, MComparatorLogic.LT, datasetProperties);
	} else if (comparator.EQ) {
		return parseAndValidateMComparator(comparator.EQ, MComparatorLogic.EQ, datasetProperties);
	} else if (comparator.GT) {
		return parseAndValidateMComparator(comparator.GT, MComparatorLogic.GT, datasetProperties);
	} else if (comparator.IS) {
		return parseAndValidateSComparator(comparator.IS, datasetProperties);
	} else if (comparator.NOT) {
		return new NegationComparator(parseAndValidateComparator(comparator.NOT, datasetProperties));
	} else if (comparator.AND) {
		if (!comparator.AND?.length || comparator.AND.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.AND.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, datasetProperties);
		});
		return new LogicComparator(Logic.AND, recursion as Comparator[]);
	} else if (comparator.OR) {
		if (!comparator.OR?.length || comparator.OR.length === 0) {
			throw new InsightError("OR must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.OR.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, datasetProperties);
		});
		return new LogicComparator(Logic.OR, recursion as Comparator[]);
	} else {
		throw new InsightError("Query is missing a comparator");
	}
}
function parseAndValidateMComparator(mComparator: object,
									 type: MComparatorLogic,
									 datasetProperties: DatasetProperties): MComparator {
	if (!mComparator) {
		throw new InsightError("MComparator was undefined");
	}
	const keys: string[] = Object.keys(mComparator);
	if (keys.length !== 1) {
		throw new InsightError(type + " should have only 1 mkey instead has " + keys.length);
	}
	let keyComponents: string[] = keys[0].split("_");
	let mKey: MKey;
	if (keyComponents.length !== 2) {
		throw new InsightError("Invalid MKey");
	}
	validateDatasetID(keyComponents[0], datasetProperties);
	if (keyComponents[1] in MFieldSection) {
		mKey = new MKey(keyComponents[1] as MFieldSection);
	} else {
		throw new InsightError("Invalid MField for Mkey");
	}
	const value: unknown[] = Object.values(mComparator);
	if (typeof value[0] !== "number") {
		throw new InsightError("Value of mkey must be a number");
	}
	return new MComparator(mKey, value[0], type);
}
function parseAndValidateSComparator(sComparator: object, datasetProperties: DatasetProperties): SComparator {
	if (!sComparator) {
		throw new InsightError("SComparator was undefined");
	}
	const keys: string[] = Object.keys(sComparator);
	if (keys.length !== 1) {
		throw new InsightError("IS: should have only 1 skey instead has " + keys.length);
	}
	let keyComponents: string[] = keys[0].split("_");
	let sKey: SKey;
	if (keyComponents.length !== 2) {
		throw new InsightError("Invalid MKey");
	}
	validateDatasetID(keyComponents[0], datasetProperties);
	if (keyComponents[1] in SFieldSection) {
		sKey = new SKey(keyComponents[1] as SFieldSection);
	} else {
		throw new InsightError("Invalid MField for Mkey");
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
export function parseAndValidateKey(key: string, datasetProperties: DatasetProperties): Key {
	const keyComponents: string[] = key.split("_");
	if (keyComponents.length !== 2) {
		throw new InsightError("Key is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}
	validateDatasetID(keyComponents[0], datasetProperties);
	if (keyComponents[1] in MFieldSection) {
		return new MKey(keyComponents[1] as MFieldSection);
	} else if (keyComponents[1] in SFieldSection) {
		return new SKey(keyComponents[1] as SFieldSection);
	} else {
		throw new InsightError("Invalid field for key");
	}
}
function validateDatasetID (id: string, datasetProperties: DatasetProperties): void {
	if (datasetProperties.datasetId === "" && datasetProperties.data.has(id)) {
		datasetProperties.datasetId = id;
	} else if (id.trim() === "") {
		throw new InsightError("Key: Dataset id cannot be blank/whitespace");
	} else if (!datasetProperties.data.has(id)) {
		throw new InsightError("Key: Dataset cannot be found.");
	} else if (datasetProperties.datasetId !== id) {
		throw new InsightError("Key: Can't reference multiple dataset ids");
	}
}
