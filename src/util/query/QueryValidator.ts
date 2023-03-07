import {InsightError} from "../../controller/IInsightFacade";
import {ApplyToken, Direction, Logic, MComparatorLogic, MField, SField} from "../../models/QueryModels/Enums";
import {Comparator, LogicComparator, MComparator, NegationComparator, SComparator
} from "../../models/QueryModels/Comparators";
import {AnyKey, ApplyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Options, {Order, Sort} from "../../models/QueryModels/Options";
import {
	ValidQuery, ValidOptions, ValidComparator, ValidSort, ValidTransformations, DatasetProperties
} from "./QueryInterfaces";
import Where from "../../models/QueryModels/Where";
import Query from "../../models/QueryModels/Query";
import {Data} from "../../models/DatasetModels/Data";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";

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
	if (checkQuery.TRANSFORMATIONS) {
		transformations = parseAndValidateTransformations(checkQuery.TRANSFORMATIONS, datasetProperties);
	}
	let options: Options = parseAndValidateOptions(checkQuery.OPTIONS, datasetProperties);
	if (!options) {
		throw new InsightError("Options contents was undefined");
	}
	const isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ?
		parseAndValidateComparator(checkQuery.WHERE, datasetProperties) : undefined;
	let where: Where = new Where(comparator);
	if (datasetProperties.datasetId === "") {
		throw new InsightError("No dataset id received.");
	}
	return new Query(where, options, datasetProperties.datasetId, transformations);
}
function parseAndValidateTransformations(transformations: ValidTransformations,
										 datasetProperties: DatasetProperties): Transformations {
	if (!transformations.GROUP || !transformations.APPLY) {
		throw new InsightError("Transformations is missing Apply or Group");
	}
	let group: Key[] = transformations.GROUP.map((value: string) => {
		return parseAndValidateKey(value, datasetProperties);
	});
	let apply: ApplyRule[] = transformations.APPLY.map((value: object) => {
		let applyKey: string[] = Object.keys(value);
		let conversion: object[] = Object.values(value);
		if (applyKey.length !== 1 || conversion.length !== 1) {
			throw new InsightError("Given multiple/no applyKeys or multiple/no conversions for ApplyRule");
		}
		checkApplyKeys(applyKey[0], datasetProperties.applyKeys);
		let conversionTokens: string[] = Object.keys(conversion[0]);
		let conversionKeys: string[] = Object.values(conversion[0]);
		if (conversionTokens.length !== 1 || conversionKeys.length !== 1) {
			throw new InsightError("Given multiple/no Tokens or multiple/no Keys for ApplyRule");
		}
		let applyToken: ApplyToken;
		if (conversionTokens[0] in ApplyToken) {
			applyToken = conversionTokens[0] as ApplyToken;
		} else {
			throw new InsightError("Invalid ApplyToken");
		}
		let key: Key = parseAndValidateKey(conversionKeys[0], datasetProperties);
		return new ApplyRule(new ApplyKey(applyKey[0]), applyToken, key);
	});

	return new Transformations(group, apply);
}

function parseAndValidateOptions(options: ValidOptions,
								 datasetProperties: DatasetProperties): Options {
	if (!options) {
		throw new InsightError("Options content was undefined");
	}
	const columns: string[] = options.COLUMNS;
	if (!columns) {
		throw new InsightError("Query is missing COLUMNS keyword");
	}
	if (columns.length === 0) {
		throw new InsightError("COLUMNS must be a non-empty array");
	}
	let columnKeys: AnyKey[] = parseAndValidateColumns(columns, datasetProperties);
	let sort: Sort | undefined;
	if (options?.SORT) {
		sort = parseAndValidateSort(options.SORT, columns, datasetProperties);
	}
	return new Options(columnKeys, sort);
}

function parseAndValidateColumns(columns: string[], datasetProperties: DatasetProperties): AnyKey[] {
	let columnKeys: AnyKey[] = [];
	columns.forEach((value: string) => {
		const keyComponents: string[] = value.split("_");
		if (keyComponents.length < 2)  {
			if (!datasetProperties.applyKeys.has(value)) {
				throw new InsightError("COLUMNS contains a non-existent applykey");
			}
			columnKeys.push(new ApplyKey(value));
		} else {
			columnKeys.push(parseAndValidateKey(value, datasetProperties));
		}
	});
	return columnKeys;
}

function parseAndValidateSort(sort: ValidSort, columns: string[], datasetProperties: DatasetProperties): Sort {
	let orderKey: AnyKey | Order;
	if (!sort?.ORDER) {
		throw new InsightError("ORDER must exist in SORT");
	}
	if (typeof sort.ORDER === "string") {
		if (columns.includes(sort.ORDER)) {
			let keyComponents: string[] = sort.ORDER.split("_");
			if (keyComponents.length < 2) {
				orderKey = new ApplyKey(sort.ORDER);
			} else {
				orderKey = parseAndValidateKey(sort.ORDER, datasetProperties);
			}
		} else {
			throw new InsightError("ORDER key must exist in COLUMNS");
		}
	} else {
		if(!sort.ORDER.dir) {
			throw new InsightError("Direction cannot be empty");
		} else if (!sort.ORDER.keys) {
			throw new InsightError("Keys don't exist in ORDER");
		}
		let direction: Direction;
		if (sort.ORDER.dir in Direction) {
			direction = sort.ORDER.dir as Direction;
		} else {
			throw new InsightError("Invalid Direction");
		}
		let keys: AnyKey[] = sort.ORDER.keys.map((value: string) => {
			if (columns.includes(value)) {
				let keyComponents: string[] = value.split("_");
				if (keyComponents.length < 2) {
					return new ApplyKey(value);
				} else {
					return parseAndValidateKey(value, datasetProperties);
				}
			} else {
				throw new InsightError("ORDER key must exist in COLUMNS");
			}
		});
		orderKey = new Order(direction, keys);
	}
	return new Sort(orderKey);
}

function checkApplyKeys(applyKey: string, existingApplyKeys: Set<string>): void {
	if (applyKey.length === 0) {
		throw new InsightError("Applykey cannot be empty");
	} else if (existingApplyKeys.has(applyKey)) {
		throw new InsightError("Applykey must be unique");
	} else if (applyKey.indexOf("_") !== -1) {
		throw new InsightError("Applykey cannot contain underscore.");
	} else {
		existingApplyKeys.add(applyKey);
	}
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
	if (keyComponents[1] in MField) {
		mKey = new MKey(keyComponents[1] as MField);
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
	if (keyComponents[1] in SField) {
		sKey = new SKey(keyComponents[1] as SField);
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
function parseAndValidateKey(key: string, datasetProperties: DatasetProperties): Key {
	const keyComponents: string[] = key.split("_");
	if (keyComponents.length !== 2) {
		throw new InsightError("Key is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}
	validateDatasetID(keyComponents[0], datasetProperties);
	if (keyComponents[1] in MField) {
		return new MKey(keyComponents[1] as MField);
	} else if (keyComponents[1] in SField) {
		return new SKey(keyComponents[1] as SField);
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
