import {InsightError} from "../../controller/IInsightFacade";
import {Logic, MComparatorLogic, MField, SField} from "../../models/QueryModels/Enums";
import {
	Comparator,
	LogicComparator,
	MComparator,
	NegationComparator,
	SComparator
} from "../../models/QueryModels/Comparators";
import {Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Options, {Sort} from "../../models/QueryModels/Options";
import {ValidQuery, ValidOptions, ValidComparator} from "./QueryInterfaces";
import Where from "../../models/QueryModels/Where";
import Query from "../../models/QueryModels/Query";
import {Data} from "../../models/DatasetModels/Data";
import Transformations from "../../models/QueryModels/Transformations";

/**
 * Parses, validates, and converts query object to EBNF query data model.
 * @param query -> Object received to parse, validate, and convert to Query.
 * @param data -> Data structure that holds all the dataset ids
 */
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

	let datasetId: {id: string} = {id : ""};
	let options: Options = parseAndValidateOptions(checkQuery.OPTIONS, data, datasetId);
	if (!options) {
		throw new InsightError("Options contents was undefined");
	}

	let transformations: Transformations | undefined;
	if (checkQuery.TRANSFORMATIONS) {
		transformations = parseAndValidateTransformations();
	}

	const isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ?
		parseAndValidateComparator(checkQuery.WHERE, data, datasetId) : undefined;
	let where: Where = new Where(comparator);

	if (datasetId.id === "") {
		throw new InsightError("No dataset id received.");
	}
	return new Query(where, options, datasetId.id);
}

// TODO: parse and validate transformations
function parseAndValidateTransformations(): Transformations | undefined {
	return undefined;
}

/**
 * parses, validates, and converts options object to options data model.
 * @param options -> object to be parsed, validated, and converted to options data model
 * @param data -> Data structure that has all the datasets
 * @param datasetId -> Object to persist a singular dataset id, so that we do not reference multiple.
 */
function parseAndValidateOptions(options: ValidOptions, data: Data, datasetId: {id: string}): Options {
	if (!options) {
		throw new InsightError("Options content was undefined");
	}
	const columns: string[] = options.COLUMNS;
	// Check that the COLUMNS keyword exists
	if (!columns) {
		throw new InsightError("Query is missing COLUMNS keyword");
	}
	if (columns.length === 0) {
		throw new InsightError("COLUMNS must be a non-empty array");
	}
	let columnKeys: Key[] = [];
	columns.forEach((value: string) => {
		const keyComponents: string[] = value.split("_");
		if (keyComponents.length !== 2) {
			throw new InsightError("A key in COLUMN is invalid: " + value);
		}
		validateDatasetID(keyComponents[0], data, datasetId);
		if (!(keyComponents[1] in MField) && !(keyComponents[1] in SField)) {
			throw new InsightError("A key in Column has an invalid field: " + value);
		}
		if (keyComponents[1] in MField) {
			columnKeys.push(new MKey(keyComponents[1] as MField));
		} else if (keyComponents[1] in SField) {
			columnKeys.push(new SKey(keyComponents[1] as SField));
		}
	});

	// TODO: Validate and parse the new sort case.
	let orderKey: Key | undefined;
	if (options?.SORT?.ORDER) {
		if (typeof options.SORT.ORDER === "string") {
			if (columns.includes(options.SORT.ORDER)) {
				let keyComponents: string[] = options.SORT.ORDER.split("_");
				if (keyComponents[1] in MField) {
					orderKey = new MKey(keyComponents[1] as MField);
				} else if (keyComponents[1] in SField) {
					orderKey = new SKey(keyComponents[1] as SField);
				}
			} else {
				throw new InsightError("ORDER key must exist in COLUMNS");
			}
		}

	}

	return new Options(columnKeys);
}

/**
 * Parses, validates, and converts comparator object to a comparator data model.
 * @param comparator -> object
 * @param data -> Data structure that holds all the dataset ids
 * @param datasetId -> An object to persist a singular dataset id, so that we do not reference multiple
 */
function parseAndValidateComparator(comparator: ValidComparator, data: Data, datasetId: {id: string}): Comparator {
	if (!comparator) {
		throw new InsightError("comparator is undefined");
	}
	if (Object.keys(comparator).length !== 1) {
		throw new InsightError("Should only have 1 comparator type instead has "
			+ Object.keys(comparator).length);
	}
	if (comparator.LT) {
		return parseAndValidateMComparator(comparator.LT, MComparatorLogic.LT, data, datasetId);
	} else if (comparator.EQ) {
		return parseAndValidateMComparator(comparator.EQ, MComparatorLogic.EQ, data, datasetId);
	} else if (comparator.GT) {
		return parseAndValidateMComparator(comparator.GT, MComparatorLogic.GT, data, datasetId);
	} else if (comparator.IS) {
		return parseAndValidateSComparator(comparator.IS, data, datasetId);
	} else if (comparator.NOT) {
		return new NegationComparator(parseAndValidateComparator(comparator.NOT, data, datasetId));
	} else if (comparator.AND) {
		if (!comparator.AND?.length || comparator.AND.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.AND.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, data, datasetId);
		});
		return new LogicComparator(Logic.AND, recursion as Comparator[]);
	} else if (comparator.OR) {
		if (!comparator.OR?.length || comparator.OR.length === 0) {
			throw new InsightError("OR must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.OR.map((value: ValidComparator) => {
			return parseAndValidateComparator(value, data, datasetId);
		});
		return new LogicComparator(Logic.OR, recursion as Comparator[]);
	} else {
		throw new InsightError("Query is missing a comparator");
	}
}

/**
 * Parses and Validates a MComparator object and converts it into a MComparator data model.
 * @param mComparator -> object to be converted
 * @param type -> type of MComparison
 * @param data -> Data structure that holds all the dataset ids
 * @param datasetId -> An object to persist a singular dataset id, so that we do not reference multiple
 */
function parseAndValidateMComparator(mComparator: object, type: MComparatorLogic,
									 data: Data, datasetId: {id: string}): MComparator {
	if (!mComparator) {
		throw new InsightError("MComparator was undefined");
	}

	const keys: string[] = Object.keys(mComparator);
	if (keys.length !== 1) {
		throw new InsightError(type + " should have only 1 mkey instead has " + keys.length);
	}

	const isMKey: boolean = true;
	const mKey: MKey = parseAndValidateKey(keys[0], isMKey, data, datasetId) as MKey;
	const value: unknown[] = Object.values(mComparator);

	if (typeof value[0] !== "number") {
		throw new InsightError("Value of mkey must be a number");
	}

	return new MComparator(mKey, value[0], type);
}

/**
 * Parses and Validates a SComparator object and converts it into a SComparator data model.
 * @param sComparator -> object to be converted
 * @param data -> Data structure that holds all the dataset ids
 * @param datasetId -> An object to persist a singular dataset id, so that we do not reference multiple
 */
function parseAndValidateSComparator(sComparator: object, data: Data, datasetId: {id: string}): SComparator {
	if (!sComparator) {
		throw new InsightError("SComparator was undefined");
	}
	const keys: string[] = Object.keys(sComparator);

	if (keys.length !== 1) {
		throw new InsightError("IS: should have only 1 skey instead has " + keys.length);
	}

	const isMKey: boolean = false;
	const sKey: SKey = parseAndValidateKey(keys[0], isMKey, data, datasetId) as SKey;

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

/**
 * Parses, validates, and converts key string to a Key data model
 * @param key -> string that contains dataset id and key separated by "_"
 * @param isMKey -> if true: looking for MKey, if false: looking for SKey
 * @param data -> Data structure that holds all the dataset ids
 * @param datasetId -> An object to persist a singular dataset id, so that we do not reference multiple
 */
function parseAndValidateKey(key: string, isMKey: boolean, data: Data, datasetId: {id: string}): Key {
	const keyComponents: string[] = key.split("_");

	if (keyComponents.length !== 2) {
		throw new InsightError("Key is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}
	validateDatasetID(keyComponents[0], data, datasetId);
	if (isMKey) {
		if (keyComponents[1] in MField) {
			return new MKey(keyComponents[1] as MField);
		} else {
			throw new InsightError("Invalid mfield for mkey");
		}
	} else {
		if (keyComponents[1] in SField) {
			return new SKey(keyComponents[1] as SField);
		} else {
			throw new InsightError("Invalid sfield for skey");
		}
	}
}

function validateDatasetID (id: string, data: Data, datasetId: {id: string}): void {
	if (datasetId.id === "" && data.has(id)) {
		datasetId.id = id;
	} else if (id.trim() === "") {
		throw new InsightError("Key: Dataset id cannot be blank/whitespace");
	} else if (!data.has(id)) {
		throw new InsightError("Key: Dataset cannot be found.");
	} else if (datasetId.id !== id) {
		throw new InsightError("Key: Can't reference multiple dataset ids");
	}
}
