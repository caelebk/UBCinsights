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
import Options from "../../models/QueryModels/Options";
import {ValidQuery, ValidOptions, ValidComparator} from "./QueryInterfaces";
import Where from "../../models/QueryModels/Where";
import Query from "../../models/QueryModels/Query";

/**
 * Parses, validates, and converts query object to EBNF query data model.
 * @param query -> Object received to parse, validate, and convert to Query.
 */
export default function parseAndValidateQuery(query: unknown): Query {
	// Check existence of query
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}

	const checkQuery: ValidQuery = query as ValidQuery;

	// Check that the OPTIONS keyword exists
	if (!checkQuery?.OPTIONS) {
		throw new InsightError("Query is missing OPTIONS keyword");
	}
	// Check that the WHERE keyword exists.
	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE keyword.");
	}
	let options: Options = parseAndValidateOptions(checkQuery.OPTIONS);
	// If WHERE isn't empty, we will parse, validate, and convert the comparators to data models.
	const isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ? parseAndValidateComparator(checkQuery.WHERE) : undefined;
	let where: Where = new Where(comparator);

	return new Query(where, options);
}

/**
 * parses, validates, and converts options object to options data model.
 * @param options -> object to be parsed, validated, and converted to options data model
 */
function parseAndValidateOptions(options: ValidOptions): Options {
	const columns: string[] = options.COLUMNS;
	// Check that the COLUMNS keyword exists
	if (!columns) {
		throw new InsightError("Query is missing COLUMNS keyword");
	}
	let columnKeys: Key[] = [];
	let orderKey: Key | undefined;

	// Check that each key in the Columns query is valid and convert them to key data models and append to columnKeys.
	columns.forEach((value: string) => {
		// keyComponent[0] -> id of dataset
		// keyComponent[1] -> mfield or sfield
		const keyComponents: string[] = value.split("_");

		// If after splitting the key, there aren't 2 components, then it is invalid.
		if (keyComponents.length !== 2) {
			throw new InsightError("A key in COLUMN is invalid: " + value);
		}

		// TODO: Some form of way to check if dataset id exists.

		// If mfield/sfield isn't a valid field in the key, then the key is invalid.
		if (!(keyComponents[1] in MField) && !(keyComponents[1] in SField)) {
			throw new InsightError("A key in Column has an invalid field: " + value);
		}

		// Append the keys into the columnKeys array
		if (keyComponents[1] in MField) {
			columnKeys.push(new MKey(keyComponents[1] as MField));
		} else if (keyComponents[1] in SField) {
			columnKeys.push(new SKey(keyComponents[1] as SField));
		}
	});

	// If an ORDER query exists but the key doesnt exist in columns: InsightError
	// NOTE: We do not need to check if ORDER is a valid key.
	if (options.ORDER) {
		if (columns.includes(options.ORDER)) {
			let keyComponents: string[] = options.ORDER.split("_");
			if (keyComponents[1] in MField) {
				orderKey = new MKey(keyComponents[1] as MField);
			} else if (keyComponents[1] in SField) {
				orderKey = new SKey(keyComponents[1] as SField);
			}
		} else {
			throw new InsightError("ORDER key must exist in COLUMNS");
		}
	}

	return new Options(columnKeys, orderKey);
}

/**
 * Parses, validates, and converts comparator object to a comparator data model.
 * @param comparator -> object
 */
function parseAndValidateComparator(comparator: ValidComparator): Comparator {
	// Should only have one comparator properties.
	if (Object.keys(comparator).length !== 1) {
		throw new InsightError("Should only have 1 comparator type instead has "
			+ Object.keys(comparator).length);
	}

	if (comparator.LT) {
		return parseAndValidateMKey(comparator.LT, MComparatorLogic.LT);
	} else if (comparator.EQ) {
		return parseAndValidateMKey(comparator.EQ, MComparatorLogic.EQ);
	} else if (comparator.GT) {
		return parseAndValidateMKey(comparator.GT, MComparatorLogic.GT);
	} else if (comparator.IS) {
		return parseAndValidateSKey(comparator.IS);
	} else if (comparator.NOT) {
		return new NegationComparator(parseAndValidateComparator(comparator.NOT));
	} else if (comparator.AND) {
		if (comparator.AND.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.AND.map((value: ValidComparator) => {
			return parseAndValidateComparator(value);
		});
		return new LogicComparator(Logic.AND, recursion as Comparator[]);
	} else if (comparator.OR) {
		if (comparator.OR.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		let recursion: Comparator[] = comparator.OR.map((value: ValidComparator) => {
			return parseAndValidateComparator(value);
		});
		return new LogicComparator(Logic.OR, recursion as Comparator[]);
	} else {
		throw new InsightError("Undefined passed into LogicComparator");
	}
}

/**
 * Parses and Validates a MComparator object and converts it into a MComparator data model.
 * @param mComparator -> object to be converted
 * @param type -> type of MComparison
 */
function parseAndValidateMKey(mComparator: object, type: MComparatorLogic): MComparator {

	const keys: string[] = Object.keys(mComparator);
	if (keys.length !== 1) {
		throw new InsightError(type + " should have only 1 mkey instead has " + keys.length);
	}

	// Create var for the validated MKey
	const mKey: MKey = parseAndValidateKey(keys[0], true) as MKey;
	const value: unknown[] = Object.values(mComparator);

	// Validate there is only one value in mkey.
	if (value.length !== 1) {
		throw new InsightError("There should only be one value in mkey");
	}

	if (typeof value[0] !== "number") {
		throw new InsightError("Value of mkey must be a number");
	}

	return new MComparator(mKey, value[0], type);
}

/**
 * Parses and Validates a SComparator object and converts it into a SComparator data model.
 * @param sComparator -> object to be converted
 */
function parseAndValidateSKey(sComparator: object): SComparator {

	const keys: string[] = Object.keys(sComparator);

	if (keys.length !== 1) {
		throw new InsightError("IS: should have only 1 skey instead has " + keys.length);
	}

	const sKey: SKey = parseAndValidateKey(keys[0], false) as SKey;

	const value: unknown[] = Object.values(sComparator);
	// Validate there is only one value in sComparator.
	if (value.length !== 1) {
		throw new InsightError("There should only be one value in sComparator");
	}

	if (typeof value[0] !== "string") {
		throw new InsightError("Invalid type for inputstring");
	}

	const inputString: string = value[0] as string;
	const invalidAsterisk: boolean = inputString.length > 2
		&& inputString.substring(1, inputString.length).indexOf("*") !== -1;

	if (invalidAsterisk) {
		throw new InsightError("Asterisks can only be the first or last character of the input string");
	}

	return new SComparator(sKey, inputString);
}

/**
 * Parses, validates, and converts key string to a Key data model
 * @param key -> string that contains dataset id and key separated by "_"
 * @param isMKey -> if true: looking for MKey, if false: looking for SKey
 */
function parseAndValidateKey(key: string, isMKey: boolean): Key {
	// keyComponent[0] = id of dataset
	// keyComponent[1] = sfield
	const keyComponents: string[] = key.split("_");

	// If after splitting the key, there aren't 2 components, then it is invalid.
	if (keyComponents.length !== 2) {
		throw new InsightError("Key is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}

	// TODO: Some form of way to check if dataset id exists.

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
