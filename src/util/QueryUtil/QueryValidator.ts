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
	let isWhereEmpty: boolean = Object.keys(checkQuery?.WHERE).length !== 0;
	let comparator: Comparator | undefined = isWhereEmpty ? parseAndValidateComparator(checkQuery.WHERE) : undefined;
	let where: Where = new Where(comparator);

	return new Query(where, options);
}

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

function parseAndValidateComparator(comparator: ValidComparator): Comparator {
	// Should only have one comparator properties.
	if (Object.keys(comparator).length !== 1) {
		throw new InsightError("Should only have 1 comparator type instead has "
			+ Object.keys(comparator).length);
	}

	let recursion: Comparator[];
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
		recursion = comparator.AND.map((value: ValidComparator) => {
			return parseAndValidateComparator(value);
		});
		return new LogicComparator(Logic.AND, recursion as Comparator[]);
	} else if (comparator.OR) {
		if (comparator.OR.length === 0) {
			throw new InsightError("AND must be a non-empty array");
		}
		recursion = comparator.OR.map((value: ValidComparator) => {
			return parseAndValidateComparator(value);
		});
		return new LogicComparator(Logic.OR, recursion as Comparator[]);
	} else {
		throw new InsightError("Undefined passed into LogicComparator");
	}
}

/**
 * Parses and Validates a MComparator object and converts it into a MComparator data model.
 */
function parseAndValidateMKey(mComparator: object, type: MComparatorLogic): MComparator {

	const keys: string[] = Object.keys(mComparator);
	if (keys.length !== 1) {
		throw new InsightError(type + " should have only 1 mkey instead has " + keys.length);
	}

	// keyComponent[0] = id of dataset
	// keyComponent[1] = mfield
	const keyComponents: string[] = keys[0].split("_");
	// If after splitting the key, there aren't 2 components, then it is invalid.
	if (keyComponents.length !== 2) {
		throw new InsightError("Mkey is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}

	// TODO: Some form of way to check if dataset id exists.

	// If keyComponent[1] isn't an mfield, then this is an invalid key.
	if (!(keyComponents[1] in MField)) {
		throw new InsightError("Invalid mfield for mkey");
	}

	// Create var for the validated MKey
	const validatedKey: MKey = new MKey(keyComponents[1] as MField);
	const value: any[] = Object.values(mComparator);

	// Validate there is only one value in mkey.
	if (value.length !== 1) {
		throw new InsightError("There should only be one value in mkey");
	}

	if (typeof value[0] !== "number") {
		throw new InsightError("Value of mkey must be a number");
	}

	return new MComparator(validatedKey, value[0], type);
}

function parseAndValidateKey(key: string, isMKey: boolean): Key {
	// keyComponent[0] = id of dataset
	// keyComponent[1] = sfield
	const keyComponents: string[] = key[0].split("_");

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

/**
 * Parses and Validates a SComparator object and converts it into a SComparator data model.
 */
function parseAndValidateSKey(sComparator: object): SComparator {

	const keys: string[] = Object.keys(sComparator);

	if (keys.length !== 1) {
		throw new InsightError("IS: should have only 1 skey instead has " + keys.length);
	}

	// keyComponent[0] = id of dataset
	// keyComponent[1] = sfield
	const keyComponents: string[] = keys[0].split("_");

	// If after splitting the key, there aren't 2 components, then it is invalid.
	if (keyComponents.length !== 2) {
		throw new InsightError("Skey is in invalid format: The key split into "
			+ keyComponents.length + " components");
	}

	// TODO: Some form of way to check if dataset id exists.

	// If keyComponent[1] isn't an sfield, then this is an invalid key.
	if (!(keyComponents[1] in SField)) {
		throw new InsightError("Invalid sfield for skey");
	}

	const skey: SKey = new SKey(keyComponents[1] as SField);

	const value: any[] = Object.values(sComparator);
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

	return new SComparator(skey, inputString);
}

const tempSimpleQuery = {
	WHERE : {
		AND : []
	},
	OPTIONS : {
		COLUMNS: [
			"sections_avg",
			"sections_dept"
		],
		ORDER: "sections_avg"
	}
};

const tempComplexQuery = {
	WHERE : {
		OR:[
			{
				AND:[
					{
					   GT:{
							ubc_dept:90
						}
					},
					{
						IS:{
							ubc_dept:"adhe"
						}
					}
				]
			},
			{
				EQ:{
					ubc_avg:95
				}
			}
		]
	},
	OPTIONS:{
		COLUMNS:[
			"ubc_dept",
			"ubc_id",
			"ubc_avg"
		],
		ORDER: "ubc_avg"
	}
};

console.log(parseAndValidateQuery(tempComplexQuery));
