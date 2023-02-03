import {InsightError} from "../../controller/IInsightFacade";
import {MComparatorLogic, MField, SField} from "../../models/QueryModels/Enums";
import {LogicComparator, MComparator, NegationComparator, SComparator} from "../../models/QueryModels/Comparators";
import {MKey, SKey} from "../../models/QueryModels/Keys";

interface ValidQuery {
	WHERE: ValidComparator,
	OPTIONS: ValidOptions
}

interface ValidComparator {
	AND?: ValidComparator[],
	OR?: ValidComparator[],
	LT?: object,
	GT?: object,
	EQ?: object,
	IS?: object,
	NOT?: ValidComparator
}

interface ValidOptions {
	COLUMNS: string[],
	ORDER?: string
}

export default function parseAndValidateQuery(query: unknown): boolean {
	// Check existence of query
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}

	const checkQuery: ValidQuery = query as ValidQuery;

	/**
	 * Validate OPTIONS content
	 */
	// Check that the OPTIONS keyword exists
	if (!checkQuery?.OPTIONS) {
		throw new InsightError("Query is missing OPTIONS keyword");
	}
	// Check that the COLUMNS keyword exists
	if (!checkQuery?.OPTIONS.COLUMNS) {
		throw new InsightError("Query is missing COLUMNS keyword");
	}

	// Check that each key in the Columns query is valid.
	checkQuery?.OPTIONS.COLUMNS.forEach((value: string) => {

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
	});

	const columns: string[] = checkQuery?.OPTIONS?.COLUMNS;

	// Check if an ORDER query exists then check if the ORDER key exists in COLUMNS
	// NOTE: We do not need to check if ORDER is a valid key.
	if (checkQuery?.OPTIONS?.ORDER && !(checkQuery.OPTIONS.ORDER in columns)) {
		throw new InsightError("ORDER key must exist in COLUMNS");
	}

	/**
	 * Validate WHERE content
	 */

	// Check that the WHERE keyword exists.
	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE keyword.");
	}

	// Check if WHERE isn't empty, we will validate the comparators.
	if (Object.keys(!checkQuery?.WHERE).length !== 0) {
		parseAndValidateWHERE(checkQuery.WHERE);
	}


	return false;
}

function parseAndValidateWHERE(comparator: ValidComparator): LogicComparator | MComparator | SComparator
																				| NegationComparator | undefined {
	/**
	 * Base Cases
	 */
	// Should only have one or zero comparator properties.
	if (Object.keys(comparator).length === 1) {
		throw new InsightError("Should only have 1 comparator type instead has "
			+ Object.keys(comparator).length);
	}

	switch (comparator) {
		case comparator.LT:
			return parseAndValidateMKey(comparator, MComparatorLogic.LT);
		case comparator.EQ:
			return parseAndValidateMKey(comparator, MComparatorLogic.EQ);
		case comparator.GT:
			return parseAndValidateMKey(comparator, MComparatorLogic.GT);
		case comparator.IS:
			return parseAndValidateSKey(comparator);
		case comparator.NOT:

		case comparator.AND:

		case comparator.OR:

	}

	return undefined;
}

/**
 * Validate MKey and Value
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

function parseAndValidateSKey(sComparator: object) {

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

const tempQuery = {
	WHERE : {},
	OPTIONS : {
		COLUMNS: {},
		ORDER: {}
	}
};

// parseAndValidateQuery(tempQuery);
