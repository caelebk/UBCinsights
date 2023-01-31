import {InsightError} from "../../controller/IInsightFacade";
import {MField, SField} from "../../models/QueryModels/Enums";

interface ValidQuery {
	WHERE: ValidComparator,
	OPTIONS: ValidOptions,
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

export default function validateQueryJSON(query: unknown) {
	// Check existence of query
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}

	const checkQuery: ValidQuery = query as ValidQuery;

	// Check that the WHERE keyword exists.
	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE keyword.");
	}
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

		// key[0] -> id of dataset
		// key[1] -> mfield or sfield

		const key: string[] = value.split("_");

		// If after splitting the key, there aren't 2 components, then it is invalid.
		if (key.length !== 2) {
			throw new InsightError("A key in COLUMN is invalid: " + value);
		}

		// TODO: Some form of way to check if dataset id exists.

		// If key[1] isn't a valid field, then the key is invalid.
		if (!(key[1] in MField) && !(key[1] in SField)) {
			throw new InsightError("A key in Column has an invalid field: " + value);
		}
	});

	const columns: string[] = checkQuery?.OPTIONS?.COLUMNS;

	// Check if an ORDER query exists then check if the ORDER key exists in COLUMNS
	// NOTE: We do not need to check if ORDER is a valid key.
	if (checkQuery?.OPTIONS.ORDER && !(checkQuery.OPTIONS.ORDER in columns)) {
		throw new InsightError("ORDER key must exist in COLUMNS");
	}
}

const tempQuery = {
	WHERE : {},
	OPTIONS : {
		COLUMNS: {},
		ORDER: {}
	}
};


validateQueryJSON(tempQuery);
