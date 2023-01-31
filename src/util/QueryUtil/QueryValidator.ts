import {InsightError} from "../../controller/IInsightFacade";

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

export function validateQueryJSON(query: unknown) {
	if (!query) {
		throw new InsightError("Query passed in was undefined");
	}

	const checkQuery: ValidQuery = query as ValidQuery;

	if (!checkQuery?.WHERE) {
		throw new InsightError("Query is missing WHERE.");
	}
	if (!checkQuery?.OPTIONS) {
		throw new InsightError("Query is missing OPTIONS");
	}
	if (!checkQuery?.OPTIONS?.COLUMNS) {
		throw new InsightError("Query is missing COLUMNS");
	}
}

const tempQuery = {
	WHERE : {},
	OPTIONS : {
		COLUMNS: {}
	}
};


validateQueryJSON(tempQuery);
