import {InsightError} from "../../controller/IInsightFacade";

interface ValidQuery {
	WHERE: ValidComparator,
	OPTIONS: ValidOptions,
}

interface ValidComparator {
	AND?: ValidComparator[],
	OR?: ValidComparator[],
	LT?: ValidMComparator,
	GT?: ValidMComparator,
	EQ?: ValidMComparator,
	IS?: ValidSComparator,
	NOT?: ValidComparator
}

interface ValidMComparator {
	avg: number,
	pass: number,
	fail: number,
	audit: number,
	year: number
}

interface ValidSComparator {
	dept: string,
	id: string,
	instructor: string,
	title: string,
	uuid: string
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
