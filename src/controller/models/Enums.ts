export enum MField {
	average = "avg",
	num_pass = "pass",
	num_fail = "fail",
	num_audit = "audit",
	year = "year"
}

export enum SField {
	department = "dept",
	courseID = "id",
	instructor = "instructor",
	courseName = "title",
	sectionID = "uuid"
}

export enum ComparisonType {
	logic = "LOGICCOMPARISON",
	mComparison = "MCOMPARISON",
	sComparison = "SCOMPARISON",
	negation = "NEGATION"
}

export enum Logic {
	and = "AND",
	or = "OR"
}

export enum MComparator {
	greater = "GT",
	less = "LT",
	equal = "EQ"
}


