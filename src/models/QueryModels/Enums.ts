export type MField = MFieldSection | MFieldRoom;
export type SField = SFieldSection | SFieldRoom;

export enum MFieldSection {
	avg = "avg",
	pass = "pass",
	fail = "fail",
	audit = "audit",
	year = "year",
}

export enum MFieldRoom {
	lat = "lat",
	lon = "lon",
	seats = "seats"
}

export enum SFieldSection {
	dept = "dept",
	id = "id",
	instructor = "instructor",
	title = "title",
	uuid = "uuid",
}

export enum SFieldRoom {
	fullname = "fullname",
	shortname = "shortname",
	number = "number",
	name = "name",
	address = "address",
	type = "type",
	furniture = "furniture",
	href = "href"
}

export enum Logic {
	AND = "AND",
	OR = "OR"
}

export enum MComparatorLogic {
	GT = "GT",
	LT = "LT",
	EQ = "EQ"
}

export enum ApplyToken {
	MAX = "MAX",
	MIN = "MIN",
	AVG = "AVG",
	COUNT = "COUNT",
	SUM = "SUM"
}

export enum Direction {
	UP = "UP",
	DOWN = "DOWN"
}

