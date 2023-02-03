export interface ValidQuery {
	WHERE: ValidComparator,
	OPTIONS: ValidOptions
}

export interface ValidComparator {
	AND?: ValidComparator[],
	OR?: ValidComparator[],
	LT?: object,
	GT?: object,
	EQ?: object,
	IS?: object,
	NOT?: ValidComparator
}

export interface ValidOptions {
	COLUMNS: string[],
	ORDER?: string
}
