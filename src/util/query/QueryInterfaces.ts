import {Data} from "../../models/DatasetModels/Data";

export interface ValidQuery {
	WHERE: ValidComparator,
	OPTIONS: ValidOptions,
	TRANSFORMATIONS?: ValidTransformations,
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
	SORT?: ValidSort
}

export interface ValidSort {
	ORDER: ValidOrder | string
}

export interface ValidOrder {
	dir: string,
	keys: string[]
}

export interface ValidTransformations {
	GROUP: string[],
	APPLY: object[],
}

export interface DatasetProperties {
	data: Data,
	datasetId: string,
	applyKeys: Set<string>
}
