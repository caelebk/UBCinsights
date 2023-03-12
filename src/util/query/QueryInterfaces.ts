import {Data} from "../../models/DatasetModels/Data";
import {InsightDatasetKind} from "../../controller/IInsightFacade";

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
	ORDER?: ValidOrderObject | string
}

export interface ValidOrderObject {
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
	applyKeys: Set<string>,
	dataKind: InsightDatasetKind
}
