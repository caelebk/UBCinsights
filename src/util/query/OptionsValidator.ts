import {DatasetProperties, ValidOptions} from "./QueryInterfaces";
import {AnyKey, ApplyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Options, {Order} from "../../models/QueryModels/Options";
import {InsightError} from "../../controller/IInsightFacade";
import {MField, SField} from "../../models/QueryModels/Enums";
import {parseAndValidateKey} from "./QueryValidator";
import parseAndValidateSort from "./SortValidator";

export default function parseAndValidateOptions(options: ValidOptions,
								 datasetProperties: DatasetProperties,
								 group?: Key[]): Options {
	if (!options) {
		throw new InsightError("Options content was undefined");
	}
	const columns: string[] = options.COLUMNS;
	if (!columns || columns.length === 0) {
		throw new InsightError("Query is missing COLUMNS keyword or columns must be a non-empty array");
	}
	let columnKeys: AnyKey[] = parseAndValidateColumns(columns, datasetProperties, group);
	let sort: Order | undefined;
	if (options?.ORDER) {
		sort = parseAndValidateSort(options, columns, datasetProperties);
	}
	return new Options(columnKeys, sort);
}
function parseAndValidateColumns(columns: string[], datasetProperties: DatasetProperties, group?: Key[]): AnyKey[] {
	let columnKeys: AnyKey[] = [];
	columns.forEach((value: string) => {
		const keyComponents: string[] = value.split("_");
		if (keyComponents.length < 2)  {
			if (!datasetProperties.applyKeys.has(value)) {
				throw new InsightError("COLUMNS contains a non-existent applykey");
			}
			columnKeys.push(new ApplyKey(value));
		} else {
			let columnKey: Key = parseAndValidateKey(value, datasetProperties);
			if (group) {
				let existsInGroup: boolean;
				if (columnKey instanceof MKey) {
					let columnField: MField = columnKey.mField;
					existsInGroup = group.some((groupKey: Key) => groupKey instanceof MKey ?
						groupKey.mField === columnField : false);
				} else {
					let columnField: SField = columnKey.sField;
					existsInGroup = group.some((groupKey: Key) => groupKey instanceof SKey ?
						groupKey.sField === columnField : false);
				}
				if(!existsInGroup) {
					throw new InsightError("MKey or SKey in COLUMNS must be present in GROUP");
				}
			}
			columnKeys.push(columnKey);
		}
	});
	return columnKeys;
}
