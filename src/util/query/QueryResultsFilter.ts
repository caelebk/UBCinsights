import {InsightError, InsightResult, ResultTooLargeError} from "../../controller/IInsightFacade";
import Options from "../../models/QueryModels/Options";
import {AnyKey, ApplyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import aggregateData from "./QueryAggregate";
import sortResults from "./SortResults";
import {DataModel} from "../../models/DatasetModels/DataModel";

export default function filterResults(options: Options,
									  insightData: DataModel[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {
	let results: InsightResult[];
	if (transformations) {
		results = transformationResults(transformations, options.columns, insightData, datasetId);
	} else {
		results = vanillaResults(options.columns, insightData, datasetId);
	}
	if (options.order) {
		results = sortResults(options.order, results, datasetId);
	}
	if (results.length > 5000) {
		throw new ResultTooLargeError("There were more than 5000 results with this query.");
	} else {
		return results;
	}
}

function vanillaResults(columnKeys: AnyKey[], insightDataList: DataModel[], datasetId: string): InsightResult[] {
	return insightDataList.map((value: DataModel) => {
		let insightResult: InsightResult = {};
		columnKeys.forEach((key: AnyKey) => {
			addKey(key, value, insightResult, datasetId);
		});
		return insightResult;
	});
}

function transformationResults(transformations: Transformations,
							   columns: AnyKey[],
							   insightDataList: DataModel[],
							   datasetId: string): InsightResult[] {
	let groups: Map<string, DataModel[]>;
	let insightResults: InsightResult[] = [];
	groups = groupData(transformations.group, insightDataList, new Map<string, DataModel[]>());
	groups.forEach((grouped_insightData: DataModel[]) => {
		let insightResult: InsightResult = {};
		columns.forEach((columnKey: AnyKey) => {
			if (!(columnKey instanceof ApplyKey)) {
				if (grouped_insightData.length > 0) {
					addKey(columnKey, grouped_insightData[0], insightResult, datasetId);
				}
			} else {
				transformApplyRules(transformations.applyRules, grouped_insightData, insightResult);
			}
		});
		insightResults.push(insightResult);
	});
	return insightResults;
}

function transformApplyRules(rules: ApplyRule[], insightDataList: DataModel[], insightResult: InsightResult): void {
	rules.forEach((rule: ApplyRule) => {
		insightResult[rule.id] = aggregateData(rule.key, rule.applyToken, insightDataList);
	});
}

function groupData(groupKeys: Key[],
				   insightDataList: DataModel[],
				   map: Map<string, DataModel[]>): Map<string, DataModel[]> {
	insightDataList.reduce((groups: Map<string, DataModel[]>, current: DataModel) => {
		let value: string = "";
		groupKeys.forEach((key: Key) => {
			if (key instanceof MKey) {
				value += String(current.getMFieldValue(key.mField));
			} else {
				value += current.getSFieldValue(key.sField);
			}
			value += "_";
		});
		value = value.substring(0, value.length - 1);
		updateGroup(groups, value, current);
		return groups;
	}, map);
	return map;
}

function updateGroup(groups: Map<string, DataModel[]>, value: string, insightData: DataModel): void {
	let groupedData: DataModel[] | undefined = groups.get(value);
	if (!groupedData) {
		groups.set(value, [insightData]);
	} else {
		groupedData.push(insightData);
	}
}

function addKey(key: AnyKey, insightData: DataModel, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		insightResult[datasetId.concat("_", key.mField)] = insightData.getMFieldValue(key.mField);
	} else if (key instanceof SKey) {
		insightResult[datasetId.concat("_", key.sField)] = insightData.getSFieldValue(key.sField);
	} else {
		throw new InsightError("ApplyKey shouldn't have been passed in");
	}
}

