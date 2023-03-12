import {InsightError, InsightResult, ResultTooLargeError} from "../../controller/IInsightFacade";
import Options from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {AnyKey, ApplyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import aggregateSections from "./QueryAggregate";
import sortResults from "./SortResults";
import {DataModel} from "../../models/DatasetModels/DataModel";

export default function filterResults(options: Options,
									  sections: Section[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {
	let results: InsightResult[];
	if (transformations) {
		results = transformationResults(transformations, options.columns, sections, datasetId);
	} else {
		results = vanillaResults(options.columns, sections, datasetId);
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

function vanillaResults(columnKeys: AnyKey[], sections: Section[], datasetId: string): InsightResult[] {
	return sections.map((section: Section) => {
		let insightResult: InsightResult = {};
		columnKeys.forEach((key: AnyKey) => {
			addKey(key, section, insightResult, datasetId);
		});
		return insightResult;
	});
}

function transformationResults(transformations: Transformations,
							   columns: AnyKey[],
							   sections: Section[],
							   datasetId: string): InsightResult[] {
	let groups: Map<string, DataModel[]>;
	let insightResults: InsightResult[] = [];
	groups = groupData(transformations.group, sections, new Map<string, DataModel[]>());
	groups.forEach((grouped_sections: DataModel[]) => {
		let insightResult: InsightResult = {};
		columns.forEach((columnKey: AnyKey) => {
			if (!(columnKey instanceof ApplyKey)) {
				if (grouped_sections.length > 0) {
					addKey(columnKey, grouped_sections[0], insightResult, datasetId);
				}
			} else {
				transformApplyRules(transformations.applyRules, grouped_sections, insightResult);
			}
		});
		insightResults.push(insightResult);
	});
	return insightResults;
}

function transformApplyRules(rules: ApplyRule[], insightData: DataModel[], insightResult: InsightResult): void {
	rules.forEach((rule: ApplyRule) => {
		insightResult[rule.id] = aggregateSections(rule.key, rule.applyToken, insightData);
	});
}

function groupData(groupKeys: Key[],
				   insightData: DataModel[],
				   map: Map<string, DataModel[]>): Map<string, DataModel[]> {
	insightData.reduce((groups: Map<string, DataModel[]>, current: DataModel) => {
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

function addKey(key: AnyKey, section: DataModel, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		insightResult[datasetId.concat("_", key.mField)] = section.getMFieldValue(key.mField);
	} else if (key instanceof SKey) {
		insightResult[datasetId.concat("_", key.sField)] = section.getSFieldValue(key.sField);
	} else {
		throw new InsightError("ApplyKey shouldn't have been passed in");
	}
}

