import {InsightError, InsightResult, ResultTooLargeError} from "../../controller/IInsightFacade";
import Options, {Order} from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {AnyKey, ApplyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import aggregateSections from "./QueryAggregate";

export default function filterResults(options: Options,
									  sections: Section[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {

	if (options.order) {
		sections = sortResults(options.order, sections);
	}
	let results: InsightResult[];
	if (transformations) {
		results = transformationResults(transformations, options.columns, sections, datasetId);
	} else {
		results = vanillaResults(options.columns, sections, datasetId);
	}
	if (results.length > 5000) {
		throw new ResultTooLargeError("There were more than 5000 results with this query.");
	} else {
		return results;
	}
}

export function transformationResults(transformations: Transformations,
									  columns: AnyKey[],
									  sections: Section[],
									  datasetId: string): InsightResult[] {
	let groups: Map<string, Section[]>;
	let insightResults: InsightResult[] = [];
	groups = groupData(transformations.group, sections, new Map<string, Section[]>());
	groups.forEach((grouped_sections: Section[], grouped_key: string) => {
		let insightResult: InsightResult = {};
		columns.forEach((columnKey: AnyKey) => {
			if (!(columnKey instanceof ApplyKey)) {
				if (grouped_sections.length > 0) {
					filterKeys(columnKey, grouped_sections[0], insightResult, datasetId);
				}
			} else {
				transformApplyRules(transformations.applyRules, grouped_sections, insightResult);
			}
		});
		insightResults.push(insightResult);
	});
	return insightResults;
}

function transformApplyRules(rules: ApplyRule[], sections: Section[], insightResult: InsightResult): void {
	rules.forEach((rule: ApplyRule) => {
		insightResult[rule.id] = aggregateSections(rule.key, rule.applyToken, sections);
	});
}

export function vanillaResults(columnKeys: AnyKey[], sections: Section[], datasetId: string): InsightResult[] {
	return sections.map((section: Section) => {
		let insightResult: InsightResult = {};
		columnKeys.forEach((key: AnyKey) => {
			filterKeys(key, section, insightResult, datasetId);
		});
		return insightResult;
	});
}

function groupData(groupKeys: Key[], sections: Section[], map: Map<string, Section[]>): Map<string, Section[]> {
	sections.reduce((groups: Map<string, Section[]>, current: Section) => {
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

function updateGroup(groups: Map<string, Section[]>, value: string, section: Section): void {
	let groupedSection: Section[] | undefined = groups.get(value);
	if (!groupedSection) {
		groups.set(value, [section]);
	} else {
		groupedSection.push(section);
	}
}

function filterKeys(key: AnyKey, section: Section, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		insightResult[datasetId.concat("_", key.mField)] = section.getMFieldValue(key.mField);
	} else if (key instanceof SKey) {
		insightResult[datasetId.concat("_", key.sField)] = section.getSFieldValue(key.sField);
	} else {
		throw new InsightError("ApplyKey shouldn't have been passed in");
	}
}

function sortResults(key: Order, sections: Section[]): Section[] {
	return sections.sort((section1: Section, section2: Section) => {
		return sortingPrecedence(key, section1, section2);
	});
}

function sortingPrecedence(key: Order, section1: Section, section2: Section): number {
	if (key instanceof MKey) {
		return section1.getMFieldValue(key.mField) > section2.getMFieldValue(key.mField) ? 1 : -1;
	} else if (key instanceof SKey) {
		return section1.getSFieldValue(key.sField).localeCompare(section2.getSFieldValue(key.sField));
	}
	// TODO: handle ApplyKey
	return 0;
}


