import {InsightError, InsightResult} from "../../controller/IInsightFacade";
import Options, {Order} from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {AnyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";

export default function filterResults(options: Options,
									  sections: Section[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {
	let group: Map<string, Section[]> = new Map<string, Section[]>();
	if (transformations) {
		groupData(transformations.group, sections, group);
		transformData(transformations.applyRules, sections);
	}
	if (options.order) {
		sections = sortResults(options.order, sections);
	}
	const columnKeys: AnyKey[] = options.columns;
	return sections.map((section: Section) => {
		let insightResult: InsightResult = {};
		columnKeys.forEach((key: AnyKey) => {
			filterKeys(key, section, insightResult, datasetId);
		});
		return insightResult;
	});
}

function groupData(groupKeys: Key[], sections: Section[], map: Map<string, Section[]>): Map<string, Section[]> {
	groupKeys.forEach((key: Key) => {
		sections.reduce((groups: Map<string, Section[]>, current: Section) => {
			if (key instanceof MKey) {
				let value: string = String(current.getMFieldValue(key.mField));
				updateGroup(groups, value, current);
			} else {
				let value: string = current.getSFieldValue(key.sField);
				updateGroup(groups, value, current);
			}
			return groups;
		}, map);
	});
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

function transformData(rules: ApplyRule[], sections: Section[]): Section[] {
	return [];
}

function findMax(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		return sections.reduce((prev: Section, current: Section) => {
			return (prev.getMFieldValue(key.mField) > current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MAX for SKeys");
	}
}

function findAvg(sections: Section[], key: Key): number {
	return findSum(sections, key) / sections.length;
}

function findMin(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		return sections.reduce((prev: Section, current: Section) => {
			return (prev.getMFieldValue(key.mField) < current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function findCount(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		let mFieldValues: number[] = sections.map((section: Section) => section.getMFieldValue(key.mField));
		return new Set<number>(mFieldValues).size;
	} else {
		let sFieldValues: string[] = sections.map((section: Section) => section.getSFieldValue(key.sField));
		return new Set<string>(sFieldValues).size;
	}
}

function findSum(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		return sections.reduce((sum: number, current: Section) => {
			return sum + current.getMFieldValue(key.mField);
		}, 0);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function filterKeys(key: AnyKey, section: Section, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		insightResult[datasetId.concat("_", key.mField)] = section.getMFieldValue(key.mField);
	} else if (key instanceof SKey) {
		insightResult[datasetId.concat("_", key.sField)] = section.getSFieldValue(key.sField);
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
	// TODO: handle rooms which hasn't been implemented yet
	return 0;
}
