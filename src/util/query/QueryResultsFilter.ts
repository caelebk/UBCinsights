import {InsightError, InsightResult} from "../../controller/IInsightFacade";
import Options, {Order} from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {AnyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import {SField} from "../../models/QueryModels/Enums";

export default function filterResults(options: Options,
									  sections: Section[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {
	let groups: Map<string, Section[]>;
	let transformedData: Map<string, number>;
	if (transformations) {
		groups = groupData(transformations.group, sections, new Map<string, Section[]>());
		transformData(transformations.applyRules, groups);
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

// Transform grouped sections to aggregated values.
function transformData(rules: ApplyRule[], groups: Map<string, Section[]>): Map<string, number> {
	return new Map<string, number>();
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
	} else {
		// TODO: Implement Apply Key filter.
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


let sections: Section[] = [
	new Section({id: "1", Course: "cpsc", Subject: "sci",
		Professor: "Jean",  Avg: 90, Section: "1900", Title : "210", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "2", Course: "cpsc", Subject: "sci",
		Professor: "Jean",  Avg: 80, Section: "1900", Title : "310", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "3", Course: "cpsc", Subject: "sci",
		Professor: "Casey",  Avg: 95, Section: "1900", Title : "310", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "4", Course: "cpsc", Subject: "sci",
		Professor: "Casey",  Avg: 85, Section: "1900", Title : "310", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "5", Course: "cpsc", Subject: "sci",
		Professor: "Kelly",  Avg: 74, Section: "1900", Title : "210", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "6", Course: "cpsc", Subject: "sci",
		Professor: "Kelly",  Avg: 78, Section: "1900", Title : "210", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "7", Course: "cpsc", Subject: "sci",
		Professor: "Kelly",  Avg: 72, Section: "1900", Title : "210", Year:100, Audit:10, Fail:20, Pass: 10}),
	new Section({id: "8", Course: "cpsc", Subject: "sci",
		Professor: "Eli",  Avg: 85, Section: "1900", Title : "210", Year:100, Audit:10, Fail:20, Pass: 10}),
];

console.log(groupData([new SKey(SField.instructor), new SKey(SField.title)],
	sections, new Map<string, Section[]>()));
