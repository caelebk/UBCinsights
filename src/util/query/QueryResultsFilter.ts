import {InsightError, InsightResult} from "../../controller/IInsightFacade";
import Options, {Order} from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {AnyKey, Key, MKey, SKey} from "../../models/QueryModels/Keys";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import {MField} from "../../models/QueryModels/Enums";

export default function filterResults(options: Options,
									  sections: Section[],
									  datasetId: string,
									  transformations?: Transformations): InsightResult[] {
	if (transformations) {
		groupData(transformations.group, sections);
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

function groupData(group: Key[], sections: Section[]): Section[] {
	return [];
}

function transformData(rules: ApplyRule[], sections: Section[]): Section[] {
	return [];
}

function findMax(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		return sections.reduce((prev: Section, current: Section) => {
			return (prev.getMField(key.mField) > current.getMField(key.mField)) ? prev : current;
		}).getMField(key.mField);
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
			return (prev.getMField(key.mField) < current.getMField(key.mField)) ? prev : current;
		}).getMField(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function findCount(sections: Section[], key: Key): number {
	return 0;
}

function findSum(sections: Section[], key: Key): number {
	if (key instanceof MKey) {
		return sections.reduce((sum: number, current: Section) => {
			return sum + current.getMField(MField.avg);
		}, 0);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function filterKeys(key: AnyKey, section: Section, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		insightResult[datasetId.concat("_", key.mField)] = section.getMField(key.mField);
	} else if (key instanceof SKey) {
		insightResult[datasetId.concat("_", key.sField)] = section.getSField(key.sField);
	}
}

function sortResults(key: Order, sections: Section[]): Section[] {
	return sections.sort((section1: Section, section2: Section) => {
		return sortingPrecedence(key, section1, section2);
	});
}
function sortingPrecedence(key: Order, section1: Section, section2: Section): number {
	if (key instanceof MKey) {
		return section1.getMField(key.mField) > section2.getMField(key.mField) ? 1 : -1;
	} else if (key instanceof SKey) {
		return section1.getSField(key.sField).localeCompare(section2.getSField(key.sField));
	}
	// TODO: handle rooms which hasn't been implemented yet
	return 0;
}
