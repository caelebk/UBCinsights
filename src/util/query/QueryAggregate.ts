import {Section} from "../../models/DatasetModels/Section";
import {Key, MKey} from "../../models/QueryModels/Keys";
import {InsightError} from "../../controller/IInsightFacade";
import {ApplyToken} from "../../models/QueryModels/Enums";

export default function aggregateSections(key: Key, token: ApplyToken, sections: Section[]): number {
	switch (token) {
		case ApplyToken.AVG:
			return findAvg(sections, key);
		case ApplyToken.COUNT:
			return findCount(sections, key);
		case ApplyToken.MAX:
			return findMax(sections, key);
		case ApplyToken.MIN:
			return findMin(sections, key);
		case ApplyToken.SUM:
			return findSum(sections, key);
	}
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
	return Number((findSum(sections, key) / sections.length).toFixed(2));
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
		let summed: number = sections.reduce((sum: number, current: Section) => {
			return sum + current.getMFieldValue(key.mField);
		}, 0);
		return Number(summed.toFixed(2));
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}
