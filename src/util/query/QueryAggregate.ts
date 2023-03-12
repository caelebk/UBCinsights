import {Key, MKey} from "../../models/QueryModels/Keys";
import {InsightError} from "../../controller/IInsightFacade";
import {ApplyToken} from "../../models/QueryModels/Enums";
import {DataModel} from "../../models/DatasetModels/DataModel";

export default function aggregateSections(key: Key, token: ApplyToken, insightData: DataModel[]): number {
	switch (token) {
		case ApplyToken.AVG:
			return findAvg(insightData, key);
		case ApplyToken.COUNT:
			return findCount(insightData, key);
		case ApplyToken.MAX:
			return findMax(insightData, key);
		case ApplyToken.MIN:
			return findMin(insightData, key);
		case ApplyToken.SUM:
			return findSum(insightData, key);
	}
}

function findMax(insightData: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		return insightData.reduce((prev: DataModel, current: DataModel) => {
			return (prev.getMFieldValue(key.mField) > current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MAX for SKeys");
	}
}

function findAvg(insightData: DataModel[], key: Key): number {
	return Number((findSum(insightData, key) / insightData.length).toFixed(2));
}

function findMin(insightData: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		return insightData.reduce((prev: DataModel, current: DataModel) => {
			return (prev.getMFieldValue(key.mField) < current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function findCount(insightData: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		let mFieldValues: number[] = insightData.map((value: DataModel) => value.getMFieldValue(key.mField));
		return new Set<number>(mFieldValues).size;
	} else {
		let sFieldValues: string[] = insightData.map((value: DataModel) => value.getSFieldValue(key.sField));
		return new Set<string>(sFieldValues).size;
	}
}

function findSum(insightData: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		let summed: number = insightData.reduce((sum: number, current: DataModel) => {
			return sum + current.getMFieldValue(key.mField);
		}, 0);
		return Number(summed.toFixed(2));
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}
