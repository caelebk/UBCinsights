import {Key, MKey} from "../../models/QueryModels/Keys";
import {InsightError} from "../../controller/IInsightFacade";
import {ApplyToken} from "../../models/QueryModels/Enums";
import {DataModel} from "../../models/DatasetModels/DataModel";
import Decimal from "decimal.js";

export default function aggregateData(key: Key, token: ApplyToken, insightDataList: DataModel[]): number {
	switch (token) {
		case ApplyToken.AVG:
			return findAvg(insightDataList, key);
		case ApplyToken.COUNT:
			return findCount(insightDataList, key);
		case ApplyToken.MAX:
			return findMax(insightDataList, key);
		case ApplyToken.MIN:
			return findMin(insightDataList, key);
		case ApplyToken.SUM:
			return findSum(insightDataList, key);
	}
}

function findMax(insightDataList: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		return insightDataList.reduce((prev: DataModel, current: DataModel) => {
			return (prev.getMFieldValue(key.mField) > current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MAX for SKeys");
	}
}

function findAvg(insightDataList: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		let summed: Decimal = insightDataList.reduce((sum: Decimal, current: DataModel) => {
			return Decimal.add(sum, current.getMFieldValue(key.mField));
		}, new Decimal(0));
		return Number((summed.toNumber() / insightDataList.length).toFixed(2));
	} else {
		throw new InsightError("Cannot aggregate AVG for SKeys");
	}
}

function findMin(insightDataList: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		return insightDataList.reduce((prev: DataModel, current: DataModel) => {
			return (prev.getMFieldValue(key.mField) < current.getMFieldValue(key.mField)) ? prev : current;
		}).getMFieldValue(key.mField);
	} else {
		throw new InsightError("Cannot aggregate MIN for SKeys");
	}
}

function findCount(insightDataList: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		let mFieldValues: number[] = insightDataList.map((value: DataModel) => value.getMFieldValue(key.mField));
		return new Set<number>(mFieldValues).size;
	} else {
		let sFieldValues: string[] = insightDataList.map((value: DataModel) => value.getSFieldValue(key.sField));
		return new Set<string>(sFieldValues).size;
	}
}

function findSum(insightDataList: DataModel[], key: Key): number {
	if (key instanceof MKey) {
		let summed: number = insightDataList.reduce((sum: number, current: DataModel) => {
			return sum + current.getMFieldValue(key.mField);
		}, 0);
		return Number(summed.toFixed(2));
	} else {
		throw new InsightError("Cannot aggregate SUM for SKeys");
	}
}
