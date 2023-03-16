import {Order, OrderObject} from "../../models/QueryModels/Options";
import {AnyKey, MKey, SKey} from "../../models/QueryModels/Keys";
import {Direction} from "../../models/QueryModels/Enums";
import {InsightError, InsightResult} from "../../controller/IInsightFacade";

export default function sortResults(order: Order, results: InsightResult[], datasetId: string): InsightResult[] {
	if (!order) {
		throw new InsightError("ORDER is undefined.");
	}
	if (order instanceof OrderObject) {
		const direction: Direction = order.direction;
		const keys: AnyKey[] = order.keys;
		return results.sort((result1: InsightResult, result2: InsightResult) => {
			let comparisonValue: number = -1;
			keys.some((key: AnyKey) => {
				comparisonValue = sortingPrecedence(key, result1, result2, datasetId, direction);
				return comparisonValue !== 0;
			});
			return comparisonValue;
		});
	} else {
		return results.sort((result1: InsightResult, result2: InsightResult) => {
			return sortingPrecedence(order, result1, result2, datasetId);
		});
	}
}

function sortingPrecedence(key: AnyKey,
						   result1: InsightResult,
						   result2: InsightResult,
						   datasetId: string,
						   direction?: Direction): number {
	let directionToggle = 1;
	if (direction && direction === Direction.DOWN) {
		directionToggle = -1;
	}
	let compareKeys: number;
	let compareValues: number;
	if (key instanceof MKey) {
		const resultKey: string = datasetId.concat("_", key.mField);
		compareValues = Number(result1[resultKey]) -  Number(result2[resultKey]);
		compareKeys = Number(result1[resultKey]) >  Number(result2[resultKey]) ? 1 : -1;
	} else if (key instanceof SKey) {
		const resultKey: string = datasetId.concat("_", key.sField);
		compareValues = Number(result1[resultKey]) -  Number(result2[resultKey]);
		compareKeys = String(result1[resultKey]) > String(result2[resultKey]) ? 1 : -1;
		compareKeys = String(result1[resultKey]) === String(result2[resultKey]) ? 0 : compareKeys;
	} else {
		compareValues = Number(result1[key.id]) -  Number(result2[key.id]);
		compareKeys = Number(result1[key.id]) > Number(result2[key.id]) ? 1 : -1;
	}
	if (direction && compareValues === 0) {
		return compareValues;
	}
	return directionToggle * compareKeys;
}
