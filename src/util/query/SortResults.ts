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
			keys.forEach((key: AnyKey) => {
				const comparisonValue: number = sortingPrecedence(key, result1, result2, datasetId, direction);
				if (comparisonValue !== 0) {
					return comparisonValue;
				}
			});
			return 0;
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
	if (key instanceof MKey) {
		const resultKey: string = datasetId.concat("_", key.mField);
		compareKeys = Number(result1[resultKey]) -  Number(result2[resultKey]);
	} else if (key instanceof SKey) {
		const resultKey: string = datasetId.concat("_", key.sField);
		compareKeys = String(result1[resultKey]).localeCompare(String(result2[resultKey]));
	} else {
		compareKeys = Number(result1[key.id]) - Number(result2[key.id]);
	}
	return directionToggle * compareKeys;
}
