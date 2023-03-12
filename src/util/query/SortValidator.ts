import {DatasetProperties, ValidOptions} from "./QueryInterfaces";
import {Order, OrderObject} from "../../models/QueryModels/Options";
import {AnyKey, ApplyKey} from "../../models/QueryModels/Keys";
import {InsightError} from "../../controller/IInsightFacade";
import {Direction} from "../../models/QueryModels/Enums";
import {parseAndValidateKey} from "./QueryValidator";

export default function parseAndValidateSort(options: ValidOptions,
									 columns: string[],
									 datasetProperties: DatasetProperties): Order {
	let orderKey: AnyKey | Order;
	if (!options?.ORDER) {
		throw new InsightError("ORDER must exist in SORT");
	}
	if (typeof options.ORDER === "string") {
		if (columns.includes(options.ORDER)) {
			let keyComponents: string[] = options.ORDER.split("_");
			orderKey = (keyComponents.length < 2) ? new ApplyKey(options.ORDER) :
				parseAndValidateKey(options.ORDER, datasetProperties);
		} else {
			throw new InsightError("ORDER key must exist in COLUMNS");
		}
	} else {
		const numKeys = 2;
		if (Object.keys(options.ORDER).length > numKeys) {
			throw new InsightError("Extra keys exist in Order that should not exist");
		}
		if(!options.ORDER.dir) {
			throw new InsightError("Direction cannot be empty");
		} else if (!options.ORDER.keys) {
			throw new InsightError("Keys don't exist in ORDER");
		}
		let direction: Direction;
		if (options.ORDER.dir in Direction) {
			direction = options.ORDER.dir as Direction;
		} else {
			throw new InsightError("Invalid Direction");
		}
		let keys: AnyKey[] = options.ORDER.keys.map((value: string) => {
			if (columns.includes(value)) {
				let keyComponents: string[] = value.split("_");
				return keyComponents.length < 2 ? new ApplyKey(value) : parseAndValidateKey(value, datasetProperties);
			} else {
				throw new InsightError("ORDER key must exist in COLUMNS");
			}
		});
		orderKey = new OrderObject(direction, keys);
	}
	return orderKey;
}
