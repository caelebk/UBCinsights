import {Comparator} from "./Comparators";

export default class Where {
	private readonly _comparator: Comparator | undefined;

	constructor(comparator?: Comparator) {
		this._comparator = comparator ? comparator : undefined;
	}

	public get comparator(): Comparator | undefined {
		return this._comparator;
	}
}
