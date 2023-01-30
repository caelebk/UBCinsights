import {Comparator} from "./Comparator";

export default class Where {
	private readonly _comparator: Comparator | undefined;

	constructor(comparator?: Comparator) {
		if (comparator) {
			this._comparator = comparator;
		}
	}

	public get comparator(): Comparator | undefined {
		return this._comparator;
	}
}
