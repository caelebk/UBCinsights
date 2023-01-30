import {NegationComparator, LogicComparator, MComparator, SComparator} from "./Comparators";

export default class Where {
	private readonly _comparator: LogicComparator | MComparator | SComparator | NegationComparator | undefined;

	constructor(comparator?: LogicComparator | MComparator | SComparator | NegationComparator) {
		this._comparator = comparator ? comparator : undefined;
	}

	public get comparator(): LogicComparator | MComparator | SComparator | NegationComparator | undefined {
		return this._comparator;
	}
}
