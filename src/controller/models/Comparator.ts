import {Key} from "./Keys";
import {Logic, MComparatorLogic} from "./Enums";

export class Comparator {
	private readonly _key: Key | undefined;
	private readonly _value: string | undefined;

	constructor(key?: Key, value?: string) {
		if (key) {
			this._key = key;
		}
		if (value) {
			this._value = value;
		}
	}

	public get key(): Key | undefined {
		return this._key;
	}

	public get value(): string | undefined {
		return this._value;
	}
}

export class MComparator extends Comparator {
	private readonly _logic: MComparatorLogic;

	constructor(key: Key, value: string, logic: MComparatorLogic) {
		super(key, value);
		this._logic = logic;
	}

	public get logic(): MComparatorLogic {
		return this._logic;
	}
}

export class SComparator extends Comparator {
	constructor(key: Key, input: string) {
		super(key, input);
	}
}

export class LogicComparator extends Comparator {
	private readonly _logic: Logic;
	private readonly _filters: Comparator[];

	constructor(logic: Logic, filters: Comparator[]) {
		super();
		this._logic = logic;
		this._filters = filters;
	}

	public get logic(): Logic {
		return this._logic;
	}

	public get filters(): Comparator[] {
		return this._filters;
	}
}

export class NegationComparator extends Comparator {
	private readonly _filter: Comparator;

	constructor(filter: Comparator) {
		super();
		this._filter = filter;
	}

	public get filter(): Comparator {
		return this._filter;
	}
}
