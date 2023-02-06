import {SKey, MKey} from "./Keys";
import {Logic, MComparatorLogic} from "./Enums";

export type Comparator = MComparator | SComparator | LogicComparator | NegationComparator;

export class MComparator {
	private readonly _logic: MComparatorLogic;
	private readonly _key: MKey;
	private readonly _value: number;

	constructor(key: MKey, value: number, logic: MComparatorLogic) {
		this._key = key;
		this._value = value;
		this._logic = logic;
	}

	public get key(): MKey {
		return this._key;
	}

	public get value(): number {
		return this._value;
	}

	public get logic(): MComparatorLogic {
		return this._logic;
	}
}

export class SComparator {
	private readonly _key: SKey;
	private readonly _input: string;

	constructor(key: SKey, input: string) {
		this._key = key;
		this._input = input;
	}

	public get key(): SKey {
		return this._key;
	}

	public get input(): string {
		return this._input;
	}
}

export class LogicComparator {
	private readonly _logic: Logic;
	private readonly _filters: Comparator[];

	constructor(logic: Logic, filters: Comparator[]) {
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

export class NegationComparator {
	private readonly _filter: Comparator;

	constructor(filter: Comparator) {
		this._filter = filter;
	}

	public get filter(): Comparator {
		return this._filter;
	}
}
