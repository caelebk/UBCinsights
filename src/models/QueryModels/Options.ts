import {AnyKey} from "./Keys";
import {Direction} from "./Enums";

export default class Options {
	private readonly _columns: AnyKey[];
	private readonly _sort: Sort | undefined;

	constructor(columnsQuery: AnyKey[], sort?: Sort) {
		this._columns = columnsQuery;
		if (sort) {
			this._sort = sort;
		}
	}

	public get columns(): AnyKey[] {
		return this._columns;
	}

	public get sort(): Sort | undefined {
		return this._sort;
	}
}

export class Order {
	private readonly _direction: Direction;
	private readonly _keys: AnyKey[];

	constructor(direction: Direction, keys: AnyKey[]) {
		this._direction = direction;
		this._keys = keys;
	}

	public get direction(): Direction {
		return this._direction;
	}

	public get keys(): AnyKey[] {
		return this._keys;
	}
}

export class Sort {
	private readonly _order: AnyKey | Order;

	constructor(order: AnyKey | Order) {
		this._order = order;
	}

	public get order(): AnyKey | Order {
		return this._order;
	}
}
