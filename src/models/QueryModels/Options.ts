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

interface Order {
	dir: Direction,
	keys: AnyKey[]
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
