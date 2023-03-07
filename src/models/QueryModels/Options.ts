import {AnyKey} from "./Keys";
import {Direction} from "./Enums";

export type Order = OrderObject | AnyKey | undefined;
export default class Options {
	private readonly _columns: AnyKey[];
	private readonly _order: Order;

	constructor(columnsQuery: AnyKey[], order?: Order) {
		this._columns = columnsQuery;
		if (order) {
			this._order = order;
		}
	}

	public get columns(): AnyKey[] {
		return this._columns;
	}

	public get order(): Order {
		return this._order;
	}
}

export class OrderObject {
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
