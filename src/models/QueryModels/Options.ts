import {Key} from "./Keys";

export default class Options {
	private readonly _columns: Key[];
	private readonly _order: Key | undefined;

	constructor(columnsQuery: Key[], orderQuery?: Key) {
		this._columns = columnsQuery;
		if (orderQuery) {
			this._order = orderQuery;
		}
	}

	public get columns(): Key[] {
		return this._columns;
	}

	public get order(): Key | undefined {
		return this._order;
	}
}
