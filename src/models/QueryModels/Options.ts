import {MKey, SKey} from "./Keys";

export default class Options {
	private readonly _columns: Array<MKey | SKey>;
	private readonly _order: MKey | SKey;

	constructor(columnsQuery: Array<MKey | SKey>, orderQuery: MKey | SKey) {
		this._columns = columnsQuery;
		this._order = orderQuery;
	}

	public get columns(): Array<MKey | SKey> {
		return this._columns;
	}

	public get order(): MKey | SKey {
		return this._order;
	}
}
