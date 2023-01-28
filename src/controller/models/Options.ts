export default class Options {
	private readonly _columns: string[];
	private readonly _order: string;

	constructor(columnsQuery: string[], orderQuery: string) {
		this._columns = columnsQuery;
		this._order = orderQuery;
	}

	public get columns(): string[] {
		return this._columns;
	}

	public get order(): string {
		return this._order;
	}
}
