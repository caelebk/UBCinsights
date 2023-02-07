import Where from "./Where";
import Options from "./Options";

export default class Query {
	private readonly _body: Where;
	private readonly _options: Options;
	private readonly _id: string;

	constructor(body: Where, options: Options, id: string) {
		this._body = body;
		this._options = options;
		this._id = id;
	}

	public get body(): Where {
		return this._body;
	}

	public get options(): Options {
		return this._options;
	}

	public get id(): string {
		return this._id;
	}
}
