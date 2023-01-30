import Where from "./Where";
import Options from "./Options";

export default class Query {
	private readonly _body: Where;
	private readonly _options: Options;

	constructor(body: Where, options: Options) {
		this._body = body;
		this._options = options;
	}

	public get body(): Where {
		return this._body;
	}

	public get options(): Options {
		return this._options;
	}
}
