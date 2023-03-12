import Where from "./Where";
import Options from "./Options";
import Transformations from "./Transformations";

export default class Query {
	private readonly _body: Where;
	private readonly _options: Options;
	private readonly _transformations: Transformations | undefined;
	private readonly _id: string;

	constructor(body: Where, options: Options, id: string, transformations?: Transformations) {
		this._body = body;
		this._options = options;
		this._id = id;
		if (transformations) {
			this._transformations = transformations;
		}
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

	public get transformations(): Transformations | undefined {
		return this._transformations;
	}
}
