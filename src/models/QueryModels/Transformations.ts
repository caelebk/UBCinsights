import {Key} from "./Keys";
import {ApplyToken} from "./Enums";

export default class Transformations {
	private readonly _group: Key[];
	private readonly _applyRules: ApplyRule[];

	constructor(group: Key[], apply: ApplyRule[]) {
		this._group = group;
		this._applyRules = apply;
	}

	public get group(): Key[] {
		return this._group;
	}

	public get applyRules(): ApplyRule[] {
		return this._applyRules;
	}
}

export class ApplyRule {
	private readonly _id: string;
	private readonly _applyToken: ApplyToken;
	private readonly _key: Key;

	constructor (id: string, applyToken: ApplyToken, key: Key) {
		this._id = id;
		this._applyToken = applyToken;
		this._key = key;
	}
}
