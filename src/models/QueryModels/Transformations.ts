import {ApplyKey, Key} from "./Keys";
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
	private readonly _id: ApplyKey;
	private readonly _applyToken: ApplyToken;
	private readonly _key: Key;

	constructor (id: ApplyKey, applyToken: ApplyToken, key: Key) {
		this._id = id;
		this._applyToken = applyToken;
		this._key = key;
	}

	public get id(): string {
		return this._id.id;
	}

	public get applyToken(): ApplyToken {
		return this._applyToken;
	}

	public get key(): Key {
		return this._key;
	}
}
