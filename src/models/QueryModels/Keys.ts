import {MField, SField} from "./Enums";

export type Key = SKey | MKey;
export type AnyKey = Key | ApplyKey;

export class ApplyKey {
	private readonly _id: string;

	constructor(id: string) {
		this._id = id;
	}

	public get id(): string {
		return this._id;
	}
}

export class SKey {
	private readonly _sField: SField;

	constructor(sField: SField) {
		this._sField = sField;
	}

	public get sField(): SField {
		return this._sField;
	}
}

export class MKey {
	private readonly _mField: MField;

	constructor(mField: MField) {
		this._mField = mField;
	}

	public get mField(): MField {
		return this._mField;
	}
}
