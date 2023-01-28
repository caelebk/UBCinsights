import {MField, SField} from "./Enums";

export class SKey {
	private readonly _sField: SField;
	private readonly _id: string;

	constructor(id: string, sField: SField) {
		this._sField = sField;
		this._id = id;
	}

	public get sField(): SField {
		return this._sField;
	}
}

export class MKey {
	private readonly _mField: MField;
	private readonly _id: string;

	constructor(id: string, mField: MField) {
		this._mField = mField;
		this._id = id;
	}

	public get mField(): MField {
		return this._mField;
	}
}
