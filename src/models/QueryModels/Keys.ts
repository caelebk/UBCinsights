import {MField, SField} from "./Enums";

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
