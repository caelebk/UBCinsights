import {MField, SField} from "./Enums";
export class Key {
	private readonly _field: SField | MField;

	constructor(field: SField | MField) {
		this._field = field;
	}

	public get field(): SField | MField {
		return this._field;
	}
}
