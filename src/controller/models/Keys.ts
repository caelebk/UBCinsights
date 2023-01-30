import {KeyType, MField, SField} from "./Enums";
export class Key {
	private readonly _field: SField | MField;
	private readonly _keyType: KeyType;

	constructor(field: SField | MField, keyType: KeyType) {
		this._field = field;
		this._keyType = keyType;
	}

	public get field(): SField | MField {
		return this._field;
	}

	public get keyType(): KeyType {
		return this._keyType;
	}
}
