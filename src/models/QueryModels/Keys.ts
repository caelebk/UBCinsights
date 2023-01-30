import {KeyType, MField, SField} from "./Enums";
import {InsightError} from "../../controller/IInsightFacade";

export class Key {
	private readonly _sField: SField | undefined;
	private readonly _mField: MField | undefined;
	private readonly _keyType: KeyType;

	constructor(keyType: KeyType, sField?: SField, mField?: MField) {
		if (!sField && !mField) {
			throw new InsightError("No mField and no sField inputted for key");
		}

		if (keyType === KeyType.skey) {
			if (!sField) {
				throw new InsightError("SKey with no sField inputted");
			}
			this._sField = sField;
		}

		if (keyType === KeyType.mkey) {
			if (!mField) {
				throw new InsightError("MKey with no mField inputted");
			}
			this._mField = mField;
		}

		this._keyType = keyType;
	}

	public get field(): SField | MField | undefined {
		if (this._keyType === KeyType.mkey) {
			return this._mField;
		}
		return this._sField;
	}

	public get keyType(): KeyType {
		return this._keyType;
	}
}
