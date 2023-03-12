import {MFieldSection, SFieldSection} from "./Enums";

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
	private readonly _sField: SFieldSection;

	constructor(sField: SFieldSection) {
		this._sField = sField;
	}

	public get sField(): SFieldSection {
		return this._sField;
	}
}

export class MKey {
	private readonly _mField: MFieldSection;

	constructor(mField: MFieldSection) {
		this._mField = mField;
	}

	public get mField(): MFieldSection {
		return this._mField;
	}
}
