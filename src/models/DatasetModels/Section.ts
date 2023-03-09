import {MField, SField} from "../QueryModels/Enums";
import {InsightError} from "../../controller/IInsightFacade";

export class Section {
	// variables are capitalized, so it can be written and read the same
	public id: string;
	public Course: string;
	public Title: string;
	public Professor: string;
	public Subject: string;
	public Year: number;
	public Avg: number;
	public Pass: number;
	public Fail: number;
	public Audit: number;

	constructor(json: {id: string, Course: string, Title: string, Professor: string, Subject: string,
		Year: number, Avg: number, Pass: number, Fail: number, Audit: number, Section: string}) {
		this.id = json.id;
		this.Course = json.Course;
		this.Title = json.Title;
		this.Professor = json.Professor;
		this.Subject = json.Subject;
		this.Year = json.Year;
		this.Avg = json.Avg;
		this.Pass = json.Pass;
		this.Fail = json.Fail;
		this.Audit = json.Audit;
		if (json.Section === "overall") {
			this.Year = 1900;
		}
	}

	/**
	 * Returns false if section has an undefined attribute field, otherwise true
	 * */
	public isValid(): boolean {
		for (const attribute in this) {
			if (this[attribute] === undefined) {
				return false;
			}
		}
		return true;
	}

	public getMFieldValue(mField: MField): number {
		switch (mField) {
			case MField.audit:
				return Number(this.Audit);
			case MField.avg:
				return Number(this.Avg);
			case MField.fail:
				return Number(this.Fail);
			case MField.pass:
				return Number(this.Pass);
			case MField.year:
				return Number(this.Year);
		}
		throw new InsightError("Section received a room MKey instead of a section MKey");
	}

	public getSFieldValue(sField: SField): string {
		switch (sField) {
			case SField.uuid:
				return String(this.id);
			case SField.instructor:
				return String(this.Professor);
			case SField.dept:
				return String(this.Subject);
			case SField.title:
				return String(this.Title);
			case SField.id:
				return String(this.Course);
		}
		throw new InsightError("Section received a room SKey instead of a section SKey");
	}
}
