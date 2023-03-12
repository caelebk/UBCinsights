import {MFieldSection, SFieldSection} from "../QueryModels/Enums";
import {DataModel} from "./DataModel";

export class Section implements DataModel {
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

	public getMFieldValue(mField: MFieldSection): number {
		switch (mField) {
			case MFieldSection.audit:
				return Number(this.Audit);
			case MFieldSection.avg:
				return Number(this.Avg);
			case MFieldSection.fail:
				return Number(this.Fail);
			case MFieldSection.pass:
				return Number(this.Pass);
			case MFieldSection.year:
				return Number(this.Year);
		}
	}

	public getSFieldValue(sField: SFieldSection): string {
		switch (sField) {
			case SFieldSection.uuid:
				return String(this.id);
			case SFieldSection.instructor:
				return String(this.Professor);
			case SFieldSection.dept:
				return String(this.Subject);
			case SFieldSection.title:
				return String(this.Title);
			case SFieldSection.id:
				return String(this.Course);
		}
	}
}
