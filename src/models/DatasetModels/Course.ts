import {Section} from "./Section";

export class Course {
	public result: Section[];
	public rank: number;

	constructor(data: {result: any[], rank: number}) {
		this.result = data.result.map((sectionData) => new Section(sectionData));
		this.rank = data.rank;
	}

	/**
	 * Returns true if Course is valid, otherwise false
	 * A Course is valid if it has one or more valid sections
	 * */
	public isValid(): boolean {
		if (this.result.length === 0) {
			return false;
		}
		for (const section of this.result) {
			if (section.isValid()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns a list of valid sections within the course.
	 */
	public getValidSections(): Section[] {
		let validSections: Section[] = [];
		for (let section of this.result) {
			if (section.isValid()) {
				validSections.push(section);
			}
		}
		return validSections;
	}


}
