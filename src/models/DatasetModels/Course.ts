import {Section} from "./Section";

export class Course {
	public title: string;
	public result: Section[];
	// public rank: number;

	constructor(title: string, json: {result: any[], title: string}) {
		if (json.title === undefined) {
			this.title = title;
		} else {
			this.title = json.title;
		}
		this.result = json.result.map((sectionData) => new Section(sectionData));
	}

	/**
	 * Returns true if Course is valid, otherwise false
	 * A Course is valid if it has one or more valid sections
	 * */
	public isValid(): boolean {
		if (!(this.result.length === 0)) {
			for (const section of this.result) {
				if (section.isValid()) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Filters out Sections that are not valid
	 */
	public filterSections() {
		this.result = this.result.filter((section) => section.isValid());
	}
}
