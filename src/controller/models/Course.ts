import {InsightError} from "../IInsightFacade";
import {Section} from "./Section";

export class Course {
	public title: string;
	public sections: Section[];
	// Course must contain at least one valid Section

	constructor(title: string, sections: Section[]) {
		if (sections.length === 0) {
			throw new InsightError("Course must have at least one valid Section");
		}
		this.title = title;
		this.sections = sections;
	}
}
