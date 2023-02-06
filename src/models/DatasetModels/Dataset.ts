import {InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";
import {Course} from "./Course";

export class Dataset {
	public courses: Course[];
	// Dataset must contain at least one valid Section

	constructor(courses: Course[], json?: {courses: any[]}) {
		this.courses = courses;
		if (json) {
			this.courses.push(...json.courses);
		}
	}
}
