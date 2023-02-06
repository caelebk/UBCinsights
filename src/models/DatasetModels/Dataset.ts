import {InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";
import {Course} from "./Course";

export class Dataset {
	public courses: Course[];
	// Dataset must contain at least one valid Section

	constructor(courses: Course[]) {
		if (courses.length === 0) {
			throw new InsightError("Dataset must have at least one valid Course");
		}
		this.courses = courses;
	}
}
