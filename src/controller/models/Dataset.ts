import {InsightError} from "../IInsightFacade";
import {Course} from "./Course";

export class Dataset {
	public id: string;
	public courses: Course[];
	// Dataset must contain at least one valid Course

	constructor(id: string, courses: Course[]) {
		if (courses.length === 0) {
			throw new InsightError("Dataset must have at least one valid Course");
		}
		this.id = id;
		this.courses = courses;
	}
}
