import {InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";
import {Course} from "./Course";

export class Dataset {
	public id: string;
	public courses: Course[];
	// Dataset must contain at least one valid Section

	/**
	 * if json is present, will prioritize that data first.
	 * @param id
	 * @param courses
	 * @param json
	 */
	constructor(id: string, courses: Course[], json?: {id: string, courses: any[]}) {
		this.id = id;
		this.courses = courses;
		if (json) {
			this.id = json.id;
			this.courses = (json.courses.map((course) => {
				return new Course("", course);
			}));
		}
	}
}
