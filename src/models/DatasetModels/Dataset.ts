import {InsightDatasetKind, InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";
import {Course} from "./Course";

export class Dataset {
	public id: string;
	public kind: InsightDatasetKind;
	public courses: Course[];
	// Dataset must contain at least one valid Section

	/**
	 * if json is present, will prioritize that data first.
	 *
	 * @param id
	 * @param courses
	 * @param json
	 */
	constructor(id: string, kind: InsightDatasetKind, courses: Course[],
		json?: {id: string, kind: string, courses: any[]}) {
		this.id = id;
		this.kind = kind;
		this.courses = courses;
		if (json) {
			this.id = json.id;
			this.kind = json.kind as InsightDatasetKind;
			this.courses = (json.courses.map((course) => {
				return new Course("", course);
			}));
		}
	}

	/**
	 * Returns true if Dataset contains at least one valid Course, otherwise false
	 */
	public isValid(): boolean {
		if (!(this.courses.length === 0)) {
			for (const course of this.courses) {
				if (course.isValid()) {
					return true;
				}
			}
		}
		return false;
	}
}
