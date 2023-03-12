import {InsightDatasetKind} from "../../controller/IInsightFacade";
import {Course} from "./Course";
import {Room} from "./Room";

export class Dataset {
	public id: string;
	public kind: InsightDatasetKind;
	public courses: Course[];
	// Dataset must contain at least one valid Section
	public rooms: Room[];

	/**
	 * if json is present, will prioritize that data first.
	 *
	 * @param id
	 * @param kind
	 * @param courses
	 * @param json
	 */
	constructor(id: string, kind: InsightDatasetKind, courses: Course[], rooms: Room[],
		json?: {id: string, kind: string, courses: any[], rooms: any[]}) {
		this.id = id;
		this.kind = kind;
		this.courses = courses;
		this.rooms = rooms;
		if (json) {
			this.id = json.id;
			this.kind = json.kind as InsightDatasetKind;
			this.courses = (json.courses.map((course) => {
				return new Course("", course);
			}));
			this.rooms = (json.rooms.map((room) => {
				return new Room(room);
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
