import {InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";

export class Dataset {
	public id: string;
	public sections: Section[];
	// Dataset must contain at least one valid Course

	constructor(id: string, sections: Section[]) {
		if (sections.length === 0) {
			throw new InsightError("Dataset must have at least one valid Course");
		}
		this.id = id;
		this.sections = sections;
	}
}
