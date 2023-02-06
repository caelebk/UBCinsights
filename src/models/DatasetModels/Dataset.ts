import {InsightError} from "../../controller/IInsightFacade";
import {Section} from "./Section";

export class Dataset {
	public sections: Section[];
	// Dataset must contain at least one valid Course

	constructor(sections: Section[]) {
		if (sections.length === 0) {
			throw new InsightError("Dataset must have at least one valid Course");
		}
		this.sections = sections;
	}
}
