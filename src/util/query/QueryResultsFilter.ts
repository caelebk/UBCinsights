import {InsightResult} from "../../controller/IInsightFacade";
import Options from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {Key, MKey} from "../../models/QueryModels/Keys";
import {MField, SField} from "../../models/QueryModels/Enums";

export default function filterResults(options: Options, sections: Section[], datasetId: string): InsightResult[] {
	if (options.order) {
		sections = sortResults(options.order, sections);
	}
	const columnKeys: Key[] = options.columns;
	return sections.map((section: Section) => {
		let insightResult: InsightResult = {};
		columnKeys.forEach((key: Key) => {
			filterKeys(key, section, insightResult, datasetId);
		});
		return insightResult;
	});
}

function filterKeys(key: Key, section: Section, insightResult: InsightResult, datasetId: string): void {
	if (key instanceof MKey) {
		switch (key.mField) {
			case MField.year:
				insightResult[datasetId.concat("_", MField.year)] = Number(section.Year);
				break;
			case MField.pass:
				insightResult[datasetId.concat("_", MField.pass)] = section.Pass;
				break;
			case MField.fail:
				insightResult[datasetId.concat("_", MField.fail)] = section.Fail;
				break;
			case MField.avg:
				insightResult[datasetId.concat("_", MField.avg)] = section.Avg;
				break;
			case MField.audit:
				insightResult[datasetId.concat("_", MField.audit)] = section.Audit;
				break;
		}
	} else {
		switch (key.sField) {
			case SField.uuid:
				insightResult[datasetId.concat("_", SField.uuid)] = String(section.id);
				break;
			case SField.title:
				insightResult[datasetId.concat("_", SField.title)] = section.Title;
				break;
			case SField.instructor:
				insightResult[datasetId.concat("_", SField.instructor)] = section.Professor;
				break;
			case SField.id:
				insightResult[datasetId.concat("_", SField.id)] = section.Course;
				break;
			case SField.dept:
				insightResult[datasetId.concat("_", SField.dept)] = section.Subject;
				break;
		}
	}
}

function sortResults(key: Key, sections: Section[]): Section[] {
	return sections.sort((section1: Section, section2: Section) => {
		if (key instanceof MKey) {
			switch (key.mField) {
				case MField.year:
					return section1.Year > section2.Year ? 1 : -1;
				case MField.pass:
					return section1.Pass > section2.Pass ? 1 : -1;
				case MField.fail:
					return section1.Fail > section2.Fail ? 1 : -1;
				case MField.avg:
					return section1.Avg > section2.Avg ? 1 : -1;
				case MField.audit:
					return section1.Audit > section2.Audit ? 1 : -1;
			}
		} else {
			switch (key.sField) {
				case SField.uuid:
					return section1.id.localeCompare(section2.id);
				case SField.title:
					return section1.Title.localeCompare(section2.Title);
				case SField.instructor:
					return section1.Professor.localeCompare(section2.Professor);
				case SField.id:
					return section1.Course.localeCompare(section2.Course);
				case SField.dept:
					return section1.Subject.localeCompare(section2.Subject);
			}
		}
	});
}
