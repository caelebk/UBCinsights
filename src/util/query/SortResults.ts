import {Order} from "../../models/QueryModels/Options";
import {Section} from "../../models/DatasetModels/Section";
import {MKey, SKey} from "../../models/QueryModels/Keys";

export default function sortResults(key: Order, sections: Section[]): Section[] {
	return sections.sort((section1: Section, section2: Section) => {
		return sortingPrecedence(key, section1, section2);
	});
}

function sortingPrecedence(key: Order, section1: Section, section2: Section): number {
	if (key instanceof MKey) {
		return section1.getMFieldValue(key.mField) > section2.getMFieldValue(key.mField) ? 1 : -1;
	} else if (key instanceof SKey) {
		return section1.getSFieldValue(key.sField).localeCompare(section2.getSFieldValue(key.sField));
	}
	// TODO: handle ApplyKey
	return 0;
}
