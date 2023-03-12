import Where from "../../models/QueryModels/Where";
import {Section} from "../../models/DatasetModels/Section";
import {Dataset} from "../../models/DatasetModels/Dataset";
import {Course} from "../../models/DatasetModels/Course";
import {
	Comparator,
	LogicComparator,
	MComparator,
	NegationComparator,
	SComparator
} from "../../models/QueryModels/Comparators";
import {Logic, MComparatorLogic, MField, SField} from "../../models/QueryModels/Enums";
import {InsightError} from "../../controller/IInsightFacade";

/**
 * Gets the Sections that match the comparators in the EBNF Query.
 * @param where
 * @param data
 */
export default function handleWhere(where: Where, data: Dataset): Section[] {
	let results: Section[] = [];
	data.courses.forEach((course: Course) => {
		results = results.concat(course.result.filter((section: Section) => {
			if (where.comparator) {
				return handleComparator(section, where.comparator);
			} else {
				return section;
			}
		}));
	});
	return results;
}

/**
 * Handles the recursion for comparators and sends the correct logic for the specific comparator.
 * @param section
 * @param comparator
 */
function handleComparator(section: Section, comparator: Comparator): boolean {
	if (comparator instanceof MComparator) {
		const mComparator: MComparator = comparator as MComparator;
		const mField: MField = mComparator.key.mField;
		const mValue: number = mComparator.value;
		const mLogic: MComparatorLogic = mComparator.logic;
		return handleMComparator(section, mField, mLogic, mValue);
	} else if (comparator instanceof SComparator) {
		const sComparator: SComparator = comparator as SComparator;
		const sField: SField = sComparator.key.sField;
		const sValue: string = sComparator.input;
		return handleSComparator(section, sField, sValue);
	} else if (comparator instanceof NegationComparator) {
		const negationComparator: NegationComparator = comparator as NegationComparator;
		const comparatorResult: boolean = handleComparator(section, negationComparator.filter);
		return !comparatorResult;
	} else {
		const logicComparator: LogicComparator = comparator as LogicComparator;
		const logic: Logic = logicComparator.logic;
		const logicComparators: Comparator[] = logicComparator.filters;
		switch (logic) {
			case Logic.AND:
				return logicComparators.flatMap((comp: Comparator) => {
					return handleComparator(section, comp);
				}).reduce((accumulator: boolean, current: boolean) => {
					return accumulator && current;
				});
			case Logic.OR:
				return logicComparators.flatMap((comp: Comparator) => {
					return handleComparator(section, comp);
				}).reduce((accumulator: boolean, current: boolean) => {
					return accumulator || current;
				});
			default:
				break;
		}
	}

	throw new InsightError("Invalid Comparator.");
}

/**
 * Handles the logic for MComparators
 * @param section
 * @param mField
 * @param logic
 * @param value
 */
function handleMComparator(section: Section, mField: MField, logic: MComparatorLogic, value: number): boolean {
	// has to go into at least one enum; therefore, do not need to handle outside of switch statement
	switch (logic) {
		case MComparatorLogic.EQ:
			switch(mField) {
				case MField.audit:
					return section.Audit === value;
				case MField.avg:
					return section.Avg === value;
				case MField.fail:
					return section.Fail === value;
				case MField.pass:
					return section.Pass === value;
				case MField.year:
					return section.Year === value;
			}
			break;
		case MComparatorLogic.GT:
			switch(mField) {
				case MField.audit:
					return section.Audit > value;
				case MField.avg:
					return section.Avg > value;
				case MField.fail:
					return section.Fail > value;
				case MField.pass:
					return section.Pass > value;
				case MField.year:
					return section.Year > value;
			}
			break;
		case MComparatorLogic.LT:
			switch(mField) {
				case MField.audit:
					return section.Audit < value;
				case MField.avg:
					return section.Avg < value;
				case MField.fail:
					return section.Fail < value;
				case MField.pass:
					return section.Pass < value;
				case MField.year:
					return section.Year < value;
			}
			break;
	}
	// TODO: handle all cases. This is a temp stub
	return false;
}

/**
 * Handles the logic for the SComparator
 * @param section
 * @param sField
 * @param input
 */
function handleSComparator(section: Section, sField: SField, input: string): boolean {
	// has to go into at least one enum; therefore, do not need to handle outside of switch statement
	switch (sField) {
		case SField.dept:
			return handleWildCard(section.Subject, input);
		case SField.id:
			return handleWildCard(section.Course, input);
		case SField.instructor:
			return handleWildCard(section.Professor, input);
		case SField.title:
			return handleWildCard(section.Title, input);
		case SField.uuid:
			return handleWildCard(section.id, input);
	}
	// TODO: handle all cases. This is a temp stub
	return false;
}

/**
 * Handles matching the strings for wildcards
 * @param value
 * @param input
 */
function handleWildCard(value: string, input: string): boolean {
	// double wildcard
	if (input.length > 1 && input[0] === "*" && input[input.length - 1] === "*") {
		return value.includes(input.substring(1, input.length - 1));
	} else if (input.length > 0) {
		// start wildcard
		if (input[0] === "*") {
			return input.substring(1, input.length) ===
				value.substring(value.length - (input.length - 1), value.length);
		// end wildcard
		} else if (input[input.length - 1] === "*") {
			return input.substring(0, input.length - 1) ===
				value.substring(0, input.length - 1);
		}
	}
	// no wildcards just compare value.
	return value === input;
}

