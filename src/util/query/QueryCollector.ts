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
import {InsightDatasetKind, InsightError} from "../../controller/IInsightFacade";
import {DataModel} from "../../models/DatasetModels/DataModel";
import {DatasetProperties} from "./QueryInterfaces";
import {Room} from "../../models/DatasetModels/Room";

export default function handleWhere(where: Where, data: Dataset, datasetProp: DatasetProperties): DataModel[] {
	let results: DataModel[] = [];
	if (datasetProp.dataKind === InsightDatasetKind.Rooms) {
		results = data.rooms.filter((room: Room) => {
			if (where.comparator) {
				return handleComparator(room, where.comparator);
			} else {
				return room;
			}
		});
	} else {
		data.courses.forEach((course: Course) => {
			results = results.concat(course.result.filter((section: Section) => {
				if (where.comparator) {
					return handleComparator(section, where.comparator);
				} else {
					return section;
				}
			}));
		});
	}
	return results;
}

function handleComparator(insightData: DataModel, comparator: Comparator): boolean {
	if (comparator instanceof MComparator) {
		const mComparator: MComparator = comparator as MComparator;
		const mField: MField = mComparator.key.mField;
		const mValue: number = mComparator.value;
		const mLogic: MComparatorLogic = mComparator.logic;
		return handleMComparator(insightData, mField, mLogic, mValue);
	} else if (comparator instanceof SComparator) {
		const sComparator: SComparator = comparator as SComparator;
		const sField: SField = sComparator.key.sField;
		const sValue: string = sComparator.input;
		return handleSComparator(insightData, sField, sValue);
	} else if (comparator instanceof NegationComparator) {
		const negationComparator: NegationComparator = comparator as NegationComparator;
		const comparatorResult: boolean = handleComparator(insightData, negationComparator.filter);
		return !comparatorResult;
	} else {
		const logicComparator: LogicComparator = comparator as LogicComparator;
		const logic: Logic = logicComparator.logic;
		const logicComparators: Comparator[] = logicComparator.filters;
		switch (logic) {
			case Logic.AND:
				return logicComparators.flatMap((comp: Comparator) => {
					return handleComparator(insightData, comp);
				}).reduce((accumulator: boolean, current: boolean) => {
					return accumulator && current;
				});
			case Logic.OR:
				return logicComparators.flatMap((comp: Comparator) => {
					return handleComparator(insightData, comp);
				}).reduce((accumulator: boolean, current: boolean) => {
					return accumulator || current;
				});
			default:
				break;
		}
	}

	throw new InsightError("Invalid Comparator.");
}

function handleMComparator(insightData: DataModel, mField: MField, logic: MComparatorLogic, value: number): boolean {
	switch (logic) {
		case MComparatorLogic.EQ:
			return insightData.getMFieldValue(mField) === value;
		case MComparatorLogic.GT:
			return insightData.getMFieldValue(mField) > value;
		case MComparatorLogic.LT:
			return insightData.getMFieldValue(mField) < value;
	}
}

function handleSComparator(insightData: DataModel, sField: SField, input: string): boolean {
	return handleWildCard(insightData.getSFieldValue(sField), input);
}

function handleWildCard(value: string, input: string): boolean {
	if (input.length > 1 && input[0] === "*" && input[input.length - 1] === "*") {
		return value.includes(input.substring(1, input.length - 1));
	} else if (input.length > 0) {
		if (input[0] === "*") {
			return input.substring(1, input.length) ===
				value.substring(value.length - (input.length - 1), value.length);
		} else if (input[input.length - 1] === "*") {
			return input.substring(0, input.length - 1) ===
				value.substring(0, input.length - 1);
		}
	}
	return value === input;
}

