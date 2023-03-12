import {DatasetProperties, ValidTransformations} from "../QueryInterfaces";
import Transformations, {ApplyRule} from "../../../models/QueryModels/Transformations";
import {InsightDatasetKind, InsightError} from "../../../controller/IInsightFacade";
import {ApplyKey, Key} from "../../../models/QueryModels/Keys";
import {ApplyToken, MFieldRoom, SFieldRoom} from "../../../models/QueryModels/Enums";
import {
	parseAndValidateKey,
	splitAndValidateKeyComponent,
	validateDatasetID,
	validateNumberKeys
} from "./QueryValidator";

export default function parseAndValidateTransformations(transformations: ValidTransformations,
	datasetProp: DatasetProperties): Transformations {
	if (!transformations.GROUP || !transformations.APPLY) {
		throw new InsightError("Transformations is missing Apply or Group");
	}
	const numKeys = 2;
	if (Object.keys(transformations).length > numKeys) {
		throw new InsightError("Transformations has extra keys that shouldn't exist");
	}
	if (transformations.GROUP.length === 0) {
		throw new InsightError("Group cannot be empty");
	}
	setDataKind(transformations.GROUP[0], datasetProp);
	let group: Key[] = transformations.GROUP.map((value: string) => {
		return parseAndValidateKey(value, datasetProp);
	});
	let apply: ApplyRule[] = transformations.APPLY.map((value: object) => {
		const applyKey: string[] = validateNumberKeys(Object.keys(value), 1) as string[];
		const conversion: object[] = validateNumberKeys(Object.values(value), 1) as object[];
		checkApplyKeys(applyKey[0], datasetProp.applyKeys);
		const conversionTokens: string[] = validateNumberKeys(Object.keys(conversion[0]), 1) as string[];
		const conversionKeys: string[] = validateNumberKeys(Object.values(conversion[0]), 1) as string[];
		let applyToken: ApplyToken;
		if (conversionTokens[0] in ApplyToken) {
			applyToken = conversionTokens[0] as ApplyToken;
		} else {
			throw new InsightError("Invalid ApplyToken");
		}
		let key: Key = parseAndValidateKey(conversionKeys[0], datasetProp);
		return new ApplyRule(new ApplyKey(applyKey[0]), applyToken, key);
	});
	return new Transformations(group, apply);
}

function setDataKind(value: string, datasetProp: DatasetProperties): void {
	const keyComponents: string[] = splitAndValidateKeyComponent(value);
	validateDatasetID(keyComponents[0], datasetProp);
	if (keyComponents[1] in MFieldRoom || keyComponents[1] in SFieldRoom) {
		datasetProp.dataKind = InsightDatasetKind.Rooms;
	}
}

function checkApplyKeys(applyKey: string, existingApplyKeys: Set<string>): void {
	if (applyKey.length === 0) {
		throw new InsightError("Applykey cannot be empty");
	} else if (existingApplyKeys.has(applyKey)) {
		throw new InsightError("Applykey must be unique");
	} else if (applyKey.indexOf("_") !== -1) {
		throw new InsightError("Applykey cannot contain underscore.");
	} else {
		existingApplyKeys.add(applyKey);
	}
}
