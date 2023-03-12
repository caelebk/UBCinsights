import {DatasetProperties, ValidTransformations} from "./QueryInterfaces";
import Transformations, {ApplyRule} from "../../models/QueryModels/Transformations";
import {InsightError} from "../../controller/IInsightFacade";
import {ApplyKey, Key} from "../../models/QueryModels/Keys";
import {ApplyToken} from "../../models/QueryModels/Enums";
import {parseAndValidateKey} from "./QueryValidator";

export default function parseAndValidateTransformations(transformations: ValidTransformations,
										 datasetProperties: DatasetProperties): Transformations {
	if (!transformations.GROUP || !transformations.APPLY) {
		throw new InsightError("Transformations is missing Apply or Group");
	}
	const numKeys = 2;
	if (Object.keys(transformations).length > numKeys) {
		throw new InsightError("Transformations has extra keys that shouldn't exist");
	}
	let group: Key[] = transformations.GROUP.map((value: string) => {
		return parseAndValidateKey(value, datasetProperties);
	});
	let apply: ApplyRule[] = transformations.APPLY.map((value: object) => {
		let applyKey: string[] = Object.keys(value);
		let conversion: object[] = Object.values(value);
		if (applyKey.length !== 1 || conversion.length !== 1) {
			throw new InsightError("Given multiple/no applyKeys or multiple/no conversions for ApplyRule");
		}
		checkApplyKeys(applyKey[0], datasetProperties.applyKeys);
		let conversionTokens: string[] = Object.keys(conversion[0]);
		let conversionKeys: string[] = Object.values(conversion[0]);
		if (conversionTokens.length !== 1 || conversionKeys.length !== 1) {
			throw new InsightError("Given multiple/no Tokens or multiple/no Keys for ApplyRule");
		}
		let applyToken: ApplyToken;
		if (conversionTokens[0] in ApplyToken) {
			applyToken = conversionTokens[0] as ApplyToken;
		} else {
			throw new InsightError("Invalid ApplyToken");
		}
		let key: Key = parseAndValidateKey(conversionKeys[0], datasetProperties);
		return new ApplyRule(new ApplyKey(applyKey[0]), applyToken, key);
	});
	return new Transformations(group, apply);
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
