import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError
} from "./IInsightFacade";
import JSZip from "jszip";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	constructor() {
		console.log("InsightFacadeImpl::init()");
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (id.includes("_")) {
			return Promise.reject(new InsightError("id contains an underscore"));
		}
		if (id === "") {
			return Promise.reject(new InsightError("id is empty"));
		}
		if (new RegExp("^\\s*$").test(id)) {
			return Promise.reject(new InsightError("id is only whitespace characters"));
		}
		return new Promise((resolve, reject) => {
			JSZip.loadAsync(content, {base64: true})
				.then((zip) => {
					let promises: Array<Promise<string>> = [];
					zip.folder("courses")?.forEach((relativePath, file) => {
						promises.push(file.async("string"));
					});
					return promises;
				})
				.then((promises) => { // is this really needed here or can it be combined above
					Promise.all(promises)
						.then((values) => {
							// conversion to list of JSON objects
							let asdfasdf = values.map((x) => JSON.parse(x));
						});
				})
				.catch((error) => {
					reject(new InsightError(error));
				});
		});
	}

	public removeDataset(id: string): Promise<string> {
		return Promise.reject("Not implemented.");
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		return Promise.reject("Not implemented.");
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return Promise.reject("Not implemented.");
	}
}
