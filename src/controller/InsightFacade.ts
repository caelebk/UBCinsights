import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError
} from "./IInsightFacade";
import JSZip from "jszip";
import {FileData} from "../models/DatasetModels/FileData";
import {Dataset} from "../models/DatasetModels/Dataset";

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
		if (!this.isValidId(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			return new Promise((resolve, reject) => {
				// read zip file
				JSZip.loadAsync(content, {base64: true})
					.then((zip) => { // get file names and data promises
						let fileDataPromises: Array<Promise<string>> = [];
						// open course folder
						zip.folder("courses")?.forEach((relativePath, file) => {
							// read all files and push into a list
							fileDataPromises.push(file.async("string"));
						});
						return Promise.all(fileDataPromises);
					})
					.then((result) => {
						for (let file of result) {
							//
						}
						let fileDataList: FileData[] = result.map((file) => new FileData(JSON.parse(file)));
						let something = fileDataList[0].result[1].avg;
						//
						// return Promise.all(res)
						// 	.then((values) => {
						// 		// conversion to list of JSON objects of file data
						// 		let fileDataList: FileData[] = values.map((x) => JSON.parse(x) as FileData);
						// 		fileDataList.forEach((fileData, index) => {
						// 		});
						// 	});
					})
					.catch((error) => {
						reject(new InsightError(error));
					});
			});
		}
	}

	private isValidId(id: string): boolean {
		// checks for underscore, empty, and it only has spaces
		return !(id.includes("_") || id === "" || new RegExp("^\\s*$").test(id));
	}

	private hasSectionAttributes(json: object): boolean {
		let attributes: string[] = ["id", "Course", "Title", "Professor", "Subject",
			"Year", "Avg", "Pass", "Fail", "Audit"];
		for (let a of attributes) {
			if (!(a in json)) {
				return false;
			}
		}
		return true;
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
