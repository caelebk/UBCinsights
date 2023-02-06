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
import {Course} from "../models/DatasetModels/Course";
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
					.then((zip) => {
						// use separate arrays here because want to use promise.all to sync up
						let fileNames: string[] = [];
						let fileDataPromises: Array<Promise<string>> = [];
						// open course folder
						zip.folder("courses")?.forEach((relativePath, file) => {
							// read all files and push into a list
							fileNames.push(relativePath);
							fileDataPromises.push(file.async("string"));
						});
						let result: [string[], Array<Promise<string>>] = [fileNames, fileDataPromises];
						return result;
					})
					.then((result) => {
						let [fileNames, fileDataPromises] = result;
						return Promise.all(fileDataPromises)
							.then((values) => {
								// conversion to list of JSON objects of file data
								let fileDataList: FileData[] = values.map((x) => JSON.parse(x) as FileData);
								let courses: Course[] = [];
								fileDataList.forEach((fileData, index) => {
									let course = new Course(fileNames[index], fileData.result);
									courses.push(course);
								});
								let dataset = new Dataset(id, courses);
							});
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
