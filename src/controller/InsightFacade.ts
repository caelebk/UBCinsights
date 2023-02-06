import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError
} from "./IInsightFacade";
import JSZip from "jszip";
import {Course} from "../models/DatasetModels/Course";
import {Dataset} from "../models/DatasetModels/Dataset";
import {Section} from "../models/DatasetModels/Section";
import {Data} from "../models/DatasetModels/Data";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private data: Data;
	constructor() {
		console.log("InsightFacadeImpl::init()");
		this.data = new Data();
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (!this.isValidId(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			return new Promise((resolve, reject) => {
				// read zip file
				JSZip.loadAsync(content, {base64: true})
					.then((zip) => { // get file data promises
						let fileDataPromises: Array<Promise<string>> = [];
						// open course folder
						zip.folder("courses")?.forEach((relativePath, file) => {
							// read all files and push into a list
							fileDataPromises.push(file.async("string"));
						});
						return Promise.all(fileDataPromises);
					})
					.then((result) => {
						// convert file data into class object
						let courses: Course[] = result.map((file) => new Course(JSON.parse(file)));
						let validSections: Section[] = [];
						for (const course of courses) {
							if (course.isValid()) {
								let sections = course.getValidSections();
								validSections.push(...sections);
							}
						}
						let dataset = new Dataset(validSections);
						this.data.addDataset("id", dataset);
					})
					.catch((error) => {
						reject(new InsightError(error));
					});
			});
		}
	}

	private isValidId(id: string): boolean {
		// checks for underscore, empty, and it only has spaces
		return !(id.includes("_") || id === "" || new RegExp("^\\s*$").test(id) || this.data.has(id));
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
