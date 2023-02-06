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
import * as fs from "fs-extra";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private data: Data;
	constructor() {
		console.log("InsightFacadeImpl::init()");
		try {
			let jsonData = fs.readJsonSync("./data/testfilepleasework.json");
			this.data = new Data(jsonData);
		} catch {
			console.log("failed to read data");
			this.data = new Data();
		}
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (!this.isValidId(id) || this.data.has(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			return new Promise((resolve, reject) => {
				// read zip file
				JSZip.loadAsync(content, {base64: true})
					.then((zip) => { // get file data promises
						let fileNames: string[] = [];
						let fileDataPromises: Array<Promise<string>> = [];
						// open course folder
						zip.folder("courses")?.forEach((relativePath, file) => {
							// read all files and push into a list
							fileNames.push(relativePath);
							fileDataPromises.push(file.async("string"));
						});
						return Promise.all(fileDataPromises)
							.then((fileData) => {
								return {fileNames, fileData};
							});
					})
					.then(({fileNames, fileData}) => {
						// convert file data into class object
						if (fileData.length === 0) {
							throw new InsightError("No Courses found");
						}
						let courses: Course[] = fileData.map((file, index) => {
							return new Course(fileNames[index], JSON.parse(file));
						});
						console.log("this many courses" + courses.length);
						let validCourses: Course[] = [];
						// if a course is valid, filter to only the valid sections and add the list of valid courses
						for (const course of courses) {
							if (course.isValid()) {
								course.filterSections();
								validCourses.push(course);
							}
						}
						if (validCourses.length === 0) {
							throw new InsightError("No Valid Courses Found");
						}
						// create the new dataset with the given id and valid courses
						let dataset = new Dataset(id, validCourses);
						// add it to the data
						this.data.addDataset(dataset);
						let jsonData = Object.assign(this.data);
						// create data folder if it is missing
						fs.mkdirpSync("./data");
						// write to file
						fs.writeJsonSync("./data/DataFile.json", jsonData);
						resolve(this.data.getDataset().map((ds) => ds.id));
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
