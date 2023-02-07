import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError
} from "./IInsightFacade";
import parseAndValidateQuery from "../util/query/QueryValidator";
import Query from "../models/QueryModels/Query";
import JSZip from "jszip";
import {Course} from "../models/DatasetModels/Course";
import {Dataset} from "../models/DatasetModels/Dataset";
import {Data} from "../models/DatasetModels/Data";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private data: Data;
	private readonly dataFileDirectory: string = "./data/";
	private readonly dataFilePath: string = this.dataFileDirectory + "DataFile.json";
	constructor() {
		console.log("InsightFacadeImpl::init()");
		this.data = new Data();
		try {
			this.data.read(this.dataFilePath);
		} catch {
			console.log("No existing data found, creating new data");
		}
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (!this.isValidId(id) || this.data.has(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			if (kind === InsightDatasetKind.Sections) {
				return new Promise((resolve, reject) => {
					// read zip file
					JSZip.loadAsync(content, {base64: true})
						.then((zip) => {
							return this.getFileNamesAndData(zip);
						})
						.then(({fileNames, fileData}) => {
							return this.getValidCoursesFromNamesAndData(fileNames, fileData);
						}).then((validCourses) => {
							// create the new dataset with the given id and valid courses
							let dataset = new Dataset(id, kind, validCourses);
							if (!dataset.isValid()) {
								throw new InsightError("Dataset is not valid");
							}
							// add it to the data
							this.data.addDataset(dataset);
							this.data.write(this.dataFilePath);
							resolve(this.data.getDatasets().map((ds) => ds.id));
						})
						.catch((error) => {
							reject(new InsightError(error));
						});
				});
			} else {
				return Promise.reject(new InsightError("kind = rooms"));
			}
		}
	}

	/**
	 * Takes a JSZip object and returns a list of file names as string and list of file data as string found
	 * within a folder named courses
	 *
	 * @param zip
	 * @private
	 */
	private getFileNamesAndData(zip: JSZip) {
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
	}

	/**
	 * Returns a list of Courses that contains only valid courses from given file names and file data in JSON format
	 *
	 * @param fileNames
	 * @param fileData
	 * @private
	 */
	private getValidCoursesFromNamesAndData(fileNames: string[], fileData: string[]) {
		let courses: Course[] = fileData.map((file, index) => {
			return new Course(fileNames[index], JSON.parse(file));
		});
		let validCourses: Course[] = [];
		// if a course is valid, filter to only the valid sections and add the list of valid courses
		for (const course of courses) {
			if (course.isValid()) {
				course.filterSections();
				validCourses.push(course);
			}
		}
		return validCourses;
	}

	private isValidId(id: string): boolean {
		// checks for underscore, empty, and it only has spaces
		return !(id.includes("_") || id === "" || new RegExp("^\\s*$").test(id));
	}

	public removeDataset(id: string): Promise<string> {
		if (this.isValidId(id)) {
			if (!this.data.has(id)) {
				return Promise.reject(new NotFoundError("Valid id not yet added"));
			}
			return new Promise((resolve, reject) => {
				this.data.removeDatasetWithId(id);
				this.data.write(this.dataFilePath);
				resolve(id);
			});
		} else {
			return Promise.reject(new InsightError("Invalid id"));
		}
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		try {
			const validatedQuery: Query = parseAndValidateQuery(query);
		} catch (error) {
			return Promise.reject(error);
		}
		return Promise.resolve([]);
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return new Promise((resolve, reject) => {
			let insightDatasetList: InsightDataset[] = this.data.getDatasets().map((dataset) => {
				let numSections = dataset.courses.reduce((accumulator, course) => {
					return accumulator + course.result.length;
				}, 0);
				return {
					id: dataset.id,
					kind: dataset.kind,
					numRows: numSections
				};
			});
			resolve(insightDatasetList);
		});
	}
}
