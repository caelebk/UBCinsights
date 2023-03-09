import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError
} from "./IInsightFacade";
import * as fs from "fs-extra";
import parseAndValidateQuery from "../util/query/QueryValidator";
import Query from "../models/QueryModels/Query";
import JSZip, {JSZipObject} from "jszip";
import {Course} from "../models/DatasetModels/Course";
import {Dataset} from "../models/DatasetModels/Dataset";
import {Data} from "../models/DatasetModels/Data";
import handleWhere from "../util/query/QueryCollector";
import {Section} from "../models/DatasetModels/Section";
import filterResults from "../util/query/QueryResultsFilter";
import * as parse5 from "parse5";
import {HtmlNode} from "../models/DatasetModels/HtmlNode";
import {Document} from "parse5/dist/tree-adapters/default";

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
		if (fs.pathExistsSync(this.dataFilePath)) {
			try {
				this.data.read(this.dataFilePath);
			} catch (error) {
				throw new InsightError("Failed to read path");
			}
		} else {
			console.log("No existing data found, creating new data");
		}
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (!this.isValidId(id) || this.data.has(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			if (kind === InsightDatasetKind.Sections) {
				return this.addSectionDataToDataset(id, content);
			} else {
				// TODO
				return this.addRoomDataToDataset(id, content);
			}
		}
	}

	private addRoomDataToDataset(id: string, content: string): Promise<string[]> {
		return new Promise((resolve, reject) => {
			JSZip.loadAsync(content, {base64: true})
				.then((zip: JSZip) => {
					let file: JSZipObject | null = zip.file("index.htm");
					if (file !== null) {
						return file;
					} else {
						throw new InsightError("No Index File Found");
					}
				}).then((indexObject: JSZipObject) => {
					return indexObject.async("string");
				}).then((indexData: string) => {
					return parse5.parse(indexData);
				}).then((indexDocument) => {
					let buildingsAndAddressesList: {buildings: Array<string | undefined>,
						addresses: Array<string | undefined>} = this.getBuildingsAndAddresses(indexDocument);
					resolve(this.data.getDatasets().map((ds) => ds.id));
				}).catch((error) => {
					reject(new InsightError(error));
				});
		});
	}

	private getBuildingsAndAddresses(document: Document) {
		let documentNode: HtmlNode = document as object as HtmlNode;
		let buildingCodes = this.findClassesThatContainsValue(
			documentNode,
			"views-field-field-building-code"
		).filter((node) => {
			return node.nodeName === "td";
		});
		let buildingAddresses = this.findClassesThatContainsValue(
			documentNode,
			"views-field-field-building-address"
		).filter((node) => {
			return node.nodeName === "td";
		});
		let buildingCodeList: Array<string | undefined> = buildingCodes.map((bc) => {
			if (bc.childNodes !== undefined) {
				return bc.childNodes[0].value.trim();
			} else {
				return undefined;
			}
		});
		let buildingAddressList: Array<string | undefined> = buildingAddresses.map((ba) => {
			if (ba.childNodes !== undefined) {
				return ba.childNodes[0].value.trim();
			} else {
				return undefined;
			}
		});
		return {buildings: buildingCodeList, addresses: buildingAddressList};
	}

	private findClassesThatContainsValue(node: HtmlNode, value: string): HtmlNode[] {
		let results: HtmlNode[] = [];
		if (node.attrs?.some((a) => {
			if (a.name === "class") {
				if (a.value !== undefined) {
					return a.value.includes(value);
				}
			}
		})) {
			results.push(node);
		}

		if (!(node.childNodes === undefined || node.childNodes.length === 0)) {
			for (let childNode of node.childNodes) {
				let childResult = this.findClassesThatContainsValue(childNode, value);
				results.push(...childResult);
			}
		}

		return results;
	}

	private findNodesWithNameOfValue(node: HtmlNode, value: string): HtmlNode[] {
		let results: HtmlNode[] = [];
		if (node.nodeName === value) {
			results.push(node);
		}

		if (!(node.childNodes === undefined || node.childNodes.length === 0)) {
			for (let childNode of node.childNodes) {
				// test
				let childResult = this.findNodesWithNameOfValue(childNode, value);
				results.push(...childResult);
			}
		}

		return results;
	}

	private addSectionDataToDataset(id: string, content: string): Promise<string[]> {
		return new Promise((resolve, reject) => {
			// read zip file
			JSZip.loadAsync(content, {base64: true})
				.then((zip: JSZip) => {
					return this.getFileNamesAndData(zip);
				})
				.then(({fileNames, fileData}) => {
					return this.getValidCoursesFromNamesAndData(fileNames, fileData);
				}).then((validCourses: Course[]) => {
				// create the new dataset with the given id and valid courses
					let dataset = new Dataset(id, InsightDatasetKind.Sections, validCourses, []);
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
				try {
					this.data.write(this.dataFilePath);
					resolve(id);
				} catch (error) {
					reject(new InsightError("Failed to remove dataset"));
				}
			});
		} else {
			return Promise.reject(new InsightError("Invalid id"));
		}
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		if (!this.data) {
			throw new InsightError("Data is undefined");
		} else if (this.data.getDatasets().length === 0) {
			throw new InsightError("No datasets exist; therefore unable to query");
		}
		try {
			const validatedQuery: Query = parseAndValidateQuery(query, this.data);
			const datasetId: string = this.data.has(validatedQuery?.id) ? validatedQuery.id : "";
			// If datasetId is blank, it will throw an error.
			const dataset: Dataset = this.data.get(datasetId);
			const results: Section[] = handleWhere(validatedQuery.body, dataset);
			return Promise.resolve(filterResults(validatedQuery.options, results, datasetId));
		} catch (error: unknown) {
			// for some reason, the tests would fail unless I did it like this.
			if (error instanceof ResultTooLargeError) {
				throw new ResultTooLargeError(error.message);
			} else {
				let insightError: InsightError = error as InsightError;
				throw new InsightError(insightError.message);
			}
		}
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return new Promise((resolve, reject) => {
			let insightDatasetList: InsightDataset[] = this.data.getDatasets().map((dataset: Dataset) => {
				let numSections = dataset.courses.reduce((accumulator: number, course: Course) => {
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
