import {Dataset} from "./Dataset";
import * as fs from "fs-extra";
import {InsightError, InsightDatasetKind} from "../../controller/IInsightFacade";
import JSZip from "jszip";
import {HtmlNode, RoomTableEntry} from "./HtmlNode";
import * as parse5 from "parse5";
import {Course} from "./Course";

export const dataFileDirectory: string = "./data/";
export const dataFilePath: string = dataFileDirectory + "DataFile.json";

export class Data {
	private readonly dataFileDirectory: string = "./data/";
	private readonly dataFilePath: string = this.dataFileDirectory + "DataFile.json";
	private datasets: Dataset[];

	constructor(json?: {datasets: Dataset[]}) {
		this.datasets = [];
		if (json) {
			json.datasets.forEach((dataset) => {
				this.addDataset(new Dataset("", InsightDatasetKind.Sections, [], [], dataset));
			});
		}
	}

	public addDataset(dataset: Dataset): boolean {
		if (this.has(dataset.id)) {
			return false;
		} else {
			this.datasets.push(dataset);
			return true;
		}
	}

	public removeDatasetWithId(id: string) {
		this.datasets = this.datasets.filter((dataset) => !(dataset.id === id));
	}

	public getDatasets(): Dataset[] {
		return this.datasets;
	}

	public get(id: string): Dataset {
		for (const dataset of this.datasets) {
			if (dataset.id === id) {
				return dataset;
			}
		}
		throw new InsightError("Dataset being queried doesn't exist");
	}

	public has(id: string): boolean {
		for (const dataset of this.datasets) {
			if (dataset.id === id) {
				return true;
			}
		}
		return false;
	}

	public write(path: string) {
		let jsonData = Object.assign(this);
		// create data folder if it is missing
		fs.ensureFileSync(path);
		// write to file
		fs.writeJsonSync(path, jsonData);
	}

	public read(path: string) {
		try {
			let jsonData: {datasets: any[]} = fs.readJsonSync(path);
			this.datasets = [];
			jsonData.datasets.forEach((dataset) => {
				let datasetObject: Dataset = new Dataset("", InsightDatasetKind.Sections, [], [],dataset);
				if (datasetObject.isValid()) {
					// data written to file should already be valid, this is to double-check for corruption when reading
					this.addDataset(datasetObject);
				}
			});
		} catch (error) {
			throw new InsightError("Failed to read dataset");
		}
	}

	public addRoomDatasetToData(id: string, content: string): Promise<string[]> {
		return new Promise((resolve, reject) => {
			JSZip.loadAsync(content, {base64: true})
				.then((zip: JSZip) => {
					return this.getRoomFileNamesAndData(zip);
				}).then(({filesNames, filesData}) => {
					let parsedFilesData: HtmlNode[] = filesData.map((data) => {
						return parse5.parse(data) as object as HtmlNode;
					});
					filesNames.shift(); // first value is always indexFile
					let parsedIndexFileData = parsedFilesData.shift();
					if (parsedIndexFileData === undefined) {
						throw new InsightError("error with parsed data");
					}
					let nodesWithTd: HtmlNode[] = this.findNodesWithNameOfValue(parsedIndexFileData, "td");
					let nodesWithClassCode: HtmlNode[] = this.filterNodesWithClassName(
						nodesWithTd,
						"views-field-field-building-code");
					let nodesWithClassAddress: HtmlNode[] = this.filterNodesWithClassName(
						nodesWithTd,
						"views-field-field-building-address");
					let buildingCodes: Array<string | number | undefined> = this.getTableEntryValues(
						nodesWithClassCode);
					let buildingAddresses: Array<string | number | undefined> = this.getTableEntryValues(
						nodesWithClassAddress);
					if (filesNames.length === 0) {
						throw new InsightError("No buildings or rooms files found");
					}
				// remove any files that are not mentioned in index.htm
					filesNames.forEach((fileName, index) => {
						if (buildingCodes.indexOf(fileName) === -1) {
							filesNames.splice(index, 1);
							parsedFilesData.splice(index, 1);
						};
					});
				// create a map of file name and a list of room table entries, have the building codes access the data.
					let fileNameRoomDataMap: Map<string, RoomTableEntry[]> = new Map<string, RoomTableEntry[]>();
					parsedFilesData.forEach((value, index) => {
						let roomTableEntries = this.getBuildingRoomTableEntries(value);
						fileNameRoomDataMap.set(filesNames[index], roomTableEntries);
					});
					console.log("test");
				}).catch((error) => {
					reject(new InsightError(error));
				});
		});
	}

	private getBuildingRoomTableEntries(roomFileNode: HtmlNode): RoomTableEntry[] {
		// right now room and href do not return values properly because the value is inside another child node
		let entries = this.findNodesWithNameOfValue(roomFileNode, "td");
		let roomNumbers = this.getTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-number"));
		let roomCapacities = this.getTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-capacity")
		);
		let roomFurniture = this.getTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-furniture")
		);
		let roomTypes = this.getTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-type")
		);
		let roomLinks = this.getTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-nothing")
		);
		let roomTableEntriesList: RoomTableEntry[] = [];
		roomNumbers.forEach((value, index) => {
			let roomTableEntry: RoomTableEntry = new RoomTableEntry(
				value as string,
				roomCapacities[index] as number,
				roomFurniture[index] as string,
				roomTypes[index] as string,
				roomLinks[index] as string
			);
			roomTableEntriesList.push(roomTableEntry);
		});
		return roomTableEntriesList;
	}

	private getRoomFileNamesAndData(zip: JSZip) {
		let indexFileName = "index.htm";
		let indexFileData: Promise<string> | undefined = zip.file(indexFileName)?.async("string");
		if (indexFileData === undefined) {
			throw new InsightError("Error reading " + indexFileName);
		} else {
			let filesNames: string[] = [];
			let filesDataPromises: Array<Promise<string>> = [];
			filesNames.push(indexFileName);
			filesDataPromises.push(indexFileData);
			zip.folder("campus/discover/buildings-and-classrooms")?.forEach((relativePath, fileObject) => {
				filesNames.push(relativePath);
				filesDataPromises.push(fileObject.async("string"));
			});
			filesNames = filesNames.map((fileName) => {
				return fileName.replace(/\.[^/.]+$/, "");
			});
			return Promise.all(filesDataPromises).then((filesData) => {
				return {filesNames, filesData};
			});
		}
	}

	private filterNodesWithClassName(nodesList: HtmlNode[], value: string): HtmlNode[] {
		return nodesList.filter((node) => {
			return node.attrs.some((attribute) => {
				if (attribute.name === "class") {
					return attribute.value.includes(value);
				}
				return false;
			});
		});
	}

	private getTableEntryValues(nodeList: HtmlNode[]): Array<string | number | undefined> {
		return nodeList.map((tableEntry) => {
			if (tableEntry.childNodes !== undefined) {
				return tableEntry.childNodes[0].value.trim();
			}
			return undefined;
		});
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

	public addSectionDatasetToData(id: string, content: string): Promise<string[]> {
		return new Promise((resolve, reject) => {
			// read zip file
			JSZip.loadAsync(content, {base64: true})
				.then((zip: JSZip) => {
					return this.getSectionFileNamesAndData(zip);
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
					this.addDataset(dataset);
					this.write(this.dataFilePath);
					resolve(this.getDatasets().map((ds) => ds.id));
				})
				.catch((error) => {
					reject(new InsightError(error));
				});
		});
	}

	private getSectionFileNamesAndData(zip: JSZip) {
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
}
