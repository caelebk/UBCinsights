import {Dataset} from "./Dataset";
import * as fs from "fs-extra";
import {InsightError, InsightDatasetKind} from "../../controller/IInsightFacade";
import JSZip from "jszip";
import {HtmlNode, RoomTableEntry} from "./HtmlNode";
import * as parse5 from "parse5";
import * as http from "http";
import {Course} from "./Course";
import {getSectionFileNamesAndData, getValidCoursesFromNamesAndData} from "./AddSectionDataset";

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
					if (filesNames.length === 0) {
						throw new InsightError("No buildings or rooms files found");
					}
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
					let indexBuildingCodes: Array<string | number | undefined> = this.getGeneralTableEntryValues(
						nodesWithClassCode);
					let indexBuildingAddresses: Array<string | number | undefined> = this.getGeneralTableEntryValues(
						nodesWithClassAddress);
					// let linkPrefix: string = "http://cs310.students.cs.ubc.ca:11316/api/v1/project_team136/";
					// let encode: string = encodeURIComponent("6245 Agronomy Road V6T 1Z4");
					// let test = http.get(linkPrefix + encode, (res) => {
					// 	return res.rawHeaders;
					//
					// });
				// remove any files that are not mentioned in index.htm
					filesNames.forEach((fileName, index) => {
						if (indexBuildingCodes.indexOf(fileName) === -1) {
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
		let roomNumbers = this.getDetailedTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-number"));
		let roomCapacities = this.getGeneralTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-capacity")
		);
		let roomFurniture = this.getGeneralTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-furniture")
		);
		let roomTypes = this.getGeneralTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-field-room-type")
		);
		let roomLinks = this.getHrefTableEntryValues(
			this.filterNodesWithClassName(
				entries,
				"views-field views-field-nothing")
		);
		let roomTableEntriesList: RoomTableEntry[] = [];
		roomNumbers.forEach((value, index) => {
			let roomTableEntry: RoomTableEntry = new RoomTableEntry(
				value as string,
				Number(roomCapacities[index]),
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

	private getGeneralTableEntryValues(nodeList: HtmlNode[]): Array<string | undefined> {
		return nodeList.map((tableEntry) => {
			if (tableEntry.childNodes !== undefined) {
				return tableEntry.childNodes[0].value.trim();
			}
			return undefined;
		});
	}

	private getDetailedTableEntryValues(nodeList: HtmlNode[]): Array<string | undefined> {
		return nodeList.map((tableEntry) => {
			try {
				return tableEntry.childNodes[1].childNodes[0].value.trim();
			} catch {
				return undefined;
			}
		});
	}

	private getHrefTableEntryValues(nodeList: HtmlNode[]): Array<string | undefined> {
		return nodeList.map((tableEntry) => {
			try {
				return tableEntry.childNodes[1].attrs[0].value;
			} catch {
				return undefined;
			}
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
					return getSectionFileNamesAndData(zip);
				})
				.then(({fileNames, fileData}) => {
					return getValidCoursesFromNamesAndData(fileNames, fileData);
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

}
