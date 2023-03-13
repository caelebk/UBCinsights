import {Dataset} from "./Dataset";
import * as fs from "fs-extra";
import {InsightError, InsightDatasetKind} from "../../controller/IInsightFacade";
import JSZip from "jszip";
import {GeoResponse, HtmlNode, RoomTableEntry} from "./HtmlNode";
import * as parse5 from "parse5";
import {Course} from "./Course";
import {getSectionFileNamesAndData, getValidCoursesFromNamesAndData} from "./AddSectionDataset";
import {
	filterListedDataWithEachOther,
	getBuildingRoomTableEntries,
	getGeolocationData, getIndexBuildingCodesAndAddresses,
	getRoomFileNamesAndData
} from "./AddRoomDataset";

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
					return getRoomFileNamesAndData(zip);
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
					let {buildingCodes, buildingAddresses} = getIndexBuildingCodesAndAddresses(
						parsedIndexFileData);
					let geoResponsesPromises = buildingAddresses.map((address) => {
						return getGeolocationData(address);
					});
					return Promise.all(geoResponsesPromises).then((geoResponses) => {
						return {buildingCodes, buildingAddresses, geoResponses, filesNames, parsedFilesData};
					});
				}).then(({buildingCodes, buildingAddresses, geoResponses, filesNames, parsedFilesData}) => {
					// TODO replace parsedFilesData with building room entries so you remove all buildings with empty rooms
					// remove any building codes and addresses that don't have proper geoResponses
					filterListedDataWithEachOther(buildingCodes, buildingAddresses,
						geoResponses, filesNames, parsedFilesData);
				// create a map of file name and a list of room table entries, have the building codes access the data.
					let fileNameRoomDataMap: Map<string, RoomTableEntry[]> = new Map<string, RoomTableEntry[]>();
					parsedFilesData.forEach((value, index) => {
						let roomTableEntries = getBuildingRoomTableEntries(value);
						fileNameRoomDataMap.set(filesNames[index], roomTableEntries);
					});
					console.log("test");
				}).catch((error) => {
					reject(new InsightError(error));
				});
		});
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
