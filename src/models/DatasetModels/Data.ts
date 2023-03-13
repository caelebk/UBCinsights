import {Dataset} from "./Dataset";
import * as fs from "fs-extra";
import {InsightDatasetKind, InsightError} from "../../controller/IInsightFacade";
import JSZip from "jszip";
import {RoomTableEntry} from "./HtmlNode";
import {Course} from "./Course";
import {getSectionFileNamesAndData, getValidCoursesFromNamesAndData} from "./AddSectionDataset";
import {
	filterListedDataWithEachOther,
	getBuildingRoomTableEntries,
	getIndexCodesTitlesAddressesGeoAndFileNamesAndData,
	getRoomFileNamesAndData,
	getRoomsFromData
} from "./AddRoomDataset";
import {Room} from "./Room";

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
					return getIndexCodesTitlesAddressesGeoAndFileNamesAndData(filesData, filesNames);
				}).then(({buildingCodes,buildingTitles,buildingAddresses,
					   geoResponses,filesNames,parsedFilesData}) => {
					// remove any building codes and addresses that don't have proper geoResponses
					let fileRoomEntryData: RoomTableEntry[][] = [];
					parsedFilesData.forEach((value, index) => {
						fileRoomEntryData.push(getBuildingRoomTableEntries(value));
					});
					filterListedDataWithEachOther(buildingCodes, buildingTitles, buildingAddresses,
						geoResponses, filesNames, fileRoomEntryData);
					let rooms: Room[] = getRoomsFromData(
						buildingCodes, buildingTitles, buildingAddresses, geoResponses,
						filesNames, fileRoomEntryData);
					let dataset = new Dataset(id, InsightDatasetKind.Rooms, [], rooms);
					this.addDataset(dataset);
					this.write(this.dataFilePath);
					resolve(this.datasets.map((ds) => ds.id));
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
