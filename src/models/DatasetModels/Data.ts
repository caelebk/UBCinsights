import {Dataset} from "./Dataset";
import * as fs from "fs-extra";
import {InsightError, InsightDatasetKind} from "../../controller/IInsightFacade";

export class Data {
	private datasets: Dataset[];

	constructor(json?: {datasets: Dataset[]}) {
		this.datasets = [];
		if (json) {
			json.datasets.forEach((dataset) => {
				this.addDataset(new Dataset("", InsightDatasetKind.Sections, [], dataset));
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

	/**
	 * Removes Dataset with id if it exists in Datasets
	 * @param id
	 */
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

	/**
	 * Returns true is datasets has existing id
	 * @param id
	 */
	public has(id: string): boolean {
		for (const dataset of this.datasets) {
			if (dataset.id === id) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Converts Data into a plain Object and then writes the object to a json file to path
	 *
	 * @param path
	 */
	public write(path: string) {
		let jsonData = Object.assign(this);
		// create data folder if it is missing
		fs.ensureFileSync(path);
		// write to file
		fs.writeJsonSync(path, jsonData);
	}

	/**
	 * Reads JSON object from path and overwrites the existing datasets of this
	 * @param path
	 */
	public read(path: string) {
		let jsonData: {datasets: any[]} = fs.readJsonSync(path);
		this.datasets = [];
		jsonData.datasets.forEach((dataset) => {
			let datasetObject: Dataset = new Dataset("", InsightDatasetKind.Sections, [], dataset);
			if (datasetObject.isValid()) {
				// data written to file should already be valid, this is to double-check for corruption when reading
				this.addDataset(datasetObject);
			}
		});
	}
}
