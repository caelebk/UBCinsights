import {Dataset} from "./Dataset";
import * as fs from "fs-extra";

export class Data {
	private datasets: Dataset[];

	constructor(json?: {datasets: Dataset[]}) {
		this.datasets = [];
		if (json) {
			json.datasets.forEach((dataset) => {
				this.addDataset(new Dataset("", [], dataset));
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
	public getDataset(): Dataset[] {
		return this.datasets;
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
}
