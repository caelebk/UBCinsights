import {Dataset} from "./Dataset";

export class Data {
	private datasets: Map<string, Dataset>;

	constructor(json?: {datasets: any[]}) {
		this.datasets = new Map<string, Dataset>();
		if (json) {
			// json.datasets.
			// this.datasets = data.datasets.map((map) => {
			// 	let result = new Map<string, Dataset>();
			// 	Object.entries(map).forEach(([key, value]) => result.set(key, value));
			// 	return result;
			// }) ;
			// Object.keys(data.datasets).forEach((key) => {
			// 	this.datasets.set(key, data.datasets[key]);
			// });
			// this.datasets = data.datasets;
		}
	}

	public toObject() {
		return Object.assign({
			datasets: Object.fromEntries(this.datasets)
		});
		// let jsonObj: object;
		// this.datasets.forEach((value, key) => {
		// 	jsonObj[key] = value;
		// });
		// return Object.assign({datasets: [...this.datasets]});
	}

	public addDataset(id: string, dataset: Dataset): boolean {
		if (this.datasets.has(id)) {
			return false;
		} else {
			this.datasets.set(id, dataset);
			return true;
		}
	}

	/**
	 * Returns true is datasets has existing id
	 * @param id
	 */
	public has(id: string): boolean {
		return this.datasets.has(id);
	}
}
