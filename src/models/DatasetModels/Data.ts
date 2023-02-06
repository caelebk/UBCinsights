import {Dataset} from "./Dataset";

export class Data {
	private datasets: Map<string, Dataset>;

	constructor() {
		this.datasets = new Map<string, Dataset>();
	}

	public toObject() {
		return Object.assign({data: [...this.datasets]});
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
