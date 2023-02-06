import {Section} from "./Section";

export class FileData {
	public result: Section[];
	public rank: number;

	constructor(data: {result: any[], rank: number}) {
		this.result = data.result.map((sectionData) => new Section(sectionData));
		this.rank = data.rank;
	}
}
