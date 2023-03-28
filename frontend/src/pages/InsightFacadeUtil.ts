enum DatasetKind {
	Sections = "sections",
	Rooms = "rooms"
}

interface Dataset {
	id: string,
	kind: DatasetKind,
	numRows: number
}

interface Result {
	[key: string]: string | number;
}

export async function getDatasets(state: string): Promise<string[]> {
	const response = await fetch(
		"/datasets",
		{
			method:"GET",
			headers:{}
		}
	)
	let validDatasetIDs: string[];
	const data = await response.json();
	const datasets: Dataset[] = data as Dataset[];
	if (state === "Section") {
		const sectionDatasets = datasets.filter((dataset: Dataset) => {
			return dataset.kind === DatasetKind.Sections;
		});
		validDatasetIDs = sectionDatasets.map((dataset: Dataset) => {
			return dataset.id;
		});
	} else {
		const sectionDatasets = datasets.filter((dataset: Dataset) => {
			return dataset.kind === DatasetKind.Rooms;
		});
		validDatasetIDs = sectionDatasets.map((dataset: Dataset) => {
			return dataset.id;
		});
	}
	return Promise.resolve(validDatasetIDs);
}

export async function sendQuery(query: string): Promise<string[][]> {
	const response = await fetch(
		"/query",
		{
			method:"POST",
			headers:{},
			body: query
		}
	)
	const data = await response.json();
	const results = data as Result[];
	return [];
}
