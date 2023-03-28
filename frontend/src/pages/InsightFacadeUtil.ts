enum DatasetKind {
	Sections = "sections",
	Rooms = "rooms"
}

interface Dataset {
	id: string,
	kind: DatasetKind,
	numRows: number
}

export interface InsightResult {
	[key: string]: string | number;
}

export async function getDatasets(state: string): Promise<string[]> {
	const response = await fetch(
		"http://localhost:4321/datasets",
		{
			method:"GET",
			headers:{"Content-Type": "application/json"}
		}
	)
	let validDatasetIDs: string[];
	const data = await response.json();
	const datasets: Dataset[] = data.result as Dataset[];
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

export async function sendQuery(query: string): Promise<InsightResult[]> {
	const response = await fetch(
		"http://localhost:4321/query",
		{
			method:"POST",
			headers:{"Content-Type": "application/json"},
			body: query
		}
	)
	const data = await response.json();
	if (!response.ok || response.status === 400) {
		return Promise.reject("Status code " + response.status + ":\n" + data.error);
	}
	return Promise.resolve(data.result as InsightResult[]);
}
