import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError
} from "./IInsightFacade";
import * as fs from "fs-extra";
import parseAndValidateQuery from "../util/query/QueryValidator";
import Query from "../models/QueryModels/Query";
import JSZip, {JSZipObject} from "jszip";
import {Course} from "../models/DatasetModels/Course";
import {Dataset} from "../models/DatasetModels/Dataset";
import {Data, dataFilePath} from "../models/DatasetModels/Data";
import handleWhere from "../util/query/QueryCollector";
import {Section} from "../models/DatasetModels/Section";
import filterResults from "../util/query/QueryResultsFilter";
import * as parse5 from "parse5";
import {HtmlNode, RoomTableEntry} from "../models/DatasetModels/HtmlNode";
import {Document} from "parse5/dist/tree-adapters/default";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private data: Data;
	constructor() {
		console.log("InsightFacadeImpl::init()");
		this.data = new Data();
		if (fs.pathExistsSync(dataFilePath)) {
			try {
				this.data.read(dataFilePath);
			} catch (error) {
				throw new InsightError("Failed to read path");
			}
		} else {
			console.log("No existing data found, creating new data");
		}
	}

	public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (!this.isValidId(id) || this.data.has(id)) {
			return Promise.reject(new InsightError("Invalid id"));
		} else {
			if (kind === InsightDatasetKind.Sections) {
				return this.data.addSectionDataToDataset(id, content);
			} else {
				return this.data.addRoomDataToDataset(id, content);
			}
		}
	}

	private isValidId(id: string): boolean {
		return !(id.includes("_") || id === "" || new RegExp("^\\s*$").test(id));
	}

	public removeDataset(id: string): Promise<string> {
		if (this.isValidId(id)) {
			if (!this.data.has(id)) {
				return Promise.reject(new NotFoundError("Valid id not yet added"));
			}
			return new Promise((resolve, reject) => {
				this.data.removeDatasetWithId(id);
				try {
					this.data.write(dataFilePath);
					resolve(id);
				} catch (error) {
					reject(new InsightError("Failed to remove dataset"));
				}
			});
		} else {
			return Promise.reject(new InsightError("Invalid id"));
		}
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		if (!this.data) {
			throw new InsightError("Data is undefined");
		} else if (this.data.getDatasets().length === 0) {
			throw new InsightError("No datasets exist; therefore unable to query");
		}
		try {
			const validatedQuery: Query = parseAndValidateQuery(query, this.data);
			const datasetId: string = this.data.has(validatedQuery?.id) ? validatedQuery.id : "";
			// If datasetId is blank, it will throw an error.
			const dataset: Dataset = this.data.get(datasetId);
			const results: Section[] = handleWhere(validatedQuery.body, dataset);
			return Promise.resolve(filterResults(validatedQuery.options, results, datasetId));
		} catch (error: unknown) {
			// for some reason, the tests would fail unless I did it like this.
			if (error instanceof ResultTooLargeError) {
				throw new ResultTooLargeError(error.message);
			} else {
				let insightError: InsightError = error as InsightError;
				throw new InsightError(insightError.message);
			}
		}
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return new Promise((resolve, reject) => {
			let insightDatasetList: InsightDataset[] = this.data.getDatasets().map((dataset: Dataset) => {
				let numSections = dataset.courses.reduce((accumulator: number, course: Course) => {
					return accumulator + course.result.length;
				}, 0);
				return {
					id: dataset.id,
					kind: dataset.kind,
					numRows: numSections
				};
			});
			resolve(insightDatasetList);
		});
	}
}
