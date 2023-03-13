import {GeoResponse, HtmlNode, RoomTableEntry} from "./HtmlNode";
import http from "http";
import JSZip from "jszip";
import {InsightError} from "../../controller/IInsightFacade";

let linkPrefix: string = "http://cs310.students.cs.ubc.ca:11316/api/v1/project_team136/";

export function filterListedDataWithEachOther(buildingCodes: string[],
	buildingAddresses: string[],
	geoResponses: GeoResponse[],
	filesNames: string[],
	parsedFilesData: HtmlNode[]) {
	for (let i = geoResponses.length - 1; i--; i >= 0) {
		if (geoResponses[i].error !== undefined) {
			geoResponses.splice(i, 1);
			buildingAddresses.splice(i, 1);
			buildingCodes.splice(i, 1);
		}
	}
	// remove any files and its respective data that are not mentioned in index.htm
	for (let i = filesNames.length - 1; i--; i >= 0) {
		if (!buildingCodes.includes(filesNames[i])) {
			filesNames.splice(i, 1);
			parsedFilesData.splice(i, 1);
		}
	}
	// remove any building codes that don't have a respective building file
	for (let i = buildingCodes.length - 1; i--; i >= 0) {
		if (!filesNames.includes(buildingCodes[i])) {
			buildingCodes.splice(i, 1);
			buildingAddresses.splice(i, 1);
			geoResponses.splice(i, 1);
		}
	}
}

export function getGeolocationData(address: string): Promise<GeoResponse> {
	let url: string = linkPrefix + encodeURIComponent(address);
	let geoResponse: GeoResponse = {};
// http.get is async so we need a promise
	return new Promise((resolve, reject) => {
		http.get(url, (res) => {
			res.setEncoding("utf8"); // reads data as string
			let data = "";
			res.on("data", (d) => { // when data is available to be read
				data += d;
			});
			res.on("end", () => { // when entire response is received
				geoResponse = JSON.parse(data) as GeoResponse;
				resolve(geoResponse);
			});
			res.on("error", () => {
				reject(geoResponse);
			});
		});
	});
}

export function getIndexBuildingCodesAndAddresses(parsedIndexFileData: HtmlNode): {
	buildingCodes: string[],
		buildingAddresses: string[]} {
	let nodesWithTd: HtmlNode[] = findNodesWithNameOfValue(parsedIndexFileData, "td");
	let nodesWithClassCode: HtmlNode[] = filterNodesWithClassName(
		nodesWithTd,
		"views-field-field-building-code");
	let nodesWithClassAddress: HtmlNode[] = filterNodesWithClassName(
		nodesWithTd,
		"views-field-field-building-address");
	let buildingCodes: string[] = getGeneralTableEntryValues(
		nodesWithClassCode);
	let buildingAddresses: string[] = getGeneralTableEntryValues(
		nodesWithClassAddress);
	return {buildingCodes, buildingAddresses};
}

export function getBuildingRoomTableEntries(roomFileNode: HtmlNode): RoomTableEntry[] {
	// right now room and href do not return values properly because the value is inside another child node
	let entries = findNodesWithNameOfValue(roomFileNode, "td");
	let roomNumbers = getDetailedTableEntryValues(
		filterNodesWithClassName(
			entries,
			"views-field views-field-field-room-number"));
	let roomCapacities = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"views-field views-field-field-room-capacity")
	);
	let roomFurniture = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"views-field views-field-field-room-furniture")
	);
	let roomTypes = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"views-field views-field-field-room-type")
	);
	let roomLinks = getHrefTableEntryValues(
		filterNodesWithClassName(
			entries,
			"views-field views-field-nothing")
	);
	let roomTableEntriesList: RoomTableEntry[] = [];
	roomNumbers.forEach((value, index) => {
		let roomTableEntry: RoomTableEntry = new RoomTableEntry(
			value as string,
			Number(roomCapacities[index]),
			roomFurniture[index] as string,
			roomTypes[index] as string,
			roomLinks[index] as string
		);
		roomTableEntriesList.push(roomTableEntry);
	});
	return roomTableEntriesList;
}

export function getRoomFileNamesAndData(zip: JSZip) {
	let indexFileName = "index.htm";
	let indexFileData: Promise<string> | undefined = zip.file(indexFileName)?.async("string");
	if (indexFileData === undefined) {
		throw new InsightError("Error reading " + indexFileName);
	} else {
		let filesNames: string[] = [];
		let filesDataPromises: Array<Promise<string>> = [];
		filesNames.push(indexFileName);
		filesDataPromises.push(indexFileData);
		zip.folder("campus/discover/buildings-and-classrooms")?.forEach((relativePath, fileObject) => {
			filesNames.push(relativePath);
			filesDataPromises.push(fileObject.async("string"));
		});
		filesNames = filesNames.map((fileName) => {
			return fileName.replace(/\.[^/.]+$/, "");
		});
		return Promise.all(filesDataPromises).then((filesData) => {
			return {filesNames, filesData};
		});
	}
}

export function filterNodesWithClassName(nodesList: HtmlNode[], value: string): HtmlNode[] {
	return nodesList.filter((node) => {
		return node.attrs.some((attribute) => {
			if (attribute.name === "class") {
				return attribute.value.includes(value);
			}
			return false;
		});
	});
}

export function getGeneralTableEntryValues(nodeList: HtmlNode[]): string[] {
	return nodeList.map((tableEntry) => {
		if (tableEntry.childNodes !== undefined) {
			return tableEntry.childNodes[0].value.trim();
		}
		return "";
	});
}

export function getDetailedTableEntryValues(nodeList: HtmlNode[]): string[] {
	return nodeList.map((tableEntry) => {
		try {
			return tableEntry.childNodes[1].childNodes[0].value.trim();
		} catch {
			return "";
		}
	});
}

export function getHrefTableEntryValues(nodeList: HtmlNode[]): Array<string | undefined> {
	return nodeList.map((tableEntry) => {
		try {
			return tableEntry.childNodes[1].attrs[0].value;
		} catch {
			return undefined;
		}
	});
}

export function findNodesWithNameOfValue(node: HtmlNode, value: string): HtmlNode[] {
	let results: HtmlNode[] = [];
	if (node.nodeName === value) {
		results.push(node);
	}

	if (!(node.childNodes === undefined || node.childNodes.length === 0)) {
		for (let childNode of node.childNodes) {
			// test
			let childResult = findNodesWithNameOfValue(childNode, value);
			results.push(...childResult);
		}
	}

	return results;
}
