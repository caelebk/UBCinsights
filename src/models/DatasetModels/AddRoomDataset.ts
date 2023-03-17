import {Attribute, GeoResponse, HtmlNode, RoomTableEntry} from "./HtmlNode";
import http from "http";
import JSZip from "jszip";
import {InsightError} from "../../controller/IInsightFacade";
import * as parse5 from "parse5";
import {Room} from "./Room";

let linkPrefix: string = "http://cs310.students.cs.ubc.ca:11316/api/v1/project_team136/";

export function filterListedDataWithEachOther(
	buildingCodes: string[],
	buildingTitles: string[],
	buildingAddresses: string[],
	geoResponses: GeoResponse[],
	filesNames: string[],
	fileRoomEntryData: RoomTableEntry[][]) {

	// remove any files and its respective data that are not mentioned in index.htm or has no rooms in the building
	for (let i = filesNames.length - 1; i--; i >= 0) {
		if (!buildingCodes.includes(filesNames[i]) || fileRoomEntryData[i].length === 0) {
			filesNames.splice(i, 1);
			fileRoomEntryData.splice(i, 1);
		}
	}
	// remove any files without proper geoResponse and without any building files names
	for (let i = buildingCodes.length - 1; i--; i >= 0) {
		if (geoResponses[i].error !== undefined || !filesNames.includes(buildingCodes[i])) {
			buildingCodes.splice(i, 1);
			buildingTitles.splice(i, 1);
			buildingAddresses.splice(i, 1);
			geoResponses.splice(i, 1);
		}
	}
}
export function getRoomsFromData(
	buildingCodes: string[],
	buildingTitles: string[],
	buildingAddresses: string[],
	geoResponses: GeoResponse[],
	filesNames: string[],
	fileRoomEntryData: RoomTableEntry[][]): Room[] {
	let roomsList: Room[] = [];
	for (let i = 0; i < buildingCodes.length; i++) {
		let code = buildingCodes[i];
		let title = buildingTitles[i];
		let address = buildingAddresses[i];
		let geoResponse = geoResponses[i];
		let fileIndex = filesNames.indexOf(code);
		let roomEntry: RoomTableEntry[] = fileRoomEntryData[fileIndex];
		roomEntry.forEach((tableEntry) => {
			let roomJson = {
				fullname: title,
				shortname: code,
				number: tableEntry.room,
				name: code + "_" + tableEntry.room,
				address: address,
				lat: geoResponse.lat!,
				lon: geoResponse.lon!,
				seats: tableEntry.capacity,
				type: tableEntry.roomType,
				furniture: tableEntry.furnitureType,
				href: tableEntry.href
			};
			roomsList.push(new Room(roomJson));
		});
	}
	return roomsList;
}

export function getIndexCodesTitlesAddressesGeoAndFileNamesAndData(
	filesData: Array<Awaited<string>>,
	filesNames: string[]) {
	let parsedFilesData: HtmlNode[] = filesData.map((data) => {
		return parse5.parse(data) as object as HtmlNode;
	});
	filesNames.shift(); // first value is always indexFile
	let parsedIndexFileData = parsedFilesData.shift();
	if (filesNames.length === 0) {
		throw new InsightError("No buildings or rooms files found");
	}
	if (parsedIndexFileData === undefined) {
		throw new InsightError("error with parsed data");
	}
	let {buildingCodes, buildingTitles, buildingAddresses} = getIndexBuildingCodesAndAddresses(
		parsedIndexFileData);
	let geoResponsesPromises = buildingAddresses.map((address) => {
		return getGeolocationData(address);
	});
	return Promise.all(geoResponsesPromises).then((geoResponses) => {
		return {
			buildingCodes,
			buildingTitles,
			buildingAddresses,
			geoResponses,
			filesNames,
			parsedFilesData
		};
	});
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
	buildingTitles: string[],
	buildingAddresses: string[]} {
	let nodesWithTd: HtmlNode[] = findNodesWithNameOfValue(parsedIndexFileData, "td");
	let nodesWithClassCode: HtmlNode[] = filterNodesWithClassName(nodesWithTd, "building-code");
	let nodesWithClassTitle = filterNodesWithClassName(nodesWithTd, "title");
	let nodesWithClassAddress: HtmlNode[] = filterNodesWithClassName(nodesWithTd,"building-address");
	let buildingCodes: string[] = getGeneralTableEntryValues(nodesWithClassCode);
	let buildingTitles: string[] = getDetailedTableEntryValues(nodesWithClassTitle);
	let buildingAddresses: string[] = getGeneralTableEntryValues(nodesWithClassAddress);
	return {buildingCodes, buildingTitles, buildingAddresses};
}

export function getBuildingRoomTableEntries(roomFileNode: HtmlNode): RoomTableEntry[] {
	// right now room and href do not return values properly because the value is inside another child node
	let entries = findNodesWithNameOfValue(roomFileNode, "td");
	let roomNumbers = getDetailedTableEntryValues(
		filterNodesWithClassName(
			entries,
			"room-number"));
	let roomCapacities = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"room-capacity")
	);
	let roomFurniture = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"room-furniture")
	);
	let roomTypes = getGeneralTableEntryValues(
		filterNodesWithClassName(
			entries,
			"room-type")
	);
	let roomLinks = getHrefTableEntryValues(
		filterNodesWithClassName(
			entries,
			"nothing")
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
			let text = tableEntry.childNodes.find((value) => {
				return value.nodeName === "#text";
			});
			if (text === undefined) {
				return "";
			}
			return text.value.trim();
		}
		return "";
	});
}

export function getDetailedTableEntryValues(nodeList: HtmlNode[]): string[] {
	return nodeList.map((tableEntry) => {
		try {
			let childEntry: HtmlNode | undefined = tableEntry.childNodes.find((value) => {
				return value.nodeName === "a";
			});
			if (childEntry === undefined) {
				return "";
			}
			return getGeneralTableEntryValues([childEntry])[0];
		} catch {
			return "";
		}
	});
}

export function getHrefTableEntryValues(nodeList: HtmlNode[]): Array<string | undefined> {
	function hasHrefAttribute(value: HtmlNode) {
		return value.attrs.some((attribute) => {
			return attribute.name === "href";
		});
	}

	return nodeList.map((tableEntry) => {
		try {
			let childEntryWithHref = tableEntry.childNodes.find((value) => {
				if (value.attrs === undefined) {
					return false;
				}
				return hasHrefAttribute(value);
			});
			if (childEntryWithHref === undefined) {
				return "";
			}
			let attribute = childEntryWithHref.attrs.find((a) => {
				return a.name === "href";
			});
			if (attribute === undefined) {
				return "";
			}
			return attribute.value.trim();
			// return tableEntry.childNodes[1].attrs[0].value;
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
