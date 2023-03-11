export interface HtmlNode {
	nodeName: string;
	attrs: Attribute[];
	childNodes: HtmlNode[];
	value: string
}

export interface Attribute {
	name: string;
	value: string
}

export interface RoomTableEntry {
	room: string;
	capacity: number;
	furnitureType: string;
	roomType: string;
	href: string;
}
