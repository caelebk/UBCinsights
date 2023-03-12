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

export class RoomTableEntry {
	public room: string;
	public capacity: number;
	public furnitureType: string;
	public roomType: string;
	public href: string;


	constructor(room: string, capacity: number, furnitureType: string, roomType: string, href: string) {
		this.room = room;
		this.capacity = capacity;
		this.furnitureType = furnitureType;
		this.roomType = roomType;
		this.href = href;
	}
}
