import {MFieldRoom, SFieldRoom} from "../QueryModels/Enums";
import {DataModel} from "./DataModel";

export class Room implements DataModel {
	public fullname: string;
	public shortname: string;
	public number: string;
	public name: string;
	public address: string;
	public lat: number;
	public lon: number;
	public seats: number;
	public type: string;
	public furniture: string;
	public href: string;

	constructor(json: {fullname: string, shortname: string, number: string, name: string, address: string,
		lat: number, lon: number, seats: number,
		type: string, furniture: string, href: string}) {
		this.fullname = json.fullname;
		this.shortname = json.shortname;
		this.number = json.number;
		this.name = json.name;
		this.address = json.address;
		this.lat = json.lat;
		this.lon = json.lon;
		this.seats = json.seats;
		this.type = json.type;
		this.furniture = json.furniture;
		this.href = json.href;
	}

public getMFieldValue(mField: MFieldRoom): number {
		switch (mField) {
			case MFieldRoom.lat:
				return Number(this.lat);
			case MFieldRoom.seats:
				return Number(this.seats);
			case MFieldRoom.lon:
				return Number(this.lon);
		}
	}

public getSFieldValue(sField: SFieldRoom): string {
		switch (sField) {
			case SFieldRoom.address:
				return String(this.address);
			case SFieldRoom.furniture:
				return String(this.furniture);
			case SFieldRoom.fullname:
				return String(this.fullname);
			case SFieldRoom.href:
				return String(this.href);
			case SFieldRoom.name:
				return String(this.name);
			case SFieldRoom.number:
				return String(this.number);
			case SFieldRoom.shortname:
				return String(this.shortname);
			case SFieldRoom.type:
				return String(this.type);
		}
    
public isValid(): boolean {
		for (const attribute in this) {
			if (this[attribute] === undefined) {
				return false;
			}
		}
		return true;
	}
}
