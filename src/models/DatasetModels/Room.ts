export class Room {
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
}
