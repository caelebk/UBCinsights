import Server from "../../src/rest/Server";
import InsightFacade from "../../src/controller/InsightFacade";
import {expect} from "chai";
import request, {Response} from "supertest";
import * as fs from "fs-extra";
import {Buffer} from "buffer";
import {clearDisk} from "../TestUtil";

describe("Server", () => {

	let facade: InsightFacade;
	let server: Server;
	let SERVER_URL = "http://localhost:4321";

	before(async () => {
		facade = new InsightFacade();
		server = new Server(4321);
		// TODO: start server here once and handle errors properly
		try {
			let result = await server.start();
			clearDisk();
		} catch (error) {
			console.log("Some error occurred with starting the server");
		}
	});

	after(async () => {
		// TODO: stop server here once!
		try {
			let result = await server.stop();
		} catch (error) {
			console.log("error stopping server");
		}
	});

	beforeEach(() => {
		// might want to add some process logging here to keep track of what's going on
		clearDisk();
		server.resetInsightFacade();
	});

	afterEach(() => {
		// might want to add some process logging here to keep track of what's going on
		clearDisk();
	});

	it("PUT test for courses dataset", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/pair.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/courses/sections")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res: Response) => {
					expect(res.status).to.be.equal(200);
				}).catch((error) => {
					expect.fail();
				});
		} catch (error) {
			console.log("error occurred");
		}
	});

	it("PUT test for rooms dataset", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/campus.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/rooms/rooms")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res: Response) => {
					expect(res.status).to.be.equal(200);
				}).catch((error) => {
					expect.fail();
				});
		} catch (error) {
			console.log("error occurred");
		}
	});

	it("DELETE test for dataset", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/campus.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/rooms/rooms")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then(() => {
					return request(SERVER_URL)
						.delete("/dataset/rooms")
						.then((res: Response) => {
							expect(res.status).to.be.equal(200);
						}).catch((error) => {
							expect.fail();
						});
				});
		} catch (error) {
			console.log("unexpected error");
		}
	});
	// it("")

	// Sample on how to format PUT requests
	/*
	it("PUT test for courses dataset", async () => {
		try {
			return request(SERVER_URL)
				.put(ENDPOINT_URL)
				.send(ZIP_FILE_DATA)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res: Response) => {
					expect(res.status).to.be.equal(200);
					// more assertions here
				})
				.catch((err) => {
					// some logging here please!
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
		}
	});
	 */

	// The other endpoints work similarly. You should be able to find all instructions at the chai-http documentation
});
