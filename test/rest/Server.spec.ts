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
			expect.fail("unexpected error");
		}
	});

	it("PUT test for courses dataset twice with same ID", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/pair.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/courses/sections")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res: Response) => {
					expect(res.status).to.be.equal(200);
				}).then(() => {
					return request(SERVER_URL).put("/dataset/courses/sections")
						.send(buffer)
						.set("Content-Type", "application/x-zip-compressed")
						.then((res: Response) => {
							expect(res.status).to.be.equal(400);
						});
				}).catch((error) => {
					expect.fail();
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});

	it("PUT test for bad courses dataset", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/emptycourses.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/courses/sections")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res: Response) => {
					expect(res.status).to.be.equal(400);
				}).catch((error) => {
					expect.fail();
				});
		} catch (error) {
			expect.fail("unexpected error");
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
			expect.fail("unexpected error");
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
			expect.fail("unexpected error");
		}
	});

	it("DELETE test for dataset with NotFoundError", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/campus.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/rooms/rooms")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then(() => {
					return request(SERVER_URL)
						.delete("/dataset/randomValidId")
						.then((res: Response) => {
							expect(res.status).to.be.equal(404);
						}).catch((error) => {
							expect.fail();
						});
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});
	it("DELETE test for dataset with InsightError", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/campus.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/rooms/rooms")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then(() => {
					return request(SERVER_URL)
						.delete("/dataset/roo_m")
						.then((res: Response) => {
							expect(res.status).to.be.equal(400);
						}).catch((error) => {
							expect.fail();
						});
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});

	it("POST test for dataset query", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/pair.zip");
			let buffer = Buffer.from(data);
			let query = {
				WHERE: {
					GT: {
						sections_avg: 99.1
					}
				},
				OPTIONS: {
					COLUMNS: [
						"sections_dept",
						"sections_id",
						"sections_avg"
					],
					ORDER: "sections_id"
				}
			};
			return request(SERVER_URL)
				.put("/dataset/sections/sections")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res1: Response) => {
					expect(res1.status).to.be.equal(200);
					return request(SERVER_URL)
						.post("/query")
						.send(query)
						.then((res2: Response) => {
							expect(res2.status).to.be.equal(200);
						}).catch((error) => {
							expect.fail();
						});
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});

	it("POST test for dataset query with bad query", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/pair.zip");
			let buffer = Buffer.from(data);
			let query = {
				WHERE: "we got some random text here",
				SOMETHING: "bad bad bad stuff"
			};
			return request(SERVER_URL)
				.put("/dataset/sections/sections")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((res1: Response) => {
					expect(res1.status).to.be.equal(200);
					return request(SERVER_URL)
						.post("/query")
						.send(query)
						.then((res2: Response) => {
							expect(res2.status).to.be.equal(400);
						}).catch((error) => {
							expect.fail();
						});
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});

	it("GET test for listing datatsets", async () => {
		try {
			let data = fs.readFileSync("test/resources/archives/campus.zip");
			let buffer = Buffer.from(data);
			return request(SERVER_URL)
				.put("/dataset/rooms/rooms")
				.send(buffer)
				.set("Content-Type", "application/x-zip-compressed")
				.then((asdfasdf) => {
					return request(SERVER_URL)
						.get("/datasets")
						.then((res: Response) => {
							expect(res.status).to.be.equal(200);
							let responseData = JSON.parse(res.text);
							expect(responseData.result.length).to.be.equal(1);
						}).catch((error) => {
							expect.fail(error);
						});
				});
		} catch (error) {
			expect.fail("unexpected error");
		}
	});

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
