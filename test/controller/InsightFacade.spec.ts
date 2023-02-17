import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError
} from "../../src/controller/IInsightFacade";
import InsightFacade from "../../src/controller/InsightFacade";
import {folderTest} from "@ubccpsc310/folder-test";
import {expect, use} from "chai";
import chaiAsPromised from "chai-as-promised";
import {clearDisk, getContentFromArchives} from "../TestUtil";

use(chaiAsPromised);

describe("InsightFacade", function () {
	let facade: IInsightFacade;

	let zipFiles = {
		// small valid zip
		content : getContentFromArchives("examples.zip"),
		// empty zip
		blank : getContentFromArchives("blank.zip"),
		// Contains a folder called blank with valid sections
		noCoursesFolder : getContentFromArchives("nocoursesfolder.zip"),
		// Contains a course folder that is empty
		emptyCoursesFolder : getContentFromArchives("emptycourses.zip"),
		// Contains only invalid course jsons and course jsons with improper sections
		invalid : getContentFromArchives("invalid.zip"),
		// Not a zip file
		notZip : getContentFromArchives("notzip.txt"),
		// No sections in course json
		noSections : getContentFromArchives("nosections.zip"),
		// Pair data
		pair : getContentFromArchives("pair.zip"),
		// invalid results json
		invalidCourse : getContentFromArchives("invalidCourse.zip"),
		// invalid course file type
		invalidCourseType : getContentFromArchives("invalidCourseType.zip")

	};

	before(function () {
		// Just in case there is anything hanging around from a previous run of the test suite
		clearDisk();
	});

	describe("Add/Remove/List Dataset", function () {
		before(function () {
			console.info(`Before: ${this.test?.parent?.title}`);
		});

		beforeEach(function () {
			// This section resets the insightFacade instance
			// This runs before each test
			console.info(`BeforeTest: ${this.currentTest?.title}`);
			clearDisk();
			facade = new InsightFacade();
		});

		after(function () {
			console.info(`After: ${this.test?.parent?.title}`);
		});

		afterEach(function () {
			// This section resets the data directory (removing any cached data)
			// This runs after each test, which should make each test independent of the previous one
			console.info(`AfterTest: ${this.currentTest?.title}`);
			clearDisk();
		});

		// This is a unit test. You should create more like this!
		it ("should reject with  an empty dataset id", function() {
			const result = facade.addDataset("", zipFiles.pair, InsightDatasetKind.Sections);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});

		/**
		 * Overall Valid Everything
		 */
		it ("should successfully add a valid dataset",  () => {
			return expect(facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections))
				.to.eventually.deep.equals(["ubc"]);
		});

		it ("should successfully add multiple valid dataset",  async () => {
			try {
				await facade.addDataset("ubc", zipFiles.pair, InsightDatasetKind.Sections);
				return expect(facade.addDataset("ubc2", zipFiles.content, InsightDatasetKind.Sections))
					.to.eventually.deep.equals(["ubc","ubc2"]);
			} catch (err: any) {
				expect.fail("Error shouldn't have been thrown.");
			}
		});

		/**
		 * Crashes
		 */
		it ("should successfully add a valid dataset, crash, then list them",  async () => {
			await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
			let facade2: InsightFacade = new InsightFacade();
			return expect(facade2.listDatasets()).to.eventually.deep.equals([{
				id: "ubc",
				kind: InsightDatasetKind.Sections,
				numRows: 2
			} as InsightDataset]);
		});

		it ("should successfully add a valid dataset, crash, then add another dataset",  async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				let facade2: InsightFacade = new InsightFacade();
				return expect(facade2.addDataset("ubc2", zipFiles.pair, InsightDatasetKind.Sections))
					.to.eventually.deep.equals(["ubc","ubc2"]);
			} catch (err: any) {
				expect.fail("Error shouldn't have been thrown.");
			}
		});

		it ("Should successfully remove a valid id after a crash", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc1", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc2", zipFiles.content, InsightDatasetKind.Sections);
				let facade2: InsightFacade = new InsightFacade();
				return expect(facade2.removeDataset("ubc"))
					.to.eventually.be.equals("ubc");
			} catch (error: any) {
				expect.fail("Error shouldn't have been thrown");
			}
		});

		it ("should crash and list multiple datasets with their respective ID, kind, and number of rows", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc2", zipFiles.pair, InsightDatasetKind.Sections);
			} catch (error: any) {
				expect.fail("There shouldn't have been an error thrown during addition of datasets");
			}

			let facade2: InsightFacade = new InsightFacade();

			return expect(facade2.listDatasets()).to.eventually.be.deep.equals([{
				id: "ubc",
				kind: InsightDatasetKind.Sections,
				numRows: 2
			} as InsightDataset, {
				id: "ubc2",
				kind: InsightDatasetKind.Sections,
				numRows: 64612
			} as InsightDataset]);
		});

		/** *
		 *Invalid IDs
		 ***/
		it ("should reject a dataset with an underscore in the id",  () => {
			return expect(facade.addDataset("u_bc", zipFiles.content, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset with empty id",   () => {
			return expect(facade.addDataset("", zipFiles.content, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset with an id of spaces",   () => {
			return expect(facade.addDataset("     ", zipFiles.content, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject adding a dataset with an existing id",  async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
			} catch (error: any) {
				expect.fail("Error shouldn't have been thrown in the initial addition");
			}
			return expect(facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		/** *
		 * Invalid Content
		 ***/
		it ("should reject a dataset with empty content",   () => {
			return expect(facade.addDataset("ubc", "", InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset that is not a zip",   () => {
			return expect(facade.addDataset("ubc", zipFiles.notZip, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset with valid zip but no content",  () => {
			return expect(facade.addDataset("ubc", zipFiles.blank, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset with valid zip & sections but in non-courses folder",  () => {
			return expect(facade.addDataset("ubc", zipFiles.noCoursesFolder, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject a dataset with valid zip but no courses in course folder ",  () => {
			return expect(facade.addDataset("ubc", zipFiles.emptyCoursesFolder, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		// The jsons in this file contain random keys (not "result")
		it ("should reject dataset with valid zip & course folder but invalid course JSONs",  () => {
			return expect(facade.addDataset("ubc", zipFiles.invalid, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("should reject dataset with valid zip & course folder & valid json but no sections",  () => {
			return expect(facade.addDataset("ubc", zipFiles.noSections, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		// The json in this dataset has the "results" key but wrong type of value.
		it ("should reject dataset with valid zip & course folder but a results key with wrong type value", () => {
			return expect(facade.addDataset("ubc", zipFiles.invalidCourse, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});

		// The courses folder contains a txt file and a html file.
		it ("should reject dataset with valid zip & course folder but a course that is a diff file type", () => {
			return expect(facade.addDataset("ubc", zipFiles.invalidCourse, InsightDatasetKind.Sections))
				.to.eventually.be.rejectedWith(InsightError);
		});


		/** *
		 * Invalid InsightDatasetKind
		 ***/
		it ("should reject a dataset with rooms kind",   () => {
			return expect(facade.addDataset("ubc3", zipFiles.content, InsightDatasetKind.Rooms))
				.to.eventually.be.rejectedWith(InsightError);
		});

		/** *
		 * removeDataset Unit Tests
		 ***/
		it ("Should successfully remove a valid id", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc1", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc2", zipFiles.content, InsightDatasetKind.Sections);
				return expect(facade.removeDataset("ubc"))
					.to.eventually.be.equals("ubc");
			} catch (error: any) {
				expect.fail("Error shouldn't have been thrown");
			}
		});

		it ("Should successfully remove multiple valid id", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc1", zipFiles.content, InsightDatasetKind.Sections);
				await facade.removeDataset("ubc");
				await facade.removeDataset("ubc1");
				return expect(facade.listDatasets()).to.eventually.be.deep.equals([]);
			} catch (error: any) {
				expect.fail("Error shouldn't have been thrown");
			}
		});
		it ("Should reject an id with underscore", () => {
			return expect(facade.removeDataset("ub_c"))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("Should reject a blank id", () => {
			return expect(facade.removeDataset(""))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("Should reject a id with only spaces", () => {
			return expect(facade.removeDataset("         "))
				.to.eventually.be.rejectedWith(InsightError);
		});

		it ("Should reject a non-existent id", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc1", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc2", zipFiles.content, InsightDatasetKind.Sections);
				return expect(facade.removeDataset("ubc3"))
					.to.eventually.be.rejectedWith(NotFoundError);
			} catch (error: any) {
				expect.fail("Error shouldn't have been thrown during addition of datasets");
			}
		});

		/** *
		 * ListDataset Unit Tests
		 ***/
		it ("should list multiple datasets with their respective ID, kind, and number of rows", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
				await facade.addDataset("ubc2", zipFiles.pair, InsightDatasetKind.Sections);
			} catch (error: any) {
				expect.fail("There shouldn't have been an error thrown during addition of datasets");
			}

			return expect(facade.listDatasets()).to.eventually.be.deep.equals([{
				id: "ubc",
				kind: InsightDatasetKind.Sections,
				numRows: 2
			} as InsightDataset, {
				id: "ubc2",
				kind: InsightDatasetKind.Sections,
				numRows: 64612
			} as InsightDataset]);
		});

		it ("should list a single dataset with the respective ID, kind, and number of rows", async () => {
			try {
				await facade.addDataset("ubc", zipFiles.content, InsightDatasetKind.Sections);
			} catch (error: any) {
				expect.fail("There shouldn't have been an error thrown during addition of datasets");
			}
			return expect(facade.listDatasets()).to.eventually.be.deep.equals([{
				id: "ubc",
				kind: InsightDatasetKind.Sections,
				numRows: 2
			}]);
		});

		it ("should return an empty list if there are no datasets added", () => {
			return expect(facade.listDatasets()).to.eventually.be.deep.equals([]);
		});
	});

	/*
	 * This test suite dynamically generates tests from the JSON files in test/resources/queries.
	 * You should not need to modify it; instead, add additional files to the queries directory.
	 * You can still make tests the normal way, this is just a convenient tool for a majority of queries.
	 */
	describe("PerformQuery", () => {
		before(function () {
			console.info(`Before: ${this.test?.parent?.title}`);
			clearDisk();
			facade = new InsightFacade();

			// Load the datasets specified in datasetsToQuery and add them to InsightFacade.
			// Will *fail* if there is a problem reading ANY dataset.
			const loadDatasetPromises = [
				facade.addDataset("sections", zipFiles.pair, InsightDatasetKind.Sections),
				facade.addDataset("examples", zipFiles.content, InsightDatasetKind.Sections)
			];

			return Promise.all(loadDatasetPromises);
		});

		after(function () {
			console.info(`After: ${this.test?.parent?.title}`);
			clearDisk();
		});

		type PQErrorKind = "ResultTooLargeError" | "InsightError";

		folderTest<unknown, InsightResult[], PQErrorKind>(
			"Non-Ordered InsightFacade PerformQuery tests",
			(input) => facade.performQuery(input),
			"./test/resources/queries",
			{
				assertOnResult: (actual, expected: InsightResult[]) => {
					expect(actual).to.have.deep.members(expected);
					expect(actual).to.be.lengthOf(expected.length);
				},
				errorValidator: (error): error is PQErrorKind =>
					error === "ResultTooLargeError" || error === "InsightError",
				assertOnError: (actual, expected) => {
					if (expected === "ResultTooLargeError") {
						expect(actual).to.be.instanceof(ResultTooLargeError);
					} else if (expected === "InsightError") {
						expect(actual).to.be.instanceof(InsightError);
					} else {
						expect.fail("Unexpected error");
					}
				},
			}
		);

		folderTest<unknown, InsightResult[], PQErrorKind>(
			"Ordered InsightFacade PerformQuery tests",
			(input) => facade.performQuery(input),
			"./test/resources/queries/ordered",
			{
				assertOnResult: (actual, expected: InsightResult[]) => {
					expect(actual).to.have.deep.equals(expected);
					expect(actual).to.be.lengthOf(expected.length);
				},
				errorValidator: (error): error is PQErrorKind =>
					error === "ResultTooLargeError" || error === "InsightError",
				assertOnError: (actual, expected) => {
					if (expected === "ResultTooLargeError") {
						expect(actual).to.be.instanceof(ResultTooLargeError);
					} else if (expected === "InsightError") {
						expect(actual).to.be.instanceof(InsightError);
					} else {
						expect.fail("Unexpected error");
					}
				},
			}
		);
	});
});
