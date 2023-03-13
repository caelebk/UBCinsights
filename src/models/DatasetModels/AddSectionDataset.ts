import JSZip from "jszip";
import {Course} from "./Course";

export function getSectionFileNamesAndData(zip: JSZip) {
	let fileNames: string[] = [];
	let fileDataPromises: Array<Promise<string>> = [];
	// open course folder
	zip.folder("courses")?.forEach((relativePath, file) => {
		// read all files and push into a list
		fileNames.push(relativePath);
		fileDataPromises.push(file.async("string"));
	});
	return Promise.all(fileDataPromises)
		.then((fileData) => {
			return {fileNames, fileData};
		});
}

export function getValidCoursesFromNamesAndData(fileNames: string[], fileData: string[]) {
	let courses: Course[] = fileData.map((file, index) => {
		return new Course(fileNames[index], JSON.parse(file));
	});
	let validCourses: Course[] = [];
	// if a course is valid, filter to only the valid sections and add the list of valid courses
	for (const course of courses) {
		if (course.isValid()) {
			course.filterSections();
			validCourses.push(course);
		}
	}
	return validCourses;
}
