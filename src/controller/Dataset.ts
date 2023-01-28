class Dataset {
	public id: string;
	public courses: Course[];
	// Dataset must contain at least one valid Course

	constructor(id: string, courses: Course[]) {
		if (courses.length === 0) {
			throw new Error("Dataset must have at least one valid Course");
		}
		this.id = id;
		this.courses = courses;
	}
}
