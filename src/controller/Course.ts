class Course {
	public title: string;
	public sections: Section[];

	constructor(title: string, sections: Section[]) {
		if (sections.length === 0) {
			throw new Error("Course must have at least one valid section");
		}
		this.title = title;
		this.sections = sections;
	}
}
