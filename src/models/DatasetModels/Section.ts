export class Section {
	// variables are capitalized, so it can be written and read the same
	public id: string;
	public Course: string;
	public Title: string;
	public Professor: string;
	public Subject: string;
	public Year: number;
	public Avg: number;
	public Pass: number;
	public Fail: number;
	public Audit: number;

	constructor(json: {id: string, Course: string, Title: string, Professor: string, Subject: string,
		Year: number, Avg: number, Pass: number, Fail: number, Audit: number}) {
		this.id = json.id;
		this.Course = json.Course;
		this.Title = json.Title;
		this.Professor = json.Professor;
		this.Subject = json.Subject;
		this.Year = json.Year;
		this.Avg = json.Avg;
		this.Pass = json.Pass;
		this.Fail = json.Fail;
		this.Audit = json.Audit;
	}

	/**
	 * Returns true if section has all fields filled
	 * */
	public isValid(): boolean {
		for (const attribute in this) {
			if (this[attribute] === undefined) {
				return false;
			}
		}
		return true;
	}
}
