export class Section {
	public id: string;
	public course: string;
	public title: string;
	public professor: string;
	public subject: string;
	public year: number;
	public avg: number;
	public pass: number;
	public fail: number;
	public audit: number;

	constructor(data: {id: string, Course: string, Title: string, Professor: string, Subject: string,
		Year: number, Avg: number, Pass: number, Fail: number, Audit: number}) {
		this.id = data.id;
		this.course = data.Course;
		this.title = data.Title;
		this.professor = data.Professor;
		this.subject = data.Subject;
		this.year = data.Year;
		this.avg = data.Avg;
		this.pass = data.Pass;
		this.fail = data.Fail;
		this.audit = data.Audit;
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
