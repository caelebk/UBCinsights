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

	constructor(id: string, course: string, title: string, professor: string, subject: string,
		year: number, avg: number, pass: number, fail: number, audit: number) {
		this.id = id;
		this.course = course;
		this.title = title;
		this.professor = professor;
		this.subject = subject;
		this.year = year;
		this.avg = avg;
		this.pass = pass;
		this.fail = fail;
		this.audit = audit;
	}
}
