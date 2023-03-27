import "./Query.scss";

interface Props {
	state: string
}

interface Options {
	[key: string]: string[];
}


function Query(props: Props) {
	const placeholderDataset: string[] = ["dataset1", "dataset2", "dataset3"];
	const placeholderProperty: string[] = ["property1", "property2", "property3"];
	const placeholderFilter: string[] = ["filter1", "filter2", "filter3"];
	const receivedOptions: Options = {
		Dataset: placeholderDataset,
		Property: placeholderProperty,
		Filter: placeholderFilter
	}
	const createOption = (option: string) => {
		return <option className="option" value={option}>{option}</option>
	}

	const createSelect = (name: string, list: string[]) => {
		return (
			<li className={"query" + name}>
				<span className="label">{name}:</span>
				<select className="selector" name={name}>
					{
						list.map((value: string) => {
							return createOption(value);
						})
					}
				</select>
			</li>
		);
	}
	return (
		<body key={props.state}>
			<div className="query queryOptions">
				<div className="queryHeader">
					<span className="queryTitle">{props.state}Query:</span>
				</div>
				<div className="queryContent">
					<ul className="queryFilterList">
						{
							Object.keys(receivedOptions).map((key: string) => {
								return createSelect(key, receivedOptions[key]);
							})
						}
						<li className="queryValue">
							<span className="label">Value:</span>
							<input type="text" className="selector" name="value"/>
						</li>
						<li className="querySubmit">
							<button>Submit</button>
						</li>
					</ul>
				</div>
			</div>
			<div className="query queryResults">
				<div className="queryHeader">
					<span className="queryTitle">{props.state}Results: </span>
				</div>
			</div>
		</body>
	);
}

export default Query;
