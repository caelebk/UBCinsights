import "./Query.scss";
import {useEffect, useState} from "react";
import QueryResults from "./QueryResults";
import {getDatasets, InsightResult, sendQuery} from "./InsightFacadeUtil";

interface Props {
	state: string
	values: Map<string, string>;
}

interface Options {
	[key: string]: string[];
}

function Query(props: Props) {
	const [datasets, setDatasets] = useState([] as string[]);
	const [multipleFilter, setMultipleFilter] = useState(false);
	const [visible, setVisible] = useState(false);
	const [results, setResults] = useState([] as InsightResult[]);

	useEffect(() => {
		getDatasets(props.state).then((value: string[]) => {
			setDatasets(value);
		});
	}, [props.state]);

	let properties: string[];
	let columns: string[];
	if (props.state === "Section") {
		properties = ["dept", "id", "uuid", "instructor", "avg", "year"];
		columns = properties;
	} else {
		properties = ["seats", "fullname", "shortname", "number", "address"];
		columns = properties.concat(["href"]);
	}
	const filters: string[] = ["IS", "GT", "LT", "EQ", "AND", "OR"];
	const singularFilters: string[] = ["IS", "GT", "LT", "EQ"];
 	const defaultOptions: Options = {
		Dataset: datasets,
		Filter1: filters,
	};
	const oneFilter: Options = {
		Property: properties
	};
	const multipleFilterTemplate: Options = {
		Filter: singularFilters,
		Property: properties,
	};
	const handleFirstFilter = (event: React.ChangeEvent<HTMLSelectElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		if (name !== "Dataset") {
			if (value && (value === "AND" || value === "OR")) {
				setMultipleFilter(true);
			} else {
				if (multipleFilter) {
					setMultipleFilter(false);
					const dataset = props.values.get("Dataset");
					const filter1 = props.values.get("Filter1");
					props.values.clear();
					if (filter1) {
						props.values.set("Filter1", filter1);
					}
					if (dataset) {
						props.values.set("Dataset", dataset);
					}
				}
			}
		}
		props.values.set(name, value);
	};

	const handleDropdownFilter = (event: React.ChangeEvent<HTMLSelectElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		props.values.set(name, value);
	};

	const handleInputFilter = (event: React.ChangeEvent<HTMLInputElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		props.values.set(name, value);
	};

	const createOption = (option: string) => {
		return <option className="option" value={option}>{option}</option>
	};

	const createSelect = (name: string, list: string[],
						  changeHandler: (event: React.ChangeEvent<HTMLSelectElement>) => any) => {
		if (!props.values.has(name)) {
			props.values.set(name, list[0]);
		}
		return (
			<li className={"query" + name}>
				<span className="label">{name}:</span>
				<select className="selector" name={name} onChange={changeHandler} key={name}>
					{
						list.map((value: string) => {
							return createOption(value);
						})
					}
				</select>
			</li>
		);
	};

	const createFilterSelect = (options: Options[]) => {
		return (
			<div>
				{
					options.map((option: Options, index: number) => {
						const adder: number = options.length > 1 ? 2 : 1;
						if (!props.values.has("Value" + (index+adder))) {
							props.values.set("Value" + (index + adder), "");
						}
						return (
							<div>
								{
									Object.keys(option).map((key: string) => {
										const name: string = key + (index + adder);
										return createSelect(name, option[key],
											handleDropdownFilter);
									})
								}
								<li className="queryValue">
									<span className="label">Value{index + adder}:</span>
									<input type="text" className="selector" defaultValue=""
										   name={"Value" + (index + adder)} onChange = {handleInputFilter}
									key={"Value" + (index + adder)}/>
								</li>
							</div>
						);
					})
				}
			</div>

		);
	};

	return (
		<div>
			<div className="query queryOptions">
				<div className="queryHeader">
					<span className="queryTitle">{props.state}Query:</span>
				</div>
				{
					datasets.length > 0 ? (
						<div className="queryContent">
							<ul className="queryFilterList">
								{
									Object.keys(defaultOptions).map((key: string) => {
										return createSelect(key, defaultOptions[key], handleFirstFilter);
									})
								}
								{
									multipleFilter ? createFilterSelect([multipleFilterTemplate, multipleFilterTemplate])
										: createFilterSelect([oneFilter])
								}
								<li className="querySubmit" onClick={()=> {
									let json = convertMapToJSON(props.values, props.state, columns);
									const validJson: boolean = Object.keys(json).length > 0;
									if (validJson) {
										sendQuery(JSON.stringify(json))
											.then((value: InsightResult[]) => {
												setResults(value.slice(0, 15));
											})
											.catch((error: string) => {
												alert("An error has occurred. \n" + error);
											});
									}
									validJson ? setVisible(true) : setVisible(false);
								}}>
									<button>Query</button>
								</li>
							</ul>
						</div>
					) : (<div className="queryContent">
						<ul className="queryFilterList">
							<li>
								No Datasets Added.
							</li>
						</ul>
					</div>)
				}
			</div>
			<QueryResults state={props.state} visible={visible} columns={columns} results={results}/>
		</div>
	);
}

interface QueryObject {
	[key: string]: any;
}

function convertMapToJSON(map: Map<string, string>, state: string, columns: string[]): object {
	let json: QueryObject = {}
	const filter = map.get("Filter1");
	const property = map.get("Property1");
	const value = map.get("Value1");
	const dataset = map.get("Dataset");
	if (map.size === 4) {
		if (filter && property && value && dataset) {
			const keyValue = dataset + "_" + property;
			const comparator: QueryObject = {};
			comparator[keyValue] = !isNaN(Number(value)) ? Number(value) : value;
			const filterComparator: QueryObject = {};
			filterComparator[filter] = comparator;
			json["WHERE"] = filterComparator;
		} else {
			alert("An error has occurred. Missing Field.");
			return {};
		}
	} else if (map.size === 10) {
		const filter2 = map.get("Filter2");
		const property2 = map.get("Property2");
		const value2 = map.get("Value2");
		const filter3 = map.get("Filter3");
		const property3 = map.get("Property3");
		const value3 = map.get("Value3");
		const validateUndefined = filter && dataset && filter2 && property2 && value2 && property3 && value3 && filter3;
		if (validateUndefined) {
			const comparator1: QueryObject = {};
			const comparator2: QueryObject = {};
			comparator1[dataset + "_" + property2] = !isNaN(Number(value2)) ? Number(value2) : value2;
			comparator2[dataset + "_" + property3] = !isNaN(Number(value3)) ? Number(value3) : value3;
			const filterComparator1: QueryObject = {};
			const filterComparator2: QueryObject = {};
			filterComparator1[filter2] = comparator1;
			filterComparator2[filter3] = comparator2;
			const filterAggregator: QueryObject = {};
			filterAggregator[filter] = [filterComparator1, filterComparator2];
			json["WHERE"] = filterAggregator;
		} else {
			alert("An error has occurred. Missing field.");
			return {};
		}
	} else {
		alert("An unknown error has occurred; please refresh page");
		return {};
	}
	json["OPTIONS"] = {
		"COLUMNS" : columns.map((column: string) => dataset.concat("_", column))
	}
	return json;
}

export default Query;
