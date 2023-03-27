import "./Query.scss";
import {useState} from "react";

interface Props {
	state: string
	values: Map<string, string>;
}

interface Options {
	[key: string]: string[];
}

function Query(props: Props) {
	const placeholderDataset: string[] = ["dataset1", "dataset2", "dataset3"];
	const placeholderProperty: string[] = ["property1", "property2", "property3"];
	const filters: string[] = ["IS", "GT", "LT", "EQ", "AND", "OR"];
	const singularFilters: string[] = ["IS", "GT", "LT", "EQ"];
 	const defaultOptions: Options = {
		Dataset: placeholderDataset,
		Filter1: filters,
	};
	const oneFilter: Options = {
		Property: placeholderProperty
	};
	const multipleFilterTemplate: Options = {
		Filter: singularFilters,
		Property: placeholderProperty,
	};

	const [multipleFilter, setMultipleFilter] = useState(false);

	const handleFirstFilter = (event: React.ChangeEvent<HTMLSelectElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		console.log(name);
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
		console.log(props.values);
	};

	const handleDropdownFilter = (event: React.ChangeEvent<HTMLSelectElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		props.values.set(name, value);
		console.log(props.values);
	};

	const handleInputFilter = (event: React.ChangeEvent<HTMLInputElement>) => {
		const value: string = event?.target?.value;
		const name: string = event?.target?.name;
		props.values.set(name, value);
		console.log(props.values);
	};

	const createOption = (option: string) => {
		return <option className="option" value={option}>{option}</option>
	};

	const createSelect = (name: string, list: string[],
						  changeHandler: (event: React.ChangeEvent<HTMLSelectElement>) => any) => {
		if (name !== "Filter1" && name !== String("Dataset")) {
			props.values.set(name, list[0]);
		} else {
			if (!props.values.has(name)) {
				props.values.set(name, list[0]);
			}
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
						props.values.set("Value" + (index + adder), "");
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
						<li className="querySubmit" onClick={()=>console.log(convertMapToJSON(props.values))}>
							<button>Query</button>
						</li>
					</ul>
				</div>
			</div>
			<div className="query queryResults">
				<div className="queryHeader">
					<span className="queryTitle">{props.state}Results: </span>
				</div>
			</div>
		</div>
	);
}

interface QueryObject {
	[key: string]: any;
}
function convertMapToJSON(map: Map<string, string>): object {
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

		}
	} else {

	}
	return json;
}

export default Query;
