import "./Query.scss";
import Result from "./Result";
interface Props {
	state: string,
	visible: boolean,
	columns: string[],
	results: string[][]
}
function QueryResults(props: Props) {
	return (
		<div className="query queryResults" style={{visibility: props.visible ? "visible" : "hidden"}}>
			<div className="queryHeader">
				<span className="queryTitle">{props.state}Results: </span>
			</div>
			<div className="queryResultsContent">
				<table className="queryResultsTable">
					<tr className="queryResultsHeader">
						{
							props.columns.map((column: string) => (<th> {column} </th>))
						}
					</tr>
					{
						props.results.map((values: string[]) => {
							return (<Result result={values} />);
						})
					}
				</table>
			</div>
		</div>
	);
}

export default QueryResults;
