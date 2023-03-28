import "./Query.scss";
import Result from "./Result";
import {InsightResult} from "./InsightFacadeUtil";
interface Props {
	state: string,
	visible: boolean,
	columns: string[],
	results: InsightResult[]
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
						props.results.map((values: InsightResult) => {
							return (<Result result={Object.values(values) as string[]} />);
						})
					}
				</table>
			</div>
		</div>
	);
}

export default QueryResults;
