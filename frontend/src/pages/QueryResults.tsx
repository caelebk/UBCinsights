import "./Query.scss";
interface Props {
	state: string
	visible: boolean
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
						<th>test</th>
						<th>test2</th>
						<th>test2</th>
						<th>test2</th>
						<th>test2</th>
						<th>test2</th>
					</tr>
					<tr>
						<td>temp</td>
						<td>temp2</td>
						<td>temp2</td>
						<td>temp2</td>
						<td>temp2</td>
						<td>temp2</td>
					</tr>

				</table>
			</div>
		</div>
	);
}

export default QueryResults;
