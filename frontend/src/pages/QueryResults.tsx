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
		</div>
	);
}

export default QueryResults;
