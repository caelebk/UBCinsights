interface Props {
	result: string[];
}
function Result(props: Props) {
	return (
		<tr className="result">
			{
				props.result.map((value: string) => {
					return (<td>{value}</td>)
				})
			}
		</tr>
	);
}

export default Result;
