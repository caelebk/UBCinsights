import "./Navigation.scss";

interface Props {
	state: string,
	setState: Function,
	states: string[]
}
function Navigation(props: Props) {
	let notState: string = props.state === props.states[0] ? props.states[1] : props.states[0]

	return (
		<nav className="navbar">
			<span className="title">{props.state}Learn</span>
			<span className="subtitle" onClick={()=>{props.setState(notState)}}>
			{notState}
			</span>
		</nav>
	);
}

export default Navigation;
