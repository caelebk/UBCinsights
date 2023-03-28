import React, { useState }from 'react';
import Navigation from "./pages/Navigation";
import Query from "./pages/Query";
import "./App.scss";

function App() {
	const states = ["Section", "Room"]
	const [state, setState] = useState(states[0]);
	const values: Map<string, string> = new Map<string, string>();
  return (
	<div className="App">
		<Navigation state={state} states={states} setState={setState} />
		<Query key={state} state={state} values={values} />
		<span className="footer">Inspired by PrairieLearn</span>
	</div>
  );
}

export default App;
