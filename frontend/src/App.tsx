import React, { useState }from 'react';
import Navigation from "./pages/Navigation";
import Query from "./pages/Query";
import "./App.scss";

function App() {
	const states = ["Section", "Room"]
	const [state, setState] = useState(states[0]);

  return (
	<div className="App">
		<Navigation state={state} states={states} setState={setState} />
		<Query state={state}></Query>
		<span className="footer">Inspired by PrairieLearn</span>
	</div>
  );
}

export default App;
