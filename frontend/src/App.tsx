import React, { useState }from 'react';
import Navigation from "./pages/Navigation";
import Query from "./pages/Query";

function App() {
	const states = ["Section", "Room"]
	const [state, setState] = useState(states[0]);

  return (
	<div className="App">
		<Navigation state={state} states={states} setState={setState} />
		<Query state={state}></Query>
	</div>
  );
}

export default App;
