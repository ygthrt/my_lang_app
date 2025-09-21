import './App.css'
import { useState } from "react";

function App() {
  const [code, setCode] = useState("let rec staged_fact : (int-->int code) x:int = \n  if x = 1 then .<1>. \n  else .< x * .~(staged_fact (x-1))>.\nin\nstaged_fact 5");
  const [output, setOutput] = useState("");

  const handleRun = () => {
    try {
      const ty = mylang_lib.tcheck(code);
      const result = mylang_lib.eval(code);
      setOutput(`Type: ${ty}\nResult: ${result}`);
    } catch (e) {
      setOutput(`Error: ${e}`);
    }
  };

  return (
    <div className="app">
      <div className="input">
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          className="code"
        />
      </div>

      <div className="output" >
        <button onClick={handleRun}>Run</button>
        <pre>{output}</pre>
      </div>
    </div>
  );
}

export default App;
