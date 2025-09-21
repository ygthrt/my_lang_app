import './App.css'
import { useState } from "react";

function App() {
  const [code, setCode] = useState("1 + 1");
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
