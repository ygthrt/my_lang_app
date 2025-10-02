import './App.css'
import { useState } from "react";
import Editor from './Editor';
import Outputs from './Outputs';
import Toolbar from './Toolbar';

function App() {
  const [code, setCode] = useState("");
  const [output, setOutput] = useState("");

  return (
    <div>
      <div className="app">
        <div className="input">
          <Editor code={code} changeEditor={setCode} />
        </div>
        <div className='output'>
          <Toolbar code={code} onOutput={setOutput} />
          <Outputs output={output} />
        </div>
      </div>
    </div>
  );
}

export default App;
