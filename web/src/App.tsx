import { useState } from "react";

function App() {
  const [code, setCode] = useState("");
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
    <div style={{ display: "flex", height: "100vh" }}>
      {/* 左: 入力欄 */}
      <div style={{ flex: 1, padding: "1rem" }}>
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          style={{ width: "100%", height: "100%", fontFamily: "monospace" }}
        />
      </div>

      {/* 右: 出力欄 */}
      <div style={{ width: "40%", padding: "1rem", background: "#f4f4f4" }}>
        <button onClick={handleRun}>Run</button>
        <pre>{output}</pre>
      </div>
    </div>
  );
}

export default App;
