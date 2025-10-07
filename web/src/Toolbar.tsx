
export default function Toolbar({ code, onOutput }: { code: string, onOutput: (value: string) => void }) {

    const handleEval = () => {
        try {
            const result = mylang_lib.eval(code);
            onOutput(`Result:\n\n${result}`);
        } catch (e) {
            onOutput(`Error: ${e}`);
        }
    };

    const handleTcheck = () => {
        try {
            const ty = mylang_lib.tcheck(code);
            onOutput(`Type:\n\n${ty}`);
        } catch (e) {
            onOutput(`Error: ${e}`);
        }
    };


    return (
        <div className="tool">
            <button onClick={handleTcheck}>Type Check</button>
            <button onClick={handleEval}>Eval</button>
        </div>
    )
}
