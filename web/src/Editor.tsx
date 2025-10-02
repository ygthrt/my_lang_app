
export default function Editor({ code, changeEditor }: { code: string; changeEditor: (value: string) => void }) {
    return (
        <textarea
            value={code}
            onChange={(e) => changeEditor(e.target.value)}
            className="code"
        />
    )
}
