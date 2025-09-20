# MyLang Web Tool

研究（卒論）で作った言語の処理系をWeb上で試せるツールです。

研究成果を実際に動かせる形にすること、およびTypeScript/Reactの学習を目的として開発しました。

言語仕様としてはTypeScriptのany型のような型と型安全なメタプログラミングの機構を持つ関数型言語です。

詳しくは以下の論文を参照してください: Staged Gradual Typing(GPCE 2025)

[DOIリンク](https://dl.acm.org/doi/10.1145/3742876.3742880)  
 
言語処理系としては素朴な実装ですが、OCamlで作った処理系を`js_of_ocaml`でブラウザ上で動かせる点が特徴です。

---
> [!WARNING]
> ## 注意事項
> - 現状まだ作りかけです
> - 動作がまだ不安定な場合があります
> - 論文との構文の違いやバグ等がまだあります

---

## 技術スタック
- 言語処理系: OCaml
- Webラッパー: TypeScript + React
- ビルドツール: Dune, js_of_ocaml

---

## 現状の機能
言語処理系の以下の機能を呼べるようにしました。
- 基本的な構文解析
- 型検査
- 実行時型検査の機構の挿入(詳しくは論文参照)
- 評価

---

## 今後の予定 (TODO)
- Reactを使ってUI作成
  - コマンドライン機能
  - EvalとTypeCheck機能
  - (簡単なエディター機能)
- デプロイ
- エラーメッセージ改善

- 構文をocamlに合わせる
- ocaml関数の型定義ファイルの作成

---

## 開発・ビルド方法
```bash

# 依存関係をインストール
opam install dune js_of_ocaml
cd web && npm install

# ビルド
npm run build

# Webで動作確認
npm run start

```

---

## バージョン

- OCaml: 5.3.0
- js_of_ocaml: 5.9.1
- Node.js: v22.14.0
- TypeScript: 5.8.x
- React: 19.1.x
- Vite: 7.1.x

