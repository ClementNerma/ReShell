// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";

import Wasm from "../rust-wasm/target/rust-wasm/reshell_wasm_syntax_highlighter.js";

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate() {
	Wasm.warmup();

	const tokenTypes = Wasm.list_token_types();
	const tokenModifiers = Wasm.list_token_modifiers();

	const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

	const provider: vscode.DocumentSemanticTokensProvider = {
		provideDocumentSemanticTokens(document) {
			const tokensBuilder = new vscode.SemanticTokensBuilder(legend);

			for (const token of Wasm.highlight(document.getText())) {
				const { start, end, nature, modifier } = token;

				tokensBuilder.push(
					new vscode.Range(
						new vscode.Position(start.line, start.col),
						new vscode.Position(end.line, end.col),
					),
					nature,
					modifier ? [modifier] : [],
				);
			}

			return tokensBuilder.build();
		},
	};

	vscode.languages.registerDocumentSemanticTokensProvider(
		{ language: "reshell" },
		provider,
		legend,
	);
}

// This method is called when your extension is deactivated
export function deactivate() {}
