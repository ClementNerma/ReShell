import { Writable } from 'stream'
import { Block, ClosureBody, FnType, MethodInfos, ValueType } from '../shared/ast'
import { diagnostic, Diagnostic, DiagnosticLevel } from '../shared/diagnostics'
import { CodeSection, Token } from '../shared/parsed'
import { PrecompData } from '../shared/precomp'

export type Runner<T, RetType = void> = (token: T, ctx: RunnerContext) => RunnerResult<RetType>

export type RunnerResult<T> =
  // Success
  | { ok: true; data: T }
  // Critical error
  | { ok: false; diag: Diagnostic }
  // Loop breaking or continue
  | { ok: null; breaking: 'continue' | 'break' }
  // A value needs to be returned from the current function
  | { ok: null; breaking: 'return'; value: ExecValue | null }

/**
 * Global runner context
 */
export type RunnerContext = {
  /** Hierarchical scopes */
  scopes: Scope[]

  /** Pipes output needs to be written to */
  pipeTo: null | {
    stdout: Writable
    stderr: Writable
  }

  // Pre-computed data
  typeAliases: PrecompData['typeAliases']
  callbackTypes: PrecompData['callbackTypes']
  fnOrCmdCalls: PrecompData['fnOrCmdCalls']
  closuresArgsMapping: PrecompData['closuresArgsMapping']

  // Platform path separator (usually "/" or "\")
  platformPathSeparator: string

  // Command-line arguments
  argv: string[]

  // Emit a diagnostic (notice, warning or non-critical error)
  emitDiagnostic: (diagnostic: Diagnostic) => void
}

export const createRunnerContext = (
  precompData: PrecompData,
  platformPathSeparator: string,
  argv: string[],
  diagnosticHandler: RunnerContext['emitDiagnostic']
): RunnerContext => ({
  scopes: [],
  pipeTo: null,
  typeAliases: precompData.typeAliases,
  callbackTypes: precompData.callbackTypes,
  fnOrCmdCalls: precompData.fnOrCmdCalls,
  closuresArgsMapping: precompData.closuresArgsMapping,
  platformPathSeparator,
  argv,
  emitDiagnostic: diagnosticHandler,
})

export type Scope = {
  generics: { name: string; orig: CodeSection; resolved: ValueType }[]
  methods: ScopedMethod[]
  entities: Map<string, ExecValue>
}

export type ScopedMethod = { infos: MethodInfos; body: Token<Block> }

/**
 * Value generated during execution
 */
export type ExecValue =
  | { type: 'null' }
  | { type: 'bool'; value: boolean }
  | { type: 'int'; value: number }
  | { type: 'float'; value: number }
  | { type: 'string'; value: string }
  | { type: 'path'; segments: string[] }
  | { type: 'list'; items: ExecValue[] }
  | { type: 'map'; entries: Map<string, ExecValue> }
  | { type: 'struct'; members: Map<string, ExecValue> }
  | { type: 'enum'; variant: string }
  | { type: 'fn'; body: ClosureBody; fnType: FnType; argsMapping: Map<string, string | null> | null }
  | { type: 'failable'; success: boolean; value: ExecValue }
  | { type: 'rest'; content: string[] }

export function success<T>(data: T): RunnerResult<T> {
  return { ok: true, data }
}

export function err<T>(at: CodeSection, message: string): RunnerResult<T> {
  return { ok: false, diag: diagnostic(at, message, DiagnosticLevel.Error) }
}

export function ensureCoverage(_: never): never {
  throw new Error('Internal error: reached a theorically unreachable statement')
}
