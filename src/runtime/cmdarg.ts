import { CmdArg } from '../shared/ast'
import { CodeSection } from '../shared/parsed'
import { matchUnion } from '../shared/utils'
import { err, ExecValue, Runner, RunnerContext, RunnerResult, success } from './base'
import { runExpr } from './expr'
import { executeFnCallByName } from './fncall'
import { getEntityInScope } from './scope'
import { runValue } from './value'

export const runCmdArg: Runner<CmdArg, string> = (cmdArg, ctx) =>
  matchUnion(cmdArg, 'type', {
    flag: ({ prefixSym, name, directValue }) => {
      const out: string[] = [prefixSym.parsed, name.parsed]

      if (directValue) {
        out.push('=')

        const execValue = runExpr(directValue.parsed, ctx)
        if (execValue.ok !== true) return execValue

        const stringified = stringifyExecValue(directValue.at, execValue.data, ctx)
        if (stringified.ok !== true) return stringified

        out.push(stringified.data)
      }

      return success(out.join(''))
    },

    action: ({ name }) => success(name.parsed),

    expr: ({ expr }) => {
      const execExpr = runExpr(expr.parsed, ctx)
      return execExpr.ok === true ? stringifyExecValue(expr.at, execExpr.data, ctx) : execExpr
    },

    fnCall: ({ content }) => {
      const execCall = executeFnCallByName(content.parsed.name, ctx)
      return execCall.ok === true ? stringifyExecValue(content.at, execCall.data, ctx) : execCall
    },

    value: ({ value }) => {
      const execValue = runValue(value, ctx)
      return execValue.ok === true ? stringifyExecValue(value.at, execValue.data, ctx) : execValue
    },

    rest: ({ varname }) => {
      const value = getEntityInScope(varname, ctx)
      return value.ok === true ? stringifyExecValue(varname.at, value.data, ctx) : value
    },
  })

/**
 * Convert a command-line argument to a string
 * Used to pass arguments to external commands who will only accept string values due to compatibility
 */
function stringifyExecValue(at: CodeSection, value: ExecValue, ctx: RunnerContext): RunnerResult<string> {
  switch (value.type) {
    case 'int':
    case 'float':
      return success(value.value.toString())

    case 'string':
      return success(value.value)

    case 'path':
      return success(value.segments.join(ctx.platformPathSeparator))

    default:
      return err(
        at,
        `internal error: expected command argument to be either "number", "string" or "path", found internal type "${value.type}`
      )
  }
}
