import { Statement } from '../shared/ast'
import { CodeSection, Token } from '../shared/parsed'
import { matchUnion } from '../shared/utils'
import { err, ExecValue, Runner, RunnerResult, Scope, success } from './base'
import { runBlock } from './block'
import { runPropertyAccess } from './chainings'
import { runCmdCall } from './cmdcall'
import { runCondOrTypeAssertion, runDoubleOp, runExpr } from './expr'
import { expectValueType } from './value'

export const runStatement: Runner<Token<Statement>> = (stmt, ctx) =>
  matchUnion(stmt.parsed, 'type', {
    variableDecl: ({ varname, expr }) => {
      const scope = ctx.scopes[ctx.scopes.length - 1]
      const evaluated = runExpr(expr.parsed, ctx)
      if (evaluated.ok !== true) return evaluated

      scope.entities.set(varname.parsed, evaluated.data)
      return success(void 0)
    },

    assignment: ({ varname, propAccesses, prefixOp, listPush, expr }) => {
      let found: { scope: Scope; target: ExecValue } | null = null

      for (let s = ctx.scopes.length - 1; s >= 0; s--) {
        const entityValue = ctx.scopes[s].entities.get(varname.parsed)

        if (entityValue) {
          found = { scope: ctx.scopes[s], target: entityValue }
          break
        }
      }

      if (found === null) {
        return err(varname.at, 'internal error: variable not found in scope')
      }

      const computeValue = (leftAt: CodeSection, left: ExecValue | undefined): RunnerResult<ExecValue> => {
        const execExpr = runExpr(expr.parsed, ctx)
        if (execExpr.ok !== true) return execExpr
        if (prefixOp === null) return success(execExpr.data)

        if (left === undefined) {
          return err(leftAt, 'internal error: left value is undefined when applying prefix op during assignment')
        }

        return runDoubleOp(
          {
            leftAt,
            left,
            op: prefixOp,
            rightAt: expr.at,
            right: execExpr.data,
          },
          ctx
        )
      }

      let targetAt = varname.at
      let target = found.target

      if (propAccesses.length > 0) {
        const treatAtOnce = listPush ? propAccesses : propAccesses.slice(0, propAccesses.length - 1)

        for (const { at, parsed: propAccess } of treatAtOnce) {
          const resolved = runPropertyAccess({ propAccessAt: at, propAccess, value: target }, ctx)
          if (resolved.ok !== true) return resolved
          target = resolved.data
          targetAt = { start: targetAt.start, next: at.next }
        }

        if (listPush === null) {
          const last = propAccesses[propAccesses.length - 1]

          const written = runPropertyAccess(
            {
              propAccessAt: last.at,
              propAccess: last.parsed,
              value: target,
              write: (value) => computeValue(targetAt, value),
              writeAllowNonExistentMapKeys: prefixOp === null,
            },
            ctx
          )

          return written.ok !== true ? written : success(void 0)
        }
      }

      if (listPush) {
        if (target.type !== 'list') {
          return err(
            listPush.at,
            `internal error: expected left value to be a "list", found internal type "${target.type}"`
          )
        }

        const newItem = computeValue(targetAt, target)
        if (newItem.ok !== true) return newItem

        target.items.push(newItem.data)
        return success(void 0)
      }

      const newValue = computeValue(targetAt, target)
      if (newValue.ok !== true) return newValue

      found.scope.entities.set(varname.parsed, newValue.data)
      return success(void 0)
    },

    ifBlock: ({ cond, then, elif, els }) => {
      const result = runCondOrTypeAssertion(cond.parsed, ctx)
      if (result.ok !== true) return result

      const check = expectValueType(cond.at, result.data.result, 'bool')
      if (check.ok !== true) return check
      if (check.data.value) {
        return runBlock(
          then,
          result.data.type === 'assertion'
            ? { ...ctx, scopes: ctx.scopes.concat([result.data.normalAssertionScope]) }
            : ctx
        )
      }

      for (const { cond, body } of elif) {
        const result = runCondOrTypeAssertion(cond.parsed, ctx)
        if (result.ok !== true) return result

        const check = expectValueType(cond.at, result.data.result, 'bool')
        if (check.ok !== true) return check
        if (check.data.value)
          return runBlock(
            body,
            result.data.type === 'assertion'
              ? { ...ctx, scopes: ctx.scopes.concat([result.data.normalAssertionScope]) }
              : ctx
          )
      }

      return els
        ? runBlock(
            els,
            result.data.type === 'assertion'
              ? { ...ctx, scopes: ctx.scopes.concat([result.data.oppositeAssertionScope]) }
              : ctx
          )
        : success(void 0)
    },

    forLoop: ({ loopVar, subject, body }) => {
      const result = runExpr(subject.parsed, ctx)
      if (result.ok !== true) return result

      const list = expectValueType(subject.at, result.data, 'list')
      if (list.ok !== true) return list

      const iterateOn = list.data.items.slice()

      const scope: Scope = { generics: [], methods: [], entities: new Map() }
      ctx = { ...ctx, scopes: ctx.scopes.concat(scope) }

      for (const value of iterateOn) {
        scope.entities.set(loopVar.parsed, value)

        const result = runBlock(body, ctx)
        if (result.ok === null && result.breaking === 'continue') continue
        if (result.ok === null && result.breaking === 'break') break
        if (result.ok !== true) return result
      }

      return success(void 0)
    },

    forLoopDuo: ({ keyVar, valueVar, subject, body }) => {
      const evalSubject = runExpr(subject.parsed, ctx)
      if (evalSubject.ok !== true) return evalSubject

      let iterator: Iterable<[number | string, ExecValue]>

      if (evalSubject.data.type === 'list') {
        iterator = evalSubject.data.items.entries()
      } else if (evalSubject.data.type === 'map') {
        iterator = evalSubject.data.entries.entries()
      } else {
        return err(
          subject.at,
          `internal error: type mismatch (expected a list or map, found "${evalSubject.data.type}")`
        )
      }

      // This is required to avoid problems if the subject would happen to be modified
      // while the loop was being run
      const iterateOn = [...iterator]

      const scope: Scope = { generics: [], methods: [], entities: new Map() }
      ctx = { ...ctx, scopes: ctx.scopes.concat(scope) }

      for (const [key, value] of iterateOn) {
        const iterValue: ExecValue =
          typeof key === 'number' ? { type: 'int', value: key } : { type: 'string', value: key }

        scope.entities.set(keyVar.parsed, iterValue)
        scope.entities.set(valueVar.parsed, value)

        const result = runBlock(body, ctx)
        if (result.ok === null && result.breaking === 'continue') continue
        if (result.ok === null && result.breaking === 'break') break
        if (result.ok !== true) return result
      }

      return success(void 0)
    },

    whileLoop: ({ cond, body }) => {
      for (;;) {
        const result = runCondOrTypeAssertion(cond.parsed, ctx)
        if (result.ok !== true) return result

        const evalCond = expectValueType(cond.at, result.data.result, 'bool')
        if (evalCond.ok !== true) return evalCond
        if (!evalCond.data.value) break

        const exec = runBlock(
          body,
          result.data.type === 'assertion'
            ? { ...ctx, scopes: ctx.scopes.concat([result.data.normalAssertionScope]) }
            : ctx
        )

        if (exec.ok === null && exec.breaking === 'continue') continue
        if (exec.ok === null && exec.breaking === 'break') break
        if (exec.ok !== true) return exec
      }

      return success(void 0)
    },

    continue: () => ({ ok: null, breaking: 'continue' }),

    break: () => ({ ok: null, breaking: 'break' }),

    typeAlias: () => success(void 0),

    enumDecl: () => success(void 0),

    match: ({ subject, arms }) => {
      const result = runExpr(subject.parsed, ctx)
      if (result.ok !== true) return result

      const evalSubject = expectValueType(subject.at, result.data, 'enum')
      if (evalSubject.ok !== true) return evalSubject

      const relevantArm =
        arms.parsed.find((arm) => arm.variant.parsed === evalSubject.data.variant) ??
        arms.parsed.find((arm) => arm.variant.parsed === '_')

      if (!relevantArm) {
        return err(
          arms.at,
          `internal error: no match arm nor fallback found for current variant "${evalSubject.data.variant}"`
        )
      }

      return runBlock(relevantArm.matchWith.parsed, ctx)
    },

    // Nothing to do (already treated in blocks' runtime)
    fnDecl: () => success(void 0),
    methodDecl: () => success(void 0),

    return: ({ expr }) => {
      if (!expr) return { ok: null, breaking: 'return' as const, value: null }

      const evalRetExpr = runExpr(expr.parsed, ctx)
      if (evalRetExpr.ok !== true) return evalRetExpr

      return { ok: null, breaking: 'return' as const, value: evalRetExpr.data }
    },

    panic: ({ message }) => {
      const expr = runExpr(message.parsed, ctx)
      if (expr.ok !== true) return expr

      const messageStr = expectValueType(message.at, expr.data, 'string')
      if (messageStr.ok !== true) return messageStr

      return err(message.at, `Panicked: ${messageStr.data.value}`)
    },

    cmdCall: ({ content }) => runCmdCall(content, ctx),

    cmdDecl: () => success(void 0),

    fileInclusion: () => err(stmt.at, 'internal error: file inclusion was not flattened before running statement'),

    runExpr: ({ content }) => {
      const result = runExpr(content.parsed, ctx)
      return result.ok === true ? success(void 0) : result
    },
  })
