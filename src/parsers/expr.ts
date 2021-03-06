import {
  AssertionContent,
  CondOrTypeAssertion,
  ElIfExpr,
  Expr,
  ExprElement,
  ExprElementContent,
  ExprOrNever,
  TypeAssertionAgainst,
} from '../shared/ast'
import { valueChaining } from './chaining'
import { Parser } from './lib/base'
import { combine } from './lib/combinations'
import { extract } from './lib/conditions'
import { never, not } from './lib/consumeless'
import { failure } from './lib/errors'
import { maybe_s, maybe_s_nl, s } from './lib/littles'
import { takeWhile } from './lib/loops'
import { exact, oneOfMap } from './lib/matchers'
import { mappedCases } from './lib/switches'
import { map, toOneProp } from './lib/transform'
import { selfRef, withLatelyDeclared } from './lib/utils'
import { doubleOp, singleOp } from './operators'
import { identifier } from './tokens'
import { valueType } from './types'
import { value } from './value'

export const exprElementContent: Parser<ExprElementContent> = selfRef((simpleExpr) =>
  mappedCases<ExprElementContent>()(
    'type',
    {
      // "(" expr ")"
      paren: map(
        combine(
          combine(exact('('), maybe_s_nl),
          failure(
            withLatelyDeclared(() => expr),
            'expected an expression after an opening parenthesis'
          ),
          combine(maybe_s_nl, exact(')'))
        ),
        ([_, inner]) => ({
          inner,
        })
      ),

      // <single operator> s expr
      singleOp: map(
        combine(
          singleOp,
          maybe_s,
          failure(
            withLatelyDeclared(() => simpleExpr),
            'expected an expression after the operator'
          )
        ),
        ([op, _, right]) => ({ op, right })
      ),

      // if <cond> { <then> } else { <else> }
      ternary: map(
        combine(
          combine(exact('if'), s),
          failure(
            withLatelyDeclared(() => condOrTypeAssertion),
            'expected a condition'
          ),
          combine(maybe_s_nl, exact('{', 'expected an opening brace ({)'), maybe_s_nl),
          failure(
            withLatelyDeclared(() => exprOrNever),
            'expected an expression'
          ),
          combine(maybe_s_nl, exact('}', 'expected a closing brace (}) to close the "if" body'), maybe_s_nl),
          extract(
            takeWhile<ElIfExpr>(
              map(
                combine(
                  combine(exact('elif'), s),
                  failure(
                    withLatelyDeclared(() => condOrTypeAssertion),
                    'expecting a condition'
                  ),
                  combine(maybe_s_nl, exact('{', 'expected an opening brace ({)'), maybe_s_nl),
                  failure(
                    withLatelyDeclared(() => exprOrNever),
                    'expected an expression'
                  ),
                  combine(maybe_s_nl, exact('}', 'expected a closing brace (}) to close the "elif" body'))
                ),
                ([_, cond, __, expr]) => ({ cond, expr })
              ),
              { inter: maybe_s_nl, interExpect: false }
            )
          ),
          combine(
            maybe_s_nl,
            exact('else', 'expected an "else" variant'),
            maybe_s_nl,
            exact('{', 'expected an opening brace ({)'),
            maybe_s_nl
          ),
          failure(
            withLatelyDeclared(() => exprOrNever),
            'expected an expression'
          ),
          combine(maybe_s_nl, exact('}', 'expected a closing brace (}) to close the "else" body'))
        ),
        ([_, cond, __, then, ___, { parsed: elif }, ____, els]) => ({ cond, then, elif, els })
      ),

      // value
      value: map(value, (_, content) => ({ content })),

      // Internal
      synth: never(),
    },
    'failed to parse expression'
  )
)

export const exprElement: Parser<ExprElement> = map(
  combine(exprElementContent, takeWhile(withLatelyDeclared(() => valueChaining))),
  ([content, { parsed: chainings }]) => ({ content, chainings })
)

export const expr: Parser<Expr> = map(
  combine(
    exprElement,
    takeWhile(
      map(
        combine(maybe_s, doubleOp, maybe_s_nl, failure(exprElement, 'expected an expression after the operator')),
        ([_, op, __, right]) => ({
          type: 'doubleOp',
          op,
          right,
        })
      )
    )
  ),
  ([from, { parsed: doubleOps }]) => ({
    from,
    doubleOps,
  })
)

export const typeAssertionAgainst: Parser<TypeAssertionAgainst> = mappedCases<TypeAssertionAgainst>()('against', {
  null: map(exact('null'), () => ({})),
  ok: map(exact('ok'), () => ({})),
  err: map(exact('err'), () => ({})),
  custom: map(valueType, (_, type) => ({ type })),
})

const assertionContent: Parser<AssertionContent> = map(
  combine(
    oneOfMap([
      ['isnt', true],
      ['is', false],
    ]),
    failure(s, 'expected space after "is" or "isnt" keyword'),
    failure(typeAssertionAgainst, 'expected an assertion type')
  ),
  ([{ parsed: inverted }, _, minimum]) => ({ inverted, minimum })
)

export const condOrTypeAssertion: Parser<CondOrTypeAssertion> = mappedCases<CondOrTypeAssertion>()('type', {
  directAssertion: map(combine(identifier, s, assertionContent), ([varname, _, { parsed: assertion }]) => ({
    varname,
    assertion,
  })),

  aliasedAssertion: map(
    combine(
      expr,
      combine(
        s,
        not(exact('is'), 'expression assertions need to be aliased with the "as" keyword'),
        exact('as'),
        failure(s, 'expected space after "as" keyword')
      ),
      failure(identifier, 'expected a type assertion alias'),
      failure(s, 'expected space after the type assertion alias'),
      assertionContent
    ),
    ([subject, _, alias, __, { parsed: assertion }]) => ({ subject, alias, assertion })
  ),

  expr: toOneProp('inner', expr),
})

export const exprOrNever: Parser<ExprOrNever> = mappedCases<ExprOrNever>()('type', {
  return: map(combine(exact('return'), s, expr), ([_, __, expr]) => ({ expr })),
  panic: map(combine(exact('panic'), s, expr), ([_, __, message]) => ({ message })),
  expr: toOneProp('content', expr),
})
