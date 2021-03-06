import { LiteralValue } from '../shared/ast'
import { Token } from '../shared/parsed'
import { Parser } from './lib/base'
import { combine } from './lib/combinations'
import { accelerateWithLookahead, failIfMatches, notFollowedBy } from './lib/conditions'
import { lookahead } from './lib/consumeless'
import { buildUnicodeRegexMatcher, unicodeAlphanumericUnderscore } from './lib/littles'
import { takeWhileN } from './lib/loops'
import { exact, match, oneOfFirstChar, oneOfMap, regex } from './lib/matchers'
import { mappedCases, or } from './lib/switches'
import { map, toOneProp } from './lib/transform'

export const rawPath: Parser<Token<string>[]> = notFollowedBy(
  takeWhileN(
    buildUnicodeRegexMatcher((letter, digit) => `(${letter}|${digit}|\\.|\\\\.)+`),
    { inter: exact('/'), minimum: 2, interExpect: false }
  ),
  exact('$')
)

export const rawString: Parser<string> = map(
  combine(
    exact('"'),
    match(/([^\\"$\n]|\\[^\n])*/),
    failIfMatches(lookahead(exact('$'))),
    exact('"', 'opened string has not been closed with a quote (")')
  ),
  ([_, { parsed: content }]) => content
)

export const literalValue: Parser<LiteralValue> = mappedCases<LiteralValue>()('type', {
  null: map(exact('null'), () => ({})),

  bool: map(
    oneOfMap([
      ['true', true],
      ['false', false],
    ]),
    (_, value) => ({ value })
  ),

  int: toOneProp(
    'value',
    accelerateWithLookahead(
      oneOfFirstChar(['-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
      notFollowedBy(
        or([
          regex(/(-)?0x([0-9a-fA-F]+)/, ([_, neg, num]) => parseInt(num, 16) * (neg ? -1 : 1)),
          regex(/(-)?0b([0-1]+)/, ([_, neg, num]) => parseInt(num, 2) * (neg ? -1 : 1)),
          regex(/(-)?0o([0-7]+)/, ([_, neg, num]) => parseInt(num, 8) * (neg ? -1 : 1)),
          regex(/(-)?0*(\d+)(?!\.)/, ([_, neg, num]) => parseFloat(num) * (neg ? -1 : 1)),
        ]),
        unicodeAlphanumericUnderscore,
        'unexpected token in integer'
      )
    )
  ),

  float: toOneProp(
    'value',
    accelerateWithLookahead(
      oneOfFirstChar(['-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
      notFollowedBy(
        regex(/(-)?0*(\d+\.\d+)/, ([_, neg, num]) => parseFloat(num) * (neg ? -1 : 1)),
        unicodeAlphanumericUnderscore,
        'unexpected token in floating-point number'
      )
    )
  ),

  string: toOneProp('value', rawString),

  path: toOneProp('segments', rawPath),
})
