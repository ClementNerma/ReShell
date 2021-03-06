import { matchStatementClose } from './context'
import { Parser } from './lib/base'
import { combine } from './lib/combinations'
import { failIfMatches, notStartingWith } from './lib/conditions'
import { buildUnicodeRegexMatcher, maybe_s, unicodeAlphanumericUnderscore, unicodeDigit } from './lib/littles'
import { eol, exact, oneOfWords } from './lib/matchers'
import { or } from './lib/switches'
import { map, silence } from './lib/transform'

export const keyword: Parser<string> = oneOfWords([
  'if',
  'else',
  'elif',
  'fn',
  'return',
  'for',
  'while',
  'let',
  'mut',
  'struct',
  'enum',
  '_',
])

export const identifier: Parser<string> = map(
  combine(
    failIfMatches(keyword, 'cannot use a reserved keyword as an identifier'),
    failIfMatches(unicodeDigit),
    unicodeAlphanumericUnderscore
  ),
  ([_, __, { parsed: keyword }]) => keyword
)

export const cmdName: Parser<string> = buildUnicodeRegexMatcher((l, d) => `(${l}|${d}|[_\\-])+`)

export const cmdAction: Parser<string> = notStartingWith(
  unicodeDigit,
  buildUnicodeRegexMatcher((l, d) => `(${l}|${d}|[_\\-])+`)
)

export const stmtEnd: Parser<void> = silence(combine(maybe_s, or<unknown>([eol(), exact(';'), matchStatementClose])))
