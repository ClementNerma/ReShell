/**
 * This is only a test program, not meant for final use
 */

import chalk = require('chalk')
import { existsSync, readFileSync } from 'fs'
import { delimiter, dirname, join, relative, sep } from 'path'
import { install } from 'source-map-support'
import { deflateSync, inflateSync } from 'zlib'
import { langParser } from './parsers'
import { initContext } from './parsers/context'
import { parseSource } from './parsers/lib/base'
import { execProgram } from './runtime'
import { createRunnerContext } from './runtime/base'
import { Diagnostic, DiagnosticFormatters, DiagnosticLevel, formatErr } from './shared/diagnostics'
import { SourceFilesServer } from './shared/files-server'
import { langTypechecker } from './typechecker'
import { createTypecheckerContext } from './typechecker/base'

install()
Error.stackTraceLimit = 100

function fail(message: string): never {
  console.error(message)
  process.exit(1)
}

const argv = process.argv.slice(2)

if (!argv[0]) fail('Please provide an example name')

const inputPath = argv[0]

if (!existsSync(inputPath)) fail('Input file not found')

const source = readFileSync(inputPath, 'utf-8')

const iter = argv[1] && argv[1].match(/^\d+$/) ? parseInt(argv[1]) : 1

const iterSrc = iter > 1 ? `if true { ${source} }\n`.repeat(iter) : source

const errorFormatters: (cat: string | null) => (level: DiagnosticLevel) => DiagnosticFormatters = (cat) => (level) => ({
  header: chalk.yellowBright,
  filePath: chalk.cyanBright,
  location: chalk.magentaBright,
  gutter: chalk.cyanBright,
  locationPointer: level === DiagnosticLevel.Warning ? chalk.yellowBright : chalk.redBright,
  message: (msg) =>
    (cat === null ? '' : chalk.cyanBright(`[${cat}] `)) +
    (level === DiagnosticLevel.Warning ? chalk.yellowBright : chalk.redBright)(msg),
})

const kb = (bytes: number) => (bytes / 1024).toFixed(2).padStart(8, ' ') + ' kB'
const ms = (ms: number) => ms.toString().padStart(5, ' ') + ' ms'

const measurePerf = <T>(runner: () => T): [number, T] => {
  const started = Date.now()
  const out = runner()
  const elapsed = Date.now() - started
  return [elapsed, out]
}

const sourceServer = new SourceFilesServer(
  (path) => (existsSync(path) ? readFileSync(path, 'utf8') : false),
  (path, relativeTo) => join(dirname(relativeTo), path),
  relative(process.cwd(), inputPath),
  iterSrc
)

const [parsingDuration, parsed] = measurePerf(() => parseSource(sourceServer, langParser, initContext()))

if (!parsed.ok) {
  const error = parsed.history[0] ?? '<no error provided>'
  console.error(formatErr(error, sourceServer, errorFormatters(null)))
  process.exit(1)
}

const infos = [`Parsed              | ${ms(parsingDuration)} | ${kb(source.length * iter)}`]

if (argv.includes('--compression-perf')) {
  const [jsonStrDuration, jsonStr] = measurePerf(() => JSON.stringify(parsed.data))

  const [compressDuration, compressed1] = measurePerf(() => deflateSync(jsonStr, { level: 3 }))
  const [decompressDuration, decompressed1] = measurePerf(() => inflateSync(compressed1))

  if (decompressed1.toString('utf-8') !== jsonStr /* || decompressed9.toString('utf-8') !== jsonStr*/) {
    fail('Decompressed data is not the same as the source!')
  }

  const [jsonParseDuration, reparsed] = measurePerf(() => JSON.parse(jsonStr) as unknown)

  if (JSON.stringify(reparsed) !== jsonStr) {
    fail('Reparsed data is not the same as the source!')
  }

  infos.push(
    `Minimified AST JSON | ${ms(jsonStrDuration)} | ${kb(JSON.stringify(parsed.data).length)}`,
    `Compression         | ${ms(compressDuration)} | ${kb(compressed1.byteLength)}`,
    `Decompression       | ${ms(decompressDuration)} |`,
    `JSON AST parsing    | ${ms(jsonParseDuration)} |`
  )
}

if (argv.includes('--ast-perf')) {
  infos.forEach((info) => console.log(info))
  process.exit(0)
}

if (argv.includes('--ast')) {
  console.dir(parsed.data, { depth: null })
  infos.forEach((info) => console.log(info))
  process.exit(0)
}

const isWindows = process.platform === 'win32' || process.platform === 'cygwin'

const RAW_PATH = process.env['PATH']
if (RAW_PATH === undefined) throw new Error('Failed to fetch PATH system variable')

const PATH = RAW_PATH.split(delimiter).map((entry) =>
  entry.startsWith('"') && entry.endsWith('"') ? entry.substr(1, entry.length - 2) : entry
)

const diagnosticHandler = (diag: Diagnostic) => console.error(formatErr(diag, sourceServer, errorFormatters(null)))

const typecheckerContext = createTypecheckerContext((cmd) => {
  for (const entry of PATH) {
    if (existsSync(join(entry, cmd))) return true

    if (isWindows) {
      for (const ext of ['.exe', '.cmd', '.bat', '.com']) {
        if (existsSync(join(entry, cmd + ext))) return true
        if (existsSync(join(entry, cmd + ext.toLocaleUpperCase()))) return true
      }
    }
  }

  return false
}, diagnosticHandler)

const [typecheckerDuration, typechecked] = measurePerf(() => langTypechecker(parsed.data, typecheckerContext))

if (!typechecked.ok) {
  console.error(formatErr(typechecked, sourceServer, errorFormatters(null)))
  process.exit(1)
}

infos.forEach((info) => console.log(info))
console.log(`Typechecked         | ${ms(typecheckerDuration)} |`)
console.log(`Parsing + typecheck | ${ms(parsingDuration + typecheckerDuration)} |`)

if (argv.includes('--exec')) {
  console.log(chalk.greenBright('\nExecuting the program...'))

  const argv = process.argv.includes('--args') ? process.argv.slice(process.argv.indexOf('--args') + 1) : []

  const runnerContext = createRunnerContext(typechecked.data, sep, argv, diagnosticHandler)

  const [execDuration, result] = measurePerf(() => execProgram(parsed.data, runnerContext))

  if (!result.ok) {
    console.error(formatErr(result.diag, sourceServer, errorFormatters('runtime')))
    process.exit(1)
  }

  console.log(chalk.greenBright(`SUCCESS in ${execDuration} ms.`))
}
