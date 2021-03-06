import { FnDeclArg, FnType, MethodInfos, PrimitiveValueType, ValueType } from './ast'
import { CodeSection, Token } from './parsed'

export const nativeLibraryTypeAliases = ensureValueTypes<ValueType>()({
  LsItem: {
    type: 'struct',
    members: [
      { name: 'type', type: { type: 'aliasRef', typeAliasName: _forgeToken('LsItemType') } },
      { name: 'name', type: { type: 'string' } },
      { name: 'size', type: { type: 'nullable', inner: { type: 'int' } } },
      { name: 'ctime', type: { type: 'int' } },
      { name: 'mtime', type: { type: 'int' } },
      { name: 'atime', type: { type: 'int' } },
    ],
  },

  LsItemType: { type: 'enum', variants: _forgeTokens(['File', 'Dir', 'Symlink', 'Unknown']) },
})

export const nativeLibraryVarTypes = ensureValueTypes<ValueType>()({
  argv: { type: 'list', itemsType: { type: 'string' } },
  PATH: { type: 'list', itemsType: { type: 'string' } },
})

export const nativeLibraryFnTypes = ensureValueTypes<FnType>()({
  // Integers
  randInt: _buildNativeLibraryFn({
    args: () => [{ name: 'max', type: 'int' }],
    returnType: () => 'int',
  }),

  // Floats
  rand: _buildNativeLibraryFn({
    args: () => [],
    returnType: () => 'float',
  }),

  // Lists
  seq: _buildNativeLibraryFn({
    args: () => [
      { name: 'from', type: 'int' },
      { name: 'to', type: 'int' },
    ],
    returnType: () => ({ type: 'list', itemsType: { type: 'int' } }),
  }),

  // Failables
  ok: _buildNativeLibraryFn({
    generics: ['T', 'E'],
    args: ({ T }) => [{ name: 'value', type: T }],
    returnType: ({ T, E }) => ({ type: 'failable', successType: _forgeToken(T), failureType: _forgeToken(E) }),
  }),

  err: _buildNativeLibraryFn({
    generics: ['T', 'E'],
    args: ({ E }) => [{ name: 'error', type: E }],
    returnType: ({ T, E }) => ({ type: 'failable', successType: _forgeToken(T), failureType: _forgeToken(E) }),
  }),

  // Type utilities
  typed: _buildNativeLibraryFn({
    generics: ['T'],
    args: ({ T }) => [{ name: 'value', type: T }],
    returnType: ({ T }) => T,
  }),

  // Debug utilities
  debugStr: _buildNativeLibraryFn({
    methodFor: () => 'unknown',
    args: () => [{ flag: '--', name: 'pretty', type: 'bool' }],
    returnType: () => 'string',
  }),

  dump: _buildNativeLibraryFn({
    args: () => [
      { name: 'value', type: 'unknown' },
      { flag: '--', name: 'pretty', type: 'bool' },
    ],
  }),

  trace: _buildNativeLibraryFn({ args: () => [{ name: 'message', type: 'string', optional: true }] }),

  // Terminal utilities
  echo: _buildNativeLibraryFn({
    args: () => [
      { name: 'message', type: 'string' },
      { flag: '-', name: 'n', type: 'bool' },
    ],
  }),

  // Filesystem utilities
  ls: _buildNativeLibraryFn({
    args: () => [{ name: 'path', type: 'path' }],
    returnType: () =>
      _failableType(
        { type: 'list', itemsType: { type: 'aliasRef', typeAliasName: _forgeToken('LsItem') } },
        { type: 'string' }
      ),
  }),
})

export const nativeLibraryMethodsTypes = ensureArrayValuesType<FnType>()({
  // Numbers
  toFixed: _buildNativeLibraryFn({
    methodFor: () => 'float',
    args: () => [{ name: 'precision', type: 'int' }],
    returnType: () => 'string',
  }),

  // Strings
  includes: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [{ name: 'lookup', type: 'string' }],
    returnType: () => 'bool',
  }),

  lines: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [],
    returnType: () => ({ type: 'list', itemsType: { type: 'string' } }),
  }),

  charAt: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [{ name: 'index', type: 'int' }],
    returnType: () => ({ type: 'nullable', inner: { type: 'string' } }),
  }),

  indexOf: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [{ name: 'lookup', type: 'string' }],
    returnType: () => ({ type: 'nullable', inner: { type: 'int' } }),
  }),

  replace: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [
      { name: 'model', type: 'string' },
      { name: 'replacement', type: 'string' },
    ],
    returnType: () => 'string',
  }),

  repeat: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [{ name: 'repeat', type: 'int' }],
    returnType: () => 'string',
  }),

  split: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [{ name: 'delimiter', type: 'string' }],
    returnType: () => ({ type: 'list', itemsType: { type: 'string' } }),
  }),

  toPath: _buildNativeLibraryFn({
    methodFor: () => 'string',
    args: () => [],
    returnType: () => 'path',
  }),

  // Paths
  tostr: _buildNativeLibraryFn({
    methodFor: () => 'path',
    args: () => [],
    returnType: () => 'string',
  }),

  segments: _buildNativeLibraryFn({
    methodFor: () => 'path',
    args: () => [],
    returnType: () => ({ type: 'list', itemsType: { type: 'string' } }),
  }),

  isAbsolute: _buildNativeLibraryFn({
    methodFor: () => 'path',
    args: () => [],
    returnType: () => 'bool',
  }),

  // Lists
  at: _buildNativeLibraryFn({
    generics: ['T'],
    methodFor: ({ T }) => [[T.name], { type: 'list', itemsType: T }],
    args: () => [{ name: 'index', type: 'int' }],
    returnType: ({ T }) => ({ type: 'nullable', inner: T }),
  }),

  rev: _buildNativeLibraryFn({
    generics: ['T'],
    methodFor: ({ T }) => [[T.name], { type: 'list', itemsType: T }],
    args: () => [],
    returnType: ({ T }) => ({ type: 'list', itemsType: T }),
  }),

  join: _buildNativeLibraryFn({
    methodFor: () => ({ type: 'list', itemsType: { type: 'string' } }),
    args: () => [{ name: 'glue', type: 'string' }],
    returnType: () => 'string',
  }),

  filter: _buildNativeLibraryFn({
    generics: ['T'],
    methodFor: ({ T }) => [[T.name], { type: 'list', itemsType: T }],
    args: ({ T }) => [
      {
        name: 'mapper',
        type: {
          type: 'fn',
          fnType: _buildNativeLibraryFn({
            args: () => [
              { name: 'value', type: T },
              { name: 'index', type: 'int' },
            ],
            returnType: () => 'bool',
          }),
        },
      },
    ],
    returnType: ({ T }) => ({ type: 'list', itemsType: T }),
  }),

  map: _buildNativeLibraryFn({
    generics: ['T', 'O'],
    methodFor: ({ T }) => [[T.name], { type: 'list', itemsType: T }],
    args: ({ T, O }) => [
      {
        name: 'mapper',
        type: {
          type: 'fn',
          fnType: _buildNativeLibraryFn({
            args: () => [
              { name: 'value', type: T },
              { name: 'index', type: 'int' },
            ],
            returnType: () => O,
          }),
        },
      },
    ],
    returnType: ({ O }) => ({ type: 'list', itemsType: O }),
  }),

  reduce: _buildNativeLibraryFn({
    generics: ['T', 'A'],
    methodFor: ({ T }) => [[T.name], { type: 'list', itemsType: T }],
    args: ({ T, A }) => [
      { name: 'init', type: A },
      {
        name: 'reducer',
        type: {
          type: 'fn',
          fnType: _buildNativeLibraryFn({
            args: () => [
              { name: 'acc', type: A },
              { name: 'value', type: T },
            ],
            returnType: () => A,
          }),
        },
      },
    ],
    returnType: ({ A }) => A,
  }),

  joinPaths: _buildNativeLibraryFn({
    methodFor: () => ({ type: 'list', itemsType: { type: 'path' } }),
    args: () => [],
    returnType: () => 'path',
  }),

  composePath: _buildNativeLibraryFn({
    methodFor: () => ({ type: 'list', itemsType: { type: 'string' } }),
    args: () => [],
    returnType: () => 'path',
  }),

  // Nullables
  unwrap: _buildNativeLibraryFn({
    generics: ['T'],
    methodFor: ({ T }) => [[T.name], { type: 'nullable', inner: T }],
    args: () => [],
    returnType: ({ T }) => T,
  }),

  expect: _buildNativeLibraryFn({
    generics: ['T'],
    methodFor: ({ T }) => [[T.name], { type: 'nullable', inner: T }],
    args: () => [{ name: 'message', type: 'string' }],
    returnType: ({ T }) => T,
  }),

  // Common on multiple types
  len: [
    _buildNativeLibraryFn({
      methodFor: () => 'string',
      args: () => [],
      returnType: () => 'int',
    }),

    _buildNativeLibraryFn({
      methodFor: () => ({ type: 'list', itemsType: { type: 'unknown' } }),
      args: () => [],
      returnType: () => 'int',
    }),
  ],

  empty: [
    _buildNativeLibraryFn({
      methodFor: () => 'string',
      args: () => [],
      returnType: () => 'bool',
    }),

    _buildNativeLibraryFn({
      methodFor: () => ({ type: 'list', itemsType: { type: 'unknown' } }),
      args: () => [],
      returnType: () => 'bool',
    }),
  ],
})

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Native library builder utilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

function ensureValueTypes<V>(): <K extends string>(obj: { [key in K]: V }) => { [key in K]: Token<V> } {
  return <K extends string>(obj: { [key in K]: V }) =>
    fromEntries<K, Token<V>>(
      Object.entries<V>(obj).map<[K, Token<V>]>(([name, value]) => [name as K, _forgeToken(value)])
    )
}

function ensureArrayValuesType<V>(): <K extends string>(obj: { [key in K]: V | V[] }) => {
  [key in K]: Token<V>[]
} {
  return <K extends string>(obj: { [key in K]: V | V[] }) =>
    fromEntries<K, Token<V>[]>(
      Object.entries<V | V[]>(obj).map<[K, Token<V>[]]>(([name, value]) => [
        name as K,
        Array.isArray(value) ? value.map((v) => _forgeToken(v)) : [_forgeToken(value)],
      ])
    )
}

function fromEntries<K extends string, P>(entries: [K, P][]): { [key in K]: P } {
  // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-explicit-any
  return Object.fromEntries(entries) as any
}

export function _nativeLibAt(): CodeSection {
  return {
    start: { file: { type: 'internal', path: '<native library>' }, col: 0, line: 0 },
    next: { file: { type: 'internal', path: '<native library>' }, col: 0, line: 1 },
  }
}

export function _forgeToken<T>(data: T): Token<T> {
  return {
    at: _nativeLibAt(),
    matched: -1,
    parsed: data,
  }
}

function _forgeTokens<T>(data: T[]): Token<T>[] {
  return data.map((item) => _forgeToken(item))
}

function _failableType(successType: ValueType, failureType: ValueType): ValueType {
  return { type: 'failable', successType: _forgeToken(successType), failureType: _forgeToken(failureType) }
}

type _Generic = Extract<ValueType, { type: 'generic' }>

function _buildNativeLibraryFn<G extends string>({
  generics,
  args,
  restArg,
  returnType,
  methodFor,
}: {
  generics?: G[]
  args: (forgedGenerics: { [name in G]: _Generic }) => {
    flag?: '-' | '--'
    name: string
    optional?: true
    type: ValueType | PrimitiveValueType['type'] | 'unknown'
  }[]
  restArg?: string
  returnType?: (forgedGenerics: { [name in G]: _Generic }) => ValueType | PrimitiveValueType['type'] | 'unknown'
  methodFor?: (forgedGenerics: { [name in G]: _Generic }) =>
    | ValueType
    | PrimitiveValueType['type']
    | 'unknown'
    | [methodGenerics: Token<string>[], type: ValueType | PrimitiveValueType['type'] | 'unknown']
}): FnType {
  const forgedGenerics = fromEntries(
    (generics ?? []).map<[G, _Generic]>((name) => [
      name,
      { type: 'generic', name: _forgeToken(name), orig: _nativeLibAt(), fromFnCallAt: null },
    ])
  )

  const ret = returnType?.(forgedGenerics)

  let methodInfos: MethodInfos | null = null

  if (methodFor) {
    let forType = methodFor(forgedGenerics)

    let methodGenerics: Token<string>[] = []

    if (Array.isArray(forType)) {
      methodGenerics = forType[0]
      forType = forType[1]
    }

    methodInfos = {
      selfArg: _forgeToken('self'),
      forType: _forgeToken(typeof forType === 'string' ? { type: forType } : forType),
      generics: methodGenerics,
    }
  }

  return {
    generics: Object.values<_Generic>(forgedGenerics)
      .map((g) => g.name)
      .filter((name) => (methodInfos ? !methodInfos.generics.find((c) => c.parsed === name.parsed) : true)),
    args: args(forgedGenerics).map(
      ({ flag, name, type, optional }): Token<FnDeclArg> =>
        _forgeToken({
          flag: flag ? _forgeToken(flag) : null,
          name: _forgeToken(name),
          defaultValue: null,
          optional: optional ?? false,
          type: _forgeToken(typeof type === 'string' ? { type } : type),
        })
    ),
    restArg: restArg !== undefined ? _forgeToken(restArg) : null,
    returnType: ret !== undefined ? _forgeToken(typeof ret === 'string' ? { type: ret } : ret) : null,
    method: methodInfos,
  }
}
