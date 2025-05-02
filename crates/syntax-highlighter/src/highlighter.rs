//!
//! This module defines the rule set and systems used for highlighting ReShell programs
//! using the [`super::utils::syntax`] module.
//!

use std::{collections::HashSet, sync::LazyLock};

use pomsky_macro::pomsky;
use regex::Regex;

use crate::{
    CheckCmdType,
    elements::ItemType,
    nesting::NestingOpeningType,
    syntax::{
        NestedContentRules, Rule, RuleSet, RuleStylization, RulesForNesting, SimpleRule,
        ValidatedRuleSet,
    },
};

// Import all item types for easier use
use crate::elements::{
    IdentifierType::*, InvalidType::*, ItemType::*, OperatorType::*, SymbolType::*,
    SyntaxErrorType::*, ValueType::*, WrapperType::*,
};

pub type CmdChecker = Box<dyn Fn(&str, CheckCmdType) -> bool + Send + Sync>;

type SharedCmdChecker = Option<CmdChecker>;

// Style for valid method names
fn method_name_style(name: &str, cmd_checker: &SharedCmdChecker) -> ItemType {
    if cmd_checker
        .as_ref()
        .is_none_or(|cmd_checker| cmd_checker(name, CheckCmdType::Method))
    {
        Identifier(Method)
    } else {
        Invalid(MethodNotFound)
    }
}

// Style for function names
fn fn_name_style(name: &str, cmd_checker: &SharedCmdChecker) -> ItemType {
    if cmd_checker
        .as_ref()
        .is_none_or(|cmd_checker| cmd_checker(name, CheckCmdType::Method))
    {
        Identifier(Function)
    } else {
        Invalid(FunctionNotFound)
    }
}

pub static RULE_SET: LazyLock<ValidatedRuleSet<SharedCmdChecker>> = LazyLock::new(|| {
    /// Create a simple rule
    fn simple(regex: &'static str, item_types: impl Into<Vec<ItemType>>) -> Rule<SharedCmdChecker> {
        Rule::Simple(SimpleRule {
            matches: Regex::new(regex).unwrap(),
            inside: None,
            preceded_by: None,
            followed_by: None,
            followed_by_nesting: None,
            style: RuleStylization::Static(item_types.into()),
        })
    }

    /// Create a group inclusion rule
    fn include_group(name: &'static str) -> Rule<SharedCmdChecker> {
        Rule::Group(name.to_owned())
    }


    // Match method calls
    let method_call = || {
        Rule::Simple(SimpleRule {
            matches: Regex::new(pomsky!(
                :('.') :([Letter '_'] [Letter d '_']*) $
            ))
            .unwrap(),
            inside: None,
            preceded_by: None,
            followed_by: None,
            followed_by_nesting: Some(HashSet::from([NestingOpeningType::ExprWithParen])),
            style: RuleStylization::Dynamic(Box::new(|matched, cmd_checker: &SharedCmdChecker| {
                vec![Symbol(MethodDotPrefix), method_name_style(&matched[2], cmd_checker)]
            })),
        })
    };

    // Match function calls
    let fn_call = || {
        Rule::Simple(SimpleRule {
            matches: Regex::new(pomsky!(
                (^ [s]* | %) :([Letter '_'] [Letter d '_']*) $
            ))
            .unwrap(),
            inside: None,
            followed_by: None,
            followed_by_nesting: Some(HashSet::from([NestingOpeningType::ExprWithParen])),
            preceded_by: None,
            style: RuleStylization::Dynamic(Box::new(|matched, cmd_checker: &SharedCmdChecker| {
                vec![fn_name_style(&matched[2], cmd_checker)]
            })),
        })
    };

    // Build the rule set
    let rule_set = RuleSet {
        groups: [
            ("instructions", vec![
                // Comments
                simple(pomsky!( :('#') :(.*) ), [Symbol(CommentsMarker), Comment]),
                
                // Mutable variable declaration
                simple(pomsky!( % :("let") [s]+ :("mut") [s]+ :([Letter '_'] [Letter d '_']*) [s]* :('=') ), [Keyword, Keyword, Identifier(Variable), Operator(Assignment)]),
                simple(pomsky!( % :("let") [s]+ :("mut") [s]+ :([Letter '_'] [Letter d '_']*) % ), [Keyword, Keyword, Identifier(Variable)]),
                simple(pomsky!( % :("let") [s]+ :("mut") % ), [Keyword, Keyword]),
                
                // Immutable variable declaration
                simple(pomsky!( % :("let") [s]+ :([Letter '_'] [Letter d '_']*) [s]* :('=') ), [Keyword, Identifier(Constant), Operator(Assignment)]),
                simple(pomsky!( % :("let") [s]+ :([Letter '_'] [Letter d '_']*) % ), [Keyword, Identifier(Constant)]),
                simple(pomsky!( % :("let") % ), [Keyword]),

                // For loop
                simple(pomsky!( % :("for") [s]+ :([Letter '_'] [Letter d '_']*) [s]+ :("in") % ), [Keyword, Identifier(Constant), Keyword]),
                simple(pomsky!( % :("for") [s]+ :([Letter '_'] [Letter d '_']*) % ), [Keyword, Identifier(Constant)]),
                
                // Function declaration
                simple(pomsky!( % :("fn") [s]+ :([Letter '_'] [Letter d '_']*) % ), [Keyword, Identifier(FunctionOrMethod)]),

                // Function return type
                simple(pomsky!( :("->") [s]* :([Letter '_'] [Letter d '_']*) % ), [Symbol(FnReturnTypePrefix), Identifier(Type)]),

                // Command aliases
                simple(pomsky!( % :("alias") [s]+ :([Letter '_'] [Letter d '_']*) [s]+ :('=') ), [Keyword, Identifier(CmdNameOrPath), Operator(Assignment)]),
                simple(pomsky!( % :("alias") [s]+ :([Letter '_'] [Letter d '_']*) % ), [Keyword, Identifier(CmdNameOrPath)]),

                // Commands
                include_group("commands"),
            ]),
            ("commands", vec![
                // Pipes
                simple(pomsky!( :('!'? '|') ), [Symbol(CmdPipe)]),

                // Markers
                simple(pomsky!( ^ [s]* :("include") % ), [Keyword]),

                // Flags
                simple(
                    pomsky!( [s] :('-' [Letter d '_' '-' '+' ':']*) ($ | ![Letter d '_' '-' '+' ':'])),
                    [Identifier(FlagName)]
                ),

                // Keywords
                simple(pomsky!( % :("alias" | "fn" | "for" | "while" | "if" | "else" | "continue" | "typematch" | "match" | "break" | "throw" | "try" | "catch" | "return" | "do") % ), [Keyword]),

                // 'self' keyword
                simple(pomsky!( % :("self") %), [Keyword]),

                // Numbers
                simple(pomsky!( % :([d]+ ('.' [d]+)?) %), [Value(Number)]),

                // Symbols and operators
                simple(pomsky!( [s] :("&&" | "||" | "!") ( [s] | $ ) ), [Operator(Logic)]),
                simple(pomsky!( [s] :("==" | "!=" | ['<' '>']) ( [s] | $ ) ), [Operator(Logic)]),
                simple(pomsky!( [s] :(['+' '-' '*' '/' '%' '&' '|'] | "??") ( [s] | $ ) ), [Operator(Arithmetic)]),
                simple(pomsky!( [s] :('=') ( [s] | $ ) ), [Operator(Assignment)]),
                simple(pomsky!( [s] :(';') ( [s] | $ ) ), [Symbol(CmdSeparator)]),
                simple(pomsky!( [s] :(',') ( [s] | $ ) ), [Symbol(ArgSeparator)]),
                simple(pomsky!( [s] :(':') ( [s] | $ ) ), [Symbol(Colon)]),
                simple(pomsky!( [s] :(['(' ')']) ( [s] | $ ) ), [Symbol(Parenthesis)]),
                simple(pomsky!( [s] :(['[' ']']) ( [s] | $ ) ), [Symbol(Bracket)]),
                simple(pomsky!( [s] :(['{' '}']) ( [s] | $ ) ), [Symbol(Brace)]),
                simple(pomsky!( [s] :('?') ( [s] | $ ) ), [Symbol(OptionalArgMarker)]),

                // Method called as commands
                Rule::Simple(SimpleRule {
                    matches: Regex::new(pomsky!(
                        :('.') :([Letter '_'] [Letter d '_']*) %
                    ))
                    .unwrap(),
                    inside: None,
                    preceded_by: Some(
                        Regex::new(pomsky!( (^ |  ['|' ';' n]) [s]* $) )
                        .unwrap(),
                    ),
                    followed_by: None,
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new(|matched, cmd_checker: &SharedCmdChecker| {
                        vec![Symbol(MethodDotPrefix), method_name_style(&matched[2], cmd_checker)]
                    })),
                }),

                // Method calls
                method_call(),

                // Command names
                Rule::Simple(SimpleRule {
                    matches: Regex::new(pomsky!(
                        let delimiter = [s '(' ')' '[' ']' '{' '}' '<' '>' ';' '|' "'" '"' '`' '$' '#' '^'];

                        :('^' | "")
                        :(!delimiter+)
                    )).unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new(pomsky!(
                        :(^ | ('$' '^'? | '@') '(' | ['|' n ';' '{'])
                        [s]* $
                    )).unwrap()),
                    followed_by: None,
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new( |matched, cmd_checker: &SharedCmdChecker| {
                        let is_external = !matched[1].is_empty();

                        let cmd_type = if is_external {
                            CheckCmdType::ExternalCmd
                        } else {
                            CheckCmdType::BroadCmd
                        };

                        let item_type = if cmd_checker.as_ref().is_none_or(|cmd_checker| cmd_checker(&matched[2], cmd_type)) {
                            Identifier(CmdNameOrPath)
                        } else {
                            Invalid(CmdPathNotFound)
                        };

                        vec![Symbol(ExternalCmdMarker), item_type]
                    }))
                }),

                // Function calls
                fn_call(),

                // Variables
                simple(pomsky!( :('$' ([Letter '_'] [Letter d '_']*)?) % ), [Identifier(VariableOrConstant)]),

                // Booleans
                simple(pomsky!( % :("true" | "false") % ), [Value(Boolean)]),

                // Expansions
                simple(pomsky!( [s ','] :("...") (% | ['$' '(']) ), [Operator(Spread)]),
                
                // Any other character
                simple(pomsky!( :(.) ), [Value(RawCharacter)]),
            ]),
            ("literal-strings", vec![
                // Escaped characters
                simple(pomsky!( :("\\.") ), [Value(EscapedCharacter)]),

                // Any other character
                simple(pomsky!( :(.) ), [Value(LiteralCharacter)]),
            ]),
            ("computed-strings", vec![
                // Escaped characters
                simple(pomsky!( :('\\' .) ), [Value(EscapedCharacter)]),

                // Variables
                simple(pomsky!( :( '$' ([Letter '_'] [Letter d '_']*)? ) ), [Identifier(VariableOrConstant)]),

                // Any other character
                simple(pomsky!( :(.) ), [Value(LiteralCharacter)]),
            ]),
            ("expressions", vec![
                // Method calls
                method_call(),

                // Function calls
                fn_call(),

                // Flags
                simple(
                    pomsky!( (^ | ',' [s]*) :('-' [Letter d '_' '-' '+' ':']*) ($ | ![Letter d '_' '-' '+' ':'])),
                    [Identifier(FlagName)],
                ),

                // Types
                simple(pomsky!(% :("any" | "bool" | "int" | "float" | "string" | "list" | "map" | "error" | "struct" | "fn" | "cmdcall") %), [Keyword]),

                // Booleans
                simple(pomsky!(% :("true" | "false") %), [Value(Boolean)]),

                // The null value
                simple(pomsky!(% :("null") %), [Value(Null)]),

                // Variables
                simple(pomsky!( :('$' ([Letter '_'] [Letter d '_']*)?) ), [Identifier(VariableOrConstant)]),

                // Struct member access
                Rule::Simple(SimpleRule {
                    matches: Regex::new(pomsky!(
                        :('?'? '.') :([Letter '_'] [Letter d '_']*)
                    )).unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new(pomsky!(
                        ('$' [Letter '_'] [Letter d '_' ]* | [')' ']' '}']) $
                    )).unwrap()),
                    followed_by: None,
                    followed_by_nesting: None,
                    style: RuleStylization::Static(vec![
                        Symbol(StructMemberDotPrefix),
                        Identifier(StructMember)
                    ]) }
                ),

                // Functions as values
                simple(pomsky!( :('@' ([Letter '_'] [Letter d '_']*)?) ), [Identifier(Function)]),

                // Numbers
                simple(pomsky!( % :([d]+ ('.' [d]+)?) % ), [Value(Number)]),

                // Symbols and operators
                // TODO: deduplicate from above
                simple(pomsky!( [s] :("&&" | "||" | "!") ( [s] | $ ) ), [Operator(Logic)]),
                simple(pomsky!( [s] :("==" | "!=" | ['<' '>']) ( [s] | $ ) ), [Operator(Logic)]),
                simple(pomsky!( [s] :(['+' '-' '*' '/' '%' '&' '|'] | "??") ( [s] | $ ) ), [Operator(Arithmetic)]),
                simple(pomsky!( [s] :('=') ( [s] | $ ) ), [Operator(Assignment)]),
                simple(pomsky!( [s] :(';') ( [s] | $ ) ), [Symbol(CmdSeparator)]),
                simple(pomsky!( [s] :(',') ( [s] | $ ) ), [Symbol(ArgSeparator)]),
                simple(pomsky!( [s] :(':') ( [s] | $ ) ), [Symbol(Colon)]),
                simple(pomsky!( [s] :(['(' ')']) ( [s] | $ ) ), [Symbol(Parenthesis)]),
                simple(pomsky!( [s] :(['[' ']']) ( [s] | $ ) ), [Symbol(Bracket)]),
                simple(pomsky!( [s] :(['{' '}']) ( [s] | $ ) ), [Symbol(Brace)]),
                simple(pomsky!( [s] :('?') ( [s] | $ ) ), [Symbol(OptionalArgMarker)]),
                // 'typeis' operator
                simple(pomsky!( % :("typeis") % ), [Keyword]),

                // Function argument name (TODO: hacky, should not be in "expressions" but in "fn args" or something)
                simple(pomsky!( (% | ',') :([Letter '_'] [Letter d '_']*) [s]* :('?'? ':' | '=') ), [Identifier(FnArgument), Symbol(FnArgumentTypeOrValueSpecifier)]),

                // Other characters
                simple(pomsky!( :(.) ), [Value(LiteralCharacter)])
            ]),
            ("var-spreading", vec![
                // Identifiers
                simple(pomsky!( :([Letter '_']) ), [Identifier(VariableOrConstant)]),

                // Binding
                simple(pomsky!( :(':') ), [Symbol(BindInSpreading)])
            ])
        ]
        .into_iter().map(|(group, rules)| (group.to_owned(), rules)).collect(),

        nested_content_rules: RulesForNesting {
            block: NestedContentRules {
                opening_style: |nesting_level| Wrapper(Block(nesting_level)),
                closing_style: |nesting_level| Wrapper(Block(nesting_level)),
                rules: vec![
                    include_group("instructions")
                ]
            },

            var_spreading: NestedContentRules {
                opening_style: |nesting_level| Wrapper(VarSpreading(nesting_level)),
                closing_style: |nesting_level| Wrapper(VarSpreading(nesting_level)),
                rules: vec![
                    include_group("var-spreading")
                ]
            },

            list: NestedContentRules {
                opening_style: |nesting_level| Wrapper(List(nesting_level)),
                closing_style: |nesting_level| Wrapper(List(nesting_level)),
                rules: vec![
                    include_group("expressions")
                ]
            },

            expr_with_paren: NestedContentRules {
                opening_style: |nesting_level| Wrapper(ExpressionParen(nesting_level)),
                closing_style: |nesting_level| Wrapper(ExpressionParen(nesting_level)),
                rules: vec![
                    include_group("expressions")
                ]
            },

            literal_string: NestedContentRules {
                opening_style: |nesting_level| Wrapper(LiteralString(nesting_level)),
                closing_style: |nesting_level| Wrapper(LiteralString(nesting_level)),
                rules: vec![
                    include_group("literal-strings")
                ]
            },

            computed_string: NestedContentRules {
                opening_style: |nesting_level| Wrapper(ComputedString(nesting_level)),
                closing_style: |nesting_level| Wrapper(ComputedString(nesting_level)),
                rules: vec![
                    include_group("computed-strings")
                ]
            },

            expr_in_string: NestedContentRules {
                opening_style: |nesting_level| Wrapper(ExprInString(nesting_level)),
                closing_style: |nesting_level| Wrapper(ExprInString(nesting_level)),
                rules: vec![
                    include_group("expressions")
                ]
            },

            cmd_output: NestedContentRules {
                opening_style: |nesting_level| Wrapper(CmdOutput(nesting_level)),
                closing_style: |nesting_level| Wrapper(CmdOutput(nesting_level)),
                rules: vec![
                    include_group("commands")
                ]
            },

            cmd_call: NestedContentRules {
                opening_style: |nesting_level| Wrapper(CmdCall(nesting_level)),
                closing_style: |nesting_level| Wrapper(CmdCall(nesting_level)),
                rules: vec![
                    include_group("commands")
                ]
            },

            lambda: NestedContentRules {
                opening_style: |nesting_level| Wrapper(Lambda(nesting_level)),
                closing_style: |nesting_level| Wrapper(Lambda(nesting_level)),
                rules: vec![
                    include_group("commands")
                ]
            },

            fn_args: NestedContentRules {
                opening_style: |nesting_level| Wrapper(FnArgs(nesting_level)),
                closing_style: |nesting_level| Wrapper(FnArgs(nesting_level)),
                rules: vec![
                    // Flags
                    simple(
                        pomsky!( (^ | ',' [s]*) :('-' [Letter d '_']*) % ),
                        [Identifier(FlagName)],
                    ),

                    // Types
                    simple(pomsky!( ':' [s]+ :([Letter '_'] [Letter d '_']*) % ), [Identifier(Type)]),

                    // Variables
                    simple(pomsky!(:([Letter '_'] [Letter d '_']*) %), [Identifier(VariableOrConstant)]),
                ]
            }
        },

        non_nested_content_rules: vec![
            include_group("instructions")
        ],

        closing_without_opening_style: SyntaxError(ClosingWithoutOpening),
        unclosed_style: SyntaxError(UnclosedOpening),
        command_separator_style: Symbol(CmdSeparator),
        
        use_arguments_separator: false
    };

    ValidatedRuleSet::validate(rule_set).unwrap()
});
