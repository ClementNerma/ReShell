macro_rules! define_item_types {
    ($pub:vis $enum_name:ident {
        $( $category:ident ( $($category_item:ident $(($holding_type: ty))?,)+ ), )+
        $( | $leaf: ident ),+
    }) => {
        #[derive(Debug, Clone, Copy)]
        $pub enum $enum_name {
            $($category(::paste::paste!([<$category Type>])),)+
            $($leaf),+
        }

        $(
            ::paste::paste! {
                #[derive(Debug, Clone, Copy)]
                $pub enum [<$category Type>] {
                    $( $category_item $(($holding_type))?, )+
                }

                impl From<[<$category Type>]> for $enum_name {
                    fn from(value: [<$category Type>]) -> Self {
                        Self::$category(value)
                    }
                }
            }
        )+
    };
}

define_item_types!(pub ItemType {
    IdentifierDeclaration(
        VariableDecl,
        ConstantDecl,
        FunctionDecl,
        FunctionOrMethodDecl,
        FnVariableArgDecl,
        FnFlagArgDecl,
        TypeDecl,
        AliasDecl,
    ),

    Identifier(
        Variable,
        Constant,
        VariableOrConstant,
        Function,
        FunctionOrMethod,
        Method,
        CmdNameOrPath,
        StructMember,
        StructOrTupleMemberDestructuring,
        FnArgument,
        FlagName,
        Type,
    ),

    Value(
        Null,
        Boolean,
        Number,
        RawCharacters,
        EscapedCharacter,
        LiteralCharacter,
        NamedFunction,
    ),

    Operator(
        Arithmetic,
        Logic,
        Comparison,
        Assignment,
        Spread,
    ),

    Symbol(
        MethodDotPrefix,
        CommentsMarker,
        FlagDashes,
        CmdPipe,
        FnReturnTypePrefix,
        CmdSeparator,
        ArgSeparator,
        Colon,
        OptionalArgMarker,
        ExternalCmdMarker,
        StructMemberDotPrefix,
        FnArgumentTypeOrValueSpecifier,
        BindInSpreading,
    ),

    Wrapper(
        Block(usize),
        List(usize),
        VarSpreading(usize),
        ExpressionParen(usize),
        LiteralString(usize),
        ComputedString(usize),
        ExprInString(usize),
        CmdOutput(usize),
        CmdCall(usize),
        Lambda(usize),
        FnArgs(usize),
    ),

    Invalid(
        FunctionNotFound,
        MethodNotFound,
        CmdPathNotFound,
    ),

    SyntaxError(
        ClosingWithoutOpening,
        UnclosedOpening,
    ),

    | Keyword,
    | Comment
});
