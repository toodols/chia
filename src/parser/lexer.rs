use logos::Logos;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
pub enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    #[token("::")]
    Scope,
    #[token("->")]
    Arrow,

    #[regex(r#"'([^\\"]|\\')*'"#)]
    #[regex(r#""([^\\"]|\\")*""#)]
    String,

    #[regex("\"")]
    Quote,

    #[regex("-?[0-9]+")]
    Number,
    #[token["!"]]
    Not,

    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("=")]
    Assign,
    #[token("||=")]
    OrAssign,
    #[token("&&=")]
    AndAssign,
    #[token("**=")]
    ExponentiateAssign,
    #[token("%=")]
    ModuloAssign,

    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[token("**")]
    Exponentiate,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Modulo,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("<=")]
    LessThanOrEqual,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[regex("//.*", logos::skip)]
    #[regex("/\\*([^*]|(\\*[^/]))*\\*/", logos::skip)]
    Comment,
    #[regex("[ \t\n]+", logos::skip)]
    Whitespace,
    #[error]
    Error,
    #[token("..")]
    Range,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("ref")]
    Ref,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("loop")]
    Loop,
    #[token("in")]
    In,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
    #[token("mod")]
    Mod,
    #[token("return")]
    Return,
    #[token("enum")]
    Enum,
    #[token("struct")]
    Struct,
}

impl Token {
    pub fn is_expression_start(&self) -> bool {
        matches!(
            self,
            Token::Identifier
                | Token::Number
                | Token::String
                | Token::True
                | Token::False
                | Token::LParen
                | Token::LBrace
                | Token::LBracket
                | Token::Not
                | Token::Sub
                | Token::Ref
        )
    }
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            Token::Add
                | Token::Sub
                | Token::Mul
                | Token::Div
                | Token::Assign
                | Token::AddAssign
                | Token::SubAssign
                | Token::MulAssign
                | Token::DivAssign
                | Token::Equal
                | Token::NotEqual
                | Token::GreaterThan
                | Token::LessThan
                | Token::Range
        )
    }
}
