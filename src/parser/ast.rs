use super::lexer::Token;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Span(String);
impl ToString for Span {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}
impl From<String> for Span {
    fn from(s: String) -> Self {
        Self(s)
    }
}
impl From<&str> for Span {
    fn from(s: &str) -> Self {
        Self(s.to_owned())
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    pub path: Vec<Span>,
    pub id: usize,
}
impl Path {
    pub fn last(&self) -> String {
        self.path.last().unwrap().to_string()
    }
}

#[derive(Debug, Clone)]
pub struct TyPath {
    pub path: Vec<Span>,
    pub id: usize,
}

// Patterns look like "Some(s)"
// right now they are just "s"
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Pattern {
    Path(Path),
    Tuple(Vec<Pattern>),
}

// impl Pattern {
//     fn as_path_symbol(&self) -> &TyPath {
//         match self {
//             Pattern::Path(path) => path.symbol(),
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct Program {
    pub id: usize,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Parameters(pub Vec<(Pattern, TypeExpr)>);

impl Parameters {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub id: usize,
    pub span: Span,
    pub parameters: Parameters,
    pub body: Block,
    pub return_type: TypeExpr,
}

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDeclaration(FunctionDeclaration),
    TupleStructDeclaration(TupleStructDeclaration),
    StructDeclaration(StructDeclaration),
    Mod(Mod),
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub span: Span,
    pub body: Program,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ExpAssign,
    LessThanOrEqual,
    GreaterThanOrEqual,
    ModAssign,
    Exp,
    And,
    Or,
    Range,
}

// https://doc.rust-lang.org/reference/expressions.html#expression-precedence
#[derive(Debug, PartialEq, Eq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
}

impl BinaryOperator {
    #[inline]
    pub fn precedence(self) -> (Associativity, u16) {
        match self {
            BinaryOperator::Exp => (Associativity::LeftToRight, 0),
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                (Associativity::LeftToRight, 1)
            }
            BinaryOperator::Add | BinaryOperator::Sub => (Associativity::LeftToRight, 2),
            BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThan => (Associativity::LeftToRight, 3), // no idea what "require parentheses" is
            BinaryOperator::Equal | BinaryOperator::NotEqual => (Associativity::LeftToRight, 4),
            BinaryOperator::And => (Associativity::LeftToRight, 5),
            BinaryOperator::Or => (Associativity::LeftToRight, 6),
            BinaryOperator::Range => (Associativity::LeftToRight, 7), // or here
            BinaryOperator::Assign
            | BinaryOperator::AddAssign
            | BinaryOperator::SubAssign
            | BinaryOperator::MulAssign
            | BinaryOperator::DivAssign
            | BinaryOperator::ModAssign
            | BinaryOperator::ExpAssign => (Associativity::RightToLeft, 8),
        }
    }
}

impl From<Token> for BinaryOperator {
    /// panics if token is not a binary operator
    fn from(token: Token) -> BinaryOperator {
        match token {
            Token::Add => BinaryOperator::Add,
            Token::Sub => BinaryOperator::Sub,
            Token::Mul => BinaryOperator::Mul,
            Token::Div => BinaryOperator::Div,
            Token::Modulo => BinaryOperator::Mod,
            Token::Equal => BinaryOperator::Equal,
            Token::NotEqual => BinaryOperator::NotEqual,
            Token::GreaterThan => BinaryOperator::GreaterThan,
            Token::LessThan => BinaryOperator::LessThan,
            Token::Assign => BinaryOperator::Assign,
            Token::AddAssign => BinaryOperator::AddAssign,
            Token::SubAssign => BinaryOperator::SubAssign,
            Token::MulAssign => BinaryOperator::MulAssign,
            Token::DivAssign => BinaryOperator::DivAssign,
            Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            Token::ModuloAssign => BinaryOperator::ModAssign,
            Token::Exponentiate => BinaryOperator::Exp,
            Token::ExponentiateAssign => BinaryOperator::ExpAssign,
            Token::And => BinaryOperator::And,
            Token::Or => BinaryOperator::Or,
            Token::Range => BinaryOperator::Range,
            _ => panic!("Token {:?} is not a binary operator", token),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

impl From<Token> for UnaryOperator {
    fn from(token: Token) -> UnaryOperator {
        match token {
            Token::Sub => UnaryOperator::Negate,
            Token::Not => UnaryOperator::Not,
            _ => panic!("Token {:?} is not a unary operator", token),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Identifier(String),
    Tuple(Vec<TypeExpr>),
    Unit,
    Untyped,
}

#[derive(Debug)]
pub enum TopLevelStatement {
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Empty,
    Expression(Expression),
    LetDeclaration(LetDeclaration),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: usize,
    pub statements: Vec<Statement>,
    /// Whether the last statement is to be returned
    pub does_return: bool,
}

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: String,
    pub fields: Vec<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<(String, TypeExpr)>,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub value: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone)]
pub enum Expression {
    Break(Box<Expression>),
    Index(Index),
    Block(Block),
    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
    Literal(Literal),
    FunctionCall(FunctionCall),
    IfExpression(IfExpression),
    Return(Box<Expression>),
    ForLoop(ForLoop),
    Path(Path),
}
impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(arg0) => arg0.fmt(f),
            Self::Block(arg0) => arg0.fmt(f),
            Self::BinaryOperation(arg0) => arg0.fmt(f),
            Self::UnaryOperation(arg0) => arg0.fmt(f),
            Self::Literal(arg0) => arg0.fmt(f),
            Self::FunctionCall(arg0) => arg0.fmt(f),
            Self::IfExpression(arg0) => arg0.fmt(f),
            Self::ForLoop(arg0) => arg0.fmt(f),
            Self::Break(arg0) => f.debug_tuple("Break").field(arg0).finish(),
            Self::Return(arg0) => f.debug_tuple("Return").field(arg0).finish(),
            Self::Path(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub else_body: Option<Box<Expression>>,
}

impl Expression {
    /// True if the expression is self terminating; does not need a semicolon
    pub fn terminating(&self) -> bool {
        matches!(self, Expression::IfExpression(_))
    }
}

#[derive(Debug, Clone)]
pub struct LetDeclaration {
    pub id: usize,
    pub pat: Pattern,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub value: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ForLoop {
    pub pat: Pattern,
    pub iter: Box<Expression>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Unit,
    Array(Vec<Expression>),
    ArraySized(Box<Expression>, u32),
    Number(i32),
    String(String),
    Boolean(bool),
}
