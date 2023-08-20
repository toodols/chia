use super::lexer::Token;
use std::fmt::Debug;

/// Each node represents a section of code
#[derive(Debug, Clone)]
pub struct Node<T: Debug + Clone> {
    pub inner: T,
    pub id: usize,
    // span: Span,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SymbolName {
    External(String),
    Internal(usize),
}

impl SymbolName {
    pub fn ident(&self) -> String {
        match self {
            SymbolName::External(s) => s.clone(),
            SymbolName::Internal(s) => format!("_{s}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path(pub Vec<SymbolName>);
// Paths look like "std::mem::drop"
// Right now they are just "drop"
impl Path {
    // temporary solution so i don't have to deal with actual paths.
    pub fn symbol(&self) -> &SymbolName {
        &self.0[0]
    }
}

// Patterns look like "Some(s)"
// right now they are just "s"
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Pattern {
    Path(Node<Path>),
}

impl Pattern {
    pub fn as_path_symbol(&self) -> SymbolName {
        match self {
            Pattern::Path(s) => SymbolName::External(s.inner.symbol().ident()),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Parameters(pub Vec<(Pattern, Node<TypeExpr>)>);

impl Parameters {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: SymbolName,
    pub parameters: Node<Parameters>,
    pub body: Node<Block>,
    pub return_type: Node<TypeExpr>,
}

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDeclaration(Node<FunctionDeclaration>),
    TupleStructDeclaration(Node<TupleStructDeclaration>),
    StructDeclaration(Node<StructDeclaration>),
    Mod(Node<Mod>),
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub name: String,
    pub body: Node<Program>,
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
    Tuple(Vec<Node<TypeExpr>>),
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
    LetDeclaration(Node<LetDeclaration>),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    /// Whether the last statement is to be returned
    pub does_return: bool,
}

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: SymbolName,
    pub fields: Vec<Node<TypeExpr>>,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: SymbolName,
    pub fields: Vec<(SymbolName, Node<TypeExpr>)>,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub value: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone)]
pub enum Expression {
    Break(Box<Expression>),
    Index(Node<Index>),
    Block(Node<Block>),
    BinaryOperation(Node<BinaryOperation>),
    UnaryOperation(Node<UnaryOperation>),
    Literal(Node<Literal>),
    FunctionCall(Node<FunctionCall>),
    IfExpression(Node<IfExpression>),
    Return(Box<Expression>),
    ForLoop(Node<ForLoop>),
    Path(Node<Path>),
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
    pub body: Node<Block>,
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
