use lalrpop_util::lalrpop_mod;

pub mod ast;
mod lexer;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    java
);

/// Parse error type returned by [`parse`].
pub type ParseError<'a> =
    lalrpop_util::ParseError<usize, lexer::Token<'a>, lexer::LexicalError<'a>>;

/// Result alias for parsing `.java` sources.
pub type ParseResult<'a> = Result<ast::Root, ParseError<'a>>;

#[allow(clippy::needless_lifetimes)]
pub fn parse<'a>(data: &'a str) -> ParseResult<'a> {
    let lexer = lexer::Lexer::new(data);
    let parser = java::RootParser::new();

    parser.parse(data, lexer)
}
