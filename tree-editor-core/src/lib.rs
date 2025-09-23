pub mod lang;
pub mod tree;

// Re-export the macro when the feature is enabled
#[cfg(feature = "macros")]
pub use tree_editor_macro::ebnf_grammar;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use crate::{choice, make_grammar, rep, rule, seq, term};

    #[cfg(feature = "macros")]
    use crate::ebnf_grammar;

    #[test]
    fn it_works() {
        let grammar = make_grammar!(expr,
            expr ::= seq![rule!(term), rep![seq![choice![term!("+"), term!("-")], rule!(term)]]],
            term ::= seq![rule!(factor), rep![seq![choice![term!("*"), term!("/")], rule!(factor)]]],
            factor ::= choice![rule!(number), seq![term!("("), rule!(expr), term!(")")]],
            number ::= term!("/[0-9]+/")
        );
        println!("{}", grammar);
    }

    #[test]
    #[cfg(feature = "macros")]
    fn ebnf_syntax_test() {
        let mut grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        grammar.process_precedence();
        println!("EBNF Grammar:\n{}", grammar);
    }
}
