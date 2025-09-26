pub mod lang;
pub mod parse;
pub mod predict;
pub mod pretty;
pub mod tree;
mod utils;

// Re-export the macro when the feature is enabled
#[cfg(feature = "macros")]
pub use tree_editor_macro::ebnf_grammar;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use crate::lang::Grammar;

    #[cfg(feature = "macros")]
    use crate::ebnf_grammar;

    #[test]
    #[cfg(feature = "macros")]
    fn ebnf_syntax_test() {
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        println!("EBNF Grammar:\n{}", grammar);
    }
}
