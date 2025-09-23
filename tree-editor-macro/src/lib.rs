use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Error, Ident, LitStr, Token,
    parse::{Parse, ParseStream, Result},
    parse_macro_input,
};

/// A proc macro that enables elegant EBNF syntax for defining grammars
///
/// Usage:
/// ```rust
/// use tree_editor_macro::ebnf_grammar;
///
/// let grammar = ebnf_grammar! {
///     expr ::= term { ("+" | "-") term } ;
///     term ::= factor { ("*" | "/") factor } ;
///     factor ::= number | "(" expr ")" | some_func(<number>, "literal") ;
///     number ::= /[0-9]+/ ;
/// };
/// ```
///
/// Function calls are supported with `xxx(...)` syntax.
/// EBNF expressions as arguments should be wrapped in `<>`.
/// Regular Rust expressions can be passed directly as arguments.
#[proc_macro]
pub fn ebnf_grammar(input: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(input as EbnfGrammar);
    let expanded = grammar.expand();
    TokenStream::from(expanded)
}

struct EbnfGrammar {
    rules: Vec<EbnfRule>,
}

struct EbnfRule {
    name: Ident,
    definition: EbnfExpression,
}

#[derive(Debug, Clone)]
enum EbnfExpression {
    Terminal(String),
    Regex(String),
    NonTerminal(String),
    Choice(Vec<EbnfExpression>),
    Sequence(Vec<EbnfExpression>),
    Optional(Box<EbnfExpression>),
    Repetition(Box<EbnfExpression>),
    Group(Box<EbnfExpression>),
    RustFunctionCall(String, Vec<FunctionArg>),
}

#[derive(Debug, Clone)]
enum FunctionArg {
    EbnfExpression(EbnfExpression),
    RustExpression(proc_macro2::TokenStream),
}

impl Parse for EbnfGrammar {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = Vec::new();

        while !input.is_empty() {
            rules.push(input.parse()?);
        }

        if rules.is_empty() {
            return Err(Error::new(
                Span::call_site(),
                "Grammar must contain at least one rule",
            ));
        }

        Ok(EbnfGrammar { rules })
    }
}

impl Parse for EbnfRule {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![::]>()?;
        input.parse::<Token![=]>()?;
        let definition = input.parse()?;
        input.parse::<Token![;]>()?;

        Ok(EbnfRule { name, definition })
    }
}

impl Parse for EbnfExpression {
    fn parse(input: ParseStream) -> Result<Self> {
        parse_choice(input)
    }
}

fn parse_choice(input: ParseStream) -> Result<EbnfExpression> {
    let mut alternatives = vec![parse_sequence(input)?];

    while input.peek(Token![|]) {
        input.parse::<Token![|]>()?;
        alternatives.push(parse_sequence(input)?);
    }

    Ok(if alternatives.len() == 1 {
        alternatives.into_iter().next().unwrap()
    } else {
        EbnfExpression::Choice(alternatives)
    })
}

fn parse_sequence(input: ParseStream) -> Result<EbnfExpression> {
    let mut elements = Vec::new();

    // Parse elements until we hit a delimiter or end of input
    while !input.is_empty() && !input.peek(Token![|]) && !input.peek(Token![;]) {
        // Break if we see nothing parseable ahead
        if !input.peek(Ident)
            && !input.peek(LitStr)
            && !input.peek(syn::token::Paren)
            && !input.peek(syn::token::Bracket)
            && !input.peek(syn::token::Brace)
        {
            break;
        }

        elements.push(parse_term(input)?);
    }

    // Handle empty sequences more gracefully
    if elements.is_empty() {
        // This might be valid in certain contexts (like inside parentheses for choices)
        Ok(EbnfExpression::Sequence(vec![]))
    } else if elements.len() == 1 {
        Ok(elements.into_iter().next().unwrap())
    } else {
        Ok(EbnfExpression::Sequence(elements))
    }
}

fn parse_term(input: ParseStream) -> Result<EbnfExpression> {
    let mut expr = parse_primary(input)?;

    // Handle postfix operators
    while !input.is_empty() {
        if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            expr = EbnfExpression::Repetition(Box::new(expr));
        } else if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            // Convert + to repetition with at least one occurrence
            expr = EbnfExpression::Sequence(vec![
                expr.clone(),
                EbnfExpression::Repetition(Box::new(expr)),
            ]);
        } else if input.peek(Token![?]) {
            input.parse::<Token![?]>()?;
            expr = EbnfExpression::Optional(Box::new(expr));
        } else {
            break;
        }
    }

    Ok(expr)
}

fn parse_primary(input: ParseStream) -> Result<EbnfExpression> {
    if input.peek(syn::token::Paren) {
        let content;
        syn::parenthesized!(content in input);
        let inner = content.parse()?;
        Ok(EbnfExpression::Group(Box::new(inner)))
    } else if input.peek(syn::token::Bracket) {
        let content;
        syn::bracketed!(content in input);
        let inner = content.parse()?;
        Ok(EbnfExpression::Optional(Box::new(inner)))
    } else if input.peek(syn::token::Brace) {
        let content;
        syn::braced!(content in input);
        let inner = content.parse()?;
        Ok(EbnfExpression::Repetition(Box::new(inner)))
    } else if input.peek(LitStr) {
        let lit: LitStr = input.parse()?;
        let value = lit.value();

        if value.starts_with('/') && value.ends_with('/') && value.len() > 2 {
            Ok(EbnfExpression::Regex(value))
        } else {
            Ok(EbnfExpression::Terminal(value))
        }
    } else if input.peek(Ident) {
        let ident: Ident = input.parse()?;

        // Check if this is a function call
        if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);

            let mut args = Vec::new();

            // Parse function arguments
            while !content.is_empty() {
                if content.peek(Token![<]) {
                    // Parse EBNF expression wrapped in angle brackets
                    content.parse::<Token![<]>()?;
                    let ebnf_expr = content.parse()?;
                    content.parse::<Token![>]>()?;
                    args.push(FunctionArg::EbnfExpression(ebnf_expr));
                } else {
                    // Parse regular Rust expression
                    let rust_expr = parse_rust_expression(&content)?;
                    args.push(FunctionArg::RustExpression(rust_expr));
                }

                // If there's more content, expect a comma
                if !content.is_empty() {
                    content.parse::<Token![,]>()?;
                }
            }

            Ok(EbnfExpression::RustFunctionCall(ident.to_string(), args))
        } else {
            Ok(EbnfExpression::NonTerminal(ident.to_string()))
        }
    } else {
        Err(Error::new(
            input.span(),
            "Expected terminal, non-terminal, or grouped expression",
        ))
    }
}

fn parse_rust_expression(input: ParseStream) -> Result<proc_macro2::TokenStream> {
    // Parse the remaining tokens as a Rust expression
    let mut tokens = Vec::new();

    while !input.is_empty() {
        if input.peek(Token![,]) {
            // Stop at comma at the top level
            break;
        } else {
            // Parse any token tree (which handles balanced delimiters automatically)
            let token = input.parse::<proc_macro2::TokenTree>()?;
            tokens.push(token);
        }
    }

    Ok(quote! { #(#tokens)* })
}

impl EbnfGrammar {
    fn expand(self) -> proc_macro2::TokenStream {
        if self.rules.is_empty() {
            return quote! { compile_error!("Grammar must contain at least one rule"); };
        }

        let start_rule = &self.rules[0].name;
        let start_rule_str = start_rule.to_string();

        let rule_definitions: Vec<_> = self
            .rules
            .iter()
            .map(|rule| {
                let name_str = rule.name.to_string();
                let definition = rule.definition.to_tokens();
                quote! {
                    grammar.add_rule(#name_str, #definition);
                }
            })
            .collect();

        quote! {
            {
                use crate::lang::*;
                let mut grammar = Grammar::new(#start_rule_str);
                #(#rule_definitions)*
                grammar
            }
        }
    }
}

impl EbnfExpression {
    fn to_tokens(&self) -> proc_macro2::TokenStream {
        match self {
            EbnfExpression::Terminal(s) => {
                quote! { RuleNode::Terminal(#s) }
            }
            EbnfExpression::Regex(r) => {
                quote! { RuleNode::Terminal(#r) }
            }
            EbnfExpression::NonTerminal(name) => {
                quote! { RuleNode::RuleRef(#name) }
            }
            EbnfExpression::Choice(alts) => {
                let alt_tokens: Vec<_> = alts.iter().map(|alt| alt.to_tokens()).collect();
                quote! { RuleNode::Choice(vec![#(#alt_tokens),*]) }
            }
            EbnfExpression::Sequence(seq) => {
                let seq_tokens: Vec<_> = seq.iter().map(|elem| elem.to_tokens()).collect();
                quote! { RuleNode::Sequence(vec![#(#seq_tokens),*]) }
            }
            EbnfExpression::Optional(expr) => {
                let inner = expr.to_tokens();
                quote! { RuleNode::Optional(Box::new(#inner)) }
            }
            EbnfExpression::Repetition(expr) => {
                let inner = expr.to_tokens();
                quote! { RuleNode::Repetition(Box::new(#inner)) }
            }
            EbnfExpression::Group(expr) => {
                expr.to_tokens() // Groups are just for parsing precedence
            }
            EbnfExpression::RustFunctionCall(name, args) => {
                let func_name = syn::Ident::new(name, proc_macro2::Span::call_site());

                // Convert arguments to the appropriate format
                let arg_tokens: Vec<proc_macro2::TokenStream> = args
                    .iter()
                    .map(|arg| {
                        match arg {
                            FunctionArg::EbnfExpression(expr) => {
                                // For EBNF expressions, pass them as RuleNode
                                expr.to_tokens()
                            }
                            FunctionArg::RustExpression(tokens) => {
                                // For Rust expressions, pass them directly
                                tokens.clone()
                            }
                        }
                    })
                    .collect();

                // Generate a direct function call
                quote! { #func_name(#(#arg_tokens),*) }
            }
        }
    }
}
