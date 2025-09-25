use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    Error, Ident, LitStr, Token,
    parse::{Parse, ParseStream, Result},
};

// --------------- Public macro ---------------
#[proc_macro]
pub fn ebnf_grammar(input: TokenStream) -> TokenStream {
    match syn::parse::<EbnfGrammar>(input) {
        Ok(grammar) => grammar.expand().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

// --------------- AST ---------------
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
    RustExpression(TokenStream2),
}

// --------------- Parsing ---------------
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
        let name: Ident = input.parse()?;
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
    let mut alts = vec![parse_sequence(input)?];
    while input.peek(Token![|]) {
        input.parse::<Token![|]>()?;
        alts.push(parse_sequence(input)?);
    }
    Ok(if alts.len() == 1 {
        alts.into_iter().next().unwrap()
    } else {
        EbnfExpression::Choice(alts)
    })
}
fn parse_sequence(input: ParseStream) -> Result<EbnfExpression> {
    let mut elems = Vec::new();
    while !input.is_empty() && !input.peek(Token![|]) && !input.peek(Token![;]) {
        if !(input.peek(Ident)
            || input.peek(LitStr)
            || input.peek(syn::token::Paren)
            || input.peek(syn::token::Bracket)
            || input.peek(syn::token::Brace))
        {
            break;
        }
        elems.push(parse_term(input)?);
    }
    if elems.is_empty() {
        Ok(EbnfExpression::Sequence(vec![]))
    } else if elems.len() == 1 {
        Ok(elems.into_iter().next().unwrap())
    } else {
        Ok(EbnfExpression::Sequence(elems))
    }
}
fn parse_term(input: ParseStream) -> Result<EbnfExpression> {
    let mut expr = parse_primary(input)?;
    while !input.is_empty() {
        if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            expr = EbnfExpression::Repetition(Box::new(expr));
        } else if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
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
        Ok(EbnfExpression::Group(Box::new(content.parse()?)))
    } else if input.peek(syn::token::Bracket) {
        let content;
        syn::bracketed!(content in input);
        Ok(EbnfExpression::Optional(Box::new(content.parse()?)))
    } else if input.peek(syn::token::Brace) {
        let content;
        syn::braced!(content in input);
        Ok(EbnfExpression::Repetition(Box::new(content.parse()?)))
    } else if input.peek(LitStr) {
        let lit: LitStr = input.parse()?;
        let v = lit.value();
        if v.starts_with('/') && v.ends_with('/') && v.len() > 2 {
            Ok(EbnfExpression::Regex(v))
        } else {
            Ok(EbnfExpression::Terminal(v))
        }
    } else if input.peek(Ident) {
        let ident: Ident = input.parse()?;
        if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            let mut args = Vec::new();
            while !content.is_empty() {
                if content.peek(Token![<]) {
                    content.parse::<Token![<]>()?;
                    let eb = content.parse()?;
                    content.parse::<Token![>]>()?;
                    args.push(FunctionArg::EbnfExpression(eb));
                } else {
                    args.push(FunctionArg::RustExpression(parse_rust_expression(
                        &content,
                    )?));
                }
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
fn parse_rust_expression(input: ParseStream) -> Result<TokenStream2> {
    let mut tokens = Vec::new();
    while !input.is_empty() {
        if input.peek(Token![,]) {
            break;
        }
        tokens.push(input.parse::<proc_macro2::TokenTree>()?);
    }
    Ok(quote! { #(#tokens)* })
}

// --------------- Extraction (terminals -> lexer rules) ---------------
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum LexerPatternToken {
    Terminal(String),
    Regex(String),
}

impl EbnfExpression {
    fn extract_rules(
        &self,
        acc: &mut Vec<(String, LexerPatternToken)>,
        _counter: &mut usize,
    ) -> EbnfExpression {
        match self {
            EbnfExpression::Terminal(s) => {
                let name = format!("_{}", s);
                if !acc.iter().any(|(n, _)| n == &name) {
                    acc.push((name.clone(), LexerPatternToken::Terminal(s.clone())));
                }
                EbnfExpression::NonTerminal(name)
            }
            EbnfExpression::Regex(r) => {
                let pat = if r.starts_with('/') && r.ends_with('/') && r.len() > 2 {
                    r[1..r.len() - 1].to_string()
                } else {
                    r.clone()
                };
                let name = format!("_{}", pat);
                if !acc.iter().any(|(n, _)| n == &name) {
                    acc.push((name.clone(), LexerPatternToken::Regex(pat)));
                }
                EbnfExpression::NonTerminal(name)
            }
            EbnfExpression::NonTerminal(n) => EbnfExpression::NonTerminal(n.clone()),
            EbnfExpression::Choice(v) => {
                EbnfExpression::Choice(v.iter().map(|e| e.extract_rules(acc, _counter)).collect())
            }
            EbnfExpression::Sequence(v) => {
                EbnfExpression::Sequence(v.iter().map(|e| e.extract_rules(acc, _counter)).collect())
            }
            EbnfExpression::Optional(e) => {
                EbnfExpression::Optional(Box::new(e.extract_rules(acc, _counter)))
            }
            EbnfExpression::Repetition(e) => {
                EbnfExpression::Repetition(Box::new(e.extract_rules(acc, _counter)))
            }
            EbnfExpression::Group(e) => e.extract_rules(acc, _counter),
            EbnfExpression::RustFunctionCall(name, args) => {
                let new_args = args
                    .iter()
                    .map(|a| match a {
                        FunctionArg::EbnfExpression(e) => {
                            FunctionArg::EbnfExpression(e.extract_rules(acc, _counter))
                        }
                        FunctionArg::RustExpression(ts) => FunctionArg::RustExpression(ts.clone()),
                    })
                    .collect();
                EbnfExpression::RustFunctionCall(name.clone(), new_args)
            }
        }
    }
}

// --------------- Codegen ---------------
impl EbnfExpression {
    fn to_rule_tokens(&self) -> TokenStream2 {
        match self {
            EbnfExpression::NonTerminal(n) => quote! { grammar.rule_ref(#n) },
            EbnfExpression::Choice(alts) => {
                let parts: Vec<_> = alts.iter().map(|a| a.to_rule_tokens()).collect();
                quote! { choice(vec![#(#parts),*]) }
            }
            EbnfExpression::Sequence(seq) => {
                let elems: Vec<_> = seq.iter().map(|e| e.to_rule_tokens()).collect();
                quote! { seq(vec![#(#elems),*]) }
            }
            EbnfExpression::Optional(e) => {
                let inner = e.to_rule_tokens();
                quote! { optional(#inner) }
            }
            EbnfExpression::Repetition(e) => {
                let inner = e.to_rule_tokens();
                quote! { repeat(#inner) }
            }
            EbnfExpression::RustFunctionCall(name, args) => {
                let func_ident = Ident::new(name, Span::call_site());
                let arg_tokens: Vec<_> = args
                    .iter()
                    .map(|a| match a {
                        FunctionArg::EbnfExpression(e) => e.to_rule_tokens(),
                        FunctionArg::RustExpression(ts) => ts.clone(),
                    })
                    .collect();
                if name == "prec_left" || name == "prec_right" {
                    quote! { #func_ident(#(#arg_tokens),*) }
                } else {
                    quote! { #func_ident(#(#arg_tokens),*) }
                }
            }
            EbnfExpression::Group(e) => e.to_rule_tokens(),
            EbnfExpression::Terminal(_) | EbnfExpression::Regex(_) => {
                unreachable!("Terminals should have been extracted")
            }
        }
    }
}

// --------------- Expansion ---------------
impl EbnfGrammar {
    fn expand(self) -> TokenStream2 {
        let start = self.rules[0].name.to_string();
        let mut all_rule_names: std::collections::HashSet<String> =
            self.rules.iter().map(|r| r.name.to_string()).collect();

        let mut per_rule_data = Vec::new();
        let mut counter = 0usize;
        for r in &self.rules {
            let mut lexer_rules = Vec::new();
            let transformed = r.definition.extract_rules(&mut lexer_rules, &mut counter);
            // Collect all generated lexer rule names for interning
            for (name, _) in &lexer_rules {
                all_rule_names.insert(name.clone());
            }
            per_rule_data.push((r.name.to_string(), lexer_rules, transformed));
            counter += 1;
        }

        let intern_stmts: Vec<_> = all_rule_names
            .iter()
            .map(|n| quote! { let _ = grammar.name_table.intern(#n); })
            .collect();

        let mut add_rule_stmts = Vec::new();
        let mut pure_lexer_rules = std::collections::HashSet::new();

        for (name, lexers, expr) in &per_rule_data {
            // If a rule is just a non-terminal that points to an extracted lexer rule, treat it as a pure lexer rule.
            if let EbnfExpression::NonTerminal(nt_name) = expr {
                if lexers.iter().any(|(lname, _)| lname == nt_name) {
                    pure_lexer_rules.insert(name.clone());
                    // This was a trivial rule, so we just need its lexer part.
                    // The lexer rule will be added with the original name.
                    if let Some((_, pattern)) = lexers.iter().find(|(lname, _)| lname == nt_name) {
                        match pattern {
                            LexerPatternToken::Terminal(s) => add_rule_stmts.push(quote! { grammar.add_lexer_rule(#name, LexerPattern::Terminal(#s.to_string())); }),
                            LexerPatternToken::Regex(r) => add_rule_stmts.push(quote! { grammar.add_lexer_rule(#name, LexerPattern::Regex(#r.to_string())); }),
                        };
                    }
                    continue;
                }
            }

            let add_lexers: Vec<_> = lexers.iter().map(|(lname, pat)| match pat {
                LexerPatternToken::Terminal(s) => quote! { grammar.add_lexer_rule(#lname, LexerPattern::Terminal(#s.to_string())); },
                LexerPatternToken::Regex(r) => quote! { grammar.add_lexer_rule(#lname, LexerPattern::Regex(#r.to_string())); },
            }).collect();

            let expr_tokens = expr.to_rule_tokens();
            add_rule_stmts.push(quote! {
                {
                    #(#add_lexers)*
                    let rule_expr = { #expr_tokens };
                    grammar.add_parser_rule(#name, rule_expr);
                }
            });
        }

        quote! {
            {
                use crate::lang::*;
                let mut grammar = Grammar::new();
                grammar.set_start_rule(#start);
                #(#intern_stmts)*
                #(#add_rule_stmts)*
                grammar.process_precedence();
                grammar
            }
        }
    }
}
