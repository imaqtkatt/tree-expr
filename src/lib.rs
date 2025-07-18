use std::{collections::BTreeMap, iter::Peekable, str::Chars};

#[derive(Clone, Debug)]
pub struct Rule {
    name: String,
    arguments: u8,
    variadic: bool,
}

impl Rule {
    fn new(name: impl Into<String>, arguments: u8, variadic: bool) -> Self {
        Self {
            name: name.into(),
            arguments,
            variadic,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
}

impl Delimiter {
    pub(crate) const fn open_char(self) -> char {
        match self {
            Delimiter::Parentheses => '(',
            Delimiter::Brackets => '[',
            Delimiter::Braces => '{',
        }
    }

    pub(crate) const fn close_char(self) -> char {
        match self {
            Delimiter::Parentheses => ')',
            Delimiter::Brackets => ']',
            Delimiter::Braces => '}',
        }
    }
}

pub trait Specialize<T> {
    type Err;

    fn specialize(self) -> std::result::Result<T, Self::Err>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Form {
    Tall,
    Wide,
}

#[derive(Clone, Debug)]
pub enum Tree<Atom: Sized> {
    Atom(Atom),
    Tree {
        rule: Rule,
        arguments: Vec<Tree<Atom>>,
        varargs: Vec<Tree<Atom>>,
    },
    Group {
        delimiter: Delimiter,
        children: Vec<Tree<Atom>>,
    },
}

pub struct Parser<'src, Atom> {
    src: &'src str,
    peekable: Peekable<Chars<'src>>,
    index: usize,
    start: usize,

    rules: BTreeMap<String, Rule>,
    atom_parser: Box<dyn Fn(&'src str) -> ParseResult<Atom>>,
}

type ParseResult<T> = std::result::Result<T, String>;

impl<'src, Atom> Parser<'src, Atom> {
    const RESTRICT: &'static str = ";()[]{} \n\t\r";

    pub fn new(
        rules: BTreeMap<String, Rule>,
        atom_parser: Box<dyn Fn(&'src str) -> ParseResult<Atom>>,
        src: &'src str,
    ) -> Self {
        Self {
            src,
            peekable: src.chars().peekable(),
            index: 0,
            start: 0,
            rules,
            atom_parser,
        }
    }

    fn save(&mut self) {
        self.start = self.index;
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.peekable.peek()
    }

    pub fn consume(&mut self, c: char) -> bool {
        if self.peek().is_some_and(|c_| *c_ == c) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        let c = self.peekable.next()?;
        self.index += c.len_utf8();
        Some(c)
    }

    fn skip_while(&mut self, f: impl Fn(&char) -> bool) {
        while let Some(c) = self.peek() {
            if f(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn at_least_one_whitespace(&mut self) -> ParseResult<()> {
        let should_continue = match self.advance() {
            Some(c) if c.is_ascii_whitespace() => true,
            None | _ => false,
        };

        if !should_continue {
            Err("expected a whitespace")?
        }

        self.skip_while(char::is_ascii_whitespace);

        Ok(())
    }

    fn skip(&mut self) {
        self.skip_while(char::is_ascii_whitespace);
    }

    fn peek_group_close(&mut self, delimiter: Delimiter) -> Option<()> {
        let expected = delimiter.close_char();
        let c = self.peek()?;

        if *c == expected { Some(()) } else { None }
    }

    fn parse_group_close(&mut self, delimiter: Delimiter) -> ParseResult<()> {
        let expected = delimiter.close_char();
        let a = self.advance().ok_or("reached eof")?;

        if a == expected {
            Ok(())
        } else {
            Err(format!("expected '{expected}' but found '{a}'"))
        }
    }

    fn gap(&mut self) -> ParseResult<()> {
        let mut count = 0u32;

        while let Some(c) = self.peek() {
            match c {
                ' ' => count += 1,
                '\n' => count += 2,
                '\r' => (),
                '\t' => Err("can't read tabs".to_string())?,
                _ => break,
            }
            self.advance();
        }

        if count >= 2 {
            Ok(())
        } else {
            Err("expected a gap".to_string())
        }
    }

    fn peek_terminator(&mut self) -> Option<()> {
        let mut peekable = self.peekable.clone();

        let a = peekable.next()?;
        let b = peekable.next()?;

        match (a, b) {
            (';', ';') => Some(()),
            _ => None,
        }
    }

    fn parse_terminator(&mut self) -> ParseResult<()> {
        let a = self.advance().ok_or("reached eof")?;
        let b = self.advance().ok_or("reached eof")?;
        match (a, b) {
            (';', ';') => Ok(()),
            _ => Err("expected terminator".to_string()),
        }
    }

    fn parse_group(&mut self) -> ParseResult<Tree<Atom>> {
        let c = self.advance().ok_or("expected group delimiter")?;
        let delimiter = match c {
            '(' => Delimiter::Parentheses,
            '[' => Delimiter::Brackets,
            '{' => Delimiter::Braces,
            _ => Err("expected a valid group delimiter")?,
        };

        if self.peek_group_close(delimiter).is_some() {
            return Ok(Tree::Group {
                delimiter,
                children: vec![],
            });
        }

        let mut children = vec![];
        children.push(self.parse(false)?);

        while !self.peek_group_close(delimiter).is_some() {
            self.at_least_one_whitespace()?;
            children.push(self.parse(false)?);
        }

        self.parse_group_close(delimiter)?;

        Ok(Tree::Group {
            delimiter,
            children,
        })
    }

    fn parse_rule(&mut self) -> ParseResult<Tree<Atom>> {
        self.save();
        self.skip_while(|c| !Self::RESTRICT.contains(*c));

        let rule_str = &self.src[self.start..self.index];

        let rule = match self.rules.get(rule_str).cloned() {
            Some(value) => value,
            None => {
                let atom = (*self.atom_parser)(&self.src[self.start..self.index])?;
                println!("parse atom");
                return Ok(Tree::Atom(atom));
            }
        };

        println!("rule {rule_str}");
        let form = if self.consume('(') {
            Form::Wide
        } else {
            Form::Tall
        };
        println!("form = {form:?}");

        let mut arguments = vec![];

        for i in 0..rule.arguments {
            println!("arg {i}");
            match form {
                Form::Tall => self.gap()?,
                // Form::Wide if i >= rule.arguments - 1 => self.skip(),
                Form::Wide if i != 0 => self.at_least_one_whitespace()?,
                Form::Wide => (),
            }
            arguments.push(self.parse(false)?);
        }
        println!("here");

        if self.peek().is_some_and(|c| *c == ')') {
            self.advance();
            println!("leave {rule_str} wide");
            return Ok(Tree::Tree {
                rule,
                arguments,
                varargs: vec![],
            });
        }

        let mut varargs = vec![];
        let mut first = true;

        if rule.variadic {
            loop {
                if self.peek().is_some_and(|c| *c == ')') {
                    break;
                }

                match form {
                    Form::Tall => self.gap()?,
                    Form::Wide if !first => self.at_least_one_whitespace()?,
                    Form::Wide => (),
                }

                first = false;

                let should_stop = match form {
                    Form::Tall => self.peek_terminator().is_some(),
                    Form::Wide => self.peek().is_some_and(|c| *c == ')'),
                };
                if should_stop {
                    break;
                }

                varargs.push(self.parse(false)?);
            }

            match form {
                Form::Tall => self.parse_terminator()?,
                Form::Wide => self
                    .advance()
                    .filter(|c| *c == ')')
                    .map(|_| ())
                    .ok_or("unexpected char".to_string())?,
            }
        } else if form == Form::Wide {
            self.advance()
                .filter(|c| *c == ')')
                .map(|_| ())
                .ok_or("unexpected char".to_string())?
        }

        println!("leave {rule_str}");
        Ok(Tree::Tree {
            rule,
            arguments,
            varargs,
        })
    }

    pub fn parse(&mut self, should_skip: bool) -> ParseResult<Tree<Atom>> {
        if should_skip {
            self.skip();
            self.save();
        } else {
            self.save();
        }

        if self.peek_terminator().is_some() {
            Err("unexpected terminator")?
        }

        let c = self.peek();

        if let Some(c) = c {
            match c {
                ')' | ']' | '}' => Err("unexpected delimiter".to_string()),
                '(' | '[' | '{' => self.parse_group(),
                _ => self.parse_rule(),
            }
        } else {
            Err("Reached EOF".to_string())
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use crate::{Parser, Rule};

    #[test]
    fn parse_test() {
        let mut rules = BTreeMap::new();
        rules.insert("+".to_string(), Rule::new("+", 2, false));
        rules.insert("do".to_string(), Rule::new("do", 0, true));

        let atom_parser = Box::new(|s: &str| {
            s.parse::<i32>()
                .map_err(|_| format!("cannot parse integer from '{s}'"))
        });

        let src = r#"
        do
          +
            +  1  2
            3
          (1 2 3)
          +(3 4)
          do(1 2 3)
        ;;
        "#;

        let mut parser = Parser::new(rules, atom_parser, src);
        match parser.parse(true) {
            Ok(tree) => println!("{tree:#?}"),
            Err(e) => panic!("{e}"),
        }
    }
}
