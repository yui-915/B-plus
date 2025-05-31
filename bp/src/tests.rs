use super::*;

#[test]
fn test_parse_expr() {
    fn test(input: &str, expected: &str) {
        let tokens = tokenize(&input, "test");
        let mut c = Compiler {
            tokens: Iter::new(tokens),
            source: input.to_owned(),
        };
        let expr = parse_expr(&mut c, 0., TokenKind::Eof);
        let str = expr.to_string();
        if str != expected {
            eprintln!("parse_expr test failed!");
            eprintln!("while parsing: `{}`", input);
            eprintln!("     expected: `{}`", expected);
            eprintln!("        found: `{}`", str);
            panic!();
        }
    }

    test("1", "1");
    test("((((1))))", "1");
    test("1 + 2 * 3", "(+ 1 (* 2 3))");
    test("1 * 2 + 3", "(+ (* 1 2) 3)");
    test("0 + 1 * 2 * 3 + 4", "(+ (+ 0 (* (* 1 2) 3)) 4)");
    test("0 + 1 * 2 + 3 * 4", "(+ (+ 0 (* 1 2)) (* 3 4))");
    test("(1 + 2) * 3", "(* (+ 1 2) 3)");
    test("1 * (2 + 3)", "(* 1 (+ 2 3))");
    test("1 * (2 + 3)", "(* 1 (+ 2 3))");
    test("3 - 2 - 1", "(- (- 3 2) 1)");
    test("-1", "(- 1)");
    test("-  1", "(- 1)");
    test("-(-1)", "(- (- 1))");
    test("1 - - 1 - 1", "(- (- 1 (- 1)) 1)");
    test("++1", "(++ 1)");
    test("1++", "(++ 1)");
    test("-++1", "(- (++ 1))");
    test("-1++", "(- (++ 1))");
    test("1 + 2 [ 3(4) ]", "(+ 1 ([ 2 (f 3 4)))");
    test("++1[2]", "(++ ([ 1 2))");
    test("1++[2]", "([ (++ 1) 2)");
    test("1[2]++", "(++ ([ 1 2))");
}
