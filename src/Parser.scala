/* Grammar:

expr -> expr "+" expr
| expr "-" expr
| expr "*" expr
| expr "/" expr
| Num
| "(" expr ")"

Transformed grammar:

expr -> term { additive_op term }
term -> factor { mult_op factor }
factor -> Num | "(" expr ")"

additive_op -> "+" | "-"
mult_op -> "*" | "/"

 */
class Parser(scanner: Scanner) {

    var currentToken = scanner.next()

    def nextToken(): Unit =
        currentToken = scanner.next()

    def error(msg: String): Nothing = {
        println(msg)
        sys.exit(1)
    }

    def expect(kind: TokenKind): Unit =
        if currentToken.kind == kind then
            nextToken()
        else
            error(s"Parsing error: expected $kind token, but found $currentToken")

    /* prog -> expr EOF
     */
    def prog(): Unit = {
        expr()
        expect(EOF)
    }

    /* expr -> term { additive_op term }
     */
    def expr(): Unit = {
        term()
        while currentToken.kind == PLUS || currentToken.kind == MINUS do
            nextToken()
            term()
    }

    /* term -> factor { mult_op factor }
     */
    def term(): Unit = {
        factor()
        while currentToken.kind == TIMES || currentToken.kind == DIV do
            nextToken()
            factor()
    }

    /* factor -> Num | "(" expr ")"
     */
    def factor(): Unit = {
        if currentToken.kind == INTLIT then
            nextToken()
        else if currentToken.kind == LPAREN then
            nextToken()
            expr()
            expect(RPAREN)
        else
            error(s"Parsing error: expected INTLIT or LPAREN token, but found $currentToken")
    }
}
