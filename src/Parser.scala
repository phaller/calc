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
    def prog(): Expr = {
        val e = expr()
        expect(EOF)
        e
    }

    /* expr -> term { additive_op term }
     */
    def expr(): Expr = {
        var lhs = term()
        while currentToken.kind == PLUS || currentToken.kind == MINUS do
            val opKind = currentToken.kind
            nextToken()
            val rhs = term()
            lhs = EBinOp(opKind, lhs, rhs)
        lhs
    }

    /* term -> factor { mult_op factor }
     */
    def term(): Expr = {
        // example: (3 * 5) * 2
        var lhs = factor() // 3
        while currentToken.kind == TIMES || currentToken.kind == DIV do
            val opKind = currentToken.kind  // *
            nextToken()
            val rhs = factor() // 2
            lhs = EBinOp(opKind, lhs, rhs) // EBinOp(*, EBinOp(*, 3, 5), 2)
        lhs
    }

    /* factor -> Num | "(" expr ")"
     */
    def factor(): Expr = currentToken match {
        case tok: IntToken =>
            nextToken()
            ENum(tok.value)
        case Token(LPAREN) =>
            nextToken()
            val e = expr()
            expect(RPAREN)
            e
        case _ =>
            error(s"Parsing error: expected INTLIT or LPAREN token, but found $currentToken")
    }
}
