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

    def error(msg: String) =
        println("Parsing error: " + msg)
        sys.exit(1)

    def expect(kind: TokenKind) =
        if currentToken.kind == kind then
            nextToken()
        else
            error(s"expected $kind token, but found $currentToken")

    /* prog -> expr EOF
     */
    def prog(): Expr =
        val tree = expr()
        expect(EOF)
        tree

    /* expr -> term { additive_op term }
     */
    def expr(): Expr =
        var lhs = term()
        // example: 4 - 5 + 7 - 9
        // ((4 - 5) + 7) - 9
        while currentToken.kind == PLUS || currentToken.kind == MINUS do
            val opKind = currentToken.kind
            nextToken()
            val rhs = term()
            lhs = EBinOp(opKind, lhs, rhs)
        lhs

    /* term -> factor { mult_op factor }
     */
    def term(): Expr =
        var lhs = factor()
        while currentToken.kind == TIMES || currentToken.kind == DIV do
            val opKind = currentToken.kind
            nextToken()
            val rhs = factor()
            lhs = EBinOp(opKind, lhs, rhs)
        lhs

    /* factor -> Num | "(" expr ")"
     */
    def factor(): Expr =
        currentToken match {
            case tok: IntToken =>
                val value = tok.value
                nextToken()
                ENum(value)
            case Token(LPAREN) =>
                nextToken()
                val tree = expr()
                expect(RPAREN)
                tree
            case _ =>
                error("expected INTLIT or LPAREN token")
        }

}
