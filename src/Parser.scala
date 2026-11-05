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
        val e = expr(1)
        expect(EOF)
        e
    }

    def atom(): Expr = currentToken match {
        case t: IntToken =>
            nextToken()
            ENum(t.value)
        case Token(LPAREN) =>
            nextToken()
            val e = expr(1)
            expect(RPAREN)
            e
        case _ =>
            error("Expected atom, but found: " + currentToken)
    }

    val precedence = Map[TokenKind, Int](
        PLUS -> 1,
        MINUS -> 1,
        TIMES -> 2,
        DIV -> 2,
        EXP -> 3
    )

    // left = 1, right = 0
    val assoc = Map[TokenKind, Int](
        PLUS -> 1,
        MINUS -> 1,
        TIMES -> 1,
        DIV -> 1,
        EXP -> 0
    )

    def isBinOp(t: TokenKind) = precedence.keySet.contains(t)

    def expr(minPrec: Int): Expr = {
        var lhs = atom()
        var done = false
        while !done do {
            val op = currentToken.kind
            if !isBinOp(op) || precedence(op) < minPrec then
                done = true
            else
                val prec = precedence(op)
                nextToken()
                val rhs = expr(prec + assoc(op))
                lhs = EBinOp(op, lhs, rhs)
        }
        lhs
    }

}
