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

}
