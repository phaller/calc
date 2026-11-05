
case class Token(kind: TokenKind)

class IntToken(val value: Int) extends Token(INTLIT) {
    override def toString(): String =
        s"INTLIT($value)"
}

trait TokenKind

case object PLUS extends TokenKind
case object MINUS extends TokenKind
case object TIMES extends TokenKind
case object DIV extends TokenKind
case object EXP extends TokenKind
case object LPAREN extends TokenKind
case object RPAREN extends TokenKind
case object INTLIT extends TokenKind

case object EOF extends TokenKind
case object BAD extends TokenKind
