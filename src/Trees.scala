sealed trait Expr
case class EBinOp(op: TokenKind, lhs: Expr, rhs: Expr) extends Expr
case class ENum(value: Int) extends Expr
