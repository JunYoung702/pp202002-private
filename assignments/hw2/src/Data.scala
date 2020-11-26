package pp202002.hw2.Data

sealed abstract class Token
case class TLit(c: Char) extends Token
case class TOr(c: Char) extends Token
case class TStar(c: Char) extends Token
case class TPar(c: Char) extends Token

sealed abstract class Exp
case class EChar(c:Char) extends Exp
case class EStar(e: Exp) extends Exp
case class EConcat(e1: Exp, e2: Exp) extends Exp
case class EOr(e1:Exp, e2:Exp) extends Exp
case object EEpsilon extends Exp
case object EError extends Exp