package com.simplytyped.yoyak.parser.cil

import com.simplytyped.yoyak.parser.cil.CommonILParser._

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional

class CommonILParser extends JavaTokenParsers {
  def clazz : Parser[Clazz] = positioned(
    identifier~"{"~rep(method)~"}" ^^ { case name~_~list~_ => Clazz(name,list) }
  )
  def method : Parser[Method] = positioned(
    identifier~"("~opt(identifierWithType)~rep(","~>identifierWithType)~")"~block ^^
      { case name~_~firstParam~params~_~body => Method(name, firstParam.toList++params, body) }
  )
  def identifierWithType : Parser[(Ident,Type)] = identifier~":"~ty ^^ { case x~_~y => (x,y) }

  def block : Parser[Block] = positioned("{"~>rep(cilstmt <~ ";")<~"}" ^^ Block)

  def cilstmt : Parser[CILStmt] = positioned(ifstmt | assignstmt | invokestmt | returnstmt)
  def ifstmt : Parser[If] = positioned(
    "if"~"("~value~")"~block~"else"~block ^^ { case _~_~v~_~b1~_~b2 => If(v,b1,b2) }
  )
  def assignstmt : Parser[Assign] = positioned(
    identifier~"="~value ^^ { case lv~_~rv => Assign(lv,rv) }
  )
  def invokestmt : Parser[Invoke] = positioned(
    identifier~"("~opt(value)~rep(","~>value)~")" ^^
      { case callee~_~firstOpt~list~_ => Invoke(callee,firstOpt.toList++list) }
  )
  def returnstmt : Parser[Return] = positioned(
    "return"~>value ^^ Return
  )

  def value : Parser[Value] = positioned(identifier | cinteger | cstring | exp)
  def identifier : Parser[Ident] = positioned(ident ^^ Ident)
  def cinteger : Parser[CInteger] = positioned(wholeNumber ^^ {x => CInteger(x.toInt)})
  def cstring : Parser[CString] = positioned(stringLiteral ^^ CString)
  def exp : Parser[Exp] = positioned(value ~ operator ~ value ^^ {case lv~op~rv => Exp(lv,op,rv)})

  def operator : Parser[Operator] =
    "+" ^^ { _ => Add } |
    "-" ^^ { _ => Sub } |
    "*" ^^ { _ => Mul } |
    "/" ^^ { _ => Div }

  def ty : Parser[Type] =
    "int" ^^ { _ => IntegerType } |
    "string" ^^ { _ => StringType }
}

object CommonILParser {
  case class Clazz(name: Ident, methods: List[Method]) extends Positional
  case class Method(name: Ident, params: List[(Ident, Type)], body: Block) extends Positional

  case class Block(stmts: List[CILStmt]) extends Positional

  abstract class CILStmt extends Positional
  case class If(cond: Value, thenBlock: Block, elseBlock: Block) extends CILStmt
  case class Assign(lv: Ident, rv: Value) extends CILStmt
  case class Invoke(callee: Ident, params: List[Value]) extends CILStmt
  case class Return(v: Value) extends CILStmt

  abstract class Value extends Positional
  case class Ident(id: String) extends Value
  case class CInteger(v: Int) extends Value
  case class CString(v: String) extends Value
  case class Exp(lv: Value, op: Operator, rv: Value) extends Value

  abstract class Operator
  case object Add extends Operator
  case object Sub extends Operator
  case object Mul extends Operator
  case object Div extends Operator

  abstract class Type
  case object IntegerType extends Type
  case object StringType extends Type
}