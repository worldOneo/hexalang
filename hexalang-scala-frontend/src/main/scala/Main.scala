import java.util.ArrayList

enum TokenValue {
  case Identifier
  case String
  case Number
  case InlineComment
  case BraceOpen
  case BraceClose
  case ParenOpen
  case ParenClose
  case GT
  case LT
  case EQ
  case GTEQ
  case LTEQ
  case EQEQ
  case SquareOpen
  case SquareClose
  case Protocol
  case Process
  case Fn
  case If
  case Else
  case For
  case Var
  case Val
  case Plus
  case Minus
  case Mul
  case Div
  case Mod
  case Pipe
  case Hat
  case And
  case Land
  case Or
  case Lor
  case Dot
  case Colon
  case Semicolon
  case Comma
  case Not
  case ShiftL
  case ShiftR
  case Whitespace
  case Generic
  case Struct
  case Enum
  case Tuple
  // Phantom
  case PhantomBraceClose
  case PhantomSquareClose
  case PhantomParenClose
}

class Token(tokenValue: TokenValue, offset: Int) {
  def value() = tokenValue
}

case class Source(val tokens: List[Token], val _offset: Int) {
  def next(): (Source, Option[Token]) = {
    if tokens.length <= this._offset then (this, None)
    else (this.copy(_offset = _offset + 1), Some(this.tokens(this._offset)))
  }

  def offset(): Int = _offset
}

@FunctionalInterface
trait Parser[T] extends ((Source) => ParserResult[T]) {}

enum ParserResult[T] {
  case Ok(value: T, source: Source)
  case Error(message: String)
  case NoMatch()

  def morph[B](): ParserResult[B] = {
    return this match {
      case Error(message)    => Error(message)
      case NoMatch()         => NoMatch()
      case Ok(value, source) => throw IllegalStateException("Morphed Ok enum")
    }
  }
}

def parseConsecutive2[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] =
  (arg: Source) => {
    a(arg) match {
      case ParserResult.Ok(valuea, sourcea) =>
        b(sourcea) match {
          case ParserResult.Ok(value, source) =>
            ParserResult.Ok(value = (valuea, value), source = source)
          case default => default.morph()
        }
      case default => default.morph()
    }
  }

def parseConsecutive3[A, B, C](
    a: Parser[A],
    b: Parser[B],
    c: Parser[C]
): Parser[(A, B, C)] =
  (arg: Source) =>
    parseConsecutive2(a, b)(arg) match {
      case ParserResult.Ok(value, source) =>
        c(source) match {
          case ParserResult.Ok(valueb, source) =>
            ParserResult.Ok(
              value = (value(0), value(1), valueb),
              source = source
            )
          case default => default.morph()
        }
      case default => default.morph()
    }

def parseConsecutive4[A, B, C, D](
    a: Parser[A],
    b: Parser[B],
    c: Parser[C],
    d: Parser[D]
): Parser[(A, B, C, D)] =
  (arg: Source) =>
    parseConsecutive2(a, b)(arg) match {
      case ParserResult.Ok(value, source) =>
        parseConsecutive2(c, d)(source) match {
          case ParserResult.Ok(valueb, source) =>
            ParserResult.Ok(
              value = (value(0), value(1), valueb(0), valueb(1)),
              source = source
            )
          case default => default.morph()
        }
      case default => default.morph()
    }

def parseConsecutive5[A, B, C, D, E](
    a: Parser[A],
    b: Parser[B],
    c: Parser[C],
    d: Parser[D],
    e: Parser[E]
): Parser[(A, B, C, D, E)] =
  (arg: Source) =>
    parseConsecutive3(a, b, c)(arg) match {
      case ParserResult.Ok(value, source) =>
        parseConsecutive2(d, e)(source) match {
          case ParserResult.Ok(valueb, source) =>
            ParserResult.Ok(
              value = (value(0), value(1), value(2), valueb(0), valueb(1)),
              source = source
            )
          case default => default.morph()
        }
      case default => default.morph()
    }

def parseAnyOf[A](a: Parser[A]*): Parser[A] =
  def anon(arg: Source): ParserResult[A] =
    a.map(_(arg))
      .foreach(_ match {
        case ParserResult.Error(message) => return ParserResult.Error(message)
        case ParserResult.NoMatch()      =>
        case ParserResult.Ok(value, source) =>
          return ParserResult.Ok(value, source)
      })
    ParserResult.NoMatch()
  anon

def parseEither[A, B](a: Parser[A], b: Parser[B]): Parser[Either[A, B]] =
  (arg: Source) =>
    a(arg) match {
      case ParserResult.NoMatch() =>
        b(arg) match {
          case ParserResult.Ok(value, source) =>
            ParserResult.Ok(Right(value), source)
          case default => default.morph()
        }
      case ParserResult.Ok(value, source) =>
        ParserResult.Ok(Left(value), source)
      case default => default.morph()
    }

def parseWhileFollows[A, B](
    parser: Parser[A],
    follows: Parser[B]
): Parser[ArrayList[A]] = {
  def anon(arg: Source): ParserResult[ArrayList[A]] = {
    var nsource = arg
    var res = ArrayList[A]()
    while (true) {
      parser(nsource) match {
        case ParserResult.Ok(value, source) => {
          res.add(value)
          nsource = source
        }
        case default => return default.morph()
      }

      follows(nsource) match {
        case ParserResult.Ok(value, source) => nsource = source
        case default                        => return default.morph()
      }
    }
    return ParserResult.Ok(res, nsource)
  }
  return anon
}

def parseExpectToken(value: TokenValue): Parser[Token] =
  (source: Source) =>
    source.next() match
      case (nsource, Some(t)) => ParserResult.Ok(t, nsource)
      case default            => ParserResult.NoMatch()

def parseExpectTokenAnyOf(value: TokenValue*): Parser[Token] =
  parseAnyOf(value.map(parseExpectToken(_))*)

def mustMatch[A](p: Parser[A], message: String): Parser[A] =
  (arg: Source) =>
    p(arg) match {
      case ParserResult.NoMatch() => ParserResult.Error(message)
      case default                => default
    }

def parseExpression(): Parser[Node] =
  parseAnyOf(
    parseUnaryExpression(),
    parseBinaryExpression,
    parseFunction
  )

def parseInit(): Parser[Node] =
  (arg: Source) =>
    parseConsecutive3(
      parseExpectToken(TokenValue.Identifier),
      parseExpectTokenAnyOf(TokenValue.Val, TokenValue.Var),
      mustMatch(parseExpression(), "Initialisation expected expression")
    )(arg) match {
      case ParserResult.Ok((_, valvar, expr), source) =>
        ParserResult.Ok(
          Node(
            FunctionalAST.Init(valvar.value() == TokenValue.Val, expr),
            arg.offset()
          ),
          source
        )
      case default => default.morph()
    }

def parseUnaryExpression(): Parser[Node] =
  (arg: Source) =>
    parseConsecutive2(
      parseExpectTokenAnyOf(
        TokenValue.Plus,
        TokenValue.Minus,
        TokenValue.Not
      ),
      mustMatch(parseExpression(), "Unary prefix requires expression")
    )(arg) match
      case ParserResult.Ok((op, expr), source) =>
        ParserResult.Ok(
          Node(FunctionalAST.UnaryExpr(op.value(), expr), source.offset()),
          source
        )
      case default => default.morph()

def parseExtensionMaxWeight(weight: Int): Parser[Node] =
  val operatorWeights = List(
    (TokenValue.ShiftL, 25),
    (TokenValue.ShiftR, 25),
    (TokenValue.Or, 25),
    (TokenValue.And, 25),
    (TokenValue.Mul, 20),
    (TokenValue.Div, 20),
    (TokenValue.Mod, 20),
    (TokenValue.Plus, 15),
    (TokenValue.Minus, 15),
    (TokenValue.EQEQ, 10),
    (TokenValue.GT, 10),
    (TokenValue.LT, 10),
    (TokenValue.GTEQ, 10),
    (TokenValue.LTEQ, 10),
    (TokenValue.Lor, 5),
    (TokenValue.Land, 5)
  )
  val possibleOperators = operatorWeights.filter(_(1) < weight).map(_(0))

@main def hello(): Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
