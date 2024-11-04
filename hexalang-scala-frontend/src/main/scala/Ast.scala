case class Node(ast: FunctionalAST, location: Int)

enum TypeNode {
  case Identifier(value: String)
}

class FunctionSignature(
    args: Map[String, Option[TypeNode]],
    returnType: Option[TypeNode],
    expression: Node
)

enum FunctionalAST {
  case Int(value: String)
  case Float(value: String)
  case Text(value: String)
  case Identifier(name: String)
  case BinaryExpr(left: Node, op: TokenValue, right: Node)
  case UnaryExpr(op: TokenValue, expr: Node)
  case If(
      condition: Node,
      thenblock: Node,
      elseblock: Option[Node]
  )
  case Function(
      signature: FunctionSignature,
      body: Node
  )
  case Return(expression: Node)
  case Struct(fields: Map[String, TypeNode], functions: Map[String, TypeNode])
  case Init(asVal: Boolean, expr: Node)
}
