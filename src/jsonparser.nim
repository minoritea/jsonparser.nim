import std/unicode, std/strutils, std/strformat

type
  TokenKind* = enum
    None,
    Space,
    BeginObject,
    EndObject,
    BeginArray,
    EndArray,
    Colon,
    Comma,
    String,
    Number,
    True,
    False,
    Null

  Token* = object
    case kind: TokenKind
    of None: nil
    of Space: nil
    of BeginObject: nil
    of EndObject: nil
    of BeginArray: nil
    of EndArray: nil
    of Colon: nil
    of Comma: nil
    of String: stringValue: string
    of Number: numberValue: string
    of True: nil
    of False:nil
    of Null: nil
  
  JSONType* = enum
    Top,
    Object,
    ObjectField,
    Array,
    String,
    Number,
    True,
    False,
    Null

  Node* = ref object
    parent {. cursor .}: Node
    case kind*: JSONType
    of Top: topValue*: Node
    of Object:
      fields: seq[Node]
      objectCommaCount: int
    of ObjectField:
      key: string
      value: Node
      withColon: bool
    of Array:
      items: seq[Node]
      arrayCommaCount: int
    of String: stringValue: string
    of Number: numberValue: string
    of True: nil
    of False: nil
    of Null: nil


proc finishToken(kind: TokenKind, buf: seq[char]): Token =
  case kind
  of None:
    if buf.len > 0:
      raise newException(ValueError, "invalid token")
    return Token(kind: kind)
  of Space, BeginObject, EndObject, BeginArray, EndArray, Colon, Comma, True, False, Null:
    return Token(kind: kind)
  of String:
    return Token(kind: kind, stringValue: buf.substr)
  of Number:
    return Token(kind: kind, numberValue: buf.substr)


proc tokenize*(src: string): seq[Token] =
  var buf: seq[char]
  var kind: TokenKind = None
  var withEscape = false
  for s in src:
    case kind
    of True:
      if (s == 'r' and buf.len == 1) or (s == 'u' and buf.len == 2) or (s == 'e' and buf.len == 3):
        buf.add s
        continue
      elif buf.len != 4:
        raise newException(ValueError, &"invalid char in true token: {s}")
    of False:
      if (s == 'a' and buf.len == 1) or (s == 'l' and buf.len == 2) or (s == 's' and buf.len == 3) or (s == 'e' and buf.len == 4):
        buf.add s
        continue
      elif buf.len != 5:
        raise newException(ValueError, &"invalid char in false token: {s}")
    of Null:
      if (s == 'u' and buf.len == 1) or (s == 'l' and buf.len == 2) or (s == 'l' and buf.len == 3):
        buf.add s
        continue
      elif buf.len != 4:
        raise newException(ValueError, &"invalid char in null token: {s}")
    of Number:
      case s
      of '0'..'9', '+', '-', 'E', 'e', '.':
        buf.add s
        continue
      else:
        # go to below statement
        discard
    of String:
      case s
      of '\\':
        withEscape = not withEscape
        buf.add s
        continue
      of '"':
        if not withEscape:
          buf.add s
          result.add kind.finishToken(buf)
          kind = None
          buf = @[]
          continue
        else:
          buf.add s
          continue
      else:
        buf.add s
        continue
    else: discard

    case s
    of ' ', '\t', '\n', '\r':
      result.add kind.finishToken(buf)
      kind = Space
    of '{':
      result.add kind.finishToken(buf)
      kind = BeginObject
    of '}':
      result.add kind.finishToken(buf)
      kind = EndObject
    of '[':
      result.add kind.finishToken(buf)
      kind = BeginArray
    of ']':
      result.add kind.finishToken(buf)
      kind = EndArray
    of ':':
      result.add kind.finishToken(buf)
      kind = Colon
    of ',':
      result.add kind.finishToken(buf)
      kind = Comma
    of '"':
      withEscape = false
      buf = @[s]
      result.add kind.finishToken(buf)
      kind = String
    of 't':
      result.add kind.finishToken(buf)
      buf = @[s]
      kind = True
    of 'f':
      result.add kind.finishToken(buf)
      buf = @[s]
      kind = False
    of 'n':
      buf = @[s]
      result.add kind.finishToken(buf)
      kind = Null
    of '1'..'9', '-':
      result.add kind.finishToken(buf)
      buf = @[s]
      kind = Number
    else:
      raise newException(ValueError, &"invalid char: {s}")

proc addChild(node: Node, value: Node) =
  case node.kind
  of Top:
    if node.topValue != nil:
      raise newException(ValueError, "top node already has value")
    node.topValue = value
    value.parent = node
  of Object:
    if value.kind != ObjectField:
      raise newException(ValueError, "invalid node kind: {value.kind}")
    node.fields.add value
    value.parent = node
  of ObjectField:
    if node.value != nil:
      raise newException(ValueError, "object field already has value")
    node.value = value
    value.parent = node
  of Array:
    value.parent = node
    node.items.add value
  else:
    raise newException(ValueError, &"invalid node kind: {node.kind}")

proc parse(str: string): string =
  if str.len < 2:
    raise newException(ValueError, &"invalid string: {str}")
  if str[0] != '"' or str[^1] != '"':
    raise newException(ValueError, &"invalid string: {str}")
  let str = str[1..^2]
  if str.validateUtf8 >= 0:
    raise newException(ValueError, &"invalid UTF-8 string: {str}")
  for rune in str.toRunes:
    # TODO: unescape JSON string
    discard
  return str

proc parse*(tokens: seq[Token]): Node =
  result = Node(kind: Top)
  var current = result
  for token in tokens:
    case token.kind
    of Space, None:
      continue
    of True:
      current.addChild(Node(kind: True))
    of False:
      current.addChild(Node(kind: False))
    of Null:
      current.addChild(Node(kind: Null))
    of String:
      if current.kind == Object:
        let node = Node(kind: ObjectField, key: token.stringValue.parse)
        current.addChild(node)
        current = node
      else:
        current.addChild(Node(kind: String, stringValue: token.stringValue.parse))
    of Number:
      current.addChild(Node(kind: Number, numberValue: token.numberValue))
    of BeginObject:
      let node = Node(kind: Object)
      current.addChild(node)
      current = node
    of EndObject:
      case current.kind
      of Object:
        current = current.parent
      of ObjectField:
        let parentObject = current.parent
        if parentObject.objectCommaCount != parentObject.fields.len - 1:
          raise newException(ValueError, "missing comma")
        current = parentObject.parent
      else:
        raise newException(ValueError, &"invalid node kind: {current.kind}")
    of BeginArray:
      let node = Node(kind: Array)
      current.addChild(node)
      current = node
    of EndArray:
      if current.kind != Array:
        raise newException(ValueError, &"invalid node kind: {current.kind}")
      if current.arrayCommaCount != current.items.len - 1:
        raise newException(ValueError, "missing comma")
      current = current.parent
    of Colon:
      if current.kind != ObjectField:
        raise newException(ValueError, &"invalid node kind: {current.kind}")
      if current.withColon:
        raise newException(ValueError, "duplicate colon")
      current.withColon = true
    of Comma:
      if current.kind == ObjectField:
        current = current.parent
        current.objectCommaCount.inc
      elif current.kind == Array:
        current.arrayCommaCount.inc
      else:
        raise newException(ValueError, &"invalid node kind: {current.kind}")

proc print(node: Node, useIndent: bool = true, indentCount: int = 0): string =
  var indent = "  ".repeat(indentCount)
  case node.kind
  of Top:
    raise newException(ValueError, &"invalid node kind: {node.kind}")
  of Object:
    result.add((if useIndent: indent else: "") & "{\n")
    for i, field in node.fields:
      if i > 0:
        result.add ",\n"
      result.add field.print(true, indentCount + 1)
    result.add "\n"
    result.add(indent & "}")
  of ObjectField:
    result.add &"{indent}\"{node.key}\": "
    if node.value == nil:
      raise newException(ValueError, "object field has no value")
    result.add(node.value.print(false, indentCount))
  of Array:
    result.add((if useIndent: indent else: "") & "[\n")
    for i, item in node.items:
      if i > 0:
        result.add ",\n"
      result.add indent & "  " & item.print(false, indentCount + 1)
    result.add "\n"
    result.add(indent & "]")
  of String:
    result.add '"' & node.stringValue & '"'
  of Number:
    result.add $node.numberValue
  of True:
    result.add "true"
  of False:
    result.add "false"
  of Null:
    result.add "null"

proc `$`*(node: Node): string =
  if node.kind == Top:
    if node.topValue == nil:
      raise newException(ValueError, "top node has no value")
    return node.topValue.print
  return node.print
