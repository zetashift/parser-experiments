import
  std/[sequtils, strutils, strformat, sugar],
  either

type
  Parser* = ref object
    ## Parser is an abstraction over our parser proction, so we can hold more instructions
    f*: string -> Either ## Our actual parser proction
    supressed*: bool ## Indicates if we want to ignore the parsed text, for example discarding values in a string

proc newParser*(f: (s: string) -> Either, supressed: bool = false): Parser =
  ## Instantiate a new `Parser object`
  new result
  result.f = f
  result.supressed = supressed

proc `$`*(p: Parser): string = fmt"<Parser: >"

proc parse*(this: Parser, s: string): Either =
  this.f(s)

proc map*(this: Parser, transformer: seq[string] -> seq[string]): Parser =
  proc inner(s: string): Either = this.f(s).map(transformer)
  result = newParser(inner)

proc surpress*(this: Parser): Parser =
  result = newParser(this.f, true)

proc charParser*(c: char): Parser =
  ## A Parser that only parses a certain character
  proc curried(s: string): Either =
    ## This proction actually parses the string
    if s == "":
      let msg = "String is empty"
      return Either(kind: ekLeft, msg: msg)
    else:
      if s[0] == c:
        let rem = s[1 ..< s.len]
        let parsedString = @[$c]
        return Either(kind: ekRight, val: (parsed: parsedString,
            remaining: rem))
      else:
        return Either(kind: ekLeft, msg: &"Expecting '{$c}' but got '{$s[0]}'")

  return newParser(f = curried)

proc andThen*(p1, p2: Parser): Parser =
  ## We try to parse p1 and then p2, if both succeed we return a `Right`.
  ## If one fails we return a `Left`.
  proc curried(s: string): Either =
    let res1 = p1.parse(s)
    case res1.kind
    of ekLeft: return res1
    of ekRight:
      let res2 = p2.parse(res1.val.remaining) # parse the remaining characters
      case res2.kind
      of ekLeft: return res2
      of ekRight:
        let v1 = res1.val.parsed
        let v2 = res2.val.parsed
        var vs: seq[string] = @[]
        if not p1.supressed:
          vs.add(v1)
        if not p2.supressed:
          vs.add(v2)
        return Either(kind: ekRight, val: (parsed: vs,
            remaining: res2.val.remaining))
      else:
        return res2

  result = newParser(curried)

proc `>>`*(left: Parser, right: Parser): Parser =
  ## A wrapper around `andThen` for chaining parsers
  result = andThen(left, right)

proc orElse*(p1, p2: Parser): Parser =
  ## We try to parse1 and then pars2, if atleast one succeed we return a `Right`.
  ## If both fail, that's the only time we return a `Left`.
  proc curried(s: string): Either =
    let res = p1.parse(s)
    case res.kind
    of ekRight: return res ## If the first one succeeds we can immediately return, just like a boolean `or` operator!
    of ekLeft:
      let res = p2.parse(s)
      case res.kind
      of ekLeft:
        return Either(kind: ekLeft, msg: "Both parsers failed")
      of ekRight:
        return res

  result = newParser(curried)

proc `|`*(left: Parser, right: Parser): Parser =
  ## A wrapper around `orElse`, where we try to parse one or the other
  result = orElse(left, right)

proc multiply*(parser: Parser, count: int): Parser =
  ## Here we want to parse something `count` amount of times
  proc curried(s: string): Either =
    var copyS = s
    var fullParsed: seq[string] = @[]
    for i in 1..count:
      let res = parser.parse(copyS)
      case res.kind
      of ekLeft: return res
      of ekRight:
        let parsed = res.val.parsed
        copyS = res.val.remaining
        fullParsed.add(parsed)

    return Either(kind: ekRight, val: (parsed: fullParsed, remaining: copyS))

  result = newParser(curried)

proc `*`*(p: Parser, times: int): Parser =
  ## Multiply Parser `p` using amount `times`
  multiply(p, times)

proc choice*(parsers: seq[Parser]): Parser =
  ## Try to run a sequence of Parsers
  result = parsers.foldl(a | b)

proc anyOf*(chars: set[char]): Parser =
  ## converts a sequence of characters into a `charParser`
  result = chars.mapIt(charParser(it)).choice

proc parseString*(str: string): Parser =
  ## Parses a string
  ## So instead of charParser('a') >> charParser('b') >> charParser('c')
  ## One can do parseString("abc")
  var parsers: seq[Parser] = newSeq[Parser]()
  for char in str:
    parsers.add(charParser(char))

  var p = foldl(parsers, a >> b)
  result = p.map(l => @[join(l, "")])

proc optionally*(parser: Parser): Parser =
  ## This denotes that the given Parser is a not a hard requirement for the parser.
  ## For example if the `!` in `Hello World!` is optional we could define it like
  ## optionally(charParser('!'))
  let
    myParsed = @[""]
    nonProc = (s: string) => Either(kind: ekRight, val: (parsed: myParsed,
        remaining: ""))
    noneParser = newParser(nonProc)

  result = parser | noneParser

proc parseZeroOrMore(parser: Parser, input: string): Either =
  let res = parser.parse(input)
  case res.kind
  of ekLeft:
    let myParsed: seq[string] = @[]
    return Either(kind: ekRight, val: (parsed: myParsed, remaining: input))
  of ekRight:
    let firstVal = res.val.parsed
    let remaining = res.val.remaining
    let res = parseZeroOrMore(parser, remaining)
    case res.kind
    of ekRight:
      let subsequent = res.val.parsed
      let remaining = res.val.remaining
      var values = newSeq[string]()
      values.add(firstVal)
      values.add(subsequent)

      return Either(kind: ekRight, val: (parsed: values, remaining: remaining))
    of ekLeft:
      let myParsed: seq[string] = @[]
      return Either(kind: ekRight, val: (parsed: myParsed, remaining: input))

proc many*(parser: Parser): Parser =
  proc curried(s: string): Either = parseZeroOrMore(parser, s)
  result = newParser(curried)

proc many1*(parser: Parser): Parser =
  proc curried(s: string): Either =
    let res = parser.parse(s)
    case res.kind
    of ekLeft: return res
    of ekRight: return many(parser).parse(s)

  result = newParser(curried)

proc separatedBy1(sep: Parser, parser: Parser): Parser =
  ## Parse data that is separated with a certain `sep` more easily
  ## For example:
  ## let commaSeparated = separatedBy(charParser(',').surpress(), digit)
  ## commaSeparated.parse("1,2,3,4")
  let separateThenParser = sep >> parser
  result = parser >> many(separateThenParser)

proc separatedBy(sep: Parser, parser: Parser): Parser =
  let myParsed = @[""]
  let nonProc = (s: string) => Either(kind: ekRight, val: (parsed: myParsed, remaining: ""))

  result = separatedBy1(sep, parser) | newParser(nonProc)

