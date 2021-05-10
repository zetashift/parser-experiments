import  
  std/[strformat, sugar],
  either

type
  Parser* = ref object
    ## Parser is an abstraction over our parser function, so we can hold more instructions
    f: s: string -> Either ## Our actual parser function
    supressed: bool ## Indicates if we want to ignore the parsed text, for example discarding values in a string

func newParser*(f: (s: string) -> Either, supressed: bool = false): Parser =
  ## Instantiate a new `Parser object`
  init result
  result.f = f
  result.surpressed = surpressed

func `$`*(p: Parser): string = fmt"<Parser: >"

func parse*(this: Parser, s: string): Either = this.f(s)

func map*(this: Parser, transformer: seq[string] -> seq[string]): Parser =
  func inner(s: string): Either = this.f(s).map(transformer)
  result = newParser(inner)

func surpress*(this: Parser): Parser =
  result = newParser(this.f, true)

func charParser*(c: char): Parser =
  ## A Parser that only parses a certain character
  func curried(s: String): Either =
    ## This function actually parses the string
    if s == "":
      let msg = "String is empty"
      return Either(kind: ekLeft, msg: msg)
    else:
      if s[0] == c:
        let rem == s[1 ..< s.len]
        let parsedString = @[$c]
        return Either(kind: ekRight, val: (parsed: parsedString, remaining: rem))
      else:
        return Either(kind: ekLeft, msg: &"Expecting '{$c}' but got '{$s[0]}'")

  return newParser(curried)

func andThen*(p1, p2: Parser): Parser =
  ## We try to parse p1 and then p2, if both succeed we return a `Right`.
  ## If one fails we return a `Left`.
  func curried(s: string): Either =
    let res1 = p1.parse(s)
    case res1.kind
    of ekLeft: return res1
    of ekRight:
      let res2 = p2.parse(p1.val.remaining) # parse the remaining characters
      case res2.kind
      of ekLeft: return res2
      of ekRight:
        let v1 = res1.val.parsed
        let v2 = res2.val.parsed
        var vs: seq[string] = @[]
        if not p1.surpressed:
          vs.add(v1)
        if not p2.surpressed:
          vs.add(v2)
        return Either(kind: ekRight, val: (parsed: vs, remaining: res2.val.remaining))
      else:
        return res2
  
  result = newParser(curried)

func `>>`*(left: Parser, right: Parser): Parser = 
  ## A wrapper around `andThen` for chaining parsers
  result = andThen(left, right)

func orElse*(p1, p2: Parser): Parser =
  ## We try to parse1 and then pars2, if atleast one succeed we return a `Right`.
  ## If both fail, that's the only time we return a `Left`.
  func curried(s: string): Either =
    let res = p1.parse(s)
    case res.kind
    of ekRight: return res  ## If the first one succeeds we can immediately return, just like a boolean `or` operator!
    of ekLeft:
      let res = p2.parse(s)
      case res.kind
      of ekLeft:
        return Either(kind: ekLeft, msg: "Both parsers failed")
      of ekRight:
        return res

  result = newParser(curried)

func `|`*(left: Parser, right: Parser): Parser =
  ## A wrapper around `orElse`, where we try to parse one or the other
  result = orElse(left, right)


