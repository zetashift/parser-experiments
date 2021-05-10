import
  std/[strformat, strutils, sequtils, sugar]

type
  EitherKind* = enum
    ekLeft, ekRight
  Either* = ref object
    ## An object variant, where Left is an error message and Right contains a parsed value and the remaining output
    case kind*: EitherKind
    of ekLeft: msg*: string
    of ekRight: val*: tuple[parsed: seq[string], remaining: string]

func map*(this: Either, f: seq[string] -> seq[string]): Either =
  ## map takes an Either and a function and unwraps the `Right` side
  ## returning an Either with `f` applied to the value. In case of `Left`
  ## we simply return it without modifying it
  case this.kind
  of ekLeft: return this
  of ekRight:
    return Either(kind: ekRight, val: (parsed: f(this.val.parsed),
        remaining: this.val.remaining))

func `$`*(this: Either): string =
  ## A simple toString function for our Either
  case this.kind
  of ekLeft: return fmt"<Left {this.msg}>"
  of ekRight: return fmt"<Right parsed: {this.val.parsed}, remaining: {this.val.remaining} >"

func `==`*(this: Either, that: Either): bool =
  this.kind == that.kind

