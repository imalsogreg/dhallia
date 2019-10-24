let r    = ./combinator.dhall
let Req  = ./request.dhall
let Resp = ./response.dhall

let example1
    = r.mkRaw
      "example1"
      { name : Text }
      { greeting : Text }

      (\(i : { name : Text}) -> Req.default // { pathParts = [ i.name ] })
      (\(r : r.Response) -> { greeting = "Response"} )

in

example1