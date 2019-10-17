let Req = ./request.dhall
let Resp = ./response.dhall

let Request = Req.type
let Response = Resp.type

let MyInput = { name : Text }
let MyOutput = { greet : Text }

in
{

  example1 =

  { inputType  = MyInput,
    outputType = MyOutput,
    toRequest  =
      \(i: MyInput) ->
        Req.addPathPart i.name (Req.default // { baseUrl = "http://localhost:8080/greet" }),
    fromResponse =
      \(r: Response) ->
        Some { greet = r.body }
  },

  example2 =

  { parent     = "example1",
    outputType = { greet2 : Text },
    f = \(o: MyOutput) ->
      { greet2 = "(fmapped ${o.greet})" }
  },

  example3 =

  { parentA = "example1",
    parentB = "example2",
    outputType = { composite : Text, extra : Integer },
    f =
      \(oA : MyOutput) ->
      \(oB : {greet2 : Text}) ->
        Some { composite = "Hi. A was ${oA.greet} and B was ${oB.greet2}.", extra = +2 }
  }

}

