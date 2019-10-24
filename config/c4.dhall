let r    = ./combinator.dhall
let Req  = ./request.dhall
let Resp = ./response.dhall
let e    = ./example.dhall

let example1
    = r.mkRaw
      "example1"
      { name : Text }
      { greeting : Text }

      (\(i : { name : Text}) -> Req.default // { pathParts = [ i.name ] })
      (\(r : r.Response) -> { greeting = "Response"} )


let example2 = r.mkFmap "happy_code" { greeting : Text, code : Integer }
                      example1
                      ( \(i: { greeting : Text } )
		         -> { greeting = "hello", code = +1 }
		      )
              
let example3 = r.mkAp "happy_ap" { both : Text }  example1 example2
               (    \(a : { greeting : Text })
	        ->  \(b : { greeting : Text, code : Integer })
	        -> { both = "All done" }
	       )

in
-- r.getTypes example3
{
  e1 = example1,
  e2 = example2
}
