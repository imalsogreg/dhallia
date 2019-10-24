let Request  = (./request.dhall).type
let Response = (./response.dhall).type
let defaultRequest = (./request.dhall).default

let Optional/map =
  https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/map
let Optional/concat =
  https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/concat




let API
 =  \(I: Type)
 -> \(O: Type)
 -> { types : { input : Type, output : Type },
      name  : Text,
      io    : I -> Optional O
     }


let mkRaw
 =  \(I : Type)
 -> \(O : Type)
 -> \(n : Text)
 -> \(toReq : I -> Request)
 -> \(fromResp : Response -> Optional O)
 -> \(runHTTP : Request -> Response)
 -> { types = { input = I, output = O },
      name  = n,
      io    = \(i: I) -> fromResp (runHTTP (toReq i))
    } : API I O

let mkFmap
 =  \(I: Type)
 -> \(A: Type)
 -> \(B: Type)
 -> \(n      : Text)
 -> \(parent : API I A)
 -> \(f      : A -> B)
 -> { types = { input = I, output = B },
      name  = n,
      io    = \(i: I) -> Optional/map A B f (parent.io i)
    }


let mkAp
 =  \(I1: Type)
 -> \(A1: Type)
 -> \(I2: Type)
 -> \(A2: Type)
 -> \(B: Type)
 -> \(n      : Text)
 -> \(parent1 : API I1 A1)
 -> \(parent2 : API I2 A2)
 -> \(f      : A1 -> A2 -> B)
 -> { types = { input = {i1 : I1, i2: I2}, output = B },
      name  = n,
      io    =  \(i1: I1)
            -> \(i2: I2) ->
               Optional/concat B
	         (Optional/map A1 (Optional B)
	          (\(a1: A1) ->
		    Optional/map A2 B (\(a2: A2) -> f a1 a2
		  ) (parent2.io i2))
	          (parent1.io i1)
		 )
		 
    }


in 1

