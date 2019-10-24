-- raw1:
--   input:    { name:  Text }
--   output:   { greet: Text }
--   toReq:    \(i : { name : Text }) -> addPathPart name
--   fromResp: \(r:  { body : Text }) -> { greet = body }
--   i2o: 

-- raw2:
--   input:    { age:   Int  }
--   output:   { floor: Text }
--   toReq:    \(i : { age : age })   -> addQueryParam "age" age
--   fromResp: \(r:  { body : Text }) -> { instr = body }

-- ap1:
--   input:  { a: {name: Text}, b: {greet: Text} }
--   output: { instrs: Text, isOk : Bool }

--   parentA: raw1
--   parentAToReq: (raw1's toReq)
--   parentBFromResp: (raw1's fromResp)

--   parentB: raw2
--   parentBToReq: (raw2's toReq)
--   parentBFromResp: (raw2's fromResp)

--   toReqs:

--------

let Request  = (./request.dhall).type
let Response = (./response.dhall).type
let defaultRequest = (./request.dhall).default

let Optional/map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/map

let IO =
    \(I: Type)
 -> \(O: Type)
 -> { toRequests : I -> List Request,
      fromResponses : List Response -> Optional O
    }

let SomeIO =
  forall(Y: Kind)
  -> (   forall(I: Type)
      -> forall(O: Type)
      -> { toRequests    : I -> List Request,
           fromResponses : List Response -> Optional O
	 }
      -> Y
     )
  -> Y

-- let mapSomeIO : SomeIO -> (A -> B) -> SomeIO
--  =  \(i: Type)
--  -> \(a: Type)
--  -> \(b: Type)
--  -> \(oldIO : SomeIO)
--  -> \(f : a -> b)
--  -> (   \(Y: Type)
--      -> let consume = \(I : Type)
--                    -> \(O : Type)
-- 		   -> \(r: { toRequests    : I -> List Request,
-- 		             fromResponses : List Response -> Optional a
-- 	                   })
-- 		   -> 
-- 		      { toRequests = r.toRequests,
-- 		        fromResponses = \(rs : List Response) -> None b
-- 		      }
--         in oldIO { toRequests : i -> List Request, fromResponses : List Response -> Optional b } consume
--      )


let mkIO
 =  \(i: Type)
 -> \(o: Type)
 -> \(toReqs : i -> List Request)
 -> \(fromResps: List Response -> Optional o)
 -> \(Y : Kind)
 -> \(f : forall(I: Type)
       -> forall(O: Type)
       -> { toRequests    : I -> List Request,
            fromResponses : List Response -> Optional O
	  }
       -> Y
     )
 -> f i o { toRequests = toReqs, fromResponses = fromResps }


let RawAPI = { types : { input : Type, output : Type },
               name  : Text,
	       io    : SomeIO
	     }

let APIIO
 =  \(I : Type)
 -> \(O : Type)
 -> { types : { input : Type, output : Type },
      name  : Text,
      io    : IO I O
    }

let FmapType = \(API : Type) -> forall(A: Type) -> forall(B: Type) -> API -> (A -> B) -> API
let ApType   = \(API : Type) -> forall(A1: Type) -> forall(A2: Type) -> forall(B: Type) -> API -> API -> (A1 -> A2 -> B) -> API

let API : Type
 =  forall (API  : Type)
 -> forall (Raw  : RawAPI -> API)
 -> forall (Fmap : FmapType API)
 -> forall (Ap   : ApType API)
 -> API



let mkRaw
 =  \(I : Type)
 -> \(O : Type)
 -> \(n : Text)
 -> \(toReq : I -> Request)
 -> \(fromResp : Response -> O)
 -> let raw = { types = { input = I, output = O },
                name  = n,
		io    = mkIO I O (\(i: I) -> ([] : List Request))
		                 (\(rs : List Response) ->
                                  let mHead = List/head Response rs : Optional Response
                                  in Optional/map Response O fromResp mHead
				 )
              }
    in (  \(API : Type)
       -> \(Raw : RawAPI -> API)
       -> \(Fmap : FmapType API)
       -> \(Ap   : ApType   API)
       -> Raw raw )

let mkRaw2
 =  \(I : Type)
 -> \(O : Type)
 -> \(n : Text)
 -> \(toReq : I -> Request)
 -> \(fromResp : Response -> O)
 -> { types = { input = I, output = O },
      name  = n,
      io    = mkIO I O (\(i: I) -> ([] : List Request))
              (\(rs : List Response) ->
                let mHead = List/head Response rs : Optional Response
                in Optional/map Response O fromResp mHead
              )
    }

let mkRaw3
 =  \(I : Type)
 -> \(O : Type)
 -> \(n : Text)
 -> \(toReq : I -> Request)
 -> \(fromResp : Response -> O)
 -> { types = { input = I, output = O },
      name  = n,
      io    = { toRequests    = (\(i: I) -> ([] : List Request)),
                fromResponses =
		  (\(rs : List Response) ->
                    let mHead = List/head Response rs : Optional Response
                    in Optional/map Response O fromResp mHead
                  )
              }
    } : APIIO I O


-- let mkFmap
--  =  \(O: Type)
--  -> \(n: Text)
--  -> \(parent : RawAPI)
--  -> \(f: parent.types.output -> O)
--  -> { types = { input = parent.types.input, output = O },
--       name  = n,
--       io    = mkIO parent.types.input O
--               (parent.io.toRequests)
--               (\(rs : List Response) ->
-- 	        Optional/map parent.types.output O f (parent.io.fromRequests rs)
-- 	      )
--     }

let mkFmap3
 : forall(I : Type) -> forall(A: Type) -> forall(B: Type) -> Text -> (A -> B) -> APIIO I A -> APIIO I B
 =  \(I: Type)
 -> \(A: Type)
 -> \(B: Type)
 -> \(n: Text)
 -> \(f: A -> B)
 -> \(parent : APIIO I A)
 -> { types = { input = I, output = B },
      name  = n,
      io    = { toRequests = parent.io.toRequests,
                fromResponses =
		  (\(rs: List Response) ->
		    Optional/map A B f (parent.io.fromResponses rs)
		  )
	      }
    }

let mkAp3
 =  \(I1: Type)
 -> \(O1: Type)
 -> \(I2: Type)
 -> \(O2: Type)
 -> \(B: Type)
 -> \(n : Text)
 -> \(f: O1 -> O2 -> B)
 -> \(parent1 : APIIO I1 O1)
 -> \(parent2 : APIIO I2 O2)
 -> { types = { input = { a = I1, b = I2 }, output = B },
      name = n,
      io  = { toRequests =
               \(r: { a : I1, b : I2 }) ->
	        [parent1.io.toRequests r.a, parent2.io.toRequests r.b],
              fromResponses =
	       \(rs : List Response) ->
	        None B
            }
    }


let ex1i = { name  : Text }
let ex1o = { greet : Text }

let ex1 = mkRaw3 ex1i ex1o "ex1"
          (\(i: ex1i)     -> defaultRequest )
	  (\(r: Response) -> {greet = "Hi"} )


let ex2o = { greet2 : Text }

let ex2 = mkFmap3 ex1i ex1o ex2o "ex2" (\(a : {greet : Text}) -> { greet2 = "HEY" }) ex1

in ex2.io