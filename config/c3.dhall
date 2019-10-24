let Request = Text
let Response = Text

-- let Types = { inputType : Type, outputType : Type }

let SomeRaw =
    forall(Y: Kind)
    -> (   forall(I : Type)
        -> forall(O : Type)
        -> { inputType: Type,
             outputType: Type,
	     name: Text,
	     toReq : I -> Request,
	     fromResp : Response -> O
	   }
	-> Y
       )
    -> Y

let rawInputType
  : SomeRaw -> Type
  =  \(raw : SomeRaw)
  -> \(Y : Kind)
  -> let f = \(I : Type)
          -> \(O : Type)
	  -> \(r: {inputType : Type,
	           outputType : Type,
		   name : Text,
		   toReq : I -> Request,
		   fromResp : Response -> O
		  })
	  -> r.inputType
     in raw Type f

let SomeFmap =
    forall(Y: Kind)
    -> (   forall(I : Type)
        -> forall(O : Type)
        -> { inputType: Type,
             outputType: Type,
	     name: Text,
	     toReq : I -> Request,
	     fromResp : Response -> O
	   }
	-> Y
       )
    -> Y

let SomeAp =
    forall(Y: Kind)
    -> (   forall(I : Type)
        -> forall(O : Type)
        -> { inputType: Type,
             outputType: Type,
	     name: Text,
	     toReq : I -> Request,
	     fromResp : Response -> O
	   }
	-> Y
       )
    -> Y

-- let API =
--   < Raw  : SomeRaw
--   | Fmap : SomeFmap
--   | Ap   : SomeAp
--   >

-- let getTypes
--   : API -> { inputType : Type, outputType : Type }
--   =  \(api : API)
--   -> let
--        rawTypes 
--   merge {
--   }

-- let mkRaw =
--     \(i : Type)
--     -> \(o : Type)
--     -> \(toReq : i -> Request)
--     -> \(fromResp : Response -> o)
--     ->

in 1