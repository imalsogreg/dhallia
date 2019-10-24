let Request  = (./request.dhall).type
let Response = (./response.dhall).type

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

let rawTypes
  : SomeRaw -> { inputType : Type, outputType : Type }
  =  \(api : SomeRaw)
  -> let f = \(I : Type)
          -> \(O : Type)
	  -> \(r: {inputType : Type,
	           outputType : Type,
		   name : Text,
		   toReq : I -> Request,
		   fromResp : Response -> O
		  })
	  -> {inputType = r.inputType, outputType = r.outputType}
     in api {inputType : Type, outputType : Type} f



let SomeFmap =
    forall(Y: Kind)
    -> (   forall(I : Type)
        -> forall(O : Type)
        -> { inputType: Type,
             outputType: Type,
	     name: Text,
	     f : I -> O
	   }
	-> Y
       )
    -> Y

let fmapTypes
  : SomeFmap -> { inputType : Type, outputType : Type }
  =  \(api : SomeFmap)
  -> let f = \(I : Type)
          -> \(O : Type)
	  -> \(r: {inputType : Type,
	           outputType : Type,
		   name : Text,
                   f : I -> O
		  })
	  -> {inputType = r.inputType, outputType = r.outputType}
     in api {inputType : Type, outputType : Type} f

let SomeAp =
    forall(Y: Kind)
    -> (   forall(I : { a : Type, b : Type} )
        -> forall(O : Type)
        -> { inputType: Type,
             outputType: Type,
	     name: Text,
	     f : I.a -> I.b -> O
	   }
	-> Y
       )
    -> Y

let apTypes
  : SomeAp -> { inputType : Type, outputType : Type }
  =  \(api : SomeAp)
  -> let f = \(I : { a : Type, b : Type } )
          -> \(O : Type)
	  -> \(r: {inputType : Type,
	           outputType : Type,
		   name : Text,
                   f : I.a -> I.b -> O
		  })
	  -> {inputType = r.inputType, outputType = r.outputType}
     in api {inputType : Type, outputType : Type} f

let API =
  < Raw  : SomeRaw
  | Fmap : SomeFmap
  | Ap   : SomeAp
  >

let getTypes
  : API -> { inputType : Type, outputType : Type }
  =  \(api : API)
  -> merge
     { Raw  = \(r: SomeRaw ) -> rawTypes  r,
       Fmap = \(r: SomeFmap) -> fmapTypes r,
       Ap   = \(r: SomeAp  ) -> apTypes   r
     } api



let mkRaw =
    \(n: Text)
 -> \(i: Type)
 -> \(o: Type)
 -> \(toReq : i -> Request)
 -> \(fromResp : Response -> o)
 -> let someRaw =  \(Y : Kind)
                -> \(f: forall(I: Type)
		     -> forall(O: Type)
		     -> { inputType: Type,
		          outputType: Type,
			  name: Text,
			  toReq : I -> Request,
			  fromResp : Response -> O
			} -> Y)
	        -> f i o { inputType = i,
		           outputType = o,
			   name = n,
			   toReq = toReq,
			   fromResp = fromResp
			 }
    in API.Raw someRaw


let mkFmap =
       \(n : Text)
    -> \(o : Type)
    -> \(parent : API)
    -> \(f: (getTypes parent).outputType -> o)
    -> let apiInput = (getTypes parent).inputType
       let fnInput  = (getTypes parent).outputType
       let someFmap : SomeFmap
	           =  \(Y : Kind)
                   -> \(g : forall(I: Type)
		         -> forall(O: Type)
			 -> { inputType  : Type,
			      outputType : Type,
			      name       : Text,
			      f          : I -> O
			    } -> Y)
	           -> g fnInput o { inputType  = apiInput,
		                    outputType = o,
			            name       = n,
			            f          = f
		                  }
       in API.Fmap someFmap

let mkAp =
     \(n : Text)
  -> \(o : Type)
  -> \(parentA : API)
  -> \(parentB : API)
  -> \(f : (getTypes parentA).outputType -> (getTypes parentB).outputType -> o)
  -> let fnInputA : Type = (getTypes parentA).outputType
     let fnInputB : Type = (getTypes parentB).outputType
     let fnInputs : { a : Type, b : Type } = { a = fnInputA, b = fnInputB }
     let apiInputA : Type = (getTypes parentA).inputType
     let apiInputB : Type = (getTypes parentB).inputType
     let apiInput  : Type = { a : apiInputA, b : apiInputB }

     let someAp : SomeAp
               =  \(Y : Kind)
               -> \(g : forall(I: { a : Type, b : Type} )
     	             -> forall(O: Type)
     		     -> { inputType  : Type,
     		          outputType : Type,
     			  name       : Text,
     			  f          : I.a -> I.b -> O
     			} -> Y)
     	       -> g fnInputs o { inputType  = apiInput,
     	                                         outputType = o,
     						 name       = n,
     						 f          = f
     					       }
     in API.Ap someAp

in

{
  mkRaw  = mkRaw,
  mkFmap = mkFmap,
  mkAp   = mkAp,

  Request = Request,
  Response = Response,
  
  getTypes = getTypes

  -- -- These can not be exported!
  -- -- Error: ❰Sort❱ has no type, kind, or sort
  -- API = API
  -- SomeRaw = SomeRaw
  -- SomeFmap = SomeFmap,
  -- SomeAp = SomeAp
}