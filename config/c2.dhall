let Response : Type = Text
let Request  : Type = Text

let APIInfo = { inputType  : Type,
                outputType : Type,
		name       : Text
}

let SomeRawAPI
  =  forall(Y : Kind)
  -> (
          forall(input  : Type)
       -> forall(output : Type)
       -> { input    : Type,
            output   : Type,
	    name     : Text,
	    toReq    : input -> Request,
	    fromResp : Response -> output
	  }
       -> Y
     )
  -> Y

let SomeFmapAPI
  =  forall(Y : Kind)
  -> (
          forall(input  : Type)
       -> forall(output : Type)
       -> { input  : Type,
            output : Type,
	    f      : input -> output,
	    name   : Text
	  }
       -> Y
     )
  -> Y

let SomeApAPI
  =  forall(Y : Kind)
  -> (
          forall(input  : Type)
       -> forall(output : Type)
       -> { input  : Type,
            output : Type,
	    f      : input -> output,
	    name   : Text
	  }
       -> Y
     )
  -> Y

let SomeAPI2
  =  forall(Y : Kind)
  -> (
          forall(input  : Type)
       -> forall(output : Type)
       -> < Raw : { input  : Type,
                    output : Type,
                    toReq  : input -> Request,
		    fromResp : Response -> output,
	            name   : Text
		  }
          | Fmap : { parent : Y,
	             output : Type,
		     f      : input -> output,
		     name   : Text
		   }
          | Ap : { parentA : Y,
	           parentB : Y,
		   f       : input -> output,
		   name    : Text
		 }
	  >
	  
       -> Y
     )
  -> Y

let SomeAPI3
  =  forall(Y : Kind)
  -> (
          < Raw : 
	          forall(input : Type)
               -> forall(output : Type)
	       -> { input  : Type,
                    output : Type,
                    toReq  : input -> Request,
		    fromResp : input -> Response -> output,
	            name   : Text
		  }
          | Fmap : forall(parentInput : Type)
	        -> forall(parentOutput : Type)
	        -> forall(output : Type)
	        -> { input  : Type,
		     output : Type,
		     f      : parentInput -> parentOutput -> output,
		     name   : Text
		   }
          | Ap : forall(inputA : Type)
	      -> forall(inputB : Type)
	      -> forall(outputA : Type)
	      -> forall(outputB : Type)
	      -> forall(output : Type)
	      -> { input   : Type,
	           output  : Type,
		   f       : inputA -> inputB -> outputA -> outputB -> output,
		   name    : Text
		 }
	  >
       -> Y
     )
  -> Y

let API3 =< Raw : 
	          forall(input : Type)
               -> forall(output : Type)
	       -> { input  : Type,
                    output : Type,
                    toReq  : input -> Request,
		    fromResp : input -> Response -> output,
	            name   : Text
		  }
          | Fmap : forall(parentInput : Type)
	        -> forall(parentOutput : Type)
	        -> forall(output : Type)
	        -> { input  : Type,
		     output : Type,
		     f      : parentInput -> parentOutput -> output,
		     name   : Text
		   }
          | Ap : forall(inputA : Type)
	      -> forall(inputB : Type)
	      -> forall(outputA : Type)
	      -> forall(outputB : Type)
	      -> forall(output : Type)
	      -> { input   : Type,
	           output  : Type,
		   f       : inputA -> inputB -> outputA -> outputB -> output,
		   name    : Text
		 }
	  >


-- let getTypes
--   : SomeAPI3 -> { input: Type, output: Type }
--   = \(api : SomeAPI3) -> merge
--   { Raw = \(f: forall(input : Type)
--                -> forall(output : Type)
-- 	       -> { input  : Type,
--                     output : Type,
--                     toReq  : input -> Request,
-- 		    fromResp : input -> Response -> output,
-- 	            name   : Text
-- 		  }
--             ) ->
-- 	  { input: Bool, output : Bool },

--           -- let fRaw = \(i : Type)
--           --         -> \(o : Type)
-- 	  --         -> \(r: { input  : Type,
--           --                   output : Type,
--           --                   toReq  : i -> Request,
-- 	  -- 	            fromResp : i -> Response -> o,
-- 	  --                   name   : Text
-- 	  -- 	          })
--           --         -> { input = r.input, output = r.output }
-- 	  --  in f {itput : Type, output : Type}  fRaw

--     Fmap = \(f:
--                    forall(parentInput : Type)
--     	        -> forall(parentOutput : Type)
--     	        -> forall(output : Type)
--     	        -> { input  : Type,
--     		     output : Type,
--     		     f      : parentInput -> parentOutput -> output,
--     		     name   : Text
--     		   }
--     ) ->

--     { input : Bool, output : Bool },
    
--     -- let fFmap = \(pi: Type)
--     --          -> \(po: Type)
--     -- 	     -> \(o:  Type)
--     -- 	     -> \(r: { input : Type,
--     -- 	               output : Type,
--     -- 		       f : pi -> po -> o,
--     -- 		       name : Text
--     -- 		     })
--     --          -> { input = r.input, output = r.output }
--     -- in f Type fFmap,

--     Ap = \(f:
--     forall(inputA : Type)
--     	      -> forall(inputB : Type)
--     	      -> forall(outputA : Type)
--     	      -> forall(outputB : Type)
--     	      -> forall(output : Type)
--     	      -> { input   : Type,
--     	           output  : Type,
--     		   f       : inputA -> inputB -> outputA -> outputB -> output,
--     		   name    : Text
--     		 }
--     ) ->

--     {input : Bool, output : Bool }
    
--     -- let fAp = \(pia : Type)
--     --        -> \(pib : Type)
--     -- 	   -> \(poa : Type)
--     -- 	   -> \(pob : Type)
--     -- 	   -> \(o   : Type)
--     -- 	   -> \(r: { input : Type,
--     -- 	        output : Type,
--     -- 		f      : pia -> pib -> poa -> pob -> o,
--     -- 		name   : Text
--     -- 	      })
--     -- 	   -> { input = r.input, output = r.output }

--     -- in f Type fAp
--   } api

-- let SomeApAPI4
--   =  forall(Y : APIInfo)
--   -> (
--           < Raw : forall(input : Type)
--                -> forall(output : Type)
-- 	       -> { input  : Type,
--                     output : Type,
--                     toReq  : input -> Request,
-- 		    fromResp : Response -> output,
-- 	            name   : Text
-- 		  }
--           -- | Fmap : forall(output : Type)
-- 	  --       -> { 
-- 	  --            output : Type
-- 	  -- 	     f      : Y.inputType -> output,
-- 	  -- 	     name   : Text
-- 	  -- 	   }

--           -- | Ap : forall(output : Type)
-- 	  --     -> forall(parentA : Y)
-- 	  --     -> forall(parentB : Y)
-- 	  --     -> { f       : parentA.inputType -> parentB.inputType -> output,
-- 	  -- 	   name    : Text
-- 	  -- 	 }
-- 	  >
	  
--        -> Y
--      )
--   -> Y



let APIDict
  =  \(api : Type)
  -> { dRaw  : SomeRawAPI                -> api,
       dFmap : SomeFmapAPI -> api        -> api,
       dAp   : SomeApAPI   -> api -> api -> api
     }

let APIBB : Type = forall(a : Type) -> (APIDict a -> a)

-- mkRaw
--   :    forall(input  : Type)
--     -> forall(output : Type)
--     -> Text
--     -> (input    -> Request)
--     -> (Response -> output )
--     -> APIBB
--   =  \(input    : Type)
--   -> \(output   : Type)
--   -> \(name     : Text)
--   -> \(toReq    : input    -> Request)
--   -> \(fromResp : Response -> output )
--   ->
--   let RawExistential =
--     \(Y : Kind)

in
{
  APIDict = APIDict,
  APIBB   = APIBB
  -- mkRaw   = mkRaw
}