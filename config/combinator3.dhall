let Request  = (./request.dhall).type
let Response = (./response.dhall).type


let RawAPIInfo
  =  \(I: Type)
  -> \(O: Type)
  -> { type : {input : Type,
               output: Type
	      },
       value: { name          : Text,
                toRequest     : I -> List Request,
		fromResponses : Response -> O
              }
     }

-- let FmapAPIInfo
--   =  \(I: Type)
--   -> \(A: Type)
--   -> \(B: Type)
--   -> \(f: A -> B)
--   -> { type : { input  : Type,
--                 output : Type
-- 	      },
--        value : { name      : Text,
--                  f         : A -> B,
-- 	       }

let Nat : Type
 =  forall(Nat  : Type)
 -> forall(Zero : Nat )
 -> forall(Succ : Nat -> Nat)
 -> Nat

let APIType : Type

let API : Type
 =  forall(API  : Type)
 -> forall(Raw  : forall(I: Type)
               -> forall(O: Type)
	       -> Text
               -> (I -> Request)
	       -> (Response -> O)
               -> API)
 -> forall(Fmap : forall(A : Type)
               -> forall(B: Type)
	       -> Text
	       -> API
	       -> (A -> B)
	       -> API)
 -> forall(Ap   : forall(A1: Type)
               -> forall(A2:  Type)
	       -> forall(B: Type)
	       -> Text
	       -> API
	       -> API
	       -> (A1 -> A2 -> B)
	       -> API) 
 -> API

-- let API : Type
--  =  \(I: Type)
--  -> \(O: Type)
--  -> forall(API  : I -> O -> Type)
--  -- -> forall(Raw  : RawAPIInfo -> API I O)
--  -> forall(Fmap : forall(A : Type) -> Text -> API I A -> (A -> O) -> API I O)
--  -> API I O

in 1



