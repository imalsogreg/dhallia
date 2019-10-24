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

let constructRawAPI
  =  \(name: Text)
  -> \(I: Type)
  -> \(O: Type)
  -> \(toReq: (I -> Request))
  -> \(fromResp: (Response -> O))
  -> { type  = { input = I , output = O },
       value = { name          = name,
                 toRequest     = toReq,
		 fromResponse  = fromResp
	       }
     } : RawInfo I O

let SomeRawAPIType
  = forall(Y : Kind)
  -> ( forall(I: Type)
      -> forall(O: Type)
      -> ( RawInfo I O -> Y )
     )
  -> Y

let SomeRawAPIValue
  = forall(Y : Type)
  -> ( forall(I: Type)
      -> forall(O: Type)
      -> ( RawInfo I O -> Y )
     )
  -> Y



let SomeRawAPI = { type: SomeRawAPIType, value : SomeRawAPIValue }
in 1