let Existential = forall(Y : Type) -> (forall(X : Type) -> X -> Y) -> Y

let example1
  : Existential
  =  \(Y : Type)
  -> \(f : forall(X: Type) -> X -> Y)
  -> f Bool True

let Request =
  { baseUrl     : Text,
    verb        : Text,
    pathParts   : List Text,
    queryParams : List { key: Text, value: Text },
    requestBody : Optional Text
  }

let defaultRequest : Request =
  { baseUrl       = "example.com",
    verb          = "GET",
    pathParts     = [] : List Text,
    queryParams   = [] : List { key: Text, value: Text },
    requestBody   = None Text
  }

let Response =
  { body : Text }

let API
  =  \(I: Type)
  -> \(O: Type)
  -> { inputType     : Type,
       outputType    : Type,
       makeRequest   : I -> Request,
       parseResponse : Response -> O
     }

let mkAPI
  = \(I : Type) -> \(O : Type) -> \(mr : I -> Request) -> \(pr : Response -> O)
  -> { inputType     = I,
       outputType    = O,
       makeRequest   = mr,
       parseResponse = pr
     }

let mkFmapAPI
  =  \(I : Type)
  -> \(A : Type)
  -> \(B : Type)
  -> \(parent : API I A)
  -> \(f : A -> B)
  -> { inputType  = parent.inputType,
       outputType = B,
       makeRequest = parent.makeRequest,
       parseResponse = \(r : Response) -> f (parent.parseResponse r)
     }

-- let mkApAPI
--   =  \(I1 : Type)
--   -> \(O2 : Type)
--   -> \(I2 : Type)
--   -> \(O2 : Type)
--   -> \(O  : Type
--   -> \(parent1 : API I1 O1)
--   -> \(parent2 : API I2 O2)
--   -> \(f : O1 -> O2 -> O)
--   -> { inputType  = parent.inputType,
--        outputType = B,
--        makeRequest = parent.makeRequest,
--        parseResponse = \(r : Response) -> f (parent.parseResponse r)
--      }

let SomeAPI
  : Type
  =  forall(SomeAPI : Type)
  -> forall(RawAPI  : Type)
  -> forall(FmapAPI : Type)
  -> forall(ApAPI   : Type)
  -> < Raw  : { r : RawAPI  }
     | Fmap : { r : FmapAPI }
     | Ap   : { r : ApAPI   }
     >


let SomeAPI2
  =  forall(Y : Kind)
  -> (
          forall(I : Type)
       -> forall(O : Type)
       -> {x: Type, y: Type, n : Text, t : Type, toReq : I -> Request, fromResp : Response -> O }
       -> Y
     )
  -> Y

let SomeFmapAPI
  =  forall(Y : Kind)
  -> (
          forall(a : Type)
       -> forall(b : Type)
       -> { parent : SomeAPI, f : a -> b }
       -> Y
     )
  -> Y

-- let getRawTypes =
--   \(a : SomeApi2) ->
--   let f = \(I: Type) -> \(O: Type) -> \(r: {x: Type, y: Type, n : Text, t: Type, toReq : I -> Request, fromResp : Response -> O }) -> {input: r.x, output: r.y}
--   in a Type f

let example2
  : SomeAPI2
  =  \(Y : Kind)
  -> \(f: forall(I: Type) -> forall(O: Type) -> {x: Type, y: Type, n : Text, t: Type, toReq : I -> Request, fromResp : Response -> O } -> Y)
  -> f Text Text {x = Text, y = Text, n = "name", t = Bool, toReq = \(txt: Text) -> defaultRequest, fromResp = \(resp: Response) -> "Great" }

-- let example2b
--   : SomeAPI
--   = \(Y : Kind)
--   -> \(f: forall(

-- exampleFmap 
--   : SomeFmapAPI
--   =  \(Y : Kind)
--   -> \(g: forall(I: Type) -> forall(A : Type) -> forall(B: Type) -> { parent : SomeAPI, f: A -> B } -> Y)
--   -> g Text Text Text { parent = 

  


let example3
  : SomeAPI2
  =  \(Y : Kind)
  -> \(f: forall(I: Type) -> forall(O: Type) -> {x: Type, y: Type, n : Text, t: Type, toReq: I -> Request, fromResp : Response -> O } -> Y)
  -> f Text Text {x = Bool, y = Bool, n = "name", t = Integer, toReq = \(txt: Text) -> defaultRequest, fromResp = \(resp: Response) -> "Great" }

-- let example4
--   : SomeAPI
--   =  \(Y: Type)
--   -> \(f: forall(I: Type) -> forall(O: Type) -> 

let foo = API Text Text

let api1 = mkAPI Text Text (\(x : Text) -> defaultRequest) (\(r : Response) -> "Looks good to me: ${r.body}")
let api2 = mkFmapAPI Text Text Text api1 (\(t : Text) -> "Here it is: ${t}")

let getTypes : SomeAPI2 -> {input : Type, output : Type }
  =  \(api : SomeAPI2)
  -> let f = \(I: Type) -> \(O: Type) -> \(r: {x: Type, y: Type, n : Text, t: Type, toReq : I -> Request, fromResp : Response -> O }) -> {input =  r.x, output =  r.y}
     in api { input: Type, output : Type } f

let X = forall(Y : Type) -> < Foo : Integer | Bar : Y >

let Z =  forall(Y : Type)
      -> ( < Foo : forall(i : Type) -> { ty : Type, f : i -> Bool }
           | Bar : Type
	   > -> Y
	 )
      -> Y

-- let ex1 =
--   let unFoo = \(i : Type) -> 
--   let f =   \(r : < Foo : forall(i : Type) -> { ty : Type, f : i -> Bool } | Bar : Type >)
--          -> merge { Foo = 


-- let getType : Z -> Type
--   = \(x: Z Bool ) -> merge
--   { Foo = \(i: Integer) -> i,
--     Bar = \(_ : Bool) -> +10
--   } x



in

--   example3 Text (\(T: Type) -> \(x : forall(I: Type) -> forall(O: Type) -> {x: Type, y: Type, n : Text, t: Type }) -> x.t)
  -- let f : forall(I: Type) -> forall(O: Type) -> {x: I, y: O, n : Text, t: Type } -> Type
  --   =  \(I : Type)
  --   -> \(O : Type)
  --   -> \(r : {x : I, y: O, n : Text, t : Type})
  --   -> r.t
  --   in

  -- let f = \(I: Type) -> \(O: Type) -> \(r: {x: Type, y: Type, n : Text, t: Type, toReq : I -> Request, fromResp : Response -> O }) -> {input =  r.x, output =  r.y}
  -- in  example3 {input : Type, output : Type }  f

getTypes example3
-- getNum (Z.Foo +10)
