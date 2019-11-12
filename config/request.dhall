let Request =
      { baseUrl :
          Text
      , verb :
          Text
      , pathParts :
          List Text
      , queryParams :
          List { key : Text, value : Text }
      , requestBody :
          Optional Text
      , headers :
          List { key : Text, value : Text }
      }

let QueryParam = { key : Text, value : Text }

let Cache = { ttlSeconds : Integer }

let defaultRequest
    : Request
    = { baseUrl =
          "example.com"
      , verb =
          "GET"
      , pathParts =
          [] : List Text
      , queryParams =
          [] : List { key : Text, value : Text }
      , requestBody =
          None Text
      , headers =
            [ { key = "Content-Type", value = "application/json" } ]
          : List { key : Text, value : Text }
      }

in  { type = Request, default = defaultRequest }
