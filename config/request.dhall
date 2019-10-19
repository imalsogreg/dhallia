let List/concat = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/concat

let Request =
  { baseUrl     : Text,
    verb        : Text,
    pathParts   : List Text,
    queryParams : List { key: Text, value: Text },
    requestBody : Optional Text
  }

let QueryParam = { key: Text, value: Text }

let defaultRequest : Request =
  { baseUrl       = "example.com",
    verb          = "GET",
    pathParts     = [] : List Text,
    queryParams   = [] : List {key: Text, value: Text},
    requestBody   = None Text
  }

let addNewQueryParam : Text -> Text -> Request -> Request
 =  \(qKey: Text)
 -> \(qVal: Text)
 -> \(req: Request)
 -> let qparam = {key = qKey, value = qVal}
    in req // { queryParams = List/concat QueryParam [req.queryParams, [qparam]] }

let addPathPart : Text -> Request -> Request
 =  \(p: Text)
 -> \(r: Request)
 -> r // { pathParts = List/concat Text [r.pathParts, [p]] }

in
{ type = Request,
  default = defaultRequest,
  addPathPart = addPathPart,
  addNewQueryParam = addNewQueryParam
}
