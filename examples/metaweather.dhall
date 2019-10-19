-- metaweather
-- Thanks to:
--   https://www.metaweather.com

let Req = ../config/request.dhall

let MWLoc =
  { title : Text,
    location_type : Text,
    woeid: Integer,
    latt_long: Text
  }
    

let metaweather = "https://www.metaweather.com/api/location/search"

in
{
  mw-search =
    { inputType = {},
      outputType = List MWLoc,
      toRequest = \(query: Text) ->
        let qp = [{key = "query", value = query}] : List {key: Text, value: Text}
        in Req.default // { baseUrl = metaweather, queryParams = qp },
      fromResponse = +1
    }
}
