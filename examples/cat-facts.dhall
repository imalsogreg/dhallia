-- cat-facts
-- Thanks to:
--   https://github.com/alexwohlbruck/cat-facts

let Req = ../config/request.dhall

let User =
  { _id  : Text,
    name : { first : Text, last : Text }
  }

let ListFact =
  { _id         : Text,
    text        : Text,
    type        : Text,
    user        : Optional User,
    upvotes     : Integer,
    userUpvoted : Optional Text
  }

let Fact =
  { _id         : Text,
    text        : Text
  }

let url = "https://cat-fact.herokuapp.com/facts"

in
{
  cat-facts =
    { inputType = {},
      outputType =
        { all : List ListFact },
      toRequest = \(_: {}) ->
        Req.default // { baseUrl = url },
      fromResponse = +1
    },

  cat-fact =
    { inputType  = { _id : Text },
      outputType = Fact,
      toRequest = \(i: { _id : Text }) ->
        Req.addPathPart
	  i._id
	  (Req.default // { baseUrl = url }),
      fromResponse = +1
    }
}