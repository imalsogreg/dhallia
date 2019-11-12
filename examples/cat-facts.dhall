-- cat-facts
-- Thanks to:
--   https://github.com/alexwohlbruck/cat-facts

let Req = ../config/request.dhall

let User = { _id : Text, name : { first : Text, last : Text } }

let ListFact =
      { _id :
          Text
      , text :
          Text
      , type :
          Text
      , user :
          Optional User
      , upvotes :
          Integer
      , userUpvoted :
          Optional Text
      }

let Fact = { _id : Text, text : Text }

let url = "https://cat-fact.herokuapp.com/facts"

in  { cat-facts =
        { inputType =
            {}
        , outputType =
            { all : List ListFact }
        , toRequest =
            λ(_ : {}) → Req.default ⫽ { baseUrl = url }
        }
    , just-the-facts =
        let List/map = https://prelude.dhall-lang.org/List/map

        in  { parent =
                "cat-facts"
            , f =
                  λ(facts : { all : List ListFact })
                → List/map ListFact Text (λ(m : ListFact) → m.text) facts.all
            , outputType =
                List Text
            }
    , cat-fact =
        { inputType =
            { _id : Text }
        , outputType =
            Fact
        , toRequest =
              λ(i : { _id : Text })
            → Req.default ⫽ { baseUrl = url, pathParts = [ i._id ] }
        , cache =
            Some { ttlSeconds = +3600 }
        }
    , cat-fact-by-string =
        { inputType =
            Text
        , parent =
            "cat-fact"
        , f =
            λ(t : Text) → { _id = t }
        , cache =
            Some { ttlSeconds = +3600 }
        }
    , cat-fact-easter-egg =
        { outputType =
            Text
        , parent =
            "cat-fact-by-string"
        , f =
              λ(fact : Fact)
            →       if Text/equal fact.text "Cats can be nation president."

              then  "conCATulations, you found the egg!"

              else  fact.text
        }
    }
