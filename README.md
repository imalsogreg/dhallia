# Dhallia

An API Gateway configured in Dhall.

Dhallia lets you use Dhall types and expressions to wrap and compose underlying
APIs. Run-time type information and the self-contained nature of Dhall
expressions makes it easy to explore and distribute APIs

## Stability & Features

Nowhere close to usable. This is just an experiment. The main point of the
repository is to illustrate the inductive API concept and get feedback on it.
Nice implementation could come later.

 - [x] Composable REST APIs
 - [ ] Composable websocket APIs
 - [x] Client REPL
 - [ ] Circuit breaking
 - [ ] Dry-run mode
 - [x] Configurable caching
 - [ ] Configurable logging & metrics
 - [ ] Documentation
 - [ ] Admin UI
 - [ ] Authentication
 - [ ] Authorization
 - [ ] 
 
## Code organization

### Core library: src

 - Logic for interpreting dhall API specifications
 - Typechecking API compositions
 - Core implementations of application logic

## Inductive APIs

`dhallia` provides a format for modeling APIs in the
[dhall](https://dhall-lang.org) language and using them as an HTTP
client. API definitions are "raw" or "composite"


### Raw APIs (the base case)

We'll model an hypothetical API that creates short greetings for
users. It accepts Requests like `https://example.com/greet/<name>` and
returns responses like `{ "greeting", "Hello, Pete!" }`.


```dhall
let Req = ./Request.dhall

let ExampleInput  = { name :     Text }
let ExampleOutput = { greeting : Text }
in {
rawExample =
  { inputType  = ExampleInput,
    outputType = { greeting : Text },
    toRequest  = \(input : ExampleInput) ->
                   Req.addPathPart (input.name) Req.default
  }
}
```

The `inputType` and `toRequest` fields describe how we will generate
requests from `dhallia`. The `outputType` field describes how to
interpret the JSON response.

What makes an API "Raw" is the fact that we describe how to turn
`dhall` values into an HTTP request.


### Composite APIs (the inductive cases)

We can alternatively define an `Fmap` API in terms of some parent API
and a function `f` that maps the parent's output to a new output.

```dhall
let Req = ./Request.dhall

let ParentInput  = { name :     Text }
let ParentOutput = { greeting : Text }
let MyOutput     = { extended : Text }
in {
fmapExample =
  { parent     = "rawExample",
    outputType = MyOutput,
    f          = \(i : ParentOutput) ->
                   { extended = "${i.greeting}. How are you?" }
  }
}
```

And we can define an 'Ap' API in terms of two parent APIs and a combining
function.

```dhall
let Req = ./Request.dhall

in {
apExample =
  { parentA    = "rawExample",
    parentB    = "fmapExample",
    outputType = { compositeGreeting : Text },
    f          = \(a : { greeting : Text }) ->
                 \(b : { extended : Text }) ->
                   { compositeGreeting =
                     "We say to you, ${a.greeting} and ${b.extended}!"
                   }
  }
}
```

## Runtime

Above we _described_ our APIs, now we want to use them.

Right now the only way to use them is to parse the API definitions,
then construct inputs as `dhall` values in Haskell, and call the
[runRequests](https://github.com/imalsogreg/dhallia/blob/master/src/Dhallia/Interpreter/HTTPClient/API.hs#L47)
function.


## Why?

If you are on the receiving end of a microservice architecture,
collecting data from from many sources and combining it correctly can
be difficult.

The goal of inductive API definitions is to provide what appears
to be a single HTTP API that provides responses directly useful to
your domain, where you previously had to interact with many APIs that
each provided a just piece of that larger domain.

In addition to _calling_ a composite API, we may want other behavior
to compose - caching logic for example, or the generation of
`swagger`-like user interface, or collections of request-response
pairs as test data fixtures.


## Try it out

This requires `nix`.

### test-server

In a dedicated terminal, run the test server. It's a small `snap`
server that restarts on code change. This is the server that our
sample Raw API will wrap.

```bash
./test-server.hs
```

### HTTP client

Implementation and sample usage are currently mixed together
in [src/API.hs](https://github.com/imalsogreg/dhallia/blob/master/src/API.hs).

```bash
$ nix-shell
> cabal new-repl
*API> test
```

TODO: Obviously, improve the example case.
