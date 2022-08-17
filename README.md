# Powerhack

With great responsibility comes great power.

## What is this?

At this point it's a compiler learning playground kind of thing for a language 
I've called Powerhack.

## Some commands that do things

make build

./build/powerhack-lang examples/fib.powerhack --output out.js

make test

npx elm-review --elmjson cli/elm.json 

make integration-test


## TODO

- [ ] Compile modules one by one somehow to easily pass in source code to reports
- [ ] Make primitives somehow part of something so they aren't scattered around modules
- [ ] Move ASTs to the modules generating them
- [ ] Rename Parse to Source, Canonicalize to Canonical, InferTypes to Typed?
I'm not sure about the type inference one. In any case something feels weird about
them being named with verbs.