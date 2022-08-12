rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

.PHONY: build
build:
	rm -rf build/powerhack
	cd cli && ../elm-posix/bin/elm-cli make src/Main.elm ../build/powerhack

.PHONY: build-debug
build-debug:
	rm -rf build/powerhack
	cd cli && ../elm-posix/bin/elm-cli make --debug src/Main.elm ../build/powerhack

.PHONY: test
test:
	cd cli && elm-test $(foreach x, $(call rwildcard,compiler/,*.elm), ../$x)

.PHONY: integration-test
integration-test:
	cd cli && ../elm-posix/bin/elm-cli make src/Main.elm ../build/powerhack
	npx ava

.PHONY: elm-review
elm-review:
	npx elm-review --elmjson cli/elm.json