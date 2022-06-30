rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

.PHONY: build
build:
	rm -rf build/powerhack
	cd cli && ../albertdahlin/elm-posix/bin/elm-cli make src/Main.elm ../build/powerhack

.PHONY: test
test:
	cd cli && elm-test $(foreach x, $(call rwildcard,compiler/,*.elm), ../$x)