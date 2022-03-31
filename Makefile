.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main_with_bot.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f texas_holdem.zip
	zip -r texas_holdem.zip . -x@exclude.lst

clean:
	dune clean
	rm -f texas_holdem.zip

doc:
	dune build @doc
