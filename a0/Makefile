default:
	utop -init warmup.ml
	
test:
	ocamlbuild warmup.byte && ./warmup.byte

check:
	bash checkenv.sh && bash checktypes.sh
	
finalcheck: check
	bash finalcheck.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html warmup.ml

clean:
	ocamlbuild -clean
	rm -rf doc
