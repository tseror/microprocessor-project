simulateur:
	cd simulateur && ocamlbuild main.byte && mv main.byte ../simulateur.byte

clean:
	rm -f -R simulateur/_build
	rm -f simulateur.byte

.PHONY: simulateur