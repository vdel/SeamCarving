all: image.ml interface.ml main.ml
	ocamlc -o resize_img graphics.cma types.ml pixel.mli pixel.ml matrix.mli matrix.ml image.mli image.ml energy.mli energy.ml path.mli path.ml interface.mli interface.ml main.ml

clean:
	rm *.cmi *.cmo *.ml~ *.mli~
