all:
	ocamllex lexer.mll
	ocamllex lexer2.mll
	ocamlyacc parser.mly
	ocamlyacc parser2.mly
	ocamlc -c backend.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c parser2.mli
	ocamlc -c parser2.ml
	ocamlc -c lexer.ml
	ocamlc -c lexer2.ml
	ocamlc -c main.ml 
	ocamlc -o LogPro str.cma lexer.cmo lexer2.cmo backend.cmo parser.cmo parser2.cmo main.cmo 

clean:
	rm lexer.ml
	rm lexer2.ml
	rm lexer2.cmo
	rm lexer2.cmi
	rm parser.mli
	rm parser2.mli
	rm parser2.ml
	rm parser2.cmo
	rm parser.ml
	rm lexer.cmo 
	rm parser.cmo
	rm backend.cmo
	rm backend.cmi
	rm main.cmo
	rm main.cmi
	rm lexer.cmi
	rm parser.cmi
	rm parser2.cmi
	rm LogPro
