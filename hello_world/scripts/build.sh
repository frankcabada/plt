#!/bin/bash

cp ../src/scanner.mll ./scanner.mll
cp ../src/parser.mly ./parser.mly
cp ../src/ast.ml ./ast.ml
cp ../src/sast.ml ./sast.ml
cp ../src/semant.ml ./semant.ml
cp ../src/exceptions.ml ./exceptions.ml
cp ../src/utils.ml ./utils.ml
cp ../src/codegen.ml ./codegen.ml
cp ../src/cmat.ml ./cmat.ml

ocamlbuild -j 0 -r -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target cmat.native
#ocamllex scanner.mll && echo "Scanner lex-ed"
#echo ""
#ocamlyacc parser.mly && echo "Parser yacc-ed"
#echo ""
#ocamlc -c ast.mli && echo "Ast compiled"
#echo ""
#ocamlc -c parser.mli
#ocamlc -c scanner.ml && echo "Scanner compiled"
#echo ""
#ocamlc -c parser.ml && echo "Parser compiled"
#echo ""
#ocamlc -c sast.mli && echo "Sast compiled"
#echo ""
#ocamlc -c exceptions.ml && echo "Exceptions compiled"
#echo ""
#ocamlc -c utils.ml && echo "Utils compiled"
#echo ""
#ocamlc -c semant.ml && echo "Semant compiled"
#echo ""
#ocamlc -I ~/.opam/system/lib/llvm/ -c codegen.ml && echo "Codegen compiled"
#echo ""
#ocamlc -I ~/.opam/system/lib/llvm/ -c cmat.ml && echo "CMAT compiled"
#echo ""
#ocamlc -g -o cmatc scanner.cmo parser.cmo utils.cmo codegen.cmo exceptions.cmo cmat.cmo
