#!/bin/bash

cp ../scanner.mll ./scanner.mll
cp ../parser.mly ./parser.mly
cp ../ast.mli ./ast.mli
cp ../sast.mli ./sast.mli
cp ../semant.ml ./semant.ml
cp ../exceptions.ml ./exceptions.ml
cp ../utils.ml ./utils.ml
cp ../codegen.ml ./codegen.ml
cp ../cmat.ml ./cmat.ml

ocamllex scanner.mll && echo "Scanner lex-ed"
echo ""
ocamlyacc parser.mly && echo "Parser yacc-ed"
echo ""
ocamlc -c ast.mli && echo "Ast compiled"
echo ""
ocamlc -c parser.mli
ocamlc -c scanner.ml && echo "Scanner compiled"
echo ""
ocamlc -c parser.ml && echo "Parser compiled"
echo ""
ocamlc -c sast.mli && echo "Sast compiled"
echo ""
ocamlc -c exceptions.ml && echo "Exceptions compiled"
echo ""
ocamlc -c utils.ml && echo "Utils compiled"
echo ""
ocamlc -c semant.ml && echo "Semant compiled"
echo ""
ocamlc -I ~/.opam/system/lib/llvm/ -c codegen.ml && echo "Codegen compiled"
echo ""
ocamlc -I ~/.opam/system/lib/llvm/ -c cmat.ml && echo "CMAT compiled"
echo ""
ocamlc -o cmatc scanner.cmo parser.cmo
