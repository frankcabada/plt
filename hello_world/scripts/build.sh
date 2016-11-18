#!/bin/bash

cp ../scanner.mll ./scanner.mll
cp ../parser.mly ./parser.mly
cp ../ast.mli ./ast.mli
cp ../sast.mli ./sast.mli
cp ../semant.ml ./semant.ml
cp ../exceptions.ml ./exceptions.ml
cp ../utils.ml ./utils.ml
cp ../codegen.ml ./codegen.ml

ocamllex scanner.mll
echo ""
ocamlyacc parser.mly
echo ""
ocamlc -c ast.mli
echo ""
ocamlc -c parser.mli
echo ""
ocamlc -c scanner.ml
echo ""
ocamlc -c parser.ml
echo ""
ocamlc -c sast.mli
echo ""
ocamlc -c exceptions.ml
echo ""
ocamlc -c utils.ml
echo ""
ocamlc -c semant.ml
echo ""
ocamlc -I ~/.opam/system/lib/llvm/ -c codegen.ml
echo ""
