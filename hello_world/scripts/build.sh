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
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c sast.mli
ocamlc -I ~/.opam/system/lib/llvm/ -c codegen.ml
ocamlc -c semant.ml
ocamlc -c exceptions.ml
ocamlc -c utils.ml
