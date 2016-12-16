#!/bin/bash

cp ../src/scanner.mll ./scanner.mll
cp ../src/parser.mly ./parser.mly
cp ../src/ast.ml ./ast.ml
cp ../src/sast.ml ./sast.ml
cp ../src/semant.ml ./semant.ml
cp ../src/exceptions.ml ./exceptions.ml
cp ../src/utils.ml ./utils.ml
cp ../src/codegen.ml ./codegen.ml
cp ../src/prep.ml ./prep.ml
cp ../src/cmat.ml ./cmat.ml

ocamlbuild -j 0 -r -use-ocamlfind -pkgs str,llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target cmat.native
