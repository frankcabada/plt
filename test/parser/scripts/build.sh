#!/bin/bash

cp ../../scanner.mll ./scanner.mll
cp ../../parser.mly ./parser.mly
cp ../../ast.mli ./ast.mli

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
