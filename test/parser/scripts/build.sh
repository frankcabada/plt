#!/bin/bash

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c parserize.ml
ocamlc -o parserize parser.cmo scanner.cmo parserize.cmo
