#!/bin/bash

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c tokenize.ml
ocamlc -o parserize parser.cmo scanner.cmo parserize.cmo
