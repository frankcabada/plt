#!/bin/bash

ocamllex scanner.mll
ocamlyacc nullparser.mly
ocamlc -c nullparser.mli
ocamlc -c scanner.ml
ocamlc -c nullparser.ml
ocamlc -c tokenize.ml
ocamlc -o tokenize nullparser.cmo scanner.cmo tokenize.cmo

cat testfile.txt | ./tokenize
