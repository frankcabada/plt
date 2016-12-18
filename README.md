# plt
Programming Languages &amp; Translators project

Important Links for LLVM:
    http://llvm.org/releases/2.6/docs/LangRef.html
    http://llvm.moe/ocaml/Llvm.html

CMAT requires the OCaml llvm library, which is most easily installed through opam.

Install LLVM, its development libraries, the m4 macro preprocessor,
and opam. Then use opam to install llvm.

The version of the OCaml llvm library should match the version of the LLVM
system installed on your system.

-------------------------------------------------------------------------------
Installation under Ubuntu 16.04

LLVM 3.8 is the default under 16.04, so we ask for a matching version of the
OCaml library.

    sudo apt-get install -y ocaml m4 llvm opam
    opam init
    opam install llvm.3.8 ocamlfind
    eval `opam config env`

    make
    ./testall.sh

-------------------------------------------------------------------------------
Installation under Ubuntu 15.10

LLVM 3.6 is the default under 15.10, so we ask for a matching version of the
OCaml library.

    sudo apt-get install -y ocaml m4 llvm opam
    opam init
    opam install llvm.3.6 ocamlfind
    eval `opam config env`

    make
    ./testall.sh

-------------------------------------------------------------------------------

Installation under Ubuntu 14.04

The default LLVM package is 3.4, so we install the matching OCaml library using
opam.
The default version of opam under 14.04 is too old; we need to use a newer
package.

    sudo apt-get install m4 llvm software-properties-common
    sudo add-apt-repository --yes ppa:avsm/ppa
    sudo apt-get update -qq
    sudo apt-get install -y opam
    opam init

    eval `opam config env`

    opam install llvm.3.4 ocamlfind

-------------------------------------------------------------------------------
