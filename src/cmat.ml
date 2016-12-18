open Scanner
open Parser
open Ast
open Utils
open Codegen
open Semant
open Prep
open Llvm

type action = Ast | LLVM_IR | Compile

let _ =
    let action = if Array.length Sys.argv > 1
        then List.assoc Sys.argv.(1)
            [ ("-a",Ast); ("-l",LLVM_IR); ("-c",Compile) ]
            else Compile in
    let infile =
        if (Array.length Sys.argv > 2)
            then Sys.argv.(2)
            else raise(Exceptions.NoFileArgument)
    in
    let outfile =
        if (Array.length Sys.argv > 3 && action=Compile)
            then Sys.argv.(3)
            else "out.ll" in
    let strin = Prep.process infile in
    let lexbuf = Lexing.from_string strin in
    let ast = Parser.program Scanner.token lexbuf in

    let sast =
    let gst = Semant.check_var_decls (fst ast) in
        Semant.check_functions gst (fst ast) (snd ast) in

    match action with
        Ast -> print_string(Utils.string_of_program ast)
        | LLVM_IR -> print_string(Llvm.string_of_llmodule(Codegen.translate sast))
        | Compile -> let m = Codegen.translate sast in
            Llvm_analysis.assert_valid_module m; (* Useful built-in check *)
            print_module (outfile) (m);
