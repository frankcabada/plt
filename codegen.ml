module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate(globals, functions) =
  let context = L.global_context() in
  let the_module = L.create_module context "CMAT"

  and i32_t   = L.i32_type context
  and i8_t    = L.i8_type context
  and void_t  = L.void_type context in

  let ltype_of_typ = function
      A.Int  -> i32_t
    | A.Void -> void_t in

  let printf_t =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func =
    L.declare_function "printf" printf_t the_module in ignore()
