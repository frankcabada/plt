(*
 * COMS4115: CMAT Exceptions
 *
 * Authors:
 *  - Marissa Ojeda
 *  - Daniel Rojas
 *  - Mike Berkowitz
 *  - Frank Cabada
 *)

(* Compiler Exceptions *)
exception NoFileArgument

(* Analyzer Exceptions *)
exception AllVoidFunctionsMustNotReturn of string
exception AllNonVoidFunctionsMustEndWithReturn of string
exception AssignmentTypeMismatch of string * string
exception CannotUseRowsOnNonMatrix of string
exception CannotUseTransposeOnNonMatrix of string
exception CannotUseColsOnNonMatrix of string
exception CannotUseLenOnNonVector of string
exception DuplicateFunc of string
exception DuplicateGlobal of string
exception DuplicateLocal of string 
exception FunctionNotFound of string
exception IncorrectNumberOfArguments of string * int * int
exception InvalidBinopExpression of string
exception InvalidMatrixInit
exception InvalidUnaryOperation
exception MalformedMatrixLit
exception MatrixDimensionMustBeInt
exception MatrixAccessOnNonMatrix of string
exception MatrixColOnNonMatrix of string
exception MatrixLitMustBeOneType
exception VectorLitMustBeOneType
exception MatrixOutOfBoundsAccess of string
exception MatrixRowOnNonMatrix of string
exception MismatchedMatricesForAddSub of string
exception MismatchedMatricesForMult of string
exception MismatchedVectorsForBinop of string
exception ReturnTypeMismatch of string * string
exception UndefinedID of string
exception UnsupportedMatrixBinop of string
exception UnsupportedStringBinop of string
exception UnsupportedVectorBinop of string
exception VectorAccessOnNonMatrix of string
exception VectorDimensionMustBeIntLit
exception VoidFunctionFormal of string
exception VoidFunctionLocal of string
exception VoidFunc of string
exception VoidGlobal of string

(* Codegen Exceptions *)
exception AssignLHSMustBeAssignable
exception DecMustBeCalledOnID
exception IllegalBoolBinop
exception IllegalBoolUnop
exception IllegalIntUnop
exception IllegalCast
exception IllegalFloatBinop
exception IllegalFloatUnop
exception IllegalIntBinop
exception IllegalMatrixBinop
exception IllegalVectorBinop
exception IncMustBeCalledOnID
exception InvalidMatrixDimension
exception InvalidUnopType
exception InvalidVectorDimension
exception MatrixOutOfBoundsAccess of string
exception UnsupportedBinopType
exception UnsupportedMatrixType
exception UnsupportedVectorType
exception VectorOutOfBoundsAccess of string
