(* Compiler Exceptions *)
exception NoFileArgument

(* Processor Exceptions *)
exception MissingEOF

(* Scanner Exceptions *)
exception IllegalCharacter of string * char * int
exception UnmatchedQuotation of int
exception IllegalToken of string

(* Analyzer Exceptions *)
exception AllVoidFunctionsMustNotReturn of string
exception AllNonVoidFunctionsMustEndWithReturn of string
exception AssignmentTypeMismatch of string * string
exception CannotUseReservedFuncName of string
exception CannotUseRowsOnNonMatrix of string
exception CannotUseTransposeOnNonMatrix of string
exception CannotUseColsOnNonMatrix of string
exception CannotUseLenOnNonVector of string
exception DuplicateFunc of string
exception DuplicateFunction of string
exception DuplicateGlobal of string
exception DuplicateLocal of string
exception FunctionNotFound of string
exception IncorrectTypePassedToFunction of string * string
exception IncorrectNumberOfArguments of string * int * int
exception InvalidAccessLHS of string
exception InvalidBinopExpression of string
exception InvalidIfStatementType
exception InvalidForStatementType
exception InvalidMatrixInit
exception InvalidWhileStatementType
exception InvalidUnaryOperation
exception LocalAssignTypeMismatch of string * string
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
exception MustPassIntegerTypeToArrayCreate
exception MustPassIntegerTypeToArrayAccess
exception MultipleMainsDefined
exception ReturnTypeMismatch of string * string
exception UndefinedID of string
exception UnknownIdentifier of string
exception UnknownIdentifierForClass of string * string
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
exception InvalidBinaryOperator
exception InvalidMatrixDimension
exception InvalidUnopType
exception InvalidUnopEvaluationType
exception InvalidVectorDimension
exception MatrixOutOfBoundsAccess of string
exception UnopNotSupported
exception UnsupportedBinopType
exception UnsupportedMatrixType
exception UnsupportedVectorType
exception CannotUseNewWithVectors
exception CannotUseNewwithMatrices
exception CanOnlyUseFreeWithVariables
exception VectorOutOfBoundsAccess of string
