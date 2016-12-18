#! /bin/bash

./cmat.native -c pass/_assign.test pass/_assign.ll
lli pass/_assign.ll > pass/_assign.res
diff pass/_assign.out pass/_assign.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      COMPILER: ASSIGN TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      COMPILER: ASSIGN TEST FAILED     |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_include.test pass/_include.ll
lli pass/_include.ll > pass/_include.res
diff pass/_include.out pass/_include.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     COMPILER: INCLUDE TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     COMPILER: INCLUDE TEST FAILED     |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_func_call.test pass/_func_call.ll
lli pass/_func_call.ll > pass/_func_call.res
diff pass/_func_call.out pass/_func_call.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    COMPILER: FUNC CALL TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    COMPILER: FUNC CALL TEST FAILED    |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_arithmetic_binops.test pass/_arithmetic_binops.ll
lli pass/_arithmetic_binops.ll > pass/_arithmetic_binops.res
diff pass/_arithmetic_binops.out pass/_arithmetic_binops.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  COMPILER: ARITH BINOPS TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  COMPILER: ARITH BINOPS TEST FAILED   |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_return.test pass/_return.ll
lli pass/_return.ll > pass/_return.res
diff pass/_return.out pass/_return.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     COMPILER: RETURN TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     COMPILER: RETURN TEST FAILED      |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_control_flow.test pass/_control_flow.ll
lli pass/_control_flow.ll > pass/_control_flow.res
diff pass/_control_flow.out pass/_control_flow.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  COMPILER: CONTROL FLOW TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  COMPILER: CONTROL FLOW TEST FAILED   |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_matrices.test pass/_matrices.ll
lli pass/_matrices.ll > pass/_matrices.res
diff pass/_matrices.out pass/_matrices.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     COMPILER: MATRIX TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     COMPILER: MATRIX TEST FAILED      |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_vectors.test pass/_vectors.ll
lli pass/_vectors.ll > pass/_vectors.res
diff pass/_vectors.out pass/_vectors.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     COMPILER: VECTOR TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     COMPILER: VECTOR TEST FAILED      |"
  echo "-----------------------------------------"
fi

./cmat.native -c pass/_mat_vec.test pass/_mat_vec.ll
lli pass/_mat_vec.ll > pass/_mat_vec.res
diff pass/_mat_vec.out pass/_mat_vec.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  COMPILER: MATRIX/VECTOR TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  COMPILER: MATRIX/VECTOR TEST FAILED  |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_illegal_float_equality.test fail/_illegal_float_equality.ll >& fail/_illegal_float_equality.res
diff fail/_illegal_float_equality.out fail/_illegal_float_equality.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: FLOAT EQUALITY TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: FLOAT EQUALITY TEST FAILED  |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_matrix_equality.test fail/_matrix_equality.ll >& fail/_matrix_equality.res
diff fail/_matrix_equality.out fail/_matrix_equality.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: MATRIX EQUALITY TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: MATRIX EQUALITY TEST FAILED |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_duplicate_global.test fail/_duplicate_global.ll >& fail/_duplicate_global.res
diff fail/_duplicate_global.out fail/_duplicate_global.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: DUPLICATE GLOBAL TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: DUPLICATE GLOBAL TEST FAILED |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_function_nonexistent.test fail/_function_nonexistent.ll >& fail/_function_nonexistent.res
diff fail/_function_nonexistent.out fail/_function_nonexistent.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER:MISSING FUNCTION TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER:MISSING FUNCTION TEST FAILED |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_incorrect_function_args.test fail/_incorrect_function_args.ll >& fail/_incorrect_function_args.res
diff fail/_incorrect_function_args.out fail/_incorrect_function_args.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  COMPILER: FUNCTION ARGS TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  COMPILER: FUNCTION ARGS TEST FAILED  |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_matrix_mismatch_add.test fail/_matrix_mismatch_add.ll >& fail/_matrix_mismatch_add.res
diff fail/_matrix_mismatch_add.out fail/_matrix_mismatch_add.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: MATRIX MISMATCH TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: MATRIX MISMATCH TEST FAILED |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_no_return.test fail/_no_return.ll >& fail/_no_return.res
diff fail/_no_return.out fail/_no_return.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: MISSING RETURN TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: MISSING RETURN TEST FAILED  |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_return_mismatch.test fail/_return_mismatch.ll >& fail/_return_mismatch.res
diff fail/_return_mismatch.out fail/_return_mismatch.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| COMPILER: RETURN MISMATCH TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| COMPILER: RETURN MISMATCH TEST FAILED |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_void_formal.test fail/_void_formal.ll >& fail/_void_formal.res
diff fail/_void_formal.out fail/_void_formal.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID FORMAL TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID FORMAL TEST FAILED   |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_void_global.test fail/_void_global.ll >& fail/_void_global.res
diff fail/_void_global.out fail/_void_global.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID GLOBAL TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID GLOBAL TEST FAILED   |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_void_local.test fail/_void_local.ll >& fail/_void_local.res
diff fail/_void_local.out fail/_void_local.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID LOCAL TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID LOCAL TEST FAILED    |"
  echo "-----------------------------------------"
fi

./cmat.native -c fail/_void_return.test fail/_void_return.ll >& fail/_void_return.res
diff fail/_void_return.out fail/_void_return.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID RETURN TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   COMPILER: VOID RETURN TEST FAILED   |"
  echo "-----------------------------------------"
fi