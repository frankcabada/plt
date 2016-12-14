#! /bin/bash

cat pass/_vdecl.test | ./cmat.native -c pass/_vdecl.ll
diff pass/_vdecl.out pass/_vdecl.ll > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      COMPILER: VDECL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      COMPILER: VDECL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat pass/_assign_int_float.test | ./cmat.native -c pass/_assign_int_float.ll
lli pass/_assign_int_float.ll > pass/_assign_int_float.res
diff pass/_assign_int_float.out pass/_assign_int_float.res > /dev/null
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

cat pass/_func_call.test | ./cmat.native -c pass/_func_call.ll
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

cat pass/_arithmetic_binops.test | ./cmat.native -c pass/_arithmetic_binops.ll
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

cat pass/_return.test | ./cmat.native -c pass/_return.ll
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

cat pass/_control_flow.test | ./cmat.native -c pass/_control_flow.ll
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
