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
