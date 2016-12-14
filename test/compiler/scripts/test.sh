#! /bin/bash

cat _assign_int_float.test | ./cmat.native -c _assign_int_float.ll
lli _assign_int_float.ll > _assign_int_float.res
diff _assign_int_float.out _assign_int_float.res > /dev/null
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

cat _func_call.test | ./cmat.native -c _func_call.ll
lli _func_call.ll > _func_call.res
diff _func_call.out _func_call.res > /dev/null
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
