#! /bin/bash

cat _assign.test | ./cmat.native -c assign.out
diff _assign.out _assign.res > /dev/null
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

cat _func_call.test | ./cmat.native -c func_call.out
diff _func_call.out _func_call.res > /dev/null
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

