cat pass/_base_scanner.test | ./tokenize > pass/_base_scanner.res
diff pass/_base_scanner.out pass/_base_scanner.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: FIRST TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: FIRST TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat pass/_delimiters.test | ./tokenize > pass/_delimiters.res
diff pass/_delimiters.out pass/_delimiters.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   SCANNER: DELIMITERS TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   SCANNER: DELIMITERS TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat pass/_control_flow.test | ./tokenize > pass/_control_flow.res
diff pass/_control_flow.out pass/_control_flow.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  SCANNER: CONTROL FLOW TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  SCANNER: CONTROL FLOW TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat pass/_conditionals.test | ./tokenize > pass/_conditionals.res
diff pass/_conditionals.out pass/_conditionals.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  SCANNER: CONDITIONALS TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  SCANNER: CONDITIONALS TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat pass/_arithmetic.test | ./tokenize > pass/_arithmetic.res
diff pass/_arithmetic.out pass/_arithmetic.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   SCANNER: ARITHMETIC TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   SCANNER: ARITHMETIC TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat pass/_types.test | ./tokenize > pass/_types.res
diff pass/_types.out pass/_types.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: TYPES TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: TYPES TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat pass/_matrix.test | ./tokenize > pass/_matrix.res
diff pass/_matrix.out pass/_matrix.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     SCANNER: MATRIX TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     SCANNER: MATRIX TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat pass/_comment.test | ./tokenize > pass/_comment.res
diff pass/_comment.out pass/_comment.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     SCANNER: COMMENTS TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     SCANNER: COMMENTS TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat pass/_identifier.test | ./tokenize > pass/_identifier.res
diff pass/_identifier.out pass/_identifier.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    SCANNER: IDENTIFIER TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    SCANNER: IDENTIFIER TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat pass/_mixed_arithmetic.test | ./tokenize > pass/_mixed_arithmetic.res
diff pass/_mixed_arithmetic.out pass/_mixed_arithmetic.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| SCANNER: MIXED ARITHMETIC TEST PASSED |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| SCANNER: MIXED ARITHMETIC TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat pass/_literal.test | ./tokenize > pass/_literal.res
diff pass/_literal.out pass/_literal.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|     SCANNER: LITERAL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|     SCANNER: LITERAL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat pass/_assignment.test | ./tokenize > pass/_assignment.res
diff pass/_assignment.out pass/_assignment.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    SCANNER: ASSIGNMENT TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    SCANNER: ASSIGNMENT TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat pass/_main_function.test | ./tokenize > pass/_main_function.res
diff pass/_main_function.out pass/_main_function.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   SCANNER: MAIN FUNCTION TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   SCANNER: MAIN FUNCTION TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat pass/_function.test | ./tokenize > pass/_function.res
diff pass/_function.out pass/_function.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    SCANNER: FUNCTION TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    SCANNER: FUNCTION TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat pass/_misc.test | ./tokenize > pass/_misc.res
diff pass/_misc.out pass/_misc.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  SCANNER: MISCELLANEOUS TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  SCANNER: MISCELLANEOUS TEST FAILED   |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_carrot.test | ./tokenize >& fail/_illegal_carrot.res
diff fail/_illegal_carrot.out fail/_illegal_carrot.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: ^ FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: ^ FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_dollar.test | ./tokenize >& fail/_illegal_dollar.res
diff fail/_illegal_dollar.out fail/_illegal_dollar.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: $ FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: $ FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_percent.test | ./tokenize >& fail/_illegal_percent.res
diff fail/_illegal_percent.out fail/_illegal_percent.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: % FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: % FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_period.test | ./tokenize >& fail/_illegal_period.res
diff fail/_illegal_period.out fail/_illegal_period.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: . FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: . FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_pound.test | ./tokenize >& fail/_illegal_pound.res
diff fail/_illegal_pound.out fail/_illegal_pound.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: # FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: # FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_tilde.test | ./tokenize >& fail/_illegal_tilde.res
diff fail/_illegal_tilde.out fail/_illegal_tilde.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|      SCANNER: ~ FAIL TEST PASSED      |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|      SCANNER: ~ FAIL TEST FAILED      |"
  echo "-----------------------------------------"
fi