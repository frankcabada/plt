cat _base_scanner.test | ./tokenize > _base_scanner.res
diff _base_scanner.out _base_scanner.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|      SCANNER: FIRST TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|      SCANNER: FIRST TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat _delimiters.test | ./tokenize > _delimiters.res
diff _delimiters.out _delimiters.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|   SCANNER: DELIMITERS TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|   SCANNER: DELIMITERS TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat _control_flow.test | ./tokenize > _control_flow.res
diff _control_flow.out _control_flow.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|  SCANNER: CONTROL FLOW TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|  SCANNER: CONTROL FLOW TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat _conditionals.test | ./tokenize > _conditionals.res
diff _conditionals.out _conditionals.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|  SCANNER: CONDITIONALS TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|  SCANNER: CONDITIONALS TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat _arithmetic.test | ./tokenize > _arithmetic.res
diff _arithmetic.out _arithmetic.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|   SCANNER: ARITHMETIC TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|   SCANNER: ARITHMETIC TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat _types.test | ./tokenize > _types.res
diff _types.out _types.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|      SCANNER: TYPES TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|      SCANNER: TYPES TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat _misc.test | ./tokenize > _misc.res
diff _misc.out _misc.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|  SCANNER: MISCELLANEOUS TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|  SCANNER: MISCELLANEOUS TEST FAILED   |"
  echo "-----------------------------------------"
fi

cat _program.test | ./tokenize > _program.res
diff _program.out _program.res > /dev/null
if [ $? = 0 ]; then
  echo "-----------------------------------------"
  echo "|  SCANNER: SIMPLE PROGRAM TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo "-----------------------------------------"
  echo "|  SCANNER: SIMPLE PROGRAM TEST FAILED  |"
  echo "-----------------------------------------"
fi
