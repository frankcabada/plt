cp ../../parser.mly ./parser.mly

cat _base_parser.test | menhir --interpret --interpret-show-cst parser.mly > _base_parser.res
diff _base_parser.out _base_parser.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|       PARSER: FIRST TEST PASSED       |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|       PARSER: FIRST TEST FAILED       |"
  echo "-----------------------------------------"
fi

cat _main_with_return.test | menhir --interpret --interpret-show-cst parser.mly > _main_with_return.res
diff _main_with_return.out _main_with_return.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| PARSER: MAIN WITH RETURN TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| PARSER: MAIN WITH RETURN TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat _main_with_return.test | menhir --interpret --interpret-show-cst parser.mly > _main_with_return.res
diff _main_with_return.out _main_with_return.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  PARSER: MAIN WITH FDECL TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  PARSER: MAIN WITH FDECL TEST FAILED  |"
  echo "-----------------------------------------"
fi
