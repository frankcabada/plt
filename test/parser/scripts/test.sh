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

cat _main_with_assign.test | menhir --interpret --interpret-show-cst parser.mly > _main_with_assign.res
diff _main_with_assign.out _main_with_assign.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| PARSER: MAIN WITH ASSIGN TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| PARSER: MAIN WITH ASSIGN TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat _primitive_decls.test | menhir --interpret --interpret-show-cst parser.mly > _primitive_decls.res
diff _primitive_decls.out _primitive_decls.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "| PARSER: PRIMITIVE VDECLS TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "| PARSER: PRIMITIVE VDECLS TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat _formal_opts.test | menhir --interpret --interpret-show-cst parser.mly > _formal_opts.res
diff _formal_opts.out _formal_opts.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    PARSER: FORMAL OPTS TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    PARSER: FORMAL OPTS TEST FAILED    |"
  echo "-----------------------------------------"
fi

cat _stmts.test | menhir --interpret --interpret-show-cst parser.mly > _stmts.res
diff _stmts.out _stmts.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|    PARSER: STATEMENTS TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|    PARSER: STATEMENTS TEST FAILED     |"
  echo "-----------------------------------------"
fi
