cp ../../src/parser.mly ./parser.mly

cat pass/_base_parser.test | menhir --interpret --interpret-show-cst parser.mly > pass/_base_parser.res
diff pass/_base_parser.out pass/_base_parser.res > /dev/null
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

cat pass/_main_with_return.test | menhir --interpret --interpret-show-cst parser.mly > pass/_main_with_return.res
diff pass/_main_with_return.out pass/_main_with_return.res > /dev/null
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

cat pass/_main_with_return.test | menhir --interpret --interpret-show-cst parser.mly > pass/_main_with_return.res
diff pass/_main_with_return.out pass/_main_with_return.res > /dev/null
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

cat pass/_main_with_assign.test | menhir --interpret --interpret-show-cst parser.mly > pass/_main_with_assign.res
diff pass/_main_with_assign.out pass/_main_with_assign.res > /dev/null
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

cat pass/_primitive_decls.test | menhir --interpret --interpret-show-cst parser.mly > pass/_primitive_decls.res
diff pass/_primitive_decls.out pass/_primitive_decls.res > /dev/null
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

cat pass/_formal_opts.test | menhir --interpret --interpret-show-cst parser.mly > pass/_formal_opts.res
diff pass/_formal_opts.out pass/_formal_opts.res > /dev/null
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

cat pass/_stmts.test | menhir --interpret --interpret-show-cst parser.mly > pass/_stmts.res
diff pass/_stmts.out pass/_stmts.res > /dev/null
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

cat pass/_expr.test | menhir --interpret --interpret-show-cst parser.mly > pass/_expr.res
diff pass/_expr.out pass/_expr.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   PARSER: EXPRESSIONS TEST PASSED     |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   PARSER: EXPRESSIONS TEST FAILED     |"
  echo "-----------------------------------------"
fi

cat fail/_vdecl_without_semi.test | menhir --interpret --interpret-show-cst parser.mly > fail/_vdecl_without_semi.res
diff fail/_vdecl_without_semi.out fail/_vdecl_without_semi.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  PARSER: VDECL W/O SEMI TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  PARSER: VDECL W/O SEMI TEST FAILED   |"
  echo "-----------------------------------------"
fi

cat fail/_illegal_binop.test | menhir --interpret --interpret-show-cst parser.mly > fail/_illegal_binop.res
diff fail/_illegal_binop.out fail/_illegal_binop.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   PARSER: ILLEGAL BINOP TEST PASSED   |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   PARSER: ILLEGAL BINOP TEST FAILED   |"
  echo "-----------------------------------------"
fi

cat fail/_internal_fdecl.test | menhir --interpret --interpret-show-cst parser.mly > fail/_internal_fdecl.res
diff fail/_internal_fdecl.out fail/_internal_fdecl.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   PARSER: INTERNAL FDECL TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   PARSER: INTERNAL FDECL TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat fail/_malformed_fdecl.test | menhir --interpret --interpret-show-cst parser.mly > fail/_malformed_fdecl.res
diff fail/_malformed_fdecl.out fail/_malformed_fdecl.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  PARSER: MALFORMED FDECL TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  PARSER: MALFORMED FDECL TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat fail/_malformed_matrix_decl.test | menhir --interpret --interpret-show-cst parser.mly > fail/_malformed_matrix_decl.res
diff fail/_malformed_matrix_decl.out fail/_malformed_matrix_decl.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|  PARSER: BAD MATRIX DECL TEST PASSED  |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|  PARSER: BAD MATRIX DECL TEST FAILED  |"
  echo "-----------------------------------------"
fi

cat fail/_postfix_unop.test | menhir --interpret --interpret-show-cst parser.mly > fail/_postfix_unop.res
diff fail/_postfix_unop.out fail/_postfix_unop.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "-----------------------------------------"
  echo "|   PARSER: POSTFIX UNOP TEST PASSED    |"
  echo "-----------------------------------------"
else
  echo -e "\e[0;31m"
  echo "-----------------------------------------"
  echo "|   PARSER: POSTFIX UNOP TEST FAILED    |"
  echo "-----------------------------------------"
fi
