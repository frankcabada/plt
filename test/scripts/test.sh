cat scanner_test.txt | ./tokenize > scanner_test.res
diff scanner_test.out scanner_test.res > /dev/null
if [ $? = 0 ]; then
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST PASSED      |"
  echo "---------------------------------------"
else
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST FAILED      |"
  echo "--------------------------------------"
fi
