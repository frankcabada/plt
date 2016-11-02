cat base_scanner_test.txt | ./tokenize > base_scanner_test.res
diff base_scanner_test.out base_scanner_test.res > /dev/null
if [ $? = 0 ]; then
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST PASSED      |"
  echo "---------------------------------------"
else
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST FAILED      |"
  echo "--------------------------------------"
fi
