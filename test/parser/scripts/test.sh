cat base_scanner_test.txt | ./tokenize > base_scanner_test.res
diff base_scanner_test.out base_scanner_test.res > /dev/null
if [ $? = 0 ]; then
  echo -e "\e[0;32m"
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST PASSED      |"
  echo "---------------------------------------"
else
  echo -e "\e[0;31m"
  echo "---------------------------------------"
  echo "|      FIRST SCANNER TEST FAILED      |"
  echo "---------------------------------------"
fi
