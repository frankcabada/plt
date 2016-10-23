cat scanner_test.txt | ./tokenize > scanner_test.res
diff scanner_test.out scanner_test.res > /dev/null
if [ $? = 0 ]; then
  echo "-------------------------"
  echo "|      TEST PASSED      |"
  echo "-------------------------"
else
  echo "-------------------------"
  echo "|      TEST FAILED      |"
  echo "-------------------------"
fi
