branch=$(git branch | grep '*')

if [ $branch == "* master" ]; then
	echo "NEVER PUSH FROM MASTER!!!"
	exit 0;
