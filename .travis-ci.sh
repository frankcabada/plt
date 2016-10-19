if [ "$TRAVIS_BRANCH" == "master" ]; then
	echo "WHY THE FUCK ARE YOU PUSHING ON MASTER?!"
	exit 0; 
fi