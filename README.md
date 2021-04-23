![alt text](bin/xirtamLogo.png)

(Logo Designed by Bailey Hwa)

# XIRTAM Programming Language:



Bailey Nozomu Hwa( bnh2128)

Shida Jing( sj2670) 

Andrew Peter Yevsey Gorovoy( apg2165) 

Annie Wang ( aw3168) 
Lior Attias (lra2135) 

## About

Xirtam is a language meant to work with matrices, done for Prof. Edwards's 2021 Spring PLT class

RUNNING THE DOCKER FILE:
1. cd into the main directory, where the docker file is
2. "docker build ."
3. "docker images"
4. copy the ID (should be a number)
5. (run it!) docker run -it 'pwd':
	"enter in the exact directory where this entire project is located specificaly where the docker file is located" -r:"enter in that same exact path" "the numerical ID of the image"

	Here is an example of mine:

	docker run --rm -it -v `pwd`:/home/Documents/GitHub/PLT-Project -w=/home/Documents/GitHub/PLT-Project 5aeb403d9695 

You can also name your docker image by saying:
	"docker build -t myimage ."
and then run it via:
	"docker run --rm -it -v `pwd`:/home/Documents/GitHub/PLT-Project -w=/home/Documents/GitHub/PLT-Project myimage" 
	
	as a note, 'pwd' needs to be in single quotes
