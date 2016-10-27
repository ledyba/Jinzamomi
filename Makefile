.PHONY: build test atom code serv

build:
	stack build jinzamomi

test: build
	rm -rf .test-out
	stack exec jinzamomi krkr build ext/kag3/data .test-out
	gjslint --nojsdoc --nostrict --max_line_length 10000 --disable 0110,0131,0002,0013,0300 -r .test-out

atom:
	stack exec atom .
code:
	stack exec code .

serv:
	python -m SimpleHTTPServer 3000
	echo "http://localhost:3000/runtime/test/"
