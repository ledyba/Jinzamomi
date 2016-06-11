.PHONY: build test atom serv

build:
	stack build jinzamomi

test: build
	rm -rf .test-out
	stack exec jinzamomi krkr build ext/kag3/data .test-out
	gjslint --nojsdoc --nostrict --max_line_length 10000 --disable 0110,0131,0002,0013,0300 -r .test-out

atom:
	stack exec atom .

serv:
	python -m SimpleHTTPServer 3000
