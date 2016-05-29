.PHONY: build test

build:
	stack build jinzamomi

test: build
	stack exec jinzamomi krkr build ext .test-out
