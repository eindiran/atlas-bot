# Makefile for Atlasbot
# Description: A small, easy to configure IRC bot
#              written in Haskell.
# Author: Elliott Indiran <eindiran@uchicago.edu>
# Version: v0.1.0

VERSION="0.1.0"

# Compile the main binary
bin/atlasbot:
	ghc --make src/atlasbot.hs -o bin/atlasbot

# Alias "build" for compiling the main binary
.PHONY: build
build: bin/atlasbot

# Run the binary after it has been compiled and linked
.PHONY: run
run: build
	./build/atlasbot

# Compile the tests
.PHONY: compile_tests
compile_tests:
	cd test/
	ghc --make atlasbot_test.hs -o atlasbot_test

# Run the tests, after both the tests and the main binary
# have been compiled and linked
.PHONY: run_tests
run_tests: build compile_tests
	cd test/
	./atlasbot_test

# Alias "test" to run all tests
.PHONY: test
test: run_tests

# Run the hlint linter over the source code, including
# the tests
.PHONY: lint
lint: build
	hlint src/
	hlint test/

# Remove the intermediate objects and the binaries from
# the test directory
.PHONY: clean_test
clean_test:
	rm test/*.o
	rm test/*.hi
	rm test/atlasbot_test

# Clean the bin, build, and test directories
.PHONY: clean
clean: clean_test
	rm build/*.o
	rm build/*.hi
	rm bin/atlasbot
