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
	ghc --make test/atlasbot_test.hs -o test/atlasbot_test

# Run the tests, after the tests have been compiled
# and linked
.PHONY: run_tests
run_tests: compile_tests
	./test/atlasbot_test

# Alias "test" to run all tests, after building a new binary
.PHONY: test
test: build run_tests

# Run the hlint linter over the source code, including
# the tests
.PHONY: lint
lint:
	-hlint src/
	-hlint test/

# Generate a hlint report
.PHONY: report_lint_status
report_lint_status:
	-hlint src/ --report
	-hlint test/ --report

# Remove the intermediate objects and the binaries from
# the test directory
.PHONY: clean_test
clean_test:
	rm -f test/*.o
	rm -f test/*.hi
	rm -f test/atlasbot_test

# Clean the bin, build, and test directories
# as well as the report produced by hlint
.PHONY: clean
clean: clean_test
	rm -f build/*.o
	rm -f build/*.hi
	rm -f bin/atlasbot
	rm -f report.html
