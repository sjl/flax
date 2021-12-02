.PHONY: test

# Test ------------------------------------------------------------------------
test:
	sbcl --noinform --load test/test.lisp --eval '(quit)'
