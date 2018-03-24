.PHONY: vendor test

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Test ------------------------------------------------------------------------
test:
	sbcl --noinform --load test/test.lisp --eval '(quit)'
