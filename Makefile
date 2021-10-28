LISP ?= sbcl

build:
	$(LISP) --non-interactive \
		--load ptql.asd \
		--eval '(ql:quickload :ptql)' \
			--eval '(in-package :ptql)' \
			--eval '(asdf:make :ptql)'
