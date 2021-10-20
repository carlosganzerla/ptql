LISP ?= sbcl

build:
	$(LISP) --load ptql.asd \
		--eval '(ql:quickload :ptql)' \
			--eval '(in-package :ptql)' \
			--eval '(asdf:make :ptql)' \
			--eval '(quit)'
