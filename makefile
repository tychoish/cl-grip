project := cl-grip

test:sbcl

test-sbcl:
	sbcl --disable-debugger --eval '(asdf:test-system :$(project))' --quit
test-ecl:
	ecl --nodebug --eval '(asdf:test-system :$(project))' --eval '(quit)'

coverage:
	COVERAGE=1 rove $(project).asd
