project := cl-grip

test:
	sbcl --disable-debugger --eval '(asdf:test-system :$(project))' --quit
coverage:
	COVERAGE=1 rove $(project).asd
