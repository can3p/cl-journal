
buildapp:
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval '(ql:quickload "cl-journal")' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

	buildapp --manifest-file quicklisp-manifest.txt \
		--load-system cl-journal \
		--output cl-journal \
		--entry cl-journal.main:main

	rm quicklisp-manifest.txt
