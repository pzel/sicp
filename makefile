test:
	@if [ -z "$$c" ] ;then set -e; for f in `ls c*tests.scm`; do echo -n "$$f : " && csi -qnb $$f; done; else csi -qnb ./c$c-tests.scm; fi

repl:   c41-cli.scm c41.scm
	csc ./c41-cli.scm -o repl
	./repl

repl-lazy:   c42-cli.scm c42.scm
	csc ./c42-cli.scm -o repl-lazy
	./repl-lazy

