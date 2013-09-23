test:
	@if [ -z "$$c" ] ;then for f in `ls c*tests.scm`; do echo -n "$$f : " && csi -qnb $$f; done; else csi -qnb ./c$c-tests.scm; fi

repl:   c41-cli.scm
	csc ./c41-cli.scm -o repl
	./repl

