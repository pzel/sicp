
test:
	csi -qnb ./c$c-tests.scm

all:
	for f in `ls c*tests.scm`; do echo -n "$$f : " && csi -qnb $$f; done
