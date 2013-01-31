MKSHELL=/bin/bash
test%:Q:
   csi -qnb ./c$stem-tests.scm

all:Q:
  for f in `ls c*tests.scm`; do echo -n "$f : " && csi -qnb $f; done