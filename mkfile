MKSHELL=/bin/sh

t([0-9]+):VR:
   csi -qnb ./c$stem1-tests.scm

all:Q:
  for f in `ls c*tests.scm`; do echo -n "$f : " && csi -qnb $f; done
