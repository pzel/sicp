MKSHELL=/bin/bash
test%:
   csi -qnb ./chapter$stem-tests.scm

all:
   echo; for f in `ls chapter*tests.scm`; do echo -n "$f : " && csi -qnb $f; done