MKSHELL=/bin/bash
test%:Q:
   csi -qnb ./chapter-$stem-tests.scm

all:Q:
   echo; for f in `ls chapter*tests.scm`; do echo -n "$f : " && csi -qnb $f; done