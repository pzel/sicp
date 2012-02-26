MKSHELL=/bin/bash
test%:
   csi -qnb ./chapter$stem.scm

all:
   echo; for f in `ls chapter*`; do echo -n "$f : " && csi -qnb $f; done