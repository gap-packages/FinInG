#!/bin/sh
rm -f ./doc/fining.{aux,log,dvi,ps,bbl,ilg,ind,idx,out,tex,pnr,blg,toc,six,brf}
rm -rf ./examples/output
rm -rf ./tst/output
cd ..
COPYFILE_DISABLE=1 tar --exclude=.* -cf fining-`cat ./fining/VERSION`.tar fining/doc fining/examples fining/lib fining/init.g fining/INSTALL fining/makedoc.g fining/PackageInfo.g fining/read.g fining/DATE fining/TODO fining/tst fining/VERSION fining/README.md
gzip -c9 fining-`cat ./fining/VERSION`.tar > fining-`cat ./fining/VERSION`.tar.gz
bzip2 -9 fining-`cat ./fining/VERSION`.tar
cd fining
