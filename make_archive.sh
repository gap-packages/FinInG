cd ..
COPYFILE_DISABLE=1 tar --exclude=.* -cf fining-`cat ./fining/VERSION`.tar fining/doc fining/examples fining/lib fining/init.g fining/INSTALL fining/makedoc.g fining/PackageInfo.g fining/read.g fining/DATE fining/TODO fining/tst fining/VERSION fining/README
zip -rq fining-`cat ./fining/VERSION`-win.zip  fining/doc fining/examples fining/lib fining/init.g fining/INSTALL fining/makedoc.g fining/PackageInfo.g fining/read.g fining/DATE fining/TODO fining/tst fining/VERSION fining/README
gzip -c9 fining-`cat ./fining/VERSION`.tar > fining-`cat ./fining/VERSION`.tar.gz
bzip2 -9 fining-`cat ./fining/VERSION`.tar
cd fining
