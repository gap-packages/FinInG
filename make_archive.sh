cd ..
tar -czf fining-`cat ./fining/VERSION`.tar.gz fining/doc fining/examples fining/lib fining/init.g fining/INSTALL fining/make_filtergraph.g fining/make_inventory.g fining/makedoc.g fining/PackageInfo.g fining/patch fining/read.g fining/scripts fining/TODO fining/tst fining/VERSION fining/README
zip -rq fining-`cat ./fining/VERSION`.zip  fining/doc fining/examples fining/lib fining/init.g fining/INSTALL fining/make_filtergraph.g fining/make_inventory.g fining/makedoc.g fining/PackageInfo.g fining/patch fining/read.g fining/scripts fining/TODO fining/tst fining/VERSION fining/README
cd fining
