cd ..
tar -czf fining-`cat ./fining/VERSION`.tgz fining/doc fining/examples fining/gap fining/init.g fining/INSTALL fining/make_filtergraph.g fining/make_inventory.g fining/makedoc.g fining/PackageInfo.g fining/patch fining/read.g fining/scripts fining/TODO fining/tst fining/VERSION
zip -rq fining-`cat ./fining/VERSION`.zip  fining/doc fining/examples fining/gap fining/init.g fining/INSTALL fining/make_filtergraph.g fining/make_inventory.g fining/makedoc.g fining/PackageInfo.g fining/patch fining/read.g fining/scripts fining/TODO fining/tst fining/VERSION  
cd fining