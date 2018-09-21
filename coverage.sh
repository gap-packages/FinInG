#!/usr/bin/env bash

gap4r9 -l "./;/opt/gap-4.9.3/" -a 500M -m 500M -q -A --cover ./pkg/fining/tmp.json ~/pkg/fining/tst/testall.g

gap4r9 -l "./;/opt/gap-4.9.3/" -a 500M -m 500M <<GAPInput
if LoadPackage("profiling") <> true then
    Print("ERROR: could not load profiling package");
    FORCE_QUIT_GAP(1);
fi;
x := ReadLineByLineProfile("./pkg/fining/tmp.json");;
OutputAnnotatedCodeCoverageFiles(x,"./pkg/fining","./pkg/fining/tmp"); 
QUIT_GAP(0);
GAPInput

