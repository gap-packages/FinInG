LoadPackage("fining");

exclude := [];
#if not IsBound(DescribesInvariantQuadraticForm) then
  # classic.tst should only run in 4.12
#  Add( exclude, "adv/classic.tst" );
#fi;

TestDirectory(DirectoriesPackageLibrary("fining", "tst"),
    rec(
      exitGAP := true,
      exclude := exclude,
      #rewriteToFile := true,  # enable this line to update tests
    ));
FORCE_QUIT_GAP(1);
