#############################################################################
##  
##  PackageInfo.g for the package `FinInG'                 
##                                                               John Bamberg
##                                                               Anton Betten
##                                                              Philippe Cara
##                                                               Jan De Beule
##                                                             Michel Lavrauw
##                                                            Max Neunhoeffer
##  

SetPackageInfo( rec(

PackageName := "FinInG",
Subtitle := "Finite Incidence Geometry",
Version := "1.5.6",
Date := "27/07/2023", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

IssueTrackerURL := "https://github.com/gap-packages/FinInG/issues",
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/FinInG",
),

# TODO: change PackageWWWHome back to http://www.fining.org
# (or better, https://www.fining.org) once it has the new
# version
PackageWWWHome  := "https://gap-packages.github.io/FinInG",
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", LowercaseString(~.PackageName), "-", ~.Version ),

ArchiveFormats := ".tar.gz .tar.bz2",

Persons := [
  rec( 
    LastName      := "Bamberg",
    FirstNames    := "John",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "bamberg@maths.uwa.edu.au",
    WWWHome       := "http://school.maths.uwa.edu.au/~bamberg/",
    PostalAddress := Concatenation( [
                       "John Bamberg\n",
                       "School of Mathematics and Statistics\n",
                       "The University of Western Australia\n",
                       "35 Stirling Highway\n",
                       "CrawleyY WA 6009, Perth\n",
                       "Australia" ] ),
    Place         := "Perth",
    Institution   := "The University of Western Australia",
  ),
  rec( 
    LastName      := "Betten",
    FirstNames    := "Anton",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "betten@math.colostate.edu",
    WWWHome       := "http://www.math.colostate.edu/~betten",
    PostalAddress := Concatenation( [
                       "Anton Betten\n",
                       "Department of Mathematics\n",
                       "Colorado State University\n",
                       "Fort Collins, CO 80523\n",
                       "USA" ] ),
    Place         := "Colorado",
    Institution   := "Colorado State University",
  ),
  rec(
    LastName      := "Cara",
    FirstNames    := "Philippe",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "pcara@vub.ac.be",
    WWWHome       := "http://homepages.vub.ac.be/~pcara",
    PostalAddress := Concatenation( [
                       "Philippe Cara\n",
                       "Department of Mathematics\n",
                       "Vrije Universiteit Brussel\n",
                       "Pleinlaan 2\n",
                       "B-1050 Brussel\n",
                       "Belgium" ] ),
    Place         := "Brussel",
    Institution   := "Vrije Universiteit Brussel",
  ),
  rec(
    LastName      := "De Beule",
    FirstNames    := "Jan",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jan@debeule.eu",
    WWWHome       := "http://www.debeule.eu",
    PostalAddress := Concatenation( [
    "Jan De Beule\n",
    "Department of Mathematics\n",
    "Vrije Universiteit Brussel\n",
    "Pleinlaan 2\n",
    "B-1050 Brussel\n",
    "Belgium" ] ),
    Place         := "Brussels",
    Institution   := "Vrije Universiteit Brussel",
  ),
  rec( 
    LastName      := "Lavrauw",
    FirstNames    := "Michel",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "michel.lavrauw@unipd.it",
    WWWHome       := "http://people.sabanciuniv.edu/~mlavrauw/",
    PostalAddress := Concatenation( [
                       "Michel Lavrauw\n",
                       "Faculty of Engineering and Natural Sciences\n",
                       "Sabanci Universitesi\n",
                       "Istanbul\n",
                       "Turkey\n" ] ),
    Place         := "Istanbul, and Vicenza",
    Institution   := "Sabancı Üniversitesi, and Università degli Studi di Padova",
  ),
  rec( 
    LastName      := "Neunhoeffer",
    FirstNames    := "Max",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "neunhoef@mcs.st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~neunhoef/",
    PostalAddress := Concatenation( [
                       "Max Neunhoeffer\n",
                       "School of Mathematics and Statistics\n",
                       "University of St Andrews\n",
                       "Mathematical Institute\n",
                       "North Haugh\n",
                       "St Andrews, Fife KY16 9SS\n",
                       "Scotland, UK" ] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  ),
  rec(
    LastName      := "Horn",
    FirstNames    := "Max",
    IsAuthor      := false,
    IsMaintainer  := true,
    Email         := "mhorn@rptu.de",
    WWWHome       := "https://www.quendi.de/math",
    PostalAddress := Concatenation(
                       "Fachbereich Mathematik\n",
                       "RPTU Kaiserslautern-Landau\n",
                       "Gottlieb-Daimler-Straße 48\n",
                       "67663 Kaiserslautern\n",
                       "Germany" ),
    Place         := "Kaiserslautern, Germany",
    Institution   := "RPTU Kaiserslautern-Landau",
  ),
],

Status := "accepted",
CommunicatedBy := "Olexandr Konovalov (St Andrews)",
AcceptDate := "11/2017",

AbstractHTML := "<span class=\"pkgname\">FinInG</span> is a package for computation\
 in Finite Incidence Geometry. It provides users with the basic tools to work in \
 various areas of finite geometry from the realms of projective spaces to the flat \
 lands of generalised polygons. The algebraic power of GAP is employed, particularly \
 in its facility with matrix and permutation groups.",

PackageDoc := rec(
  # use same as in GAP            
  BookName  := "FinInG",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  # the path to the .six file used by GAP's help system
  SixFile   := "doc/manual.six",
  # a longer title of the book, this together with the book name should
  # fit on a single text line (appears with the '?books' command in GAP)
  # LongTitle := "Elementary Divisors of Integer Matrices",
  LongTitle := "FinInG - Finite Incidence Geometry",
),


##  Are there restrictions on the operating system for this package? Or does
##  the package need other packages to be available?
Dependencies := rec(
  GAP := ">=4.10",
  NeededOtherPackages := [
          ["cvec", ">=2.7.6"],
          ["Forms", ">=1.2.5"],
          ["GAPDoc", ">= 1.6.3"],
          ["GenSS", ">=1.6.6"],
          ["GRAPE", ">=4.8.2"],
          ["Orb", ">=4.8.3"],
      ],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation(
    "---------------------------------------------------------------------\n",
    "             ______________       ________      _________            \n",
    "             ___  ____/__(_)__________  _/________  ____/            \n",
    "             __  /_   __  /__  __ \__  / __  __ \  / __              \n",
    "             _  __/   _  / _  / / /_/ /  _  / / / /_/ /              \n",
    "             /_/      /_/  /_/ /_//___/  /_/ /_/\____/               \n",
    "---------------------------------------------------------------------\n",
    "Loading  FinInG ", ~.Version, " (Finite Incidence Geometry) \n",
    "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
        " (", ~.Persons[1].WWWHome, ")\n",
    "   ", ~.Persons[2].FirstNames, " ", ~.Persons[2].LastName,
        " (", ~.Persons[2].WWWHome, ")\n",
    "   ", ~.Persons[3].FirstNames, " ", ~.Persons[3].LastName,
        " (", ~.Persons[3].WWWHome, ")\n",
    "   ", ~.Persons[4].FirstNames, " ", ~.Persons[4].LastName,
        " (", ~.Persons[4].WWWHome, ")\n",
    "   ", ~.Persons[5].FirstNames, " ", ~.Persons[5].LastName,
        " (", ~.Persons[5].WWWHome, ")\n",
    "   ", ~.Persons[6].FirstNames, " ", ~.Persons[6].LastName,
        " (", ~.Persons[6].WWWHome, ")\n",
    "For help, type: ?FinInG \n",
    "---------------------------------------------------------------------\n" ),

TestFile := "tst/testall.g",

Keywords := ["FinInG", "finite", "geometry"],


AutoDoc := rec(
    entities := rec(
        VERSION := ~.Version,
        DATE := ~.Date,
        YEAR := ~.Date{[7..10]},
    ),
    MainPage := false,
    TitlePage := false,
),

));
