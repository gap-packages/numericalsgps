#############################################################################
##
##  PackageInfo.g for the package `NumericalSgps'              Manuel Delgado
##                                                    Pedro A. Garcia-Sanchez

SetPackageInfo( rec(

PackageName := "NumericalSgps",
Subtitle := "A package for numerical semigroups",
Version := "1.4.0",
Date := "30/08/2024", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

##  Information about authors and maintainers.
Persons := [
 rec(
    LastName      := "Delgado",
    FirstNames    := "Manuel",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "mdelgado@fc.up.pt",
    WWWHome       := "http://www.fc.up.pt/cmup/mdelgado/",
    PostalAddress := Concatenation( [
                   "Departamento de Matemática - Faculdade de Ciências\n",
                   "Rua do Campo Alegre, 687\n",
                   "Porto\n",
                   "Portugal" ] ),
    Place         := "Porto",
    Institution   := "Faculdade de Ciências"
  ),

  rec(
    LastName      := "Garcia-Sanchez",
    FirstNames    := "Pedro A.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "pedro@ugr.es",
    WWWHome       := "http://www.ugr.es/~pedro/",
    PostalAddress := Concatenation( [
                       "Dpto. de Algebra  -  Universidad de Granada\n",
                       "Spain\n" ] ),
    Place         := "Granada",
    Institution   := "Universidad de Granada"
  ),
  rec(
    LastName      := "Morais",
    FirstNames    := "Jose",
    IsAuthor      := true,
    IsMaintainer  := false,
    PostalAddress := "No address known"

  ),
# provide such a record for each author and/or maintainer ...

rec(
  LastName      := "Cisto",
  FirstNames    := "Carmelo",
  IsAuthor      := false,
  IsMaintainer  := false,
  #WWWHome := "No address known"
),

rec(
  LastName      := "Heredia",
  FirstNames    := "Benjamín Alarcón",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome := "https://github.com/baheredia"
),


rec(
  LastName      := "García-García",
  FirstNames    := "Juan Ignacio",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome := "https://directorio.uca.es/cau/directorio.do?persona=11122"
),

rec(
  LastName      := "Gutsche",
  FirstNames    := "Sebastian",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome := "http://wwwb.math.rwth-aachen.de/~gutsche/"
),


rec(
  LastName      := "Herrera-Poyatos",
  FirstNames    := "Andrés",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://github.com/andreshp"
),

rec(
  LastName      := "Horn",
  FirstNames    := "Max",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://www.quendi.de/math"
),

rec(
  LastName      := "Martin Cruz",
  FirstNames    := "Helena",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://github.com/helenahmc"
),

rec(
  LastName      := "Maugeri",
  FirstNames    := "Nicola",
  IsAuthor      := false,
  IsMaintainer  := false,
  PostalAddress := "Università degli Studi di Catania"
),

rec(
  LastName      := "Moreno Ávila",
  FirstNames    := "Carlos Jesús",
  IsAuthor      := false,
  IsMaintainer  := false,
  PostalAddress := "No address known"
),

rec(
  LastName      := "Ojeda",
  FirstNames    := "Ignacio",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "http://matematicas.unex.es/~ojedamc"
),

rec(
  LastName      := "O'Neill",
  FirstNames    := "Chris",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://www.math.ucdavis.edu/~coneill"
),

rec(
  LastName      := "Sammartano",
  FirstNames    := "Alessio",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://sites.google.com/site/alessiosammartano"
),

rec(
  LastName      := "Sánchez-R. Navarro",
  FirstNames    := "Alfredo",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://directorio.uca.es/cau/directorio.do?persona=12969"
),

rec(
  LastName      := "Stokes",
  FirstNames    := "Klara",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "http://www.his.se/en/about-us/Facts-and-figures/staff/Klara_Stokes"
),

rec(
  LastName      := "Francesco",
  FirstNames    := "Strazzanti",
  IsAuthor      := false,
  IsMaintainer  := false,
  WWWHome       := "https://sites.google.com/site/francescostrazzanti"
),

rec(
  LastName      := "Zito",
  FirstNames    := "Giuseppe",
  IsAuthor      := false,
  IsMaintainer  := false,
  PostalAddress := "Università degli Studi di Catania"
),

rec(
  LastName      := "Angulo Rodríguez",
  FirstNames    := "Jorge",
  IsAuthor      := false,
  IsMaintainer  := false,
  PostalAddress := "Universidad de Valladolid"
)


],

Status := "accepted",
CommunicatedBy := "Leonard Soicher (QMUL)",
AcceptDate := "05/2015",


SourceRepository := rec(
  Type := "git",
  URL := "https://github.com/gap-packages/numericalsgps"
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://gap-packages.github.io/numericalsgps",
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),
ArchiveFormats := ".tar.gz .zip",



AbstractHTML :=
   "The <span class=\"pkgname\">NumericalSgps</span> package, is a package to compute with numerical semigroups.",

PackageDoc := rec(
  BookName  := "NumericalSgps",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "NumericalSgps, a GAP package for numerical semigroups",
),


Dependencies := rec(
  GAP := "4.7",
  NeededOtherPackages := [],
#  SuggestedOtherPackages := [["singular","normaliz"]],
  SuggestedOtherPackages := [],
  ExternalConditions := []

),

AvailabilityTest := ReturnTrue,
BannerString := Concatenation(
  "----------------------------------------------------------------\n",
  "Loading  NumericalSgps ", ~.Version, "\n",
#  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
#        " (", ~.Persons[1].WWWHome, ")\n",
#  "   ", ~.Persons[2].FirstNames, " ", ~.Persons[2].LastName,"\n",
#        " (", ~.Persons[2].WWWHome, ")\n",
#  "   ", ~.Persons[3].FirstNames, " ", ~.Persons[3].LastName,
#        " (", ~.Persons[3].WWWHome, ")\n",
  "For help, type: ?NumericalSgps: \n",
                   "To gain profit from other packages, please refer to chapter\n",
                   "'External Packages' in the manual, or type: ?NumSgpsUse \n",           
  "----------------------------------------------------------------\n" ),

TestFile := "tst/testall.g",

Keywords := ["Numerical Semigroups", "Affine semigroups", "Good semigroups"],

));
