#############################################################################
##
#A  testall.tst        NumericalSgps package                   Manuel Delgado
##                                                    Pedro A. Garcia-Sanchez
##  (based on the cooresponding file of the 'example' package, 
##   by Alexander Konovalov) 
##
##  To create a test file, place GAP prompts, input and output exactly as
##  they must appear in the GAP session. Do not remove lines containing 
##  START_TEST and STOP_TEST statements.
##
##  The first line starts the test. START_TEST reinitializes the caches and 
##  the global random number generator, in order to be independent of the 
##  reading order of several test files. Furthermore, the assertion level 
##  is set to 2 by START_TEST and set back to the previous value in the 
##  subsequent STOP_TEST call.
##
##  The argument of STOP_TEST may be an arbitrary identifier string.
## 
gap> START_TEST("NumericalSgps package: testall.tst");

# Note that you may use comments in the test file
# and also separate parts of the test by empty lines

# First load the package without banner (the banner must be suppressed to 
# avoid reporting discrepancies in the case when the package is already 
# loaded)
gap> LoadPackage("numericalsgps",false);
true

# Check that the data are consistent  
gap> ns := NumericalSemigroup([10..30]);
<Proportionally modular numerical semigroup satisfying 30x mod 300 <= 20x >
gap> IsNumericalSemigroup(ns);
true
gap> MinimalGeneratingSystemOfNumericalSemigroup(ns);
[ 10 .. 19 ]
gap> GapsOfNumericalSemigroup(ns);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

#############################################################################
# Some more elaborated tests

gap> NumericalSemigroupsWithFrobeniusNumber(7);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup> ]
gap> List(last, s -> MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 3, 5 ], [ 4, 5, 11 ], [ 4, 5, 6 ], [ 5, 6, 8, 9 ], [ 2, 9 ], 
  [ 3, 8, 10 ], [ 5, 8, 9, 11, 12 ], [ 4, 9, 10, 11 ], [ 4, 6, 9, 11 ], 
  [ 6, 8, 9, 10, 11, 13 ], [ 8 .. 15 ] ]

gap> NumericalSemigroupsWithGenus(5);
[ <Proportionally modular numerical semigroup satisfying 11x mod 66 <= 5x >, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Modular numerical semigroup satisfying 11x mod 22 <= x > ]
gap> List(last, s -> MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 6 .. 11 ], [ 5, 7, 8, 9, 11 ], [ 5, 6, 8, 9 ], [ 5, 6, 7, 9 ], 
  [ 5, 6, 7, 8 ], [ 4, 6, 7 ], [ 4, 7, 9, 10 ], [ 4, 6, 9, 11 ], 
  [ 4, 5, 11 ], [ 3, 8, 10 ], [ 3, 7, 11 ], [ 2, 11 ] ]

gap> ns := NumericalSemigroup([10..30]);
<Proportionally modular numerical semigroup satisfying 30x mod 300 <= 20x >
gap> IsNumericalSemigroup(ns);
true
gap> MinimalGeneratingSystemOfNumericalSemigroup(ns);
[ 10 .. 19 ]
gap> GapsOfNumericalSemigroup(ns);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
gap> genus8 := NumericalSemigroupsWithGenus(8);;
gap> List(genus8, s -> Length(GapsOfNumericalSemigroup(s)));
[ 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 ]
gap> List(genus8, s -> IsIrreducibleNumericalSemigroup(s));
[ false, false, false, false, false, false, true, true, true, false, false, 
  false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, true, false, false, false, false, true, false, 
  false, false, false, false, true, false, false, false, false, false, false, 
  false, false, false, false, false, true, false, false, false, false, true, 
  false, false, false, false, true, false, true, false, true, false, false, 
  true, true ]

#############################################################################
#############################################################################
# Examples from the manual
# (These examples use at least a funtion from each file)

#Generating_Numerical_Semigroups.xml
gap> s1 := NumericalSemigroup("generators",3,5,7);
<Numerical semigroup with 3 generators>
gap> s2 := NumericalSemigroup("generators",[3,5,7]);
<Numerical semigroup with 3 generators>
gap> s1=s2;
true
gap> s := NumericalSemigroup("minimalgenerators",3,7);
<Numerical semigroup with 2 generators>
gap> s := NumericalSemigroup("modular",3,5);
<Modular numerical semigroup satisfying 3x mod 5 <= x >
gap> s1:=NumericalSemigroup("generators",2,5);       
<Modular numerical semigroup satisfying 5x mod 10 <= x >
gap> s = s1;
true
gap> s:=NumericalSemigroup(4,5,6);
<Proportionally modular numerical semigroup satisfying 6x mod 24 <= 2x >

gap> NumericalSemigroup(1);
<The numerical semigroup N>
gap> NumericalSemigroupByInterval(1/3,1/2);
<The numerical semigroup N>

gap> ModularNumericalSemigroup(3,7);
<Modular numerical semigroup satisfying 3x mod 7 <= x >

gap> ProportionallyModularNumericalSemigroup(3,7,12);
<Proportionally modular numerical semigroup satisfying 3x mod 7 <= 12x >

gap> s:=NumericalSemigroup(3,11);
<Modular numerical semigroup satisfying 22x mod 33 <= x >
gap> GapsOfNumericalSemigroup(s);
[ 1, 2, 4, 5, 7, 8, 10, 13, 16, 19 ]
gap> t:=NumericalSemigroupByGaps(last);
<Numerical semigroup>
gap> s=t;
true

gap> AperyListOfNumericalSemigroupWRTElement(s,20);;
gap> t:=NumericalSemigroupByAperyList(last);
<Numerical semigroup>
gap> s=t;
true

##Some_basic_tests.xml

gap> s:=NumericalSemigroup(3,7);
<Modular numerical semigroup satisfying 7x mod 21 <= x >
gap> AperyListOfNumericalSemigroupWRTElement(s,30);;
gap> t:=NumericalSemigroupByAperyList(last);
<Numerical semigroup>
gap> IsNumericalSemigroupByGenerators(s);
true
gap> IsNumericalSemigroupByGenerators(t);
false
gap> IsNumericalSemigroupByAperyList(s);
false
gap> IsNumericalSemigroupByAperyList(t);
true

gap> L:=[ 0, 3, 6, 9, 11, 12, 14, 15, 17, 18, 20 ];
[ 0, 3, 6, 9, 11, 12, 14, 15, 17, 18, 20 ]
gap> RepresentsSmallElementsOfNumericalSemigroup(L);
true
gap> L:=[ 6, 9, 11, 12, 14, 15, 17, 18, 20 ];
[ 6, 9, 11, 12, 14, 15, 17, 18, 20 ]
gap> RepresentsSmallElementsOfNumericalSemigroup(L);
false

gap> s:=NumericalSemigroup(3,7);
<Modular numerical semigroup satisfying 7x mod 21 <= x >
gap> L:=GapsOfNumericalSemigroup(s);
[ 1, 2, 4, 5, 8, 11 ]
gap> RepresentsGapsOfNumericalSemigroup(L);
true
#gap> L:=Set(List([1..21],i->RandomList([1..50])));
#[ 2, 6, 7, 8, 10, 12, 14, 19, 24, 28, 31, 35, 42, 50 ]
#gap> RepresentsGapsOfNumericalSemigroup(L);
#false

gap> IsAperyListOfNumericalSemigroup([0,21,7,28,14]);
true

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> T:=NumericalSemigroup(2,3);
<Modular numerical semigroup satisfying 3x mod 6 <= x >
gap> IsSubsemigroupOfNumericalSemigroup(T,S);
true
gap> IsSubsemigroupOfNumericalSemigroup(S,T);
false

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> BelongsToNumericalSemigroup(15,S);
false
gap> 15 in S;
false
gap> SmallElementsOfNumericalSemigroup(S);
[ 0, 11, 12, 13, 22, 23, 24, 25, 26, 32, 33, 34, 35, 36, 37, 38, 39, 43 ]
gap> BelongsToNumericalSemigroup(13,S);
true
gap> 13 in S;
true

#The_definitions.xml

gap> S := NumericalSemigroup("modular", 7,53);
<Modular numerical semigroup satisfying 7x mod 53 <= x >
gap> MultiplicityOfNumericalSemigroup(S);
8

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> GeneratorsOfNumericalSemigroup(S);
[ 11, 12, 13, 32, 53 ]
gap> S := NumericalSemigroup(3, 5, 53);
<Numerical semigroup with 3 generators>
gap> GeneratorsOfNumericalSemigroup(S);
[ 3, 5, 53 ]
gap> MinimalGeneratingSystemOfNumericalSemigroup(S);
[ 3, 5 ]
gap> ReducedSetOfGeneratorsOfNumericalSemigroup(NumericalSemigroup(5,7,9,10,25));
[ 5, 7, 9, 25 ]
gap> ReducedSetOfGeneratorsOfNumericalSemigroup(true,NumericalSemigroup(5,7,9,10,25,28));   
[ 5, 7, 9, 28 ]
gap> ReducedSetOfGeneratorsOfNumericalSemigroup(NumericalSemigroup(5,7,9,10,25,28),3);   
[ 5, 7, 9 ]

gap> SmallElementsOfNumericalSemigroup(NumericalSemigroup(3,5,7));
[ 0, 3, 5 ]

gap> FirstElementsOfNumericalSemigroup(2,NumericalSemigroup(3,5,7));
[ 0, 3 ]
gap> FirstElementsOfNumericalSemigroup(10,NumericalSemigroup(3,5,7));
[ 0, 3, 5, 6, 7, 8, 9, 10, 11, 12 ]

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> AperyListOfNumericalSemigroupWRTElement(S,12);
[ 0, 13, 26, 39, 52, 53, 54, 43, 32, 33, 22, 11 ]

gap>  s:=NumericalSemigroup(10,13,19,27);
<Numerical semigroup with 4 generators>
gap> AperyListOfNumericalSemigroupWRTInteger(s,11);
[ 0, 10, 13, 19, 20, 23, 26, 27, 29, 32, 33, 36, 39, 42, 45, 46, 52, 55 ]
gap> Length(last);
18
gap> AperyListOfNumericalSemigroupWRTInteger(s,10);
[ 0, 13, 19, 26, 27, 32, 38, 45, 51, 54 ]
gap> AperyListOfNumericalSemigroupWRTElement(s,10);
[ 0, 51, 32, 13, 54, 45, 26, 27, 38, 19 ]
gap> Length(last);
10

gap> s:=NumericalSemigroup(3,7);;
gap> AperyListOfNumericalSemigroupWRTElement(s,10);
[ 0, 21, 12, 3, 14, 15, 6, 7, 18, 9 ]
gap> AperyListOfNumericalSemigroupAsGraph(last);
[ ,, [ 3, 6, 9, 12, 15, 18, 21 ],,, [ 6, 9, 12, 15, 18, 21 ], [ 7, 14, 21 ],, 
  [ 9, 12, 15, 18, 21 ],,, [ 12, 15, 18, 21 ],, [ 14, 21 ], [ 15, 18, 21 ],,, 
  [ 18, 21 ],,, [ 21 ] ]

##Frobenius_Number.xml

gap> FrobeniusNumberOfNumericalSemigroup(NumericalSemigroup(3,5,7));
4

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> PseudoFrobeniusOfNumericalSemigroup(S);
[ 21, 40, 41, 42 ]

##Gaps.xml

gap> GapsOfNumericalSemigroup(NumericalSemigroup(3,5,7));
[ 1, 2, 4 ]

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> FundamentalGapsOfNumericalSemigroup(S);
[ 16, 17, 18, 19, 27, 28, 29, 30, 31, 40, 41, 42 ]
gap> GapsOfNumericalSemigroup(S);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 27, 28, 29,
  30, 31, 40, 41, 42 ]

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> SpecialGapsOfNumericalSemigroup(S);
[ 40, 41, 42 ]

##Presentations_of_Numerical_Semigroups.xml

gap> s:=NumericalSemigroup(3,5,7);
<Numerical semigroup with 3 generators>
gap> MinimalPresentationOfNumericalSemigroup(s);
[ [ [ 0, 2, 0 ], [ 1, 0, 1 ] ], [ [ 3, 1, 0 ], [ 0, 0, 2 ] ], 
  [ [ 4, 0, 0 ], [ 0, 1, 1 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> GraphAssociatedToElementInNumericalSemigroup(10,s);
[ [ 3, 5, 7 ], [ [ 3, 7 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> BettiElementsOfNumericalSemigroup(s);
[ 10, 12, 14 ]

##Adding_and_removing_elements_of_a_numerical_semigroup.xml

gap> s:=NumericalSemigroup(3,5,7);
<Numerical semigroup with 3 generators>
gap> RemoveMinimalGeneratorFromNumericalSemigroup(7,s);
<Numerical semigroup with 3 generators>
gap> MinimalGeneratingSystemOfNumericalSemigroup(last);
[ 3, 5 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> s2:=RemoveMinimalGeneratorFromNumericalSemigroup(5,s);
<Numerical semigroup with 3 generators>
gap> s3:=AddSpecialGapOfNumericalSemigroup(5,s2);
<Numerical semigroup>
gap> SmallElementsOfNumericalSemigroup(s) =
> SmallElementsOfNumericalSemigroup(s3);
true                
gap> s=s3;
true

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> T := NumericalSemigroup(2,17);
<Modular numerical semigroup satisfying 17x mod 34 <= x >
gap> SmallElementsOfNumericalSemigroup(S);
[ 0, 11, 12, 13, 22, 23, 24, 25, 26, 32, 33, 34, 35, 36, 37, 38, 39, 43 ]
gap> SmallElementsOfNumericalSemigroup(T);
[ 0, 2, 4, 6, 8, 10, 12, 14, 16 ]
gap> IntersectionOfNumericalSemigroups(S,T);
<Numerical semigroup>
gap> SmallElementsOfNumericalSemigroup(last);
[ 0, 12, 22, 23, 24, 25, 26, 32, 33, 34, 35, 36, 37, 38, 39, 43 ]

gap> s:=NumericalSemigroup(3,29);
<Modular numerical semigroup satisfying 58x mod 87 <= x >
gap> SmallElementsOfNumericalSemigroup(s);
[ 0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 29, 30, 32, 33, 35, 36, 38, 39, 41, 42, 
  44, 45, 47, 48, 50, 51, 53, 54, 56 ]
gap> t:=QuotientOfNumericalSemigroup(s,7);
<Numerical semigroup>
gap> SmallElementsOfNumericalSemigroup(t);
[ 0, 3, 5, 6, 8 ]
gap> u := s / 7;
<Numerical semigroup>
gap> SmallElementsOfNumericalSemigroup(u);
[ 0, 3, 5, 6, 8 ]

##Constructing_sets_of_numerical_semigroups.xml

gap> OverSemigroupsNumericalSemigroup(NumericalSemigroup(3,5,7));
[ <The numerical semigroup N>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup with 3 generators> ]
gap> List(last,s->MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 1 ], [ 2, 3 ], [ 3 .. 5 ], [ 3, 5, 7 ] ]

gap> Length(NumericalSemigroupsWithFrobeniusNumber(20));
900

gap> NumericalSemigroupsWithGenus(5);
[ <Proportionally modular numerical semigroup satisfying 11x mod 66 <= 5x >, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 4 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Modular numerical semigroup satisfying 11x mod 22 <= x > ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 6 .. 11 ], [ 5, 7, 8, 9, 11 ], [ 5, 6, 8, 9 ], [ 5, 6, 7, 9 ], 
  [ 5, 6, 7, 8 ], [ 4, 6, 7 ], [ 4, 7, 9, 10 ], [ 4, 6, 9, 11 ], 
  [ 4, 5, 11 ], [ 3, 8, 10 ], [ 3, 7, 11 ], [ 2, 11 ] ]

##Irreducible_numerical_semigroups.xml

gap> IsIrreducibleNumericalSemigroup(NumericalSemigroup(4,6,9));
true
gap> IsIrreducibleNumericalSemigroup(NumericalSemigroup(4,6,7,9));
false

gap> IsSymmetricNumericalSemigroup(NumericalSemigroup(10,23));      
true            
gap> IsSymmetricNumericalSemigroup(NumericalSemigroup(10,11,23));
false

gap> IsPseudoSymmetricNumericalSemigroup(NumericalSemigroup(6,7,8,9,11));
true
gap> IsPseudoSymmetricNumericalSemigroup(NumericalSemigroup(4,6,9));
false

gap> FrobeniusNumber(AnIrreducibleNumericalSemigroupWithFrobeniusNumber(28));
28

gap> Length(IrreducibleNumericalSemigroupsWithFrobeniusNumber(39));
227

gap> DecomposeIntoIrreducibles(NumericalSemigroup(5,6,8));
[ <Numerical semigroup with 3 generators>, 
  <Numerical semigroup with 4 generators> ]

##Complete_Intersections.xml

gap> s := NumericalSemigroup( 10, 15, 16 );                   
<Numerical semigroup with 3 generators>
gap> AsGluingOfNumericalSemigroups(s);     
[ [ [ 10, 15 ], [ 16 ] ], [ [ 10, 16 ], [ 15 ] ] ]
gap> s := NumericalSemigroup( 18, 24, 34, 46, 51, 61, 74, 8 );
<Numerical semigroup with 8 generators>
gap> AsGluingOfNumericalSemigroups(s);                        
[  ]

gap> s := NumericalSemigroup( 10, 15, 16 );       
<Numerical semigroup with 3 generators>
gap> IsACompleteIntersectionNumericalSemigroup(s);
true
gap> s := NumericalSemigroup( 18, 24, 34, 46, 51, 61, 74, 8 );
<Numerical semigroup with 8 generators>
gap> IsACompleteIntersectionNumericalSemigroup(s);            
false

gap> Length(CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber(57));
34

gap> IsFreeNumericalSemigroup(NumericalSemigroup(10,15,16));
true
gap> IsFreeNumericalSemigroup(NumericalSemigroup(3,5,7));
false

gap> Length(FreeNumericalSemigroupsWithFrobeniusNumber(57));
33
gap> IsTelescopicNumericalSemigroup(NumericalSemigroup(4,11,14));
false
gap> IsFreeNumericalSemigroup(NumericalSemigroup(4,11,14));
true

gap> Length(TelescopicNumericalSemigroupsWithFrobeniusNumber(57));
20

gap> Length(NumericalSemigroupsAssociatedIrreduciblePlanarCurveSingularityWithFrobeniusNumber(57));
7

##Almost_symmetric.xml

gap> AlmostSymmetricNumericalSemigroupsFromIrreducible(NumericalSemigroup(5,8,9,11));
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup> ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 5, 8, 9, 11 ], [ 5, 8, 11, 14, 17 ], [ 5, 9, 11, 13, 17 ] ]

gap> IsAlmostSymmetricNumericalSemigroup(NumericalSemigroup(5,8,11,14,17));
true

gap> Length(AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber(12));
15
gap> Length(IrreducibleNumericalSemigroupsWithFrobeniusNumber(12));
2

##Ideals_of_numerical_semigroups.xml

gap> IdealOfNumericalSemigroup([3,5],NumericalSemigroup(9,11));
<Ideal of numerical semigroup>
gap> [3,5]+NumericalSemigroup(9,11);
<Ideal of numerical semigroup>
gap> last=last2;
true
gap> 3+NumericalSemigroup(5,9);
<Ideal of numerical semigroup>

gap> I:=[1..7]+NumericalSemigroup(7,19);;
gap> IsIdealOfNumericalSemigroup(I);
true
gap> IsIdealOfNumericalSemigroup(2);
false

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(I);
[ 3 ]

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> GeneratorsOfIdealOfNumericalSemigroup(I);
[ 3, 5, 9 ]
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(I);
[ 3 ]
gap> GeneratorsOfIdealOfNumericalSemigroup(I);
[ 3 ]
gap> GeneratorsOfIdealOfNumericalSemigroupNC(I);
[ 3, 5, 9 ]

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> AmbientNumericalSemigroupOfIdeal(I);
<Modular numerical semigroup satisfying 11x mod 22 <= x >

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> SmallElementsOfIdealOfNumericalSemigroup(I);
[ 3, 5, 7, 9, 11, 13 ]
gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> SmallElementsOfIdealOfNumericalSemigroup(J);
[ 2, 4, 6, 8, 10 ]

gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> BelongsToIdealOfNumericalSemigroup(9,J);
false
gap> 9 in J;
false
gap> BelongsToIdealOfNumericalSemigroup(10,J);
true
gap> 10 in J;
true

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> I+J;
<Ideal of numerical semigroup>
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(last);
[ 5, 14 ]
gap> SumIdealsOfNumericalSemigroup(I,J);
<Ideal of numerical semigroup>
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(last);
[ 5, 14 ]

gap> I:=[0,1]+NumericalSemigroup(3,5,7);;
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(2*I);
[ 0, 1, 2 ]

gap> S:=NumericalSemigroup(14, 15, 20, 21, 25);;
gap> I:=[0,1]+S;;
gap> II:=S-I;;
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(I);
[ 0, 1 ]
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(II);
[ 14, 20 ]
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(I+II);
[ 14, 15, 20, 21 ]

gap> S:=NumericalSemigroup(14, 15, 20, 21, 25);;
gap> I:=[0,1]+S;
<Ideal of numerical semigroup>
gap> 2*I-2*I;
<Ideal of numerical semigroup>
gap> I-I;
<Ideal of numerical semigroup>
gap> DifferenceOfIdealsOfNumericalSemigroup(last2,last);
[ 26, 27, 37, 38 ]

gap> s:=NumericalSemigroup(13,23);;
gap> l:=List([1..6], _ -> Random([8..34]));;
#[ 22, 29, 34, 25, 10, 12 ]
gap> I:=IdealOfNumericalSemigroup(l, s);;
gap> It:=TranslationOfIdealOfNumericalSemigroup(7,I);
<Ideal of numerical semigroup>
gap> It2:=7+I;
<Ideal of numerical semigroup>
gap> It2=It;
true

gap> I:=[6,9,11]+NumericalSemigroup(6,9,11);;
gap> List([1..7],n->HilbertFunctionOfIdealOfNumericalSemigroup(n,I));
[ 3, 5, 6, 6, 6, 6, 6 ]

gap> I:=[0,2]+NumericalSemigroup(6,9,11);;
gap> BlowUpIdealOfNumericalSemigroup(I);;
gap> SmallElementsOfIdealOfNumericalSemigroup(last);
[ 0, 2, 4, 6, 8 ]

gap> I:=[0,2]+NumericalSemigroup(6,9,11);;
gap> ReductionNumberIdealNumericalSemigroup(I);
2

gap> MaximalIdealOfNumericalSemigroup(NumericalSemigroup(3,7));
<Ideal of numerical semigroup>

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> BlowUpOfNumericalSemigroup(s);
<Numerical semigroup with 10 generators>
gap> SmallElementsOfNumericalSemigroup(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]
gap> m:=MaximalIdealOfNumericalSemigroup(s);
<Ideal of numerical semigroup>
gap> BlowUpIdealOfNumericalSemigroup(m);
<Ideal of numerical semigroup>
gap> SmallElementsOfIdealOfNumericalSemigroup(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> bu:=BlowUpOfNumericalSemigroup(s);;
gap> ap:=AperyListOfNumericalSemigroupWRTElement(s,30);;
gap> apbu:=AperyListOfNumericalSemigroupWRTElement(bu,30);;
gap> (ap-apbu)/30;
[ 0, 4, 4, 3, 2, 1, 3, 4, 4, 3, 2, 3, 1, 4, 4, 3, 3, 1, 4, 4, 4, 3, 2, 4, 2,
  5, 4, 3, 3, 2 ]
gap> MicroInvariantsOfNumericalSemigroup(s)=last;
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsGradedAssociatedRingNumericalSemigroupCM(s);
false
gap> MicroInvariantsOfNumericalSemigroup(s);
[ 0, 4, 4, 3, 2, 1, 3, 4, 4, 3, 2, 3, 1, 4, 4, 3, 3, 1, 4, 4, 4, 3, 2, 4, 2,
  5, 4, 3, 3, 2 ]
gap> List(AperyListOfNumericalSemigroupWRTElement(s,30),
> w->MaximumDegreeOfElementWRTNumericalSemigroup (w,s));
[ 0, 1, 4, 1, 2, 1, 3, 1, 4, 3, 2, 3, 1, 1, 4, 3, 3, 1, 4, 1, 4, 3, 2, 4, 2,
  5, 4, 3, 1, 2 ]
gap> last=last2;
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsGradedAssociatedRingNumericalSemigroupCM(s);
true
gap> MicroInvariantsOfNumericalSemigroup(s);
[ 0, 2, 1, 1 ]
gap> List(AperyListOfNumericalSemigroupWRTElement(s,4),
> w->MaximumDegreeOfElementWRTNumericalSemigroup(w,s));
[ 0, 2, 1, 1 ]

gap> s:=NumericalSemigroup(4,6,11);;
gap> m:=MaximalIdealOfNumericalSemigroup(s);;
gap> c:=CanonicalIdealOfNumericalSemigroup(s);
<Ideal of numerical semigroup>
gap> (m-c)-c=m;
true
gap> id:=3+s;
<Ideal of numerical semigroup>
gap> (id-c)-c=id;
true

gap> i:=IdealOfNumericalSemigroup([75,89],s);;
gap> j:=IdealOfNumericalSemigroup([115,289],s);;
gap> IntersectionIdealsOfNumericalSemigroup(i,j);
<Ideal of numerical semigroup>

gap> IsMonomialNumericalSemigroup(NumericalSemigroup(4,6,7));
true
gap> IsMonomialNumericalSemigroup(NumericalSemigroup(4,6,11));
false

gap> s:=NumericalSemigroup(10,11,13);;
gap> i:=[12,14]+s;;
gap> AperyListOfIdealOfNumericalSemigroupWRTElement(i,10);
[ 40, 51, 12, 23, 14, 25, 36, 27, 38, 49 ]

gap> s:=NumericalSemigroup(10,11,13);;
gap> AperyTableOfNumericalSemigroup(s);
[ [ 0, 11, 22, 13, 24, 35, 26, 37, 48, 39 ], 
  [ 10, 11, 22, 13, 24, 35, 26, 37, 48, 39 ], 
  [ 20, 21, 22, 23, 24, 35, 26, 37, 48, 39 ], 
  [ 30, 31, 32, 33, 34, 35, 36, 37, 48, 39 ], 
  [ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49 ] ]


##Numerical_semigroups_with_maximal_embedding_dimension.xml

gap> IsMEDNumericalSemigroup(NumericalSemigroup(3,5,7)); 
true 
gap> IsMEDNumericalSemigroup(NumericalSemigroup(3,5)); 
false

gap> MEDNumericalSemigroupClosure(NumericalSemigroup(3,5)); 
<Numerical semigroup> 
gap> MinimalGeneratingSystemOfNumericalSemigroup(last); 
[ 3, 5, 7 ]

gap> MinimalMEDGeneratingSystemOfMEDNumericalSemigroup( 
> NumericalSemigroup(3,5,7)); 
[ 3, 5 ]

##Numerical_semigroups_with_the_Arf_property_and_Arf_closures.xml

gap>  IsArfNumericalSemigroup(NumericalSemigroup(3,5,7)); 
true 
gap>  IsArfNumericalSemigroup(NumericalSemigroup(3,7,11)); 
false 
gap> IsMEDNumericalSemigroup(NumericalSemigroup(3,7,11)); 
true

gap> ArfNumericalSemigroupsWithFrobeniusNumber(10);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup> ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 7, 9, 11, 12, 13, 15, 17 ], [ 3, 11, 13 ], [ 6, 9, 11, 13, 14, 16 ], 
  [ 9, 11, 12, 13, 14, 15, 16, 17, 19 ], [ 4, 11, 13, 14 ], 
  [ 8, 11, 12, 13, 14, 15, 17, 18 ], [ 7, 11, 12, 13, 15, 16, 17 ], 
  [ 6, 11, 13, 14, 15, 16 ], [ 11 .. 21 ] ]

gap> MinimalArfGeneratingSystemOfArfNumericalSemigroup( 
> NumericalSemigroup(3,7,8)); 
[ 3, 7 ]

##Saturated_Numerical_semigroups.xml

gap> IsSaturatedNumericalSemigroup(NumericalSemigroup(4,6,9,11));
true
gap> IsSaturatedNumericalSemigroup(NumericalSemigroup(8, 9, 12, 13, 15, 19 ));
false

gap> SaturatedNumericalSemigroupClosure(NumericalSemigroup(8, 9, 12, 13, 15));
<Numerical semigroup>
gap> MinimalGeneratingSystemOfNumericalSemigroup(last);
[ 8 .. 15 ]

gap> SaturatedNumericalSemigroupsWithFrobeniusNumber(10);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup> ]
gap>  List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 3, 11, 13 ], [ 4, 11, 13, 14 ], [ 6, 9, 11, 13, 14, 16 ], 
  [ 6, 11, 13, 14, 15, 16 ], [ 7, 11, 12, 13, 15, 16, 17 ], 
  [ 8, 11, 12, 13, 14, 15, 17, 18 ], [ 9, 11, 12, 13, 14, 15, 16, 17, 19 ], 
  [ 11 .. 21 ] ]

##catenary-tame.xml

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], 
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> FactorizationsElementWRTNumericalSemigroup(1100,s);
[ [ 0, 8, 1, 0, 0, 0 ], [ 0, 0, 0, 2, 2, 0 ], [ 5, 1, 1, 0, 0, 1 ], 
  [ 0, 2, 3, 0, 0, 1 ] ]

gap> LengthsOfFactorizationsIntegerWRTList(100,[11,13,15,19]);
[ 6, 8 ]

gap> s:=NumericalSemigroup(10,11,19,23);;
gap> BettiElementsOfNumericalSemigroup(s);
[ 30, 33, 42, 57, 69 ]
gap> FactorizationsElementWRTNumericalSemigroup(69,s);
[ [ 5, 0, 1, 0 ], [ 2, 1, 2, 0 ], [ 0, 0, 0, 3 ] ]
gap> RClassesOfSetOfFactorizations(last);
[ [ [ 2, 1, 2, 0 ], [ 5, 0, 1, 0 ] ], [ [ 0, 0, 0, 3 ] ] ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> LengthsOfFactorizationsElementWRTNumericalSemigroup(1100,s);
[ 4, 6, 8, 9 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> ElasticityOfFactorizationsElementWRTNumericalSemigroup(1100,s);
9/4

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> ElasticityOfNumericalSemigroup(s);
286/101

gap> LengthsOfFactorizationsIntegerWRTList(100,[11,13,15,19]);
[ 6, 8 ]
gap> DeltaSetOfSetOfIntegers(last);
[ 2 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> DeltaSetOfFactorizationsElementWRTNumericalSemigroup(1100,s);
[ 1, 2 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> MaximumDegreeOfElementWRTNumericalSemigroup(1100,s);
9

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], 
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> CatenaryDegreeOfSetOfFactorizations(last);
5

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> CatenaryDegreeOfNumericalSemigroup(s);
8

gap> CatenaryDegreeOfElementInNumericalSemigroup(157,NumericalSemigroup(13,18));
0
gap> CatenaryDegreeOfElementInNumericalSemigroup(1157,NumericalSemigroup(13,18));
18

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], 
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> TameDegreeOfSetOfFactorizations(last);
4

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> TameDegreeOfNumericalSemigroup(s);
14

gap> s:=NumericalSemigroup(10,11,13);
<Numerical semigroup with 3 generators>
gap> TameDegreeOfElementInNumericalSemigroup(100,s); 
5

gap> s:=NumericalSemigroup(10,11,13);        
<Numerical semigroup with 3 generators>
gap> OmegaPrimalityOfElementInNumericalSemigroup(100,s);
13

gap> s:=NumericalSemigroup(10,11,13);        
<Numerical semigroup with 3 generators>
gap> OmegaPrimalityOfNumericalSemigroup(s);
5

##generalstuff.xml

gap> BezoutSequence(4/5,53/27);
[ 4/5, 1, 3/2, 5/3, 7/4, 9/5, 11/6, 13/7, 15/8, 17/9, 19/10, 21/11, 23/12,
  25/13, 27/14, 29/15, 31/16, 33/17, 35/18, 37/19, 39/20, 41/21, 43/22,
  45/23, 47/24, 49/25, 51/26, 53/27 ]

gap> IsBezoutSequence([ 4/5, 1, 3/2, 5/3, 7/4, 9/5, 11/6]);
true
gap> IsBezoutSequence([ 4/5, 1, 3/2, 5/3, 7/4, 9/5, 11/3]);
Take the 6 and the 7 elements of the sequence
false

gap> CeilingOfRational(3/5);
1

gap> RepresentsPeriodicSubAdditiveFunction([1,2,3,4,0]);
true

gap> IsListOfIntegersNS([1,-1,0]);
true
gap> IsListOfIntegersNS(2);
false
gap> IsListOfIntegersNS([[2],3]);
false
gap> IsListOfIntegersNS([]);
false

##random.xml
##contributions.xml

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsGradedAssociatedRingNumericalSemigroupBuchsbaum(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsMpureNumericalSemigroup(s);                                       
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsMpureNumericalSemigroup(s); 
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsPureNumericalSemigroup(s);                                       
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsPureNumericalSemigroup(s); 
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsGradedAssociatedRingNumericalSemigroupGorenstein(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsGradedAssociatedRingNumericalSemigroupGorenstein(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsGradedAssociatedRingNumericalSemigroupCI(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsGradedAssociatedRingNumericalSemigroupCI(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsAperySetGammaRectangular(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsAperySetGammaRectangular(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsAperySetBetaRectangular(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsAperySetBetaRectangular(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsAperySetAlphaRectangular(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsAperySetAlphaRectangular(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> TypeSequenceOfNumericalSemigroup(s);
[ 13, 3, 4, 4, 7, 3, 3, 3, 2, 2, 2, 3, 3, 2, 4, 3, 2, 1, 3, 2, 1, 1, 2, 2, 1, 
  1, 1, 2, 2, 1, 3, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 
  1, 1, 1 ]
gap> s:=NumericalSemigroup(4,6,11);;
gap> TypeSequenceOfNumericalSemigroup(s);
[ 1, 1, 1, 1, 1, 1, 1 ]

gap> STOP_TEST( "testall.tst", 10000 );
## The first argument of STOP_TEST should be the name of the test file.
## The number is a proportionality factor that is used to output a 
## "GAPstone" speed ranking after the file has been completely processed.
## For the files provided with the distribution this scaling is roughly 
## equalized to yield the same numbers as produced by the test file 
## tst/combinat.tst. For package tests, you may leave it unchnaged. 

#############################################################################
##
