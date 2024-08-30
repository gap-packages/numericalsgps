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

## Set info level to 0 as suggested by Alexander Konovalov

gap> INFO_NSGPS:=InfoLevel(InfoNumSgps);;
gap> SetInfoLevel( InfoNumSgps, 0);

# Note that you may use comments in the test file
# and also separate parts of the test by empty lines

# First load the package without banner (the banner must be suppressed to
# avoid reporting discrepancies in the case when the package is already
# loaded)
gap> LoadPackage("numericalsgps",false);
true

# Check that the data are consistent
gap> ns := NumericalSemigroup([10..30]);
<Numerical semigroup with 10 generators>
gap> IsNumericalSemigroup(ns);
true
gap> MinimalGeneratingSystemOfNumericalSemigroup(ns);
[ 10 .. 19 ]
gap> GapsOfNumericalSemigroup(ns);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

gap> AffineSemigroup([2,3]) = AffineSemigroup([[2,3]]);
true

#############################################################################
# Some more elaborated tests

gap> ls1 := NumericalSemigroupsWithFrobeniusNumberFG(7);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>, 
  <Numerical semigroup>, <Numerical semigroup> ]
gap> ls2 := NumericalSemigroupsWithFrobeniusNumber(7);;
gap> ge1 := List(ls1, s -> MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 3, 5 ], [ 4, 5, 11 ], [ 4, 5, 6 ], [ 5, 6, 8, 9 ], [ 2, 9 ], 
  [ 3, 8, 10 ], [ 5, 8, 9, 11, 12 ], [ 4, 9, 10, 11 ], [ 4, 6, 9, 11 ], 
  [ 6, 8, 9, 10, 11, 13 ], [ 8 .. 15 ] ]
gap> ge2 := List(ls2, s -> MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 2, 9 ], [ 3, 5 ], [ 3, 8, 10 ], [ 4, 5, 6 ], [ 4, 5, 11 ], 
  [ 4, 6, 9, 11 ], [ 4, 9, 10, 11 ], [ 5, 6, 8, 9 ], [ 5, 8, 9, 11, 12 ], 
  [ 6, 8, 9, 10, 11, 13 ], [ 8 .. 15 ] ]
gap> Set(ge1)=Set(ge2);
true

gap> NumericalSemigroupsWithGenus(5);
[ <Numerical semigroup with 6 generators>,
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
  <Numerical semigroup with 2 generators> ]
gap> List(last, s -> MinimalGeneratingSystemOfNumericalSemigroup(s));
[ [ 6 .. 11 ], [ 5, 7, 8, 9, 11 ], [ 5, 6, 8, 9 ], [ 5, 6, 7, 9 ],
  [ 5, 6, 7, 8 ], [ 4, 6, 7 ], [ 4, 7, 9, 10 ], [ 4, 6, 9, 11 ],
  [ 4, 5, 11 ], [ 3, 8, 10 ], [ 3, 7, 11 ], [ 2, 11 ] ]

gap> ns := NumericalSemigroup([10..30]);
<Numerical semigroup with 10 generators>
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
gap> List(genus8, s -> WilfNumber(s)+EliahouNumber(s));
[ 0, 12, 15, 24, 35, 48, 63, 80, 64, 8, 15, 18, 28, 40, 10, 12, 21, 32, 12,
  21, 32, 21, 32, 32, 29, 40, 4, 10, 18, 29, 48, 5, 18, 20, 6, 20, 32, 5, 6,
  6, 6, 4, 13, 20, 20, 0, 6, 20, 26, 8, 6, 8, 5, 12, 6, 18, 2, 6, 12, 6, 16,
  16, 16, 7, 0, 10, 0 ]

gap> a := AffineSemigroupByGenerators([5,3,1],[2,7,4],[3,1,5]);
<Affine semigroup in 3 dimensional space, with 3 generators>
gap> MinimalPresentationOfAffineSemigroup(a);
[  ]
gap> BettiElementsOfAffineSemigroup(a);
[  ]

gap> pf := [ 83, 169, 173, 214, 259 ];;
gap> ns := ANumericalSemigroupWithPseudoFrobeniusNumbers(pf);;
gap> PseudoFrobeniusOfNumericalSemigroup(ns) = pf;
true

gap> s:=NumericalSemigroup(4,5,7);;
gap> Set(MinimalPresentation(s),p->Set(p));
[ [ [ 0, 0, 2 ], [ 1, 2, 0 ] ], [ [ 0, 1, 1 ], [ 3, 0, 0 ] ],
  [ [ 0, 3, 0 ], [ 2, 0, 1 ] ] ]
gap> a:=AsAffineSemigroup(s);;
gap> Set(MinimalPresentation(a),p->Set(p));
[ [ [ 0, 0, 2 ], [ 1, 2, 0 ] ], [ [ 0, 1, 1 ], [ 3, 0, 0 ] ],
  [ [ 0, 3, 0 ], [ 2, 0, 1 ] ] ]
gap> CatenaryDegreeOfAffineSemigroup(a)=CatenaryDegreeOfNumericalSemigroup(s);
true

gap> n := 12;; ns := RandomNumericalSemigroupWithGenus(n);;
gap> Genus(ns) = n;
true

gap> n := 10;;RandomAffineSemigroupWithGenusAndDimension(n,3);;
gap> Length(Gaps(last)) = n;
true

#############################################################################
## tests aiming to improve code coverage

##some tests involving random functions
#
gap> RandomNumericalSemigroup(3,9);;
gap> IsNumericalSemigroup(last);
true
gap> ns := RandomNumericalSemigroup(3,9,55);;
gap> EmbeddingDimension(ns) > 3;
false
gap> Multiplicity(ns) >= 9;
true
gap> l := RandomListForNS(13,1,79);;
gap> Gcd(l);
1
gap> RandomModularNumericalSemigroup(9);;
gap> RandomModularNumericalSemigroup(10,25);;
gap> IsModularNumericalSemigroup(last);IsModularNumericalSemigroup(last2);
true
true
gap> pm1 := RandomProportionallyModularNumericalSemigroup(9);;
gap> pm2 := RandomProportionallyModularNumericalSemigroup(10,25);;
gap> IsProportionallyModularNumericalSemigroup(pm1);IsProportionallyModularNumericalSemigroup(pm2);
true
true
gap> RandomListRepresentingSubAdditiveFunction(7,9);;
gap> RepresentsPeriodicSubAdditiveFunction(last);
true
gap> ns := NumericalSemigroupWithRandomElementsAndFrobenius(5,10,50);;
gap> ns2 := NumericalSemigroupWithRandomElementsAndFrobenius(5,10,9); 
#I  The third argument must not be smaller than the second
fail
gap> FrobeniusNumber(ns) > 50;
false
gap> RandomNumericalSemigroupWithGenus(7);;
gap> Genus(last);
7

gap> RandomAffineSemigroupWithGenusAndDimension(10,3);;
gap> Genus(last);
10
gap> Dimension(last2);
3
gap> RandomAffineSemigroup(5,5);;
gap> RandomAffineSemigroup(5,5,3);;
gap> IsAffineSemigroup(last);IsAffineSemigroup(last2);
true
true

#gap> a := RandomFullAffineSemigroup(5,5,3);;
#gap> IsAffineSemigroup(a);IsFullAffineSemigroup(a);
#true
#true

gap> s:=RandomGoodSemigroupWithFixedMultiplicity([6,7],[30,30]);
<Good semigroup>
gap> Conductor(s) <= [30,30];
true
gap> Multiplicity(s)=[6,7];
true

##
gap> ns := NumericalSemigroup(3,5,7);;
gap> Display(ns);
[ [ 0 ], [ 3 ], [ 5, "->" ] ]
gap> ns := NumericalSemigroup(3,5,7);;
gap> IsProportionallyModularNumericalSemigroup(ns);
true
gap> ns := NumericalSemigroup(3,5,7);;
gap> IsModularNumericalSemigroup(ns);
true
gap> ns := NumericalSemigroup(7,11,19,20);;
gap> IsModularNumericalSemigroup(ns);
false
gap> ns := NumericalSemigroup(7,11,19,20);;
gap> IsProportionallyModularNumericalSemigroup(ns);
false

##
gap> ForcedIntegersForPseudoFrobenius_QV(5,8);
fail
gap> ForcedIntegersForPseudoFrobenius_QV(5,9);
fail
gap> ForcedIntegersForPseudoFrobenius_QV(7,9);
[ [ 1, 2, 3, 4, 7, 9 ], [ 0, 5, 6, 8, 10 ] ]
gap> ForcedIntegersForPseudoFrobenius_QV([9]);
[ [ 1, 3, 9 ], [ 0, 10 ] ]
gap> ForcedIntegersForPseudoFrobenius_QV(9);
[ [ 1, 3, 9 ], [ 0, 10 ] ]
gap> ForcedIntegersForPseudoFrobenius(9);
[ [ 1, 3, 9 ], [ 0, 10 ] ]
gap> ForcedIntegersForPseudoFrobenius(8);
[  ]
gap> NumericalSemigroupsWithPseudoFrobeniusNumbers(8);
[  ]
gap> record := rec(pseudo_frobenius := [ 166, 182, 269, 279, 295 ],
> max_attempts := 25);
rec( max_attempts := 25, pseudo_frobenius := [ 166, 182, 269, 279, 295 ] )
gap> ANumericalSemigroupWithPseudoFrobeniusNumbers(record);
<Numerical semigroup>

##
# Generators
gap> a:=AffineSemigroup([2,0],[0,2],[1,1],[3,1]);
<Affine semigroup in 2 dimensional space, with 4 generators>
gap> Print(a);
AffineSemigroup( [ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ], [ 3, 1 ] ] )
gap> Generators(a);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ], [ 3, 1 ] ]
gap> MinimalGenerators(a);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ] ]
gap> MinimalGenerators(a);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ] ]
gap> a:=AffineSemigroupByEquations([[1,-1]],[]);
<Affine semigroup>
gap> Print(a);
AffineSemigroupByEquations( [ [ [ 1, -1 ] ], [  ] ] )
gap> Generators(a);
[ [ 1, 1 ] ]

# HasPMInequality
gap> a:=AffineSemigroupByPMInequality([1,1],2,[-1,-1]);
<Affine semigroup>
gap> Generators(a);
[  ]
gap> a:=AffineSemigroupByPMInequality([3,1],5,[1,2]);
<Affine semigroup>
gap> [ 3, 1 ] in a;
true
gap> [ 1, 1 ] in a;
false
gap> Gaps(a);
[ [ 1, 0 ], [ 3, 0 ], [ 1, 1 ] ]

# MinimalGenerators
gap> a:=AffineSemigroupByEquations([[1,-1]],[]);
<Affine semigroup>
gap> Display(a);
<Affine semigroup>
gap> ViewString(a);
"Affine semigroup"
gap> [1,1] in a;
true
gap> [0,2] in a;
false
gap> MinimalGenerators(a);
[ [ 1, 1 ] ]
gap> a:=AffineSemigroupByInequalities([[2,-1],[-1,3]]);
<Affine semigroup>
gap> Print(a);
AffineSemigroupByInequalities( [ [ -1, 3 ], [ 2, -1 ] ] )
gap> ViewString(a);
"Affine semigroup"
gap> Display(a);
<Affine semigroup>
gap> [1,1] in a;
true
gap> [0,20] in a;
false
gap> MinimalGenerators(a);
[ [ 1, 1 ], [ 1, 2 ], [ 2, 1 ], [ 3, 1 ] ]

# Gaps
gap> a:=AffineSemigroup("pminequality",[[3,1],5,[1,2]]);
<Affine semigroup>
gap> Gaps(a);
[ [ 1, 0 ], [ 3, 0 ], [ 1, 1 ] ]
gap> Gaps(a);
[ [ 1, 0 ], [ 3, 0 ], [ 1, 1 ] ]
gap> IsFullAffineSemigroup(a);
false

# factorizations

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> MinimalGenerators(a);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ] ]
gap> Factorizations([10,10],a);
[ [ 0, 10, 0 ], [ 1, 8, 1 ], [ 2, 6, 2 ], [ 3, 4, 3 ], [ 4, 2, 4 ], 
  [ 5, 0, 5 ] ]
gap> Factorizations(a,[10,10]);
[ [ 0, 10, 0 ], [ 1, 8, 1 ], [ 2, 6, 2 ], [ 3, 4, 3 ], [ 4, 2, 4 ], 
  [ 5, 0, 5 ] ]
gap> s:=NumericalSemigroup(3,5);
<Numerical semigroup with 2 generators>
gap> DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup(s);
[ 3, 5, 15 ]
gap> MonotoneCatenaryDegreeOfNumericalSemigroup(s);
5

# presentations

gap> IsUniquelyPresented(a);
true
gap> IsGeneric(a);
true
gap> ShadedSetOfElementInAffineSemigroup([3,1],a);
[ [  ], [ [ 1, 1 ] ], [ [ 1, 1 ], [ 2, 0 ] ], [ [ 2, 0 ] ] ]
gap> MinimalGenerators(LawrenceLiftingOfAffineSemigroup(a));
[ [ 0, 0, 0, 0, 1 ], [ 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 2, 1, 0, 0 ], [ 1, 1, 0, 1, 0 ],
  [ 2, 0, 0, 0, 1 ] ]

# Arf
gap> a:=ArfNumericalSemigroupClosure(3,4);
<Numerical semigroup>
gap> a:=ArfNumericalSemigroupClosure([3,4]);
<Numerical semigroup>
gap> ArfNumericalSemigroupsWithFrobeniusNumber(-1);
[ <The numerical semigroup N> ]
gap> ArfNumericalSemigroupsWithGenusUpTo(0);
[ <The numerical semigroup N> ]
gap> ArfNumericalSemigroupsWithFrobeniusNumberUpTo(-1);
[ <The numerical semigroup N> ]
gap> ArfNumericalSemigroupsWithGenusAndFrobeniusNumber(0,-1);
[ <The numerical semigroup N> ]
gap> ArfNumericalSemigroupsWithGenusAndFrobeniusNumber(2,1);
[  ]
gap> s:=NumericalSemigroup(10,24,25,26,27,28,29,31,32,33);;
gap> ArfSpecialGaps(s);
[ 15, 22, 23 ]
gap> s:=NumericalSemigroup(6,9,11,13,14,16);;
gap> List(ArfOverSemigroups(s),MinimalGenerators);
[ [ 1 ], [ 2, 3 ], [ 2, 5 ], [ 2, 7 ], [ 2, 9 ], [ 3 .. 5 ], [ 3, 5, 7 ], [ 3, 7, 8 ], [ 3, 8, 10 ], [ 3, 10, 11 ],
  [ 3, 11, 13 ], [ 4 .. 7 ], [ 4, 6, 7, 9 ], [ 4, 6, 9, 11 ], [ 5 .. 9 ], [ 6 .. 11 ], [ 6, 8, 9, 10, 11, 13 ],
  [ 6, 9, 10, 11, 13, 14 ], [ 6, 9, 11, 13, 14, 16 ] ]
gap> s:=NumericalSemigroup(6,9,11,13,14,16);;
gap> List(DecomposeIntoArfIrreducibles(s),MinimalGenerators);
[ [ 2, 9 ], [ 3, 11, 13 ] ]
gap> s:=NumericalSemigroupBySmallElements([0,10,17,20]);;
gap> IsArfIrreducible(s);
true
gap> IsIrreducible(s);
false

# MED

gap> MEDNumericalSemigroupClosure(3,4);
<Numerical semigroup>
gap> s:=NumericalSemigroup(2,5);;
gap> MinimalMEDGeneratingSystemOfMEDNumericalSemigroup(s);
[ 2, 5 ]

# Saturated

gap> s:=NumericalSemigroup(2,3);
<Numerical semigroup with 2 generators>
gap> HasMinimalGenerators(s);
true
gap> SaturatedNumericalSemigroupClosure(s);
<Numerical semigroup>
gap> SaturatedNumericalSemigroupClosure(3,7);
<Numerical semigroup>
gap> SaturatedNumericalSemigroupsWithFrobeniusNumber(-1);
[ <The numerical semigroup N> ]
gap> Length(SaturatedNumericalSemigroupsWithFrobeniusNumber(41));
378

# dot

gap> s:=NumericalSemigroup(4,6,9);
<Numerical semigroup with 3 generators>
gap> IsString(DotTreeOfGluingsOfNumericalSemigroup(s));
true
gap> IsString(DotOverSemigroups(s));
true
gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);
<Affine semigroup in 2 dimensional space, with 3 generators>
gap> DotRosalesGraph([3,3],a);
"graph  NSGraph{\n1 [label=\"[ 0, 2 ]\"];\n2 [label=\"[ 1, 1 ]\"];\n3 [label=\"[ 2, 0 ]\"];\n2 -- 1;\
\n3 -- 1;\n3 -- 2;\n}"


# polynomials

gap> x:=Indeterminate(Rationals,"x");;
gap> IsNumericalSemigroupPolynomial(2*x+1);
false
gap> IsNumericalSemigroupPolynomial(x+1/2);
false
gap> IsNumericalSemigroupPolynomial(x+1);
false
gap> s:=NumericalSemigroup(3,5,7);
<Numerical semigroup with 3 generators>
gap> HilbertSeriesOfNumericalSemigroup(s,x);
(x^5-x^4+x^3-x+1)/(-x+1)
gap> HasAperyList(s);
false
gap> AperyList(s);
[ 0, 7, 5 ]
gap> HilbertSeriesOfNumericalSemigroup(s,x);
(x^5-x^4+x^3-x+1)/(-x+1)
gap> IsCyclotomicPolynomial(CyclotomicPolynomial(Rationals,4));
true
gap> IsCyclotomicPolynomial(2*x+1);
false
gap> IsCyclotomicPolynomial((x-1)*(x+1));
false
gap> IsKroneckerPolynomial(0*x);
false
gap> IsKroneckerPolynomial(0*x);
false
gap> IsKroneckerPolynomial(x^2+x);
true
gap> IsKroneckerPolynomial(x+1);
true
gap> IsKroneckerPolynomial(x-1);
true
gap> IsKroneckerPolynomial(0*x+1);
true
gap> IsKroneckerPolynomial(x^2+1);
true
gap> s:=NumericalSemigroup(6,9,20);
<Numerical semigroup with 3 generators>
gap> IsCyclotomicNumericalSemigroup(s);
true
gap> x:=Indeterminate(Rationals,1);; 
gap> y:=Indeterminate(Rationals,2);; 
gap> f:=((y^3-x^2)^2-x*y^2)^4-(y^3-x^2);;
gap> SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f);
<Numerical semigroup with 4 generators>
gap> IsDeltaSequence([2,4]);
false
gap> IsDeltaSequence([1,2]);
false
gap> DeltaSequencesWithFrobeniusNumber(2);
[  ]
gap> DeltaSequencesWithFrobeniusNumber(-1);
[ [ 1 ] ]
gap> x:=Indeterminate(Rationals,1);; SetName(x,"x");
gap> y:=Indeterminate(Rationals,2);; SetName(y,"y");
gap> CurveAssociatedToDeltaSequence([3,2]);
y^3-x^2
gap> s:=LegendrianGenericNumericalSemigroup(5,11);;
gap> SmallElements(s);
[ 0, 5, 6, 10, 11, 12, 13, 15 ]

# good semigroups

gap> g:=GoodSemigroup([[2,2]],[3,3]);;
gap> Conductor(g);
[ 3, 3 ]
gap> [1,1] in g;
false
gap> Display(g);
<Good semigroup>
gap> ViewString(g);
"Good semigroup"
gap> g:=GoodSemigroupBySmallElements([[0,0],[2,2],[3,3]]);;
gap> Conductor(g);
[ 3, 3 ]
gap> [1,1] in g;
false
gap> SmallElements(g);
[ [ 0, 0 ], [ 2, 2 ], [ 3, 3 ] ]
gap> s:=NumericalSemigroup(3,4);;
gap> g:=CartesianProductOfNumericalSemigroups(s,s);;
gap> Conductor(g);
[ 6, 6 ]
gap> [2,2] in g;
false
gap> [3,4] in g;
true
gap> SmallElements(g);
[ [ 0, 0 ], [ 0, 3 ], [ 0, 4 ], [ 0, 6 ], [ 3, 0 ], [ 3, 3 ], [ 3, 4 ],
  [ 3, 6 ], [ 4, 0 ], [ 4, 3 ], [ 4, 4 ], [ 4, 6 ], [ 6, 0 ], [ 6, 3 ],
  [ 6, 4 ], [ 6, 6 ] ]

#############################################################################
#############################################################################
# Examples from the manual
# (These examples use at least a funtion from each file)

#Generating_Numerical_Semigroups.xml
gap> s1 := NumericalSemigroup(3,5,7);               
<Numerical semigroup with 3 generators>
gap> s2 := NumericalSemigroup([3,5,7]);
<Numerical semigroup with 3 generators>
gap> s3 := NumericalSemigroupByGenerators(3,5,7);             
<Numerical semigroup with 3 generators>
gap> s4 := NumericalSemigroupByGenerators([3,5,7]);
<Numerical semigroup with 3 generators>
gap> s5 := NumericalSemigroup("generators",3,5,7); 
<Numerical semigroup with 3 generators>
gap> s6 := NumericalSemigroup("generators",[3,5,7]);
<Numerical semigroup with 3 generators>
gap> s1=s2;s2=s3;s3=s4;s4=s5;s5=s6;
true
true
true
true
true

gap> s := NumericalSemigroupBySubAdditiveFunction([5,4,2,0]);
<Numerical semigroup>
gap> t := NumericalSemigroup("subadditive",[5,4,2,0]);;
gap> s=t;
true

gap> s:=NumericalSemigroup(3,11);;
gap> ap := AperyListOfNumericalSemigroupWRTElement(s,20);
[ 0, 21, 22, 3, 24, 25, 6, 27, 28, 9, 30, 11, 12, 33, 14, 15, 36, 17, 18, 39 ]
gap> t:=NumericalSemigroupByAperyList(ap);;
gap> r := NumericalSemigroup("apery",ap);;
gap> s=t;t=r;
true
true

gap> s:=NumericalSemigroup(3,11);;
gap> se := SmallElements(s);
[ 0, 3, 6, 9, 11, 12, 14, 15, 17, 18, 20 ]
gap> t := NumericalSemigroupBySmallElements(se);;
gap> r := NumericalSemigroup("elements",se);;
gap> s=t;t=r;
true
true

# gap> e := [ 0, 3, 6, 9, 11, 14, 15, 17, 18, 20 ];
# [ 0, 3, 6, 9, 11, 14, 15, 17, 18, 20 ]
# gap> NumericalSemigroupBySmallElements(e);
# Error, The argument does not represent a numerical semigroup called from
# <function "NumericalSemigroupBySmallElements">( <arguments> )
#  called from read-eval loop at line 35 of *stdin*
# you can 'quit;' to quit to outer loop, or
# you can 'return;' to continue
# brk>

gap> g := [ 1, 2, 4, 5, 7, 8, 10, 13, 16 ];;
gap> s := NumericalSemigroupByGaps(g);;
gap> t := NumericalSemigroup("gaps",g);;
gap> s=t;
true

gap> s:=FiniteComplementIdealExtension([0,2],[1,1],[3,0]);;
gap> MinimalGenerators(s);
[ [ 0, 3 ], [ 1, 1 ], [ 0, 2 ], [ 3, 0 ], [ 2, 1 ], [ 1, 2 ], [ 3, 1 ], [ 5, 0 ], [ 4, 0 ] ]

# gap> h := [ 1, 2, 5, 7, 8, 10, 13, 16 ];;
# gap> NumericalSemigroupByGaps(h);
# Error, The argument does not represent the gaps of a numerical semigroup called
#  from
# <function "NumericalSemigroupByGaps">( <arguments> )
#  called from read-eval loop at line 34 of *stdin*
# you can 'quit;' to quit to outer loop, or
# you can 'return;' to continue
# brk>

gap> fg := [ 11, 14, 17, 20, 23, 26, 29, 32, 35 ];;
gap> NumericalSemigroupByFundamentalGaps(fg);
<Numerical semigroup>
gap> NumericalSemigroup("fundamentalgaps",fg);
<Numerical semigroup>
gap> last=last2;
true
gap> gg := [ 11, 17, 20, 22, 23, 26, 29, 32, 35 ];; #22 is not fundamental
gap> NumericalSemigroup("fundamentalgaps",fg);
<Numerical semigroup>

gap> s:=NumericalSemigroupByAffineMap(3,1,3);
<Numerical semigroup with 3 generators>
gap> SmallElements(s);
[ 0, 3, 6, 9, 10, 12, 13, 15, 16, 18 ]
gap> t:=NumericalSemigroup("affinemap",3,1,3);;
gap> s=t;
true

gap> ModularNumericalSemigroup(3,7);
<Modular numerical semigroup satisfying 3x mod 7 <= x >
gap> NumericalSemigroup("modular",3,7);
<Modular numerical semigroup satisfying 3x mod 7 <= x >

gap> ProportionallyModularNumericalSemigroup(3,7,12);
<Proportionally modular numerical semigroup satisfying 3x mod 7 <= 12x >
gap> NumericalSemigroup("propmodular",3,7,12);
<Proportionally modular numerical semigroup satisfying 3x mod 7 <= 12x >

gap> NumericalSemigroup("propmodular",67,98,1);
<Modular numerical semigroup satisfying 67x mod 98 <= x >

gap> NumericalSemigroupByInterval(7/5,5/3);
<Proportionally modular numerical semigroup satisfying 25x mod 35 <= 4x >
gap> NumericalSemigroup("interval",[7/5,5/3]);
<Proportionally modular numerical semigroup satisfying 25x mod 35 <= 4x >
gap> SmallElements(last);
[ 0, 3, 5 ]

gap> NumericalSemigroupByOpenInterval(7/5,5/3);
<Numerical semigroup>
gap> NumericalSemigroup("openinterval",[7/5,5/3]);
<Numerical semigroup>
gap> SmallElements(last);
[ 0, 3, 6, 8 ]

##Some_basic_tests.xml

gap> s:=NumericalSemigroup(3,7);
<Numerical semigroup with 2 generators>
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
<Numerical semigroup with 2 generators>
gap> L:=GapsOfNumericalSemigroup(s);
[ 1, 2, 4, 5, 8, 11 ]
gap> RepresentsGapsOfNumericalSemigroup(L);
true

gap> IsAperyListOfNumericalSemigroup([0,21,7,28,14]);
true

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> T:=NumericalSemigroup(2,3);
<Numerical semigroup with 2 generators>
gap> IsSubsemigroupOfNumericalSemigroup(T,S);
true
gap> IsSubsemigroupOfNumericalSemigroup(S,T);
false

gap> ns1 := NumericalSemigroup(5,7);;
gap> ns2 := NumericalSemigroup(5,7,11);;
gap> IsSubset(ns1,ns2);
false
gap> IsSubset(ns2,[5,15]);
true
gap> IsSubset(ns1,[5,11]);
false
gap> IsSubset(ns2,ns1);
true

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

##The_definitions.xml

gap> NumericalSemigroup(3,5);
<Numerical semigroup with 2 generators>
gap> Multiplicity(last);
3
gap> S := NumericalSemigroup("modular", 7,53);
<Modular numerical semigroup satisfying 7x mod 53 <= x >
gap> MultiplicityOfNumericalSemigroup(S);
8

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> Generators(S);
[ 11, 12, 13, 32, 53 ]
gap> S := NumericalSemigroup(3, 5, 53);
<Numerical semigroup with 3 generators>
gap> GeneratorsOfNumericalSemigroup(S);
[ 3, 5, 53 ]
gap> MinimalGenerators(S);
[ 3, 5 ]
gap> MinimalGeneratingSystemOfNumericalSemigroup(S);
[ 3, 5 ]
gap> MinimalGeneratingSystem(S)=MinimalGeneratingSystemOfNumericalSemigroup(S);
true
gap> s := NumericalSemigroup(3,5,7,15);
<Numerical semigroup with 4 generators>
gap> HasGenerators(s);
true
gap> HasMinimalGenerators(s);
false
gap> MinimalGenerators(s);
[ 3, 5, 7 ]
gap> Generators(s);
[ 3, 5, 7, 15 ]

gap> s := NumericalSemigroup(3,5,7,15);
<Numerical semigroup with 4 generators>
gap> EmbeddingDimension(s);
3
gap> EmbeddingDimensionOfNumericalSemigroup(s);
3

gap> SmallElements(NumericalSemigroup(3,5,7));
[ 0, 3, 5 ]
gap> SmallElementsOfNumericalSemigroup(NumericalSemigroup(3,5,7));
[ 0, 3, 5 ]

gap> Length(NumericalSemigroup(3,5,7));
2

gap> FirstElementsOfNumericalSemigroup(2,NumericalSemigroup(3,5,7));
[ 0, 3 ]
gap> FirstElementsOfNumericalSemigroup(10,NumericalSemigroup(3,5,7));
[ 0, 3, 5, 6, 7, 8, 9, 10, 11, 12 ]

gap> ns := NumericalSemigroup(5,7);;
gap> SmallElements(ns);
[ 0, 5, 7, 10, 12, 14, 15, 17, 19, 20, 21, 22, 24 ]
gap> ElementsUpTo(ns,18);
[ 0, 5, 7, 10, 12, 14, 15, 17 ]
gap> ElementsUpTo(ns,27);
[ 0, 5, 7, 10, 12, 14, 15, 17, 19, 20, 21, 22, 24, 25, 26, 27 ]

gap> S := NumericalSemigroup(7,8,17);;
gap> S[53];
68
gap> S := NumericalSemigroup(7,8,17);;
gap> S{[1..5]};
[ 0, 7, 8, 14, 15 ]


gap> S := NumericalSemigroup(7,8,17);;
gap> NextElementOfNumericalSemigroup(S,9);
14
gap> NextElementOfNumericalSemigroup(16,S);
17
gap> NextElementOfNumericalSemigroup(S,FrobeniusNumber(S))=Conductor(S);
true

gap> S := NumericalSemigroup(7,8,17);;
gap> ElementNumber_NumericalSemigroup(S,53);
68
gap> RthElementOfNumericalSemigroup(S,53);
68

gap> S := NumericalSemigroup(7,8,17);;
gap> NumberElement_NumericalSemigroup(S,68);
53

gap> S := NumericalSemigroup(7,8,17);;
gap> iter:=Iterator(S);
<iterator>
gap> NextIterator(iter);
0
gap> NextIterator(iter);
7
gap> NextIterator(iter);
8

gap> S := NumericalSemigroup("modular", 5,53);;
gap> AperyList(S,12);
[ 0, 13, 26, 39, 52, 53, 54, 43, 32, 33, 22, 11 ]
gap> AperyListOfNumericalSemigroupWRTElement(S,12);
[ 0, 13, 26, 39, 52, 53, 54, 43, 32, 33, 22, 11 ]
gap> First(S,x-> x mod 12 =1);
13

gap> AperyList(NumericalSemigroup(5,7,11));
[ 0, 11, 7, 18, 14 ]
gap> S := NumericalSemigroup("modular", 5,53);;
gap> AperyListOfNumericalSemigroup(S);
[ 0, 12, 13, 25, 26, 38, 39, 51, 52, 53, 32 ]

gap> s:=NumericalSemigroup(10,13,19,27);;
gap> AperyList(s,11);
[ 0, 10, 13, 19, 20, 23, 26, 27, 29, 32, 33, 36, 39, 42, 45, 46, 52, 55 ]
gap> AperyListOfNumericalSemigroupWRTInteger(s,11);
[ 0, 10, 13, 19, 20, 23, 26, 27, 29, 32, 33, 36, 39, 42, 45, 46, 52, 55 ]
gap> Length(last);
18
gap> AperyListOfNumericalSemigroupWRTInteger(s,10);
[ 0, 13, 19, 26, 27, 32, 38, 45, 51, 54 ]
gap> AperyListOfNumericalSemigroupWRTElement(s,10);
[ 0, 51, 32, 13, 54, 45, 26, 27, 38, 19 ]
gap> AperyList(s,10);
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

gap> s:=NumericalSemigroup(3,5,7);
<Numerical semigroup with 3 generators>
gap> KunzCoordinates(s);
[ 2, 1 ]
gap> KunzCoordinatesOfNumericalSemigroup(s);
[ 2, 1 ]
gap> KunzCoordinates(s,5);
[ 1, 1, 0, 1 ]
gap> KunzCoordinatesOfNumericalSemigroup(s,5);
[ 1, 1, 0, 1 ]

gap> KunzPolytope(3);
[ [ 1, 0, -1 ], [ 0, 1, -1 ], [ 2, -1, 0 ], [ -1, 2, 1 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> CocycleOfNumericalSemigroupWRTElement(s,3);
[ [ 0, 0, 0 ], [ 0, 3, 4 ], [ 0, 4, 1 ] ]

gap> FrobeniusNumber(NumericalSemigroup(3,5,7));
4
gap> FrobeniusNumberOfNumericalSemigroup(NumericalSemigroup(3,5,7));
4

gap> Conductor(NumericalSemigroup(3,5,7));
5
gap> ConductorOfNumericalSemigroup(NumericalSemigroup(3,5,7));
5

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> PseudoFrobenius(S);
[ 21, 40, 41, 42 ]
gap> PseudoFrobeniusOfNumericalSemigroup(S);
[ 21, 40, 41, 42 ]

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> Type(S);
4
gap> TypeOfNumericalSemigroup(S);
4

gap> Gaps(NumericalSemigroup(5,7,11));
[ 1, 2, 3, 4, 6, 8, 9, 13 ]
gap> GapsOfNumericalSemigroup(NumericalSemigroup(3,5,7));
[ 1, 2, 4 ]

gap> Weight(NumericalSemigroup(4,5,6,7));
0
gap> Weight(NumericalSemigroup(4,5));    
9

gap> s:=NumericalSemigroup(3,5,7);;
gap> Deserts(s);
[ [ 1, 2 ], [ 4 ] ]
gap> DesertsOfNumericalSemigroup(s);
[ [ 1, 2 ], [ 4 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsOrdinary(s);
false

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsAcute(s);
true

gap> s:=NumericalSemigroup(3,5);;
gap> Holes(s);
[  ]
gap> s:=NumericalSemigroup(3,5,7);;
gap> HolesOfNumericalSemigroup(s);
[ 2 ]

gap> s:=NumericalSemigroup(16,17,71,72);;
gap> LatticePathAssociatedToNumericalSemigroup(s,16,17);
[ [ 0, 14 ], [ 1, 13 ], [ 2, 12 ], [ 3, 11 ], [ 4, 10 ], [ 5, 9 ], [ 6, 8 ],
  [ 7, 7 ], [ 8, 6 ], [ 9, 5 ], [ 10, 4 ], [ 11, 3 ], [ 12, 2 ], [ 13, 1 ],
  [ 14, 0 ] ]

gap> s:=NumericalSemigroup(16,17,71,72);;
gap> Genus(s);
80
gap> GenusOfNumericalSemigroup(s);
80
gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> Genus(S);
26

gap> FundamentalGaps(NumericalSemigroup(5,7,11));
[ 6, 8, 9, 13 ]
gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> FundamentalGapsOfNumericalSemigroup(S);
[ 16, 17, 18, 19, 27, 28, 29, 30, 31, 40, 41, 42 ]
gap> GapsOfNumericalSemigroup(S);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 27, 28, 29,
  30, 31, 40, 41, 42 ]
gap> Gaps(NumericalSemigroup(5,7,11));
[ 1, 2, 3, 4, 6, 8, 9, 13 ]
gap> FundamentalGaps(NumericalSemigroup(5,7,11));
[ 6, 8, 9, 13 ]

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> SpecialGaps(S);
[ 40, 41, 42 ]
gap> SpecialGapsOfNumericalSemigroup(S);
[ 40, 41, 42 ]

##Wilf.xml

gap> s := NumericalSemigroup(13,25,37);;
gap> WilfNumber(s);
96
gap> l:=NumericalSemigroupsWithGenus(10);;
gap> Filtered(l, s->WilfNumber(s)<0);
[  ]
gap> Maximum(Set(l, s->WilfNumberOfNumericalSemigroup(s)));
70

gap> s:=NumericalSemigroupWithGivenElementsAndFrobenius([14,22,23],55);;
gap> EliahouNumber(s);
-1
gap> s:=NumericalSemigroup(5,7,9);;
gap> TruncatedWilfNumberOfNumericalSemigroup(s);
4

gap> s:=NumericalSemigroup(5,7,9);;
gap> ProfileOfNumericalSemigroup(s);
[ 2, 1 ]
gap> s:=NumericalSemigroupWithGivenElementsAndFrobenius([14,22,23],55);;
gap> ProfileOfNumericalSemigroup(s);
[ 3, 0, 0 ]

gap> s:=NumericalSemigroup(5,7,9);;
gap> EliahouSlicesOfNumericalSemigroup(s);
[ [ 5, 7 ], [ 9, 10, 12 ] ]
gap> SmallElements(s);
[ 0, 5, 7, 9, 10, 12, 14 ]

##Presentations_of_Numerical_Semigroups.xml

gap> s:=NumericalSemigroup(3,5,7);;
gap> MinimalPresentation(s);
[ [ [ 0, 0, 2 ], [ 3, 1, 0 ] ], [ [ 0, 1, 1 ], [ 4, 0, 0 ] ], 
  [ [ 0, 2, 0 ], [ 1, 0, 1 ] ] ]
gap> MinimalPresentationOfNumericalSemigroup(s);
[ [ [ 0, 0, 2 ], [ 3, 1, 0 ] ], [ [ 0, 1, 1 ], [ 4, 0, 0 ] ], 
  [ [ 0, 2, 0 ], [ 1, 0, 1 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> GraphAssociatedToElementInNumericalSemigroup(10,s);
[ [ 3, 5, 7 ], [ [ 3, 7 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> BettiElementsOfNumericalSemigroup(s);
[ 10, 12, 14 ]
gap> BettiElements(s);
[ 10, 12, 14 ]

gap> s:=NumericalSemigroup(4,6,9);;
gap> MinimalPresentation(s);
[ [ [ 0, 0, 2 ], [ 0, 3, 0 ] ], [ [ 0, 2, 0 ], [ 3, 0, 0 ] ] ]
gap> IsMinimalRelationOfNumericalSemigroup([[2,1,0],[0,0,2]],s);
false
gap> IsMinimalRelationOfNumericalSemigroup([[3,1,0],[0,0,2]],s);
true

gap> s:=NumericalSemigroup(4,6,9);;
gap> MinimalPresentation(s);
[ [ [ 0, 0, 2 ], [ 0, 3, 0 ] ], [ [ 0, 2, 0 ], [ 3, 0, 0 ] ] ]
gap> AllMinimalRelationsOfNumericalSemigroup(s);
[ [ [ 0, 3, 0 ], [ 0, 0, 2 ] ], [ [ 3, 0, 0 ], [ 0, 2, 0 ] ], [ [ 3, 1, 0 ], [ 0, 0, 2 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> DegreesOfPrimitiveElementsOfNumericalSemigroup(s);
[ 3, 5, 7, 10, 12, 14, 15, 21, 28, 35 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> ShadedSetOfElementInNumericalSemigroup(10,s);
[ [  ], [ 3 ], [ 3, 7 ], [ 5 ], [ 7 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> BinomialIdealOfNumericalSemigroup(GF(2),s);
<two-sided ideal in GF(2)[x_1,x_2,x_3], (3 generators)>
gap> GeneratorsOfTwoSidedIdeal(last);
[ x_1^3*x_2+x_3^2, x_1^4+x_2*x_3, x_1*x_3+x_2^2 ]
gap> x:=Indeterminate(Rationals,"x");;
gap> y:=Indeterminate(Rationals,"y");;
gap> z:=Indeterminate(Rationals,"z");;
gap> BinomialIdealOfNumericalSemigroup(s);
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> GeneratorsOfTwoSidedIdeal(last);
[ -x^3*y+z^2, -x^4+y*z, -x*z+y^2 ]
gap> MinimalPresentation(s);
[ [ [ 0, 0, 2 ], [ 3, 1, 0 ] ], [ [ 0, 1, 1 ], [ 4, 0, 0 ] ], [ [ 0, 2, 0 ], [ 1, 0, 1 ] ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsUniquelyPresented(s);
true
gap> IsUniquelyPresentedNumericalSemigroup(s);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsGeneric(s);
true
gap> IsGenericNumericalSemigroup(s);
true

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

#Operations_Numerical_Semigroups.xml

gap> S := NumericalSemigroup("modular", 5,53);
<Modular numerical semigroup satisfying 5x mod 53 <= x >
gap> T := NumericalSemigroup(2,17);
<Numerical semigroup with 2 generators>
gap> SmallElements(S);
[ 0, 11, 12, 13, 22, 23, 24, 25, 26, 32, 33, 34, 35, 36, 37, 38, 39, 43 ]
gap> SmallElements(T);
[ 0, 2, 4, 6, 8, 10, 12, 14, 16 ]
gap> Intersection(S,T);
<Numerical semigroup>
gap> SmallElements(last);
[ 0, 12, 22, 23, 24, 25, 26, 32, 33, 34, 35, 36, 37, 38, 39, 43 ]
gap> IntersectionOfNumericalSemigroups(S,T) = Intersection(S,T);
true

gap> s:=NumericalSemigroup(4,9);;
gap> t:=NumericalSemigroup(6,7);;
gap> MinimalGenerators(s+t);
[ 4, 6, 7, 9 ]

gap> s:=NumericalSemigroup(3,29);
<Numerical semigroup with 2 generators>
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

gap> N:=NumericalSemigroup(1);;
gap> s:=MultipleOfNumericalSemigroup(N,4,20);;
gap> SmallElements(s)=[ 0, 4, 8, 12, 16, 20 ];
true

gap> s:=NumericalSemigroup(3,4,5);;
gap> m:=MaximalIdeal(s);;
gap> SmallElements(m-2*m);
[ -3 ]
gap> d:=DilatationOfNumericalSemigroup(s,3);
<Numerical semigroup>
gap> SmallElements(d);
[ 0, 6 ]

gap> ns1 := NumericalSemigroup(5,7);;
gap> ns2 := NumericalSemigroup(7,11,12);;
gap> Difference(ns1,ns2);
[ 5, 10, 15, 17, 20, 27 ]
gap> Difference(ns2,ns1);
[ 11, 18, 23 ]
gap> DifferenceOfNumericalSemigroups(ns2,ns1);
[ 11, 18, 23 ]

gap> s:=NumericalSemigroup(3,5,7);
<Numerical semigroup with 3 generators>
gap> e:=6+s;
<Ideal of numerical semigroup>
gap> ndup:=NumericalDuplication(s,e,3);
<Numerical semigroup with 4 generators>
gap> SmallElements(ndup);
[ 0, 6, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> ndup:=NumericalDuplication(s,6+s,11);;
gap> asdup:=AsNumericalDuplication(ndup);
[ <Numerical semigroup with 3 generators>, <Ideal of numerical semigroup>, 3 ]
gap> ndup = CallFuncList(NumericalDuplication,asdup);
true


gap> s:=InductiveNumericalSemigroup([4,2],[5,23]);;
gap> SmallElements(s);
[ 0, 8, 16, 24, 32, 40, 42, 44, 46 ]


##Constructing_sets_of_numerical_semigroups.xml

gap> s := NumericalSemigroup(3,5,7);;
gap> OverSemigroups(s);
[ <The numerical semigroup N>, <Numerical semigroup with 2 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Numerical semigroup with 3 generators> ]
gap> List(last,s->MinimalGenerators(s));
[ [ 1 ], [ 2, 3 ], [ 3 .. 5 ], [ 3, 5, 7 ] ]
gap> OverSemigroupsNumericalSemigroup(s) = OverSemigroups(s);
true

gap> Length(NumericalSemigroupsWithFrobeniusNumberFG(15));
200
gap> Length(NumericalSemigroupsWithFrobeniusNumber(15));
200
gap> Length(NumericalSemigroupsWithFrobeniusNumberPC(15));
200

gap> Length(NumericalSemigroupsWithFrobeniusNumberAndMultiplicity(15,6));
28

gap> NumericalSemigroupsWithMaxPrimitive(5);
[ <Numerical semigroup with 2 generators>, 
  <Numerical semigroup with 2 generators>, 
  <Numerical semigroup with 3 generators>, 
  <Numerical semigroup with 2 generators> ]
gap> Length(NumericalSemigroupsWithMaxPrimitive(15));
194
gap> Length(NumericalSemigroupsWithMaxPrimitivePC(15));
194
gap> Length(NumericalSemigroupsWithMaxPrimitiveAndMultiplicity(15,6));
27


gap> NumericalSemigroupsWithGenus(5);
[ <Numerical semigroup with 6 generators>,
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
  <Numerical semigroup with 2 generators> ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 6 .. 11 ], [ 5, 7, 8, 9, 11 ], [ 5, 6, 8, 9 ], [ 5, 6, 7, 9 ],
  [ 5, 6, 7, 8 ], [ 4, 6, 7 ], [ 4, 7, 9, 10 ], [ 4, 6, 9, 11 ],
  [ 4, 5, 11 ], [ 3, 8, 10 ], [ 3, 7, 11 ], [ 2, 11 ] ]
gap> Length(NumericalSemigroupsWithGenusPC(15));
2857

gap> pf := [ 58, 64, 75 ];
[ 58, 64, 75 ]
gap> ForcedIntegersForPseudoFrobenius(pf);
[ [ 1, 2, 3, 4, 5, 6, 7, 8, 11, 15, 16, 17, 25, 29, 32, 58, 64, 75 ],
  [ 0, 59, 60, 67, 68, 69, 70, 71, 72, 73, 74, 76 ] ]

gap> pf := [ 15, 20, 27, 35 ];;
gap> fint := ForcedIntegersForPseudoFrobenius(pf);
[ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16, 20, 27, 35 ],
  [ 0, 19, 23, 25, 26, 28, 29, 30, 31, 32, 33, 34, 36 ] ]
gap> free := Difference([1..Maximum(pf)],Union(fint));
[ 11, 13, 14, 17, 18, 21, 22, 24 ]
gap> SimpleForcedIntegersForPseudoFrobenius(fint[1],Union(fint[2],[free[1]]),pf);
[ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 20, 24, 27, 35 ],
  [ 0, 11, 19, 22, 23, 25, 26, 28, 29, 30, 31, 32, 33, 34, 36 ] ]

gap> pf := [ 58, 64, 75 ];
[ 58, 64, 75 ]
gap> Length(NumericalSemigroupsWithPseudoFrobeniusNumbers(pf));
561
gap> pf := [11,19,22];;
gap> NumericalSemigroupsWithPseudoFrobeniusNumbers(pf);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>,
  <Numerical semigroup>, <Numerical semigroup> ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 7, 9, 17, 20 ], [ 7, 10, 13, 16, 18 ], [ 9, 12, 14, 15, 16, 17, 20 ],
  [ 10, 13, 14, 15, 16, 17, 18, 21 ],
  [ 12, 13, 14, 15, 16, 17, 18, 20, 21, 23 ] ]
gap> Set(last2,PseudoFrobeniusOfNumericalSemigroup);    
[ [ 11, 19, 22 ] ]

##Irreducible_numerical_semigroups.xml

gap> IsIrreducible(NumericalSemigroup(4,6,9));
true
gap> IsIrreducibleNumericalSemigroup(NumericalSemigroup(4,6,7,9));
false

gap> IsSymmetricNumericalSemigroup(NumericalSemigroup(10,23));
true
gap> IsSymmetricNumericalSemigroup(NumericalSemigroup(10,11,23));
false

gap> IsPseudoSymmetric(NumericalSemigroup(6,7,8,9,11));
true
gap> IsPseudoSymmetricNumericalSemigroup(NumericalSemigroup(4,6,9));
false

gap> FrobeniusNumber(AnIrreducibleNumericalSemigroupWithFrobeniusNumber(28));
28

gap> Length(IrreducibleNumericalSemigroupsWithFrobeniusNumber(19));
20

gap> Length(IrreducibleNumericalSemigroupsWithFrobeniusNumberAndMultiplicity(31,11));
16

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
gap> IsCompleteIntersection(s);
true
gap> s := NumericalSemigroup( 18, 24, 34, 46, 51, 61, 74, 8 );
<Numerical semigroup with 8 generators>
gap> IsACompleteIntersectionNumericalSemigroup(s);
false

gap> Length(CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber(57));
34

gap> IsFree(NumericalSemigroup(10,15,16));
true
gap> IsFreeNumericalSemigroup(NumericalSemigroup(3,5,7));
false

gap> Length(FreeNumericalSemigroupsWithFrobeniusNumber(57));
33

gap> IsTelescopic(NumericalSemigroup(4,11,14));
false
gap> IsTelescopicNumericalSemigroup(NumericalSemigroup(4,11,14));
false
gap> IsFree(NumericalSemigroup(4,11,14));
true

gap> Length(TelescopicNumericalSemigroupsWithFrobeniusNumber(57));
20

gap> s:=NumericalSemigroup(10,15,18);;
gap> IsUniversallyFree(s);
true
gap> s:=NumericalSemigroup(4,6,9);;
gap> IsUniversallyFree(s);
false

gap> ns := NumericalSemigroup(4,11,14);;
gap> IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity(ns);
false
gap> ns := NumericalSemigroup(4,11,19);;
gap> IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity(ns);
true

gap> Length(NumericalSemigroupsPlanarSingularityWithFrobeniusNumber(57));
7

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

##Almost_symmetric.xml

gap> AlmostSymmetricNumericalSemigroupsFromIrreducible(NumericalSemigroup(5,8,9,11));
[ <Numerical semigroup with 4 generators>,
  <Numerical semigroup with 5 generators>,
  <Numerical semigroup with 5 generators> ]
gap> List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 5, 8, 9, 11 ], [ 5, 8, 11, 14, 17 ], [ 5, 9, 11, 13, 17 ] ]

gap> ns := NumericalSemigroup(5,8,9,11);;
gap> AlmostSymmetricNumericalSemigroupsFromIrreducibleAndGivenType(ns,4);
[ <Numerical semigroup with 5 generators>, 
  <Numerical semigroup with 5 generators> ]
gap> List(last,MinimalGenerators);
[ [ 5, 8, 11, 14, 17 ], [ 5, 9, 11, 13, 17 ] ]

gap> IsAlmostSymmetric(NumericalSemigroup(5,8,11,14,17));
true
gap> IsAlmostSymmetricNumericalSemigroup(NumericalSemigroup(5,8,11,14,17));
true

gap> Length(AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber(12));
15
gap> Length(IrreducibleNumericalSemigroupsWithFrobeniusNumber(12));
2
gap> List(AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber(12,4),Type);
[ 12, 10, 8, 8, 6, 6, 6, 6, 4, 4, 4, 4, 4 ]

gap> Length(AlmostSymmetricNumericalSemigroupsWithFrobeniusNumberAndType(12,4)); 
5

gap> s:=NumericalSemigroup(3,7,8);;
gap> IsAlmostSymmetric(s);
false
gap> IsGeneralizedGorenstein(s);
true

gap> s:=NumericalSemigroup(10,11,12,25);;
gap> IsAlmostSymmetric(s);
false
gap> IsNearlyGorenstein(s);
true
gap> s:=NumericalSemigroup(3,7,8);;
gap> IsNearlyGorenstein(s);
false

gap> s:=NumericalSemigroup(3,5,7);;
gap> NearlyGorensteinVectors(s);
[ [ 4 ], [ 2, 4 ], [ 2, 4 ] ]
gap> s:=NumericalSemigroup(4,6,9);;
gap> NearlyGorensteinVectors(s);
[ [ 11 ], [ 11 ], [ 11 ] ]


gap> s:=NumericalSemigroup(9, 24, 39, 43, 77);;
gap> IsGeneralizedAlmostSymmetric(s);
true
gap> IsAlmostSymmetric(s);
false

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

gap> MinimalGenerators([3,5]+NumericalSemigroup(2,11));
[ 3 ]
gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> MinimalGeneratingSystem(I);
[ 3 ]
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(I);
[ 3 ]

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> Generators(I);
[ 3, 5, 9 ]
gap> GeneratorsOfIdealOfNumericalSemigroup(I);
[ 3, 5, 9 ]
gap> MinimalGenerators(I);
[ 3 ]

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> AmbientNumericalSemigroupOfIdeal(I);
<Numerical semigroup with 2 generators>

gap> s:=NumericalSemigroup(3,7,5);;
gap> IsIntegral(10+s);
true
gap> IsIntegral(4+s);
false
gap> IsIntegralIdealOfNumericalSemigroup(10+s);
true

gap> s:=NumericalSemigroup(10,11,15,19);;
gap> i:=[20,21,25]+s;;
gap> d:=Difference(0+s,i);;
gap> IsComplementOfIntegralIdeal(d,s);
true
gap> i=IdealByDivisorClosedSet(d,s);
true

gap> I:=[3,5,9]+NumericalSemigroup(2,11);;
gap> SmallElements(I)=[ 3, 5, 7, 9, 11, 13 ];
true
gap> SmallElements(I) = SmallElementsOfIdealOfNumericalSemigroup(I);
true
gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> SmallElements(J);
[ 2, 4, 6, 8, 10 ]

gap> s:=NumericalSemigroup(3,7,5);;
gap> Conductor(10+s);
15
gap> ConductorOfIdealOfNumericalSemigroup(10+s);
15
gap> FrobeniusNumber(0+s);
4

gap> s:=NumericalSemigroup(3,5,7);;
gap> i:=4+s;;
gap> PseudoFrobenius(i);
[ 6, 8 ]
gap> PseudoFrobenius(s)=PseudoFrobenius(0+s);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> k:=CanonicalIdeal(s);;
gap> Type(k);
1

gap> s:=NumericalSemigroup(3,5,7);;
gap> MinimalGenerators(TraceIdeal(s));
[ 3, 5, 7 ]

gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> Minimum(J);
2

gap> J:=[2,11]+NumericalSemigroup(2,11);;
gap> BelongsToIdealOfNumericalSemigroup(9,J);
false
gap> 9 in J;
false
gap> BelongsToIdealOfNumericalSemigroup(10,J);
true
gap> 10 in J;
true

gap> I := [2,5]+ NumericalSemigroup(7,8,17);;
gap> ElementNumber_IdealOfNumericalSemigroup(I,10);
19

gap> I := [2,5]+ NumericalSemigroup(7,8,17);;
gap> NumberElement_IdealOfNumericalSemigroup(I,19);
10

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
gap> MultipleOfIdealOfNumericalSemigroup(2,I) = 2*I;
true
gap> MinimalGeneratingSystemOfIdealOfNumericalSemigroup(2*I);
[ 0, 1, 2 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> I:=2+s;;
gap> J:=3+s;;
gap> Generators(Union(I,J));
[ 2, 3 ]
gap> Generators(Intersection(I,J));
[ 8, 9, 10, 11 ]


gap> S:=NumericalSemigroup(14, 15, 20, 21, 25);;
gap> I:=[0,1]+S;;
gap> II:=S-I;;
gap> MinimalGenerators(I);
[ 0, 1 ]
gap> MinimalGenerators(II);
[ 14, 20 ]
gap> MinimalGenerators(I+II);
[ 14, 15, 20, 21 ]

gap> S:=NumericalSemigroup(14, 15, 20, 21, 25);;
gap> I:=[0,1]+S;
<Ideal of numerical semigroup>
gap> 2*I-2*I;
<Ideal of numerical semigroup>
gap> I-I;
<Ideal of numerical semigroup>
gap> ii := 2*I-2*I;
<Ideal of numerical semigroup>
gap> i := I-I;
<Ideal of numerical semigroup>
gap>  Difference(last2,last);
[ 26, 27, 37, 38 ]
gap> DifferenceOfIdealsOfNumericalSemigroup(ii,i);
[ 26, 27, 37, 38 ]
gap> Difference(i,ii);
[  ]

gap> s:=NumericalSemigroup(13,23);;
gap> l:=List([1..6], _ -> Random([8..34]));;
gap> I:=IdealOfNumericalSemigroup(l, s);;
gap> It:=TranslationOfIdealOfNumericalSemigroup(7,I);
<Ideal of numerical semigroup>
gap> It2:=7+I;
<Ideal of numerical semigroup>
gap> It2=It;
true

gap> i:=IdealOfNumericalSemigroup([75,89],s);;
gap> j:=IdealOfNumericalSemigroup([115,289],s);;
gap> Intersection(i,j);
<Ideal of numerical semigroup>
gap> IntersectionIdealsOfNumericalSemigroup(i,j) = Intersection(i,j);
true

gap> s := NumericalSemigroup(3,7);;                   
gap> MaximalIdeal(s);
<Ideal of numerical semigroup>
gap> MaximalIdealOfNumericalSemigroup(s) = MaximalIdeal(s);
true

gap> s:=NumericalSemigroup(4,6,11);;
gap> m:=MaximalIdeal(s);;
gap> c:=CanonicalIdeal(s);
<Ideal of numerical semigroup>
gap> c-(c-m)=m;
true
gap> id:=3+s;
<Ideal of numerical semigroup>
gap> c-(c-id)=id;
true
gap> CanonicalIdealOfNumericalSemigroup(s) = c;
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> c:=3+CanonicalIdeal(s);;
gap> c-(c-(3+s))=3+s;
true
gap> IsCanonicalIdeal(c);
true
gap> IsCanonicalIdealOfNumericalSemigroup(c);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsAlmostSymmetric(s);
true
gap> IsAlmostCanonical(MaximalIdeal(s));
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> TypeSequence(s);
[ 13, 3, 4, 4, 7, 3, 3, 3, 2, 2, 2, 3, 3, 2, 4, 3, 2, 1, 3, 2, 1, 1, 2, 2, 1,
  1, 1, 2, 2, 1, 3, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
  1, 1, 1 ]
gap> s:=NumericalSemigroup(4,6,11);;
gap> TypeSequenceOfNumericalSemigroup(s);
[ 1, 1, 1, 1, 1, 1, 1 ]


gap> s:=NumericalSemigroup(3,5,7);;
gap> i:=[4,5]+s;;
gap> zc:=IrreducibleZComponents(i);
[ <Ideal of numerical semigroup>, <Ideal of numerical semigroup> ]
gap> List(zc,MinimalGenerators);
[ [ 2, 4 ], [ -2, 0 ] ]
gap> i=Intersection(zc);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> i:=10+s;;
gap> di:=DecomposeIntegralIdealIntoIrreducibles(i);
[ <Ideal of numerical semigroup>, <Ideal of numerical semigroup> ]
gap> List(di,MinimalGenerators);
[ [ 8, 10 ], [ 10, 12 ] ]
gap> i=Intersection(di);
true

gap> I:=[6,9,11]+NumericalSemigroup(6,9,11);;
gap> List([1..7],n->HilbertFunctionOfIdealOfNumericalSemigroup(n,I));
[ 3, 5, 6, 6, 6, 6, 6 ]

gap> I:=[0,2]+NumericalSemigroup(6,9,11);;
gap> BlowUp(I);
<Ideal of numerical semigroup>
gap> SmallElements(last);
[ 0, 2, 4, 6, 8 ]
gap> BlowUpIdealOfNumericalSemigroup(I);;
gap> SmallElementsOfIdealOfNumericalSemigroup(last);
[ 0, 2, 4, 6, 8 ]

gap> I:=[0,2]+NumericalSemigroup(6,9,11);;
gap> ReductionNumber(I);
2
gap> ReductionNumberIdealNumericalSemigroup(I);
2

gap> MaximalIdealOfNumericalSemigroup(NumericalSemigroup(3,7));
<Ideal of numerical semigroup>

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> BlowUp(s);
<Numerical semigroup with 10 generators>
gap> SmallElements(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]
gap> BlowUpOfNumericalSemigroup(s);;
gap> SmallElements(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]
gap> m:=MaximalIdeal(s);
<Ideal of numerical semigroup>
gap> BlowUp(m);
<Ideal of numerical semigroup>
gap> SmallElements(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> LipmanSemigroup(s);
<Numerical semigroup with 10 generators>
gap> SmallElementsOfNumericalSemigroup(last);
[ 0, 5, 10, 12, 15, 17, 20, 22, 24, 25, 27, 29, 30, 32, 34, 35, 36, 37, 39,
  40, 41, 42, 44 ]

gap> s:=NumericalSemigroup([9..17]);;
gap> i:=[9,10,12]+s;;
gap> RatliffRushNumber(i);
3

gap> s:=NumericalSemigroup(4,5,6,7);;
gap> i:=[4,5]+s;;
gap> MinimalGenerators(RatliffRushClosure(i));
[ 4, 5, 6, 7 ]

gap> i:=[4,5]+NumericalSemigroup([4..7]);;
gap> AsymptoticRatliffRushNumber(i);
3

gap> s:=NumericalSemigroup(3,5);;
gap> MultiplicitySequence(s);
[ 3, 2, 1 ]
gap> MultiplicitySequenceOfNumericalSemigroup(s);
[ 3, 2, 1 ]

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> bu:=BlowUpOfNumericalSemigroup(s);;
gap> ap:=AperyListOfNumericalSemigroupWRTElement(s,30);;
gap> apbu:=AperyListOfNumericalSemigroupWRTElement(bu,30);;
gap> (ap-apbu)/30;
[ 0, 4, 4, 3, 2, 1, 3, 4, 4, 3, 2, 3, 1, 4, 4, 3, 3, 1, 4, 4, 4, 3, 2, 4, 2,
  5, 4, 3, 3, 2 ]
gap> MicroInvariants(s)=last;
true
gap> MicroInvariantsOfNumericalSemigroup(s)=MicroInvariants(s);
true

gap> s:=NumericalSemigroup(10,11,13);;
gap> i:=[12,14]+s;;
gap> AperyList(i,10);
[ 40, 51, 12, 23, 14, 25, 36, 27, 38, 49 ]
gap> AperyListOfIdealOfNumericalSemigroupWRTElement(i,10);
[ 40, 51, 12, 23, 14, 25, 36, 27, 38, 49 ]

gap> s:=NumericalSemigroup(5,7,9);;
gap> i:=[0,1,2]+s;;
gap> AperyList(i);
[ 0, 1, 2, 8, 9 ]

gap> s:=NumericalSemigroup(10,11,13);;
gap> AperyTable(s);
[ [ 0, 11, 22, 13, 24, 35, 26, 37, 48, 39 ],
  [ 10, 11, 22, 13, 24, 35, 26, 37, 48, 39 ],
  [ 20, 21, 22, 23, 24, 35, 26, 37, 48, 39 ],
  [ 30, 31, 32, 33, 34, 35, 36, 37, 48, 39 ],
  [ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49 ] ]
gap> AperyTableOfNumericalSemigroup(s) = AperyTable(s);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> StarClosureOfIdealOfNumericalSemigroup([0,2]+s,[[0,4]+s]);;
gap> MinimalGenerators(last);
[ 0, 2, 4 ]

gap> IsAdmissiblePattern([1,1,-1]);
true
gap> IsAdmissiblePattern([1,-2]);
false

gap> IsAdmissiblePattern([1,-1]);
true
gap> IsStronglyAdmissiblePattern([1,-1]);
false
gap> IsStronglyAdmissiblePattern([1,1,-1]);
true

gap> s:=NumericalSemigroup(3,7,5);;
gap> t:=NumericalSemigroup(10,11,14);;
gap> AsIdealOfNumericalSemigroup(10+s,t);
fail
gap> AsIdealOfNumericalSemigroup(100+s,t);
<Ideal of numerical semigroup>

gap> BoundForConductorOfImageOfPattern([1,1,-1],10);
10

gap> s:=NumericalSemigroup(3,7,5);;
gap> i:=10+s;;
gap> ApplyPatternToIdeal([1,1,-1],i);
[ 1, <Ideal of numerical semigroup> ]

gap> s:=NumericalSemigroup(3,7,5);;
gap> ApplyPatternToNumericalSemigroup([1,1,-1],s);
[ 1, <Ideal of numerical semigroup> ]
gap> SmallElements(last[2]);
[ 0, 3, 5 ]

gap> s:=NumericalSemigroup(3,7,5);;
gap> i:=[3,5]+s;;
gap> IsAdmittedPatternByIdeal([1,1,-1],i,i);
false
gap> IsAdmittedPatternByIdeal([1,1,-1],i,0+s);
true

gap> IsAdmittedPatternByNumericalSemigroup([1,1,-1],s,s);
true
gap> IsArfNumericalSemigroup(s);
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

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsGradedAssociatedRingNumericalSemigroupBuchsbaum(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> TorsionOfAssociatedGradedRingNumericalSemigroup(s);
[ 181, 153, 157, 193, 169, 148 ]

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> BuchsbaumNumberOfAssociatedGradedRingNumericalSemigroup(s);
1
gap> IsGradedAssociatedRingNumericalSemigroupBuchsbaum(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsMpure(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsMpureNumericalSemigroup(s);
true

gap> s:=NumericalSemigroup(30, 35, 42, 47, 148, 153, 157, 169, 181, 193);;
gap> IsPure(s);
false
gap> s:=NumericalSemigroup(4,6,11);;
gap> IsPureNumericalSemigroup(s);
true

gap> s:=NumericalSemigroup(10,11,12,25);;
gap> IsHomogeneousNumericalSemigroup(s);
false
gap> s:=NumericalSemigroup(3,5,7);;
gap> IsHomogeneousNumericalSemigroup(s);
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

##Numerical_semigroups_with_maximal_embedding_dimension.xml

gap> IsMED(NumericalSemigroup(3,5,7));
true
gap> IsMEDNumericalSemigroup(NumericalSemigroup(3,5));
false

gap> s := MEDClosure(NumericalSemigroup(3,5));
<Numerical semigroup>
gap> MinimalGenerators(s);
[ 3, 5, 7 ]
gap> MEDNumericalSemigroupClosure(NumericalSemigroup(3,5)) = s;
true

gap> MinimalMEDGeneratingSystemOfMEDNumericalSemigroup(
> NumericalSemigroup(3,5,7));
[ 3, 5 ]

gap>  IsArf(NumericalSemigroup(3,5,7));
true
gap>  IsArfNumericalSemigroup(NumericalSemigroup(3,7,11));
false
gap> IsMED(NumericalSemigroup(3,7,11));
true

gap> s := NumericalSemigroup(3,7,11);;
gap> t := ArfClosure(s);
<Numerical semigroup>
gap> MinimalGenerators(t);
[ 3, 7, 8 ]
gap> ArfNumericalSemigroupClosure(s) = t;
true

gap> s := NumericalSemigroup(3,7,8);
<Numerical semigroup with 3 generators>
gap> ArfCharactersOfArfNumericalSemigroup(s);
[ 3, 7 ]
gap> MinimalArfGeneratingSystemOfArfNumericalSemigroup(s);
[ 3, 7 ]

gap> ArfNumericalSemigroupsWithFrobeniusNumber(10);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>,
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>,
  <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup> ]
gap> Set(last,MinimalGenerators);
[ [ 3, 11, 13 ], [ 4, 11, 13, 14 ], [ 6, 9, 11, 13, 14, 16 ],
  [ 6, 11, 13, 14, 15, 16 ], [ 7, 9, 11, 12, 13, 15, 17 ],
  [ 7, 11, 12, 13, 15, 16, 17 ], [ 8, 11, 12, 13, 14, 15, 17, 18 ],
  [ 9, 11, 12, 13, 14, 15, 16, 17, 19 ], [ 11 .. 21 ] ]

gap> Length(ArfNumericalSemigroupsWithFrobeniusNumberUpTo(10));
46

gap> Length(ArfNumericalSemigroupsWithGenus(10));
21

gap> Length(ArfNumericalSemigroupsWithGenusUpTo(10));
86

gap> ArfNumericalSemigroupsWithGenusAndFrobeniusNumber(10,13);
[ <Numerical semigroup>, <Numerical semigroup>, <Numerical semigroup>,
  <Numerical semigroup>, <Numerical semigroup> ]
gap> List(last,MinimalGenerators);
[ [ 8, 10, 12, 14, 15, 17, 19, 21 ], [ 6, 10, 14, 15, 17, 19 ],
  [ 5, 12, 14, 16, 18 ], [ 6, 9, 14, 16, 17, 19 ], [ 4, 14, 15, 17 ] ]

gap> IsSaturated(NumericalSemigroup(4,6,9,11));
true
gap> IsSaturatedNumericalSemigroup(NumericalSemigroup(8, 9, 12, 13, 15, 19 ));
false

gap> s := NumericalSemigroup(8, 9, 12, 13, 15);;
gap> SaturatedClosure(s);
<Numerical semigroup>
gap> MinimalGenerators(last);
[ 8 .. 15 ]
gap> SaturatedNumericalSemigroupClosure(s) = SaturatedClosure(s);
true

gap> SaturatedNumericalSemigroupsWithFrobeniusNumber(10);
[ <Numerical semigroup with 3 generators>,
  <Numerical semigroup with 4 generators>,
  <Numerical semigroup with 6 generators>,
  <Numerical semigroup with 6 generators>,
  <Numerical semigroup with 7 generators>,
  <Numerical semigroup with 8 generators>,
  <Numerical semigroup with 9 generators>,
  <Numerical semigroup with 11 generators> ]
gap>  List(last,MinimalGeneratingSystemOfNumericalSemigroup);
[ [ 3, 11, 13 ], [ 4, 11, 13, 14 ], [ 6, 9, 11, 13, 14, 16 ],
  [ 6, 11, 13, 14, 15, 16 ], [ 7, 11, 12, 13, 15, 16, 17 ],
  [ 8, 11, 12, 13, 14, 15, 17, 18 ], [ 9, 11, 12, 13, 14, 15, 16, 17, 19 ],
  [ 11 .. 21 ] ]

##catenary-tame.xml

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ],
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> Factorizations(1100,s);
[ [ 0, 8, 1, 0, 0, 0 ], [ 0, 0, 0, 2, 2, 0 ], [ 5, 1, 1, 0, 0, 1 ],
  [ 0, 2, 3, 0, 0, 1 ] ]
gap> Factorizations(s,1100)=Factorizations(1100,s);
true
gap> FactorizationsElementWRTNumericalSemigroup(1100,s)=Factorizations(1100,s);
true

gap> s:=NumericalSemigroup(10,11,13);
<Numerical semigroup with 3 generators>
gap> FactorizationsElementListWRTNumericalSemigroup([100,101,103],s);
[ [ [ 0, 2, 6 ], [ 1, 7, 1 ], [ 3, 4, 2 ], [ 5, 1, 3 ], [ 10, 0, 0 ] ],
  [ [ 0, 8, 1 ], [ 1, 0, 7 ], [ 2, 5, 2 ], [ 4, 2, 3 ], [ 9, 1, 0 ] ],
  [ [ 0, 7, 2 ], [ 2, 4, 3 ], [ 4, 1, 4 ], [ 7, 3, 0 ], [ 9, 0, 1 ] ] ]

gap> s:=NumericalSemigroup(10,11,19,23);;
gap> BettiElements(s);
[ 30, 33, 42, 57, 69 ]
gap> Factorizations(69,s);
[ [ 5, 0, 1, 0 ], [ 2, 1, 2, 0 ], [ 0, 0, 0, 3 ] ]
gap> RClassesOfSetOfFactorizations(last);
[ [ [ 2, 1, 2, 0 ], [ 5, 0, 1, 0 ] ], [ [ 0, 0, 0, 3 ] ] ]

gap> s:=NumericalSemigroup(4,6,9);;
gap> LShapes(s);
[ [ [ 0, 0 ], [ 1, 0 ], [ 0, 1 ], [ 2, 0 ], [ 1, 1 ], [ 0, 2 ], [ 2, 1 ],
      [ 1, 2 ], [ 2, 2 ] ],
  [ [ 0, 0 ], [ 1, 0 ], [ 0, 1 ], [ 2, 0 ], [ 1, 1 ], [ 3, 0 ], [ 2, 1 ],
      [ 4, 0 ], [ 5, 0 ] ] ]
gap> LShapesOfNumericalSemigroup(s) = LShapes(s);
true

gap> s:=NumericalSemigroup(6, 7, 9, 10);;
gap> RFMatrices(8,s);
[ [ [ -1, 2, 0, 0 ], [ 1, -1, 1, 0 ], [ 0, 1, -1, 1 ], [ 3, 0, 0, -1 ] ],
  [ [ -1, 2, 0, 0 ], [ 1, -1, 1, 0 ], [ 0, 1, -1, 1 ], [ 0, 0, 2, -1 ] ] ]

gap> s:=NumericalSemigroup(101,113,195,272,278,286);;
gap> DenumerantOfElementInNumericalSemigroup(1311,s);
6

gap> s:=NumericalSemigroup(101,113,195,272,278,286);;
gap> DenumerantFunction(s)(1311);
6

gap> s:=NumericalSemigroup(101,113,195,272,278,286);;
gap> 1311 in DenumerantIdeal(6,s);
false
gap> 1311 in DenumerantIdeal(5,s);
true

gap> LengthsOfFactorizationsIntegerWRTList(100,[11,13,15,19]);
[ 6, 8 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> LengthsOfFactorizationsElementWRTNumericalSemigroup(1100,s);
[ 4, 6, 8, 9 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> e := Elasticity(1100,s);
9/4
gap> Elasticity(1100,s) = Elasticity(s,1100);
true
gap> ElasticityOfFactorizationsElementWRTNumericalSemigroup(1100,s)= e;
true

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> Elasticity(s);
286/101
gap> ElasticityOfNumericalSemigroup(s);
286/101

gap> LengthsOfFactorizationsIntegerWRTList(100,[11,13,15,19]);
[ 6, 8 ]
gap> DeltaSet(last);
[ 2 ]
gap> DeltaSetOfSetOfIntegers(last2);
[ 2 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> d := DeltaSet(1100,s);
[ 1, 2 ]
gap> DeltaSet(s,1100) = d;
true
gap> DeltaSetOfFactorizationsElementWRTNumericalSemigroup(1100,s) = d;
true

gap> s:=NumericalSemigroup(5,7,11);;
gap> DeltaSetPeriodicityBoundForNumericalSemigroup(s);
60

gap> s:=NumericalSemigroup(5,7,11);;
gap> DeltaSetPeriodicityStartForNumericalSemigroup(s);
21

gap> s:=NumericalSemigroup(5,7,11);;
gap> DeltaSetListUpToElementWRTNumericalSemigroup(31,s);
[ [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ],
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [ 2 ], [  ], [  ], [ 2 ], [  ],
  [ 2 ], [  ], [ 2 ], [ 2 ], [  ] ]

gap> s:=NumericalSemigroup(5,7,11);;
gap> DeltaSetUnionUpToElementWRTNumericalSemigroup(60,s);
[ 2 ]

gap> s:=NumericalSemigroup(5,7,11);;
gap> DeltaSet(s);
[ 2 ]
gap> DeltaSetOfNumericalSemigroup(s);
[ 2 ]

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> MaximumDegree(1100,s);
9
gap> MaximumDegreeOfElementWRTNumericalSemigroup(1100,s);
9


gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> MaximalDenumerant(1100,s);
1
gap> MaximalDenumerant(s,1311);
2
gap> MaximalDenumerantOfElementInNumericalSemigroup(1311,s);
2

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], [ 5, 2, 0, 1 ],
[ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> MaximalDenumerantOfSetOfFactorizations(last);
6

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> MaximalDenumerant(s);
4
gap> MaximalDenumerantOfNumericalSemigroup(s);
4

gap> s:=NumericalSemigroup(101,113,196,272,278,286);;
gap> a := Adjustment(s);
[ 0, 12, 24, 36, 48, 60, 72, 84, 95, 96, 107, 108, 119, 120, 131, 132, 143,
  144, 155, 156, 167, 168, 171, 177, 179, 180, 183, 185, 189, 190, 191, 192,
  195, 197, 201, 203, 204, 207, 209, 213, 215, 216, 219, 221, 225, 227, 228,
  231, 233, 237, 239, 240, 243, 245, 249, 251, 252, 255, 257, 261, 263, 264,
  266, 267, 269, 273, 275, 276, 279, 280, 281, 285, 287, 288, 292, 293, 299,
  300, 304, 305, 311, 312, 316, 317, 323, 324, 328, 329, 335, 336, 340, 341,
  342, 347, 348, 352, 353, 354, 356, 359, 360, 361, 362, 364, 365, 366, 368,
  370, 371, 372, 374, 376, 377, 378, 380, 382, 383, 384, 388, 389, 390, 394,
  395, 396, 400, 401, 402, 406, 407, 408, 412, 413, 414, 418, 419, 420, 424,
  425, 426, 430, 431, 432, 436, 437, 438, 442, 444, 448, 450, 451, 454, 456,
  460, 465, 466, 472, 477, 478, 484, 489, 490, 496, 501, 502, 508, 513, 514,
  519, 520, 525, 526, 527, 531, 532, 533, 537, 539, 543, 545, 549, 551, 555,
  561, 567, 573, 579, 585, 591, 597, 603, 609, 615, 621, 622, 627, 698, 704,
  710, 716, 722 ]
gap> AdjustmentOfNumericalSemigroup(s) = a;
true



gap> l:=IrreducibleNumericalSemigroupsWithFrobeniusNumber(31);;
gap> Length(l);
109
gap> Length(Filtered(l,IsAdditiveNumericalSemigroup));
20

gap> l:=IrreducibleNumericalSemigroupsWithFrobeniusNumber(31);;
gap> Length(l);
109
gap> Length(Filtered(l,IsSuperSymmetricNumericalSemigroup));
7

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ],
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> CatenaryDegree(last);
5
gap> CatenaryDegreeOfSetOfFactorizations(last2);
5

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], [ 5, 2, 0, 1 ],
  [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> AdjacentCatenaryDegreeOfSetOfFactorizations(last);
5

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], [ 5, 2, 0, 1 ],
  [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> EqualCatenaryDegreeOfSetOfFactorizations(last);
2

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ], [ 5, 2, 0, 1 ],
  [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> MonotoneCatenaryDegreeOfSetOfFactorizations(last);
5

gap> CatenaryDegree(157,NumericalSemigroup(13,18));
0
gap> CatenaryDegree(NumericalSemigroup(13,18),1157);
18
gap> CatenaryDegreeOfElementInNumericalSemigroup(1157,NumericalSemigroup(13,18));
18

gap> FactorizationsIntegerWRTList(100,[11,13,15,19]);
[ [ 2, 6, 0, 0 ], [ 3, 4, 1, 0 ], [ 4, 2, 2, 0 ], [ 5, 0, 3, 0 ],
  [ 5, 2, 0, 1 ], [ 6, 0, 1, 1 ], [ 0, 1, 2, 3 ], [ 1, 1, 0, 4 ] ]
gap> TameDegree(last);
4
gap> TameDegreeOfSetOfFactorizations(last2);
4

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> CatenaryDegree(s);
8
gap> CatenaryDegreeOfNumericalSemigroup(s);
8

# to slow without normaliz or 4ti2
#gap> s:=NumericalSemigroup(3,5,7);;
#gap> DegreesOfEqualPrimitiveElementsOfNumericalSemigroup(s);
#[ 3, 5, 7, 10 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> EqualCatenaryDegreeOfNumericalSemigroup(s);
2

# to slow without normaliz or 4ti2
# gap> s:=NumericalSemigroup(3,5,7);;
# gap> DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup(s);
# [ 3, 5, 7, 10, 12, 14, 15, 21, 28, 35 ]

# to slow without normaliz or 4ti2
#gap> s:=NumericalSemigroup(10,23,31,44);;
#gap> CatenaryDegreeOfNumericalSemigroup(s);
#9
# to slow without normaliz or 4ti2
#gap> MonotoneCatenaryDegreeOfNumericalSemigroup(s);
#21

gap> s:=NumericalSemigroup(101,113,196,272,278,286);
<Numerical semigroup with 6 generators>
gap> TameDegree(s);
14
gap> TameDegreeOfNumericalSemigroup(s);
14

gap> s:=NumericalSemigroup(10,11,13);;
gap> TameDegree(100,s);
5
gap> TameDegree(s,100);
5
gap> TameDegreeOfElementInNumericalSemigroup(100,s);
5

gap> s:=NumericalSemigroup(10,11,13);;
gap> OmegaPrimality(100,s);
13
gap> OmegaPrimality(s,100);
13
gap> OmegaPrimalityOfElementInNumericalSemigroup(100,s);
13

gap> s:=NumericalSemigroup(10,11,13);;
gap> l:=FirstElementsOfNumericalSemigroup(100,s);;
gap> List(l,x->OmegaPrimalityOfElementInNumericalSemigroup(x,s));
[ 0, 4, 5, 5, 4, 6, 7, 6, 6, 6, 6, 7, 8, 7, 7, 7, 7, 7, 8, 7, 8, 9, 8, 8, 8,
  8, 8, 8, 8, 9, 9, 10, 9, 9, 9, 9, 9, 9, 9, 9, 10, 11, 10, 10, 10, 10, 10,
  10, 10, 10, 11, 12, 11, 11, 11, 11, 11, 11, 11, 11, 12, 13, 12, 12, 12, 12,
  12, 12, 12, 12, 13, 14, 13, 13, 13, 13, 13, 13, 13, 13, 14, 15, 14, 14, 14,
  14, 14, 14, 14, 14, 15, 16, 15, 15, 15, 15, 15, 15, 15, 15 ]
gap> OmegaPrimalityOfElementListInNumericalSemigroup(l,s);
[ 0, 4, 5, 5, 4, 6, 7, 6, 6, 6, 6, 7, 8, 7, 7, 7, 7, 7, 8, 7, 8, 9, 8, 8, 8,
  8, 8, 8, 8, 9, 9, 10, 9, 9, 9, 9, 9, 9, 9, 9, 10, 11, 10, 10, 10, 10, 10,
  10, 10, 10, 11, 12, 11, 11, 11, 11, 11, 11, 11, 11, 12, 13, 12, 12, 12, 12,
  12, 12, 12, 12, 13, 14, 13, 13, 13, 13, 13, 13, 13, 13, 14, 15, 14, 14, 14,
  14, 14, 14, 14, 14, 15, 16, 15, 15, 15, 15, 15, 15, 15, 15 ]

gap> s:=NumericalSemigroup(10,11,13);;
gap> OmegaPrimality(s);
5
gap> OmegaPrimalityOfNumericalSemigroup(s);
5

gap> s:=NumericalSemigroup(10,11,13);;
gap> BelongsToHomogenizationOfNumericalSemigroup([10,23],s);
true
gap> BelongsToHomogenizationOfNumericalSemigroup([1,23],s);
false

gap> s:=NumericalSemigroup(10,11,13);;
gap> FactorizationsInHomogenizationOfNumericalSemigroup([20,230],s);
[ [ 0, 0, 15, 5 ], [ 0, 2, 12, 6 ], [ 0, 4, 9, 7 ], [ 0, 6, 6, 8 ],
  [ 0, 8, 3, 9 ], [ 0, 10, 0, 10 ], [ 1, 1, 7, 11 ], [ 1, 3, 4, 12 ],
  [ 1, 5, 1, 13 ], [ 2, 0, 2, 16 ] ]
gap> FactorizationsElementWRTNumericalSemigroup(230,s);
[ [ 23, 0, 0 ], [ 12, 10, 0 ], [ 1, 20, 0 ], [ 14, 7, 1 ], [ 3, 17, 1 ],
  [ 16, 4, 2 ], [ 5, 14, 2 ], [ 18, 1, 3 ], [ 7, 11, 3 ], [ 9, 8, 4 ],
  [ 11, 5, 5 ], [ 0, 15, 5 ], [ 13, 2, 6 ], [ 2, 12, 6 ], [ 4, 9, 7 ],
  [ 6, 6, 8 ], [ 8, 3, 9 ], [ 10, 0, 10 ], [ 1, 7, 11 ], [ 3, 4, 12 ],
  [ 5, 1, 13 ], [ 0, 2, 16 ] ]

gap> s:=NumericalSemigroup(10,17,19);;
gap> BettiElements(s);
[ 57, 68, 70 ]
gap> HomogeneousBettiElementsOfNumericalSemigroup(s);
[ [ 5, 57 ], [ 5, 68 ], [ 6, 95 ], [ 7, 70 ], [ 9, 153 ] ]

gap> s:=NumericalSemigroup(10,17,19);;
gap> CatenaryDegree(s);
7
gap> HomogeneousCatenaryDegreeOfNumericalSemigroup(s);
9

gap> s:=NumericalSemigroup(3,5,7);;
gap> MoebiusFunctionAssociatedToNumericalSemigroup(s,10);
2
gap> MoebiusFunctionAssociatedToNumericalSemigroup(s,34);
25

gap> s:=NumericalSemigroup(5,7,11);;
gap> DivisorsOfElementInNumericalSemigroup(s,20);
[ 0, 5, 10, 15, 20 ]
gap> DivisorsOfElementInNumericalSemigroup(20,s);
[ 0, 5, 10, 15, 20 ]
gap> DivisorsOfElementInNumericalSemigroup(0,s);
[ 0 ]

gap> NuSequence:=S->List([1..2*Conductor(S)-Genus(S)], i->Length(DivisorsOfElementInNumericalSemigroup(S[i],S)));;
gap> s:=NumericalSemigroup(5,7,11);;
gap> NuSequence(s);
[ 1, 2, 2, 3, 2, 4, 3, 4, 4, 6, 4, 6, 5, 8, 9, 8, 9, 10, 12, 12 ]
gap> s=NumericalSemigroupByNuSequence(last);
true

gap> TauNS := function(i,S)
>    local d, D, si;
>    D:=DivisorsOfElementInNumericalSemigroup(S[i+1],S);
>    si:=S[i+1];
>    d:=Maximum(Intersection(D,[0..Int(si/2)]));
>    return NumberElement_NumericalSemigroup(S,d)-1;
> end;;
gap> TauSequence:=S->List([0..(2*Conductor(S)-Genus(S)+1)], i->TauNS(i,S));;
gap> s:=NumericalSemigroup(6,7,8,17);;
gap> s=NumericalSemigroupByTauSequence(TauSequence(s));
true

gap> S := NumericalSemigroup(7,9,17);;
gap> FengRaoDistance(S,6,100);
86

gap> S := NumericalSemigroup(7,8,17);;
gap> FengRaoNumber(S,209);
224
gap> FengRaoNumber(209,S);
224

##polynomial.xml

gap> x:=X(Rationals,"x");;
gap> s:=NumericalSemigroup(5,7,9);;
gap> NumericalSemigroupPolynomial(s,x);
x^14-x^13+x^12-x^11+x^9-x^8+x^7-x^6+x^5-x+1

gap> x:=X(Rationals,"x");;
gap> s:=NumericalSemigroup(5,6,7,8);;
gap> f:=NumericalSemigroupPolynomial(s,x);
x^10-x^9+x^5-x+1
gap> IsNumericalSemigroupPolynomial(f);
true

gap> x:=X(Rationals,"x");;
gap> s:=NumericalSemigroup(5,6,7,8);;
gap> f:=NumericalSemigroupPolynomial(s,x);
x^10-x^9+x^5-x+1
gap> NumericalSemigroupFromNumericalSemigroupPolynomial(f)=s;
true

gap> x:=X(Rationals,"x");;
gap> s:=NumericalSemigroup(5,7,9);;
gap> HilbertSeriesOfNumericalSemigroup(s,x);
(x^14-x^13+x^12-x^11+x^9-x^8+x^7-x^6+x^5-x+1)/(-x+1)

gap> x:=Indeterminate(Rationals,1);; SetName(x,"x");
gap> GraeffePolynomial(x^2-1);
x^2-2*x+1

gap> CyclotomicPolynomial(Rationals,3);
x^2+x+1
gap> IsCyclotomicPolynomial(last);
true

gap> x:=X(Rationals,"x");;
gap>  s:=NumericalSemigroup(3,5,7);;
gap>  t:=NumericalSemigroup(4,6,9);;
gap> p:=NumericalSemigroupPolynomial(s,x);
x^5-x^4+x^3-x+1
gap> q:=NumericalSemigroupPolynomial(t,x);
x^12-x^11+x^8-x^7+x^6-x^5+x^4-x+1
gap> IsKroneckerPolynomial(p);
false
gap> IsKroneckerPolynomial(q);
true

gap> l:=CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber(21);;
gap> ForAll(l,IsCyclotomicNumericalSemigroup);
true

gap> l:=IrreducibleNumericalSemigroupsWithFrobeniusNumber(13);;
gap> x:=X(Rationals,"x");;
gap> ForAll(l, s->
> IsSelfReciprocalUnivariatePolynomial(NumericalSemigroupPolynomial(s,x)));
true

gap> s:=NumericalSemigroup(3,4);;
gap> CyclotomicExponentSequence(s,20);
[ 1, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1 ]
gap> s:=NumericalSemigroup(3,5,7);;
gap> CyclotomicExponentSequence(s,20);
[ 1, 0, -1, 0, -1, 0, -1, 0, 0, 1, 0, 1, 0, 1, 0, 0, -1, 0, -1, 0 ]

gap> s:=NumericalSemigroup(3,4);;
gap> x:=Indeterminate(Rationals,"x");;
gap> p:=NumericalSemigroupPolynomial(s,x);;
gap> WittCoefficients(p,20);
[ 1, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]


# Semigroup of values of algebraic curves
gap> x:=Indeterminate(Rationals,1);; SetName(x,"x");
gap> y:=Indeterminate(Rationals,2);; SetName(y,"y");
gap> f:=((y^3-x^2)^2-x*y^2)^4-(y^3-x^2);;
gap> SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f,"all");
[ [ 24, 16, 28, 7 ], [ y, y^3-x^2, y^6-2*x^2*y^3+x^4-x*y^2 ] ]

gap> IsDeltaSequence([24,16,28,7]);
true

gap> DeltaSequencesWithFrobeniusNumber(21);
[ [ 8, 6, 11 ], [ 10, 4, 15 ], [ 12, 8, 6, 11 ], [ 14, 4, 11 ],
  [ 15, 10, 4 ], [ 23, 2 ] ]

gap> CurveAssociatedToDeltaSequence([24,16,28,7]);
y^24-8*x^2*y^21+28*x^4*y^18-56*x^6*y^15-4*x*y^20+70*x^8*y^12+24*x^3*y^17-56*x^\
10*y^9-60*x^5*y^14+28*x^12*y^6+80*x^7*y^11+6*x^2*y^16-8*x^14*y^3-60*x^9*y^8-24\
*x^4*y^13+x^16+24*x^11*y^5+36*x^6*y^10-4*x^13*y^2-24*x^8*y^7-4*x^3*y^12+6*x^10\
*y^4+8*x^5*y^9-4*x^7*y^6+x^4*y^8-y^3+x^2
gap> SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(last,"all");
[ [ 24, 16, 28, 7 ], [ y, y^3-x^2, y^6-2*x^2*y^3+x^4-x*y^2 ] ]

# NEEDS singular
# gap> x:=X(Rationals,"x");; y:=X(Rationals,"y");;
# gap> f:= y^4-2*x^3*y^2-4*x^5*y+x^6-x^7;
# -x^7+x^6-4*x^5*y-2*x^3*y^2+y^4
# gap> SemigroupOfValuesOfPlaneCurve(f);
# <Numerical semigroup with 3 generators>
# gap> MinimalGenerators(last);
# [ 4, 6, 13 ]
# gap> f:=(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)*(y^2-x^3);;
# gap> SemigroupOfValuesOfPlaneCurve(f);
# <Good semigroup>
# gap> MinimalGenerators(last);
# [ [ 4, 2 ], [ 6, 3 ], [ 13, 15 ], [ 29, 13 ] ]

gap> x:=Indeterminate(Rationals,"x");;
gap> SemigroupOfValuesOfCurve_Local([x^4,x^6+x^7,x^13]);
<Numerical semigroup with 4 generators>
gap> MinimalGeneratingSystem(last);
[ 4, 6, 13, 15 ]
gap> SemigroupOfValuesOfCurve_Local([x^4,x^6+x^7,x^13], "basis");
[ x^4, x^7+x^6, x^13, x^15 ]
gap> SemigroupOfValuesOfCurve_Local([x^4,x^6+x^7,x^13], 20);
x^20

gap> x:=Indeterminate(Rationals,"x");;
gap> SemigroupOfValuesOfCurve_Global([x^4,x^6+x^7,x^13]);
<Numerical semigroup with 3 generators>
gap> MinimalGeneratingSystem(last);
[ 4, 7, 13 ]
gap> SemigroupOfValuesOfCurve_Global([x^4,x^6+x^7,x^13],"basis");
[ x^4, x^7+x^6, x^13 ]
gap> SemigroupOfValuesOfCurve_Global([x^4,x^6+x^7,x^13],12);
x^12
gap> SemigroupOfValuesOfCurve_Global([x^4,x^6+x^7,x^13],6);
fail

gap> t:=Indeterminate(Rationals,"t");;
gap> A:=[t^6+t,t^4];;
gap> M:=[t^3,t^4];;
gap> GeneratorsModule_Global(A,M);
[ t^3, t^4, t^5, t^6 ]

gap> t:=Indeterminate(Rationals,"t");;
gap> GeneratorsKahlerDifferentials([t^3,t^4]);
[ t^2, t^3 ]

gap> IsMonomialNumericalSemigroup(NumericalSemigroup(4,6,7));
true
gap> IsMonomialNumericalSemigroup(NumericalSemigroup(4,6,11));
false

##affine.xml

gap> s1 := AffineSemigroup([1,3],[7,2],[1,5]);
<Affine semigroup in 2 dimensional space, with 3 generators>
gap> s2 := AffineSemigroup([[1,3],[7,2],[1,5]]);;
gap> s3 := AffineSemigroupByGenerators([1,3],[7,2],[1,5]);;
gap> s4 := AffineSemigroupByGenerators([[1,3],[7,2],[1,5]]);;
gap> s5 := AffineSemigroup("generators",[[1,3],[7,2],[1,5]]);;
gap> Length(Set([s1,s2,s3,s4,s5]));
1

gap> s1 := AffineSemigroup("equations",[[[-2,1]],[3]]);
<Affine semigroup>
gap> s2 := AffineSemigroupByEquations([[[-2,1]],[3]]);
<Affine semigroup>
gap> s1=s2;
true

gap> a1:=AffineSemigroup("inequalities",[[2,-1],[-1,3]]);
<Affine semigroup>
gap> a2:=AffineSemigroupByInequalities([[2,-1],[-1,3]]);
<Affine semigroup>
gap> a1=a2;
true

gap> s:=AffineSemigroupByPMInequality([0, 1, 1, 0, -1], 4, [1, 0, -2, -3, 1]);
<Affine semigroup>
gap> [ 3, 0, 0, 4, 12 ] in s;
true
gap> [ 3, 0, 0, 4, 1 ] in s;
false

gap> gaps := [[1,0,0,0],[1,1,0,0],[2,0,0,0],[2,1,0,0],[5,0,0,0]];;
gap> a1 := AffineSemigroup("gaps", gaps );
<Affine semigroup>
gap> a2 := AffineSemigroupByGaps( gaps );
<Affine semigroup>
gap> a1 = a2;
true
gap> Generators(a1);;
gap> Set(last);
[ [ 0, 0, 0, 1 ], [ 0, 0, 1, 0 ], [ 0, 1, 0, 0 ], [ 1, 0, 0, 1 ], 
  [ 1, 0, 1, 0 ], [ 1, 2, 0, 0 ], [ 2, 0, 0, 1 ], [ 2, 0, 1, 0 ], 
  [ 2, 2, 0, 0 ], [ 3, 0, 0, 0 ], [ 4, 0, 0, 0 ], [ 5, 1, 0, 0 ] ]

gap> a:=AffineSemigroup([[1,0,0,0],[3,1,0,0],[1,2,0,0],[0,0,1,0],
> [0,2,1,0],[0,1,1,0],[0,0,0,1],[0,2,0,1],[0,1,0,1],[0,3,0,0],
> [0,5,0,0],[0,4,0,0]]);
<Affine semigroup in 4 dimensional space, with 12 generators>
gap> Set(Gaps(a));
[ [ 0, 1, 0, 0 ], [ 0, 2, 0, 0 ], [ 1, 1, 0, 0 ], [ 2, 1, 0, 0 ] ]
gap> n := AffineSemigroup([1,1],[0,1]);;
gap> Gaps(n);
#I  The given affine semigroup has infinitely many gaps
fail

gap> a:=AffineSemigroup([[1,0,0,0],[3,1,0,0],[1,2,0,0],[0,0,1,0],
> [0,2,1,0],[0,1,1,0],[0,0,0,1],[0,2,0,1],[0,1,0,1],[0,3,0,0],
> [0,5,0,0]]);
<Affine semigroup in 4 dimensional space, with 11 generators>
gap> Genus(a);
7
gap> n := AffineSemigroup([1,1],[0,1]);;
gap> Genus(n);
#I  The given affine semigroup has infinitely many gaps
infinity
gap> last > 10^50;
true

gap> a:=AffineSemigroup([[1,0,0,0],[3,1,0,0],[1,2,0,0],[0,0,1,0],
> [0,2,1,0],[0,1,1,0],[0,0,0,1],[0,2,0,1],[0,1,0,1],[0,3,0,0],
> [0,5,0,0],[0,4,0,0]]);
<Affine semigroup in 4 dimensional space, with 12 generators>
gap> PseudoFrobenius(a);
[ [ 0, 2, 0, 0 ], [ 2, 1, 0, 0 ] ]

gap> a:=AffineSemigroup([[1,0,0,0],[3,1,0,0],[1,2,0,0],[0,0,1,0],
> [0,2,1,0],[0,1,1,0],[0,0,0,1],[0,2,0,1],[0,1,0,1],[0,3,0,0],
> [0,5,0,0],[0,4,0,0]]);
<Affine semigroup in 4 dimensional space, with 12 generators>
gap> SpecialGaps(a);
[ [ 0, 2, 0, 0 ], [ 2, 1, 0, 0 ] ]

gap> a:=AffineSemigroup([[1,0],[0,1],[1,1]]);
<Affine semigroup in 2 dimensional space, with 3 generators>
gap> Generators(a);
[ [ 0, 1 ], [ 1, 0 ], [ 1, 1 ] ]

gap> a:=AffineSemigroup([[1,0],[0,1],[1,1]]);
<Affine semigroup in 2 dimensional space, with 3 generators>
gap> MinimalGenerators(a);
[ [ 0, 1 ], [ 1, 0 ] ]

gap> a:=AffineSemigroup([2,0],[0,4]);
<Affine semigroup in 2 dimensional space, with 2 generators>
gap> b:=RemoveMinimalGeneratorFromAffineSemigroup([2,0],a);Generators(b);
<Affine semigroup in 2 dimensional space, with 4 generators>
[ [ 0, 4 ], [ 2, 4 ], [ 4, 0 ], [ 6, 0 ] ]

gap> s:=AffineSemigroup([[2,0],[3,0],[0,4],[0,5],[1,1]]);
<Affine semigroup in 2 dimensional space, with 5 generators>
gap> t:=AddSpecialGapOfAffineSemigroup([1,12],s);
<Affine semigroup in 2 dimensional space, with 6 generators>
gap> Gaps(s);
[ [ 0, 1 ], [ 0, 2 ], [ 0, 3 ], [ 0, 6 ], [ 0, 7 ], [ 0, 11 ], [ 1, 0 ], [ 1, 2 ], [ 1, 3 ], [ 1, 4 ],
[ 1, 7 ], [ 1, 8 ], [ 1, 12 ], [ 2, 1 ], [ 2, 3 ], [ 3, 2 ], [ 4, 3 ] ]
gap> Gaps(t);
[ [ 0, 1 ], [ 0, 2 ], [ 0, 3 ], [ 0, 6 ], [ 0, 7 ], [ 0, 11 ], [ 1, 0 ], [ 1, 2 ], [ 1, 3 ], [ 1, 4 ],
[ 1, 7 ], [ 1, 8 ], [ 2, 1 ], [ 2, 3 ], [ 3, 2 ], [ 4, 3 ] ]

gap>  s:=NumericalSemigroup(1310,1411,1546,1601);
<Numerical semigroup with 4 generators>
gap> a:=AsAffineSemigroup(s);
<Affine semigroup in 1 dimensional space, with 4 generators>
gap> GeneratorsOfAffineSemigroup(a);
[ [ 1310 ], [ 1411 ], [ 1546 ], [ 1601 ] ]

gap> a1:=AffineSemigroup([[3,0],[2,1],[1,2],[0,3]]);
<Affine semigroup in 2 dimensional space, with 4 generators>
gap> IsAffineSemigroupByEquations(a1);
false
gap> IsAffineSemigroupByGenerators(a1);
true
gap> ns := NumericalSemigroup(3,5);
<Numerical semigroup with 2 generators>
gap> IsAffineSemigroup(ns);
false
gap> as := AsAffineSemigroup(ns);
<Affine semigroup in 1 dimensional space, with 2 generators>
gap> IsAffineSemigroup(as);
true

gap> a:=AffineSemigroup([[2,0],[0,2],[1,1]]);;
gap> BelongsToAffineSemigroup([5,5],a);
true
gap> BelongsToAffineSemigroup([1,2],a);
false
gap> [5,5] in a;
true
gap> [1,2] in a;
false

gap> a:=AffineSemigroup("equations",[[[1,1,1],[0,0,2]],[2,2]]);;
gap> IsFull(a);
true
gap> IsFullAffineSemigroup(a);
true

gap> HilbertBasisOfSystemOfHomogeneousEquations([[1,0,1],[0,1,-1]],[2]);
[ [ 0, 2, 2 ], [ 1, 1, 1 ], [ 2, 0, 0 ] ]

gap> HilbertBasisOfSystemOfHomogeneousInequalities([[2,-3],[0,1]]);
[ [ 1, 0 ], [ 2, 1 ], [ 3, 2 ] ]

gap> EquationsOfGroupGeneratedBy([[1,2,0],[2,-2,2]]);
[ [ [ 0, 0, -1 ], [ -2, 1, 3 ] ], [ 2 ] ]

gap> BasisOfGroupGivenByEquations([[0,0,1],[2,-1,-3]],[2]);
[ [ -1, -2, 0 ], [ -2, 2, -2 ] ]

gap> a1:=AffineSemigroup([[2,0],[0,2]]);
<Affine semigroup in 2 dimensional space, with 2 generators>
gap> a2:=AffineSemigroup([[1,1]]);
<Affine semigroup in 2 dimensional space, with 1 generator>
gap> GluingOfAffineSemigroups(a1,a2);
<Affine semigroup in 2 dimensional space, with 3 generators>
gap> Generators(last);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ] ]

gap> s:=NumericalSemigroup(4,6,9);;
gap> CircuitsOfKernelCongruence([[4],[6],[9]]);
[ [ [ 3, 0, 0 ], [ 0, 2, 0 ] ], [ [ 9, 0, 0 ], [ 0, 0, 4 ] ], [ [ 0, 3, 0 ], [ 0, 0, 2 ] ] ]
gap> MinimalPresentation(s);
[ [ [ 0, 0, 2 ], [ 0, 3, 0 ] ], [ [ 0, 2, 0 ], [ 3, 0, 0 ] ] ]

gap> PrimitiveRelationsOfKernelCongruence([[4],[6],[9]]);
[ [ [ 0, 0, 2 ], [ 0, 3, 0 ] ], [ [ 0, 0, 2 ], [ 3, 1, 0 ] ], 
  [ [ 0, 0, 4 ], [ 9, 0, 0 ] ], [ [ 0, 1, 2 ], [ 6, 0, 0 ] ], 
  [ [ 0, 2, 0 ], [ 3, 0, 0 ] ] ]

gap> M := [[2,0],[0,2],[1,1]];
[ [ 2, 0 ], [ 0, 2 ], [ 1, 1 ] ]
gap> GeneratorsOfKernelCongruence(M);
[ [ [ 0, 0, 2 ], [ 1, 1, 0 ] ] ]

gap> M:=[[3],[5],[7]];;
gap> CanonicalBasisOfKernelCongruence(M,MonomialLexOrdering());
[ [ [ 0, 7, 0 ], [ 0, 0, 5 ] ], [ [ 1, 0, 1 ], [ 0, 2, 0 ] ],
[ [ 1, 5, 0 ], [ 0, 0, 4 ] ], [ [ 2, 3, 0 ], [ 0, 0, 3 ] ],
[ [ 3, 1, 0 ], [ 0, 0, 2 ] ], [ [ 4, 0, 0 ], [ 0, 1, 1 ] ] ]
gap> CanonicalBasisOfKernelCongruence(M,MonomialGrlexOrdering());
[ [ [ 0, 7, 0 ], [ 0, 0, 5 ] ], [ [ 1, 0, 1 ], [ 0, 2, 0 ] ],
[ [ 1, 5, 0 ], [ 0, 0, 4 ] ], [ [ 2, 3, 0 ], [ 0, 0, 3 ] ],
[ [ 3, 1, 0 ], [ 0, 0, 2 ] ], [ [ 4, 0, 0 ], [ 0, 1, 1 ] ] ]
gap> CanonicalBasisOfKernelCongruence(M,MonomialGrevlexOrdering());
[ [ [ 0, 2, 0 ], [ 1, 0, 1 ] ], [ [ 3, 1, 0 ], [ 0, 0, 2 ] ],
[ [ 4, 0, 0 ], [ 0, 1, 1 ] ] ]

gap> gr:=GraverBasis([[3,5,7]]);
[ [ -7, 0, 3 ], [ -5, 3, 0 ], [ -4, 1, 1 ], [ -3, -1, 2 ], [ -2, -3, 3 ],
  [ -1, -5, 4 ], [ -1, 2, -1 ], [ 0, -7, 5 ], [ 0, 7, -5 ], [ 1, -2, 1 ],
  [ 1, 5, -4 ], [ 2, 3, -3 ], [ 3, 1, -2 ], [ 4, -1, -1 ], [ 5, -3, 0 ],
  [ 7, 0, -3 ] ]

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> MinimalPresentation(a);
[ [ [ 0, 2, 0 ], [ 1, 0, 1 ] ] ]
gap> MinimalPresentationOfAffineSemigroup(a);
[ [ [ 0, 2, 0 ], [ 1, 0, 1 ] ] ]

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> BettiElements(a);
[ [ 2, 2 ] ]
gap> BettiElementsOfAffineSemigroup(a);
[ [ 2, 2 ] ]

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> DegreesOfPrimitiveElementsOfAffineSemigroup(a);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ], [ 2, 2 ] ]

gap> Set(FactorizationsVectorWRTList([5,5],[[2,0],[0,2],[1,1]]));
[ [ 0, 0, 5 ], [ 1, 1, 3 ], [ 2, 2, 1 ] ]

gap> Set(FactorizationsVectorWRTList([7000],[[101],[102],[303]]));
[ [ 2, 31, 12 ], [ 5, 31, 11 ], [ 8, 31, 10 ], [ 11, 31, 9 ], [ 14, 31, 8 ], 
  [ 17, 31, 7 ], [ 20, 31, 6 ], [ 23, 31, 5 ], [ 26, 31, 4 ], [ 29, 31, 3 ], 
  [ 32, 31, 2 ], [ 35, 31, 1 ], [ 38, 31, 0 ] ]

gap> a:=AffineSemigroup([[2,0],[0,2],[1,1]]);;
gap> Elasticity([5,5],a);
1
gap> Elasticity(a,[5,5]);
1
gap> ElasticityOfFactorizationsElementWRTAffineSemigroup([5,5],a);
1

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> Elasticity(a);
1
gap> ElasticityOfAffineSemigroup(a);
1

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> DeltaSet(a);
[  ]
gap> s:=NumericalSemigroup(10,13,15,47);;
gap> a:=AsAffineSemigroup(s);;
gap> DeltaSetOfAffineSemigroup(a);
[ 1, 2, 3, 5 ]

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> CatenaryDegree(a);
2
gap> CatenaryDegreeOfAffineSemigroup(a);
2

gap> a:=AffineSemigroup("inequalities",[[2,-1],[-1,3]]);
<Affine semigroup>
gap> GeneratorsOfAffineSemigroup(a);
[ [ 1, 1 ], [ 1, 2 ], [ 2, 1 ], [ 3, 1 ] ]
gap> CatenaryDegreeOfAffineSemigroup(a);
3
gap> EqualCatenaryDegreeOfAffineSemigroup(a);
2
gap> HomogeneousCatenaryDegreeOfAffineSemigroup(a);
3
gap> MonotoneCatenaryDegreeOfAffineSemigroup(a);
3

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> TameDegree(a);
2
gap> TameDegreeOfAffineSemigroup(a);
2

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> OmegaPrimality([5,5],a);
6
gap> OmegaPrimalityOfElementInAffineSemigroup([5,5],a);
6

gap> a:=AffineSemigroup([2,0],[0,2],[1,1]);;
gap> OmegaPrimality(a);
2
gap> OmegaPrimalityOfAffineSemigroup(a);
2

# ideals of affine semigroups

gap> a:=AffineSemigroup([2,0],[0,2]);;
gap> i:=IdealOfAffineSemigroup([[1,0],[0,3]],a);
<Ideal of affine semigroup>
gap> [[1,0],[0,3]]+a=i;
true
gap> [0,1]+a;
<Ideal of affine semigroup>
gap> IsSubset(i,[1,0]+a);
true
gap> IsSubset([1,0]+a,i);
false
gap> IsIdealOfAffineSemigroup(i);
true
gap> i:=[[1,0],[3,0]]+AffineSemigroup([2,0],[0,2]);;
gap> Generators(i);
[ [ 1, 0 ], [ 3, 0 ] ]
gap> MinimalGenerators(i);
[ [ 1, 0 ] ]
gap> AmbientAffineSemigroupOfIdeal(i)=a;
true
gap> IsIntegral([1,0]+a);
false
gap> IsIntegral([2,0]+a);
true
gap> i:=[2,0]+a;;
gap> [2,0] in i;
true
gap> [4,4] in i;
true
gap> [1,2] in i;
false
gap> j:=[[1,0],[0,1]]+a;;
gap> MinimalGenerators(i+j);
[ [ 2, 1 ], [ 3, 0 ] ]
gap> j:=[[1,0],[0,1]]+a;;
gap> MinimalGenerators(2*j);
[ [ 0, 2 ], [ 1, 1 ], [ 2, 0 ] ]
gap> j:=[[1,0],[0,1]]+a;;
gap> MinimalGenerators([2,2]+j);
[ [ 2, 3 ], [ 3, 2 ] ]
gap> i:=[2,0]+a;;
gap> j:=[[1,0],[0,1]]+a;;
gap> MinimalGenerators(Union(i,j));
[ [ 0, 1 ], [ 1, 0 ], [ 2, 0 ] ]
gap> a:=AffineSemigroup([1,0],[0,1]);;
gap> i:=[2,0]+a;;
gap> j:=[[1,0],[0,1]]+a;;
gap> MinimalGenerators(Intersection(i,j));
[ [ 2, 0 ] ]
gap> a:=AffineSemigroup([2,0],[0,2]);;
gap> MinimalGenerators(MaximalIdeal(a));
[ [ 0, 2 ], [ 2, 0 ] ]



##good-semigroups.xml

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> l:=Cartesian([1..11],[1..11]);;
gap> Intersection(dup,l);
[ [ 3, 3 ], [ 5, 5 ], [ 6, 6 ], [ 6, 7 ], [ 6, 8 ], [ 6, 9 ], [ 6, 10 ],
  [ 6, 11 ], [ 7, 6 ], [ 7, 7 ], [ 8, 6 ], [ 8, 8 ], [ 9, 6 ], [ 9, 9 ],
  [ 9, 10 ], [ 9, 11 ], [ 10, 6 ], [ 10, 9 ], [ 10, 10 ], [ 11, 6 ],
  [ 11, 9 ], [ 11, 11 ] ]
gap> [384938749837,349823749827] in dup;
true

gap> s:=NumericalSemigroup(2,3);;
gap> t:=NumericalSemigroup(3,4);;
gap> e:=3+t;;
gap> dup:=AmalgamationOfNumericalSemigroups(s,e,2);;
gap> [2,3] in dup;
true

gap> s:=NumericalSemigroup(2,3);;
gap> t:=NumericalSemigroup(3,4);;
gap> IsGoodSemigroup(CartesianProductOfNumericalSemigroups(s,t));
true

gap> G:=[[4,3],[7,13],[11,17],[14,27],[15,27],[16,20],[25,12],[25,16]];
[ [ 4, 3 ], [ 7, 13 ], [ 11, 17 ], [ 14, 27 ], [ 15, 27 ], [ 16, 20 ],
  [ 25, 12 ], [ 25, 16 ] ]
gap> C:=[25,27];
[ 25, 27 ]
gap> GoodSemigroup(G,C);
<Good semigroup>

gap> s:=NumericalSemigroup(2,3);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);;
gap> BelongsToGoodSemigroup([2,2],dup);
true
gap> [2,2] in dup;
true
gap> [3,2] in dup;
false

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> Conductor(dup);
[ 11, 11 ]
gap> ConductorOfGoodSemigroup(dup);
[ 11, 11 ]

gap> s:=GoodSemigroup([[2,2],[3,3]],[4,4]);
<Good semigroup>
gap> Multiplicity(s);
[ 2, 2 ]
gap> IsLocal(s);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> SmallElementsOfGoodSemigroup(dup);
[ [ 0, 0 ], [ 3, 3 ], [ 5, 5 ], [ 6, 6 ], [ 6, 7 ], [ 6, 8 ], [ 6, 9 ],
  [ 6, 10 ], [ 6, 11 ], [ 7, 6 ], [ 7, 7 ], [ 8, 6 ], [ 8, 8 ], [ 9, 6 ],
  [ 9, 9 ], [ 9, 10 ], [ 9, 11 ], [ 10, 6 ], [ 10, 9 ], [ 10, 10 ],
  [ 11, 6 ], [ 11, 9 ], [ 11, 11 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> SmallElements(dup);
[ [ 0, 0 ], [ 3, 3 ], [ 5, 5 ], [ 6, 6 ], [ 6, 7 ], [ 6, 8 ], [ 6, 9 ],
  [ 6, 10 ], [ 6, 11 ], [ 7, 6 ], [ 7, 7 ], [ 8, 6 ], [ 8, 8 ], [ 9, 6 ],
  [ 9, 9 ], [ 9, 10 ], [ 9, 11 ], [ 10, 6 ], [ 10, 9 ], [ 10, 10 ],
  [ 11, 6 ], [ 11, 9 ], [ 11, 11 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> SmallElementsOfGoodSemigroup(dup);
[ [ 0, 0 ], [ 3, 3 ], [ 5, 5 ], [ 6, 6 ], [ 6, 7 ], [ 6, 8 ], [ 6, 9 ], [ 6, 10 ],
  [ 6, 11 ], [ 7, 6 ], [ 7, 7 ], [ 8, 6 ], [ 8, 8 ], [ 9, 6 ], [ 9, 9 ], [ 9, 10 ],
  [ 9, 11 ], [ 10, 6 ], [ 10, 9 ], [ 10, 10 ], [ 11, 6 ], [ 11, 9 ], [ 11, 11 ] ]
gap> RepresentsSmallElementsOfGoodSemigroup(last);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> SmallElementsOfGoodSemigroup(dup);
[ [ 0, 0 ], [ 3, 3 ], [ 5, 5 ], [ 6, 6 ], [ 6, 7 ], [ 6, 8 ], [ 6, 9 ], [ 6, 10 ],
  [ 6, 11 ], [ 7, 6 ], [ 7, 7 ], [ 8, 6 ], [ 8, 8 ], [ 9, 6 ], [ 9, 9 ], [ 9, 10 ],
  [ 9, 11 ], [ 10, 6 ], [ 10, 9 ], [ 10, 10 ], [ 11, 6 ], [ 11, 9 ], [ 11, 11 ] ]
gap> G:=GoodSemigroupBySmallElements(last);
<Good semigroup>
gap> dup=G;
true

gap> G:=[[4,3],[7,13],[11,17]];;
gap> g:=GoodSemigroup(G,[11,17]);;
gap> mx:=MaximalElementsOfGoodSemigroup(g);
[ [ 0, 0 ], [ 4, 3 ], [ 7, 13 ], [ 8, 6 ] ]

gap> G:=[[4,3],[7,13],[11,17]];;
gap> g:=GoodSemigroup(G,[11,17]);;
gap> IrreducibleMaximalElementsOfGoodSemigroup(g);
[ [ 4, 3 ], [ 7, 13 ] ]

gap> G:=[[4,3],[7,13],[11,17]];;
gap> g:=GoodSemigroup(G,[11,17]);;
gap> sm:=SmallElements(g);;
gap> mx:=MaximalElementsOfGoodSemigroup(g);;
gap> s:=NumericalSemigroupBySmallElements(Set(sm,x->x[1]));;
gap> t:=NumericalSemigroupBySmallElements(Set(sm,x->x[2]));;
gap> Conductor(g);
[ 11, 15 ]
gap> gg:=GoodSemigroupByMaximalElements(s,t,mx,[11,15]);
<Good semigroup>
gap> gg=g;
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=6+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);
<Good semigroup>
gap> MinimalGoodGeneratingSystemOfGoodSemigroup(dup);
[ [ 3, 3 ], [ 5, 5 ], [ 6, 11 ], [ 7, 7 ], [ 11, 6 ] ]
gap> MinimalGoodGenerators(dup);
[ [ 3, 3 ], [ 5, 5 ], [ 6, 11 ], [ 7, 7 ], [ 11, 6 ] ]

## Nicola's implementations

gap> smalls := [ [ 0, 0 ], [ 4, 5 ], [ 4, 6 ], [ 8, 5 ],
> [ 8, 7 ], [ 8, 8 ], [ 8, 10 ], [ 11, 5 ], [ 11, 7], [ 11, 8 ], [ 11, 10 ], 
> [ 12, 5 ], [ 12, 7 ], [ 12, 8 ], [ 12, 10 ], [ 15, 5 ], [ 15, 7 ], [ 15, 8 ],
> [ 15, 10 ], [ 16, 5 ], [ 16, 7 ], [ 16, 8 ], [ 16, 10 ], [ 18, 5 ], [ 19, 7 ],
> [ 19, 8 ], [ 19, 10 ], [ 20, 7 ], [ 20, 8 ], [ 20, 10 ], [ 22, 7 ], [ 22, 8 ],
> [ 22, 10 ], [ 23, 7 ], [ 23, 8 ], [ 23, 10 ], [ 24, 7 ], [ 24, 8 ], [ 24, 10 ],
> [ 25, 7 ], [ 25, 8 ], [ 26, 7 ], [ 26, 10 ] ];
[ [ 0, 0 ], [ 4, 5 ], [ 4, 6 ], [ 8, 5 ], [ 8, 7 ], [ 8, 8 ], [ 8, 10 ], 
  [ 11, 5 ], [ 11, 7 ], [ 11, 8 ], [ 11, 10 ], [ 12, 5 ], [ 12, 7 ], 
  [ 12, 8 ], [ 12, 10 ], [ 15, 5 ], [ 15, 7 ], [ 15, 8 ], [ 15, 10 ], 
  [ 16, 5 ], [ 16, 7 ], [ 16, 8 ], [ 16, 10 ], [ 18, 5 ], [ 19, 7 ], 
  [ 19, 8 ], [ 19, 10 ], [ 20, 7 ], [ 20, 8 ], [ 20, 10 ], [ 22, 7 ], 
  [ 22, 8 ], [ 22, 10 ], [ 23, 7 ], [ 23, 8 ], [ 23, 10 ], [ 24, 7 ], 
  [ 24, 8 ], [ 24, 10 ], [ 25, 7 ], [ 25, 8 ], [ 26, 7 ], [ 26, 10 ] ]
gap> S:=GoodSemigroupBySmallElements(smalls);
<Good semigroup>

gap> S1:=ProjectionOfGoodSemigroup(S,1);;
gap> SmallElements(S1);
[ 0, 4, 8, 11, 12, 15, 16, 18, 19, 20, 22 ]
gap> S2:=ProjectionOfGoodSemigroup(S,2);;
gap> SmallElements(S2);
[ 0, 5, 6, 7, 8, 10 ]

gap> GenusOfGoodSemigroup(S);
21

gap> Length(S);
15
gap> LengthOfGoodSemigroup(S);
15

gap> AperySetOfGoodSemigroup(S);
[ [ 0, 0 ], [ 4, 6 ], [ 8, 5 ], [ 8, 7 ], [ 8, 8 ], [ 8, 12 ], [ 8, 13 ], 
  [ 8, 14 ], [ 8, 15 ], [ 11, 5 ], [ 11, 7 ], [ 11, 8 ], [ 11, 10 ], 
  [ 11, 11 ], [ 11, 12 ], [ 11, 13 ], [ 11, 14 ], [ 11, 15 ], [ 12, 5 ], 
  [ 12, 7 ], [ 12, 8 ], [ 12, 11 ], [ 12, 14 ], [ 15, 5 ], [ 15, 7 ], 
  [ 15, 8 ], [ 15, 11 ], [ 15, 14 ], [ 16, 5 ], [ 16, 7 ], [ 16, 8 ], 
  [ 16, 11 ], [ 16, 14 ], [ 18, 5 ], [ 19, 7 ], [ 19, 8 ], [ 19, 11 ], 
  [ 19, 14 ], [ 20, 7 ], [ 20, 8 ], [ 20, 11 ], [ 20, 14 ], [ 22, 7 ], 
  [ 22, 8 ], [ 22, 11 ], [ 22, 12 ], [ 22, 13 ], [ 22, 14 ], [ 22, 15 ], 
  [ 23, 7 ], [ 23, 8 ], [ 23, 10 ], [ 23, 11 ], [ 23, 14 ], [ 24, 7 ], 
  [ 24, 8 ], [ 24, 10 ], [ 24, 11 ], [ 24, 14 ], [ 25, 7 ], [ 25, 8 ], 
  [ 26, 7 ], [ 26, 10 ], [ 26, 11 ], [ 26, 14 ], [ 27, 7 ], [ 27, 10 ], 
  [ 27, 11 ], [ 27, 14 ], [ 28, 7 ], [ 28, 10 ], [ 28, 11 ], [ 28, 14 ], 
  [ 29, 7 ], [ 29, 10 ], [ 29, 11 ], [ 29, 14 ], [ 29, 15 ], [ 30, 7 ], 
  [ 30, 10 ], [ 30, 11 ], [ 30, 13 ], [ 30, 14 ] ]

gap> StratifiedAperySetOfGoodSemigroup(S);
[ [ [ 0, 0 ] ], [ [ 4, 6 ], [ 8, 5 ], [ 11, 5 ] ], 
  [ [ 8, 7 ], [ 11, 7 ], [ 12, 5 ], [ 15, 5 ], [ 16, 5 ], [ 18, 5 ] ], 
  [ [ 8, 8 ], [ 11, 8 ], [ 12, 7 ], [ 15, 7 ], [ 16, 7 ], [ 19, 7 ], 
      [ 20, 7 ], [ 22, 7 ], [ 23, 7 ], [ 24, 7 ], [ 25, 7 ] ], 
  [ [ 8, 12 ], [ 8, 13 ], [ 8, 14 ], [ 11, 10 ], [ 11, 11 ], [ 12, 8 ], 
      [ 15, 8 ], [ 16, 8 ], [ 19, 8 ], [ 20, 8 ], [ 22, 8 ], [ 23, 8 ], 
      [ 24, 8 ], [ 25, 8 ], [ 26, 7 ], [ 27, 7 ], [ 28, 7 ], [ 29, 7 ], 
      [ 30, 7 ] ], 
  [ [ 8, 15 ], [ 11, 12 ], [ 11, 13 ], [ 11, 14 ], [ 12, 11 ], [ 15, 11 ], 
      [ 16, 11 ], [ 19, 11 ], [ 20, 11 ], [ 22, 11 ], [ 23, 10 ], [ 24, 10 ], 
      [ 26, 10 ], [ 27, 10 ], [ 28, 10 ], [ 29, 10 ], [ 30, 10 ] ], 
  [ [ 11, 15 ], [ 12, 14 ], [ 15, 14 ], [ 16, 14 ], [ 19, 14 ], [ 20, 14 ], 
      [ 22, 12 ], [ 22, 13 ], [ 22, 14 ], [ 23, 11 ], [ 24, 11 ], [ 26, 11 ], 
      [ 27, 11 ], [ 28, 11 ], [ 29, 11 ], [ 30, 11 ] ], 
  [ [ 22, 15 ], [ 23, 14 ], [ 24, 14 ], [ 26, 14 ], [ 27, 14 ], [ 28, 14 ], 
  [ 29, 14 ], [ 30, 13 ] ], [ [ 29, 15 ], [ 30, 14 ] ] ]


gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=CanonicalIdealOfNumericalSemigroup(s);;
gap> e:=15+e;;
gap> dup:=NumericalSemigroupDuplication(s,e);;
gap> IsSymmetricGoodSemigroup(dup);
true

gap> G:=[[3,3],[4,4],[5,4],[4,6]];
[ [ 3, 3 ], [ 4, 4 ], [ 5, 4 ], [ 4, 6 ] ]
gap> C:=[6,6];
[ 6, 6 ]
gap> S:=GoodSemigroup(G,C);
<Good semigroup>
gap> SmallElements(S);
[ [ 0, 0 ], [ 3, 3 ], [ 4, 4 ], [ 4, 6 ], [ 5, 4 ], [ 6, 6 ] ]
gap> A:=ArfClosure(S);
<Good semigroup>
gap> SmallElements(A);
[ [ 0, 0 ], [ 3, 3 ], [ 4, 4 ] ]
gap> ArfGoodSemigroupClosure(S) = ArfClosure(S);
true

#Good ideals

gap> G:=[[4,3],[7,13],[11,17],[14,27],[15,27],[16,20],[25,12],[25,16]];
[ [ 4, 3 ], [ 7, 13 ], [ 11, 17 ], [ 14, 27 ], [ 15, 27 ], [ 16, 20 ],
[ 25, 12 ], [ 25, 16 ] ]
gap> C:=[25,27];
[ 25, 27 ]
gap> g := GoodSemigroup(G,C);
<Good semigroup>
gap> i:=GoodIdeal([[2,3]],g);
<Good ideal of good semigroup>

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> d:=NumericalSemigroupDuplication(s,e);;
gap> e:=GoodIdeal([[2,3],[3,2],[2,2]],d);;
gap> GoodGeneratingSystemOfGoodIdeal(e);
[ [ 2, 2 ], [ 2, 3 ], [ 3, 2 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> a:=AmalgamationOfNumericalSemigroups(s,e,5);;
gap> e:=GoodIdeal([[2,3],[3,2],[2,2]],a);;
gap> a=AmbientGoodSemigroupOfGoodIdeal(e);
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> d:=NumericalSemigroupDuplication(s,e);;
gap> e:=GoodIdeal([[2,3],[3,2],[2,2]],d);;
gap> MinimalGoodGeneratingSystemOfGoodIdeal(e);
[ [ 2, 3 ], [ 3, 2 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> d:=NumericalSemigroupDuplication(s,e);;
gap> e:=GoodIdeal([[2,3],[3,2]],d);;
gap> [1,1] in e;
false
gap> [2,2] in e;
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> d:=NumericalSemigroupDuplication(s,e);;
gap> e:=GoodIdeal([[2,3],[3,2]],d);;
gap> SmallElements(e);
[ [ 2, 2 ], [ 2, 3 ], [ 3, 2 ], [ 5, 5 ], [ 5, 6 ], [ 6, 5 ], [ 7, 7 ] ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> e:=10+s;;
gap> d:=NumericalSemigroupDuplication(s,e);;
gap> c:=CanonicalIdealOfGoodSemigroup(d);;
gap> MinimalGoodGeneratingSystemOfGoodIdeal(c);
[ [ 0, 0 ], [ 2, 2 ] ]

gap> S:=GoodSemigroupBySmallElements([ [ 0, 0 ], [ 5, 4 ], [ 5, 8 ], [ 5, 11 ],
> [ 5, 12 ], [ 5, 13 ], [ 6, 4 ], [ 7, 8 ], [ 7, 11 ], [ 7, 12 ], [ 7, 14 ],
> [ 8, 8 ], [ 8, 11 ], [ 8, 12 ], [ 8, 15 ], [ 8, 16 ], [ 8, 17 ], [ 8, 18 ], 
> [ 10, 8 ], [ 10, 11 ], [ 10, 12 ], [ 10, 15 ], [ 10, 16 ], [ 10, 17 ], 
> [ 10, 18 ], [ 11, 8 ], [ 11, 11 ], [ 11, 12 ], [ 11, 15 ], [ 11, 16 ],
> [ 11, 17 ], [ 12, 8 ], [ 12, 11 ], [ 12, 12 ], [ 12, 15 ], [ 12, 16 ], 
> [ 12, 18 ] ]);
<Good semigroup>
gap> AbsoluteIrreduciblesOfGoodSemigroup(S);
[ [ 5, 13 ], [ 6, 4 ], [ 7, 14 ], [ 8, infinity ], [ 10, infinity ], 
  [ 12, infinity ], [ infinity, 8 ], [ infinity, 11 ], [ infinity, 18 ] ]

gap> S:=GoodSemigroupBySmallElements([ [ 0, 0 ], [ 4, 3 ], [ 8, 6 ], [ 8, 7 ],
> [ 12, 6 ], [ 12, 9 ], [ 12, 10 ], [ 16, 6 ], [ 16, 9 ], [ 16, 12 ], [ 16, 13 ],
> [ 16, 14 ], [ 18, 6 ], [ 20, 9 ], [ 20, 12 ], [ 20, 13 ], [ 20, 15 ], [ 20, 16 ],
> [ 20, 17 ], [ 22, 9 ], [ 24, 12 ], [ 24, 13 ], [ 24, 15 ], [ 24, 16 ], [ 24, 18 ],
> [ 26, 12 ], [ 26, 13 ], [ 28, 12 ], [ 28, 15 ], [ 28, 16 ], [ 28, 18 ],[ 30, 12 ], 
> [ 30, 15 ], [ 30, 16 ], [ 30, 18 ] ]);
<Good semigroup>
gap> TracksOfGoodSemigroup(S);
[ [ [ 4, 3 ] ], [ [ 8, 7 ], [ 18, 6 ] ], 
  [ [ 30, infinity ], [ infinity, 16 ] ], 
  [ [ 31, infinity ], [ infinity, 16 ] ], [ [ 31, infinity ] ], 
  [ [ 33, infinity ], [ infinity, 16 ] ], [ [ 33, infinity ] ] ]

##dot.xml
gap> br:=BinaryRelationByElements(Domain([1,2]), [DirectProductElement([1,2])]);;
gap> Print(DotBinaryRelation(br));
digraph  NSGraph{rankdir = TB; edge[dir=back];
1 [label="1"];
2 [label="2"];
2 -> 1;
}

gap> s:=NumericalSemigroup(3,5,7);;
gap> IsHasseDiagram(HasseDiagramOfNumericalSemigroup(s,[1,2,3]));
true

gap> s:=NumericalSemigroup(3,5,7);;
gap> hb:=HasseDiagramOfBettiElementsOfNumericalSemigroup(s);;
gap> List(Source(hb));
[ 10, 12, 14 ]

gap> s:=NumericalSemigroup(3,5,7);;
gap> h:=HasseDiagramOfAperyListOfNumericalSemigroup(s);;
gap> Source(h)=Set(AperyList(s));
true
gap> h:=HasseDiagramOfAperyListOfNumericalSemigroup(s,10);;
gap> Source(h)=Set(AperyList(s,10));
true

#gap> s:=NumericalSemigroup(4,6,9);;
#gap> Print(DotTreeOfGluingsOfNumericalSemigroup(s));
#digraph  NSGraph{rankdir = TB; 
#0 [label="〈 4, 6, 9 〉"]; 
#0 [label="〈 4, 6, 9 〉", style=filled]; 
#1 [label="〈 4 〉 + 〈 6, 9 〉" , shape=box]; 
#2 [label="〈 1 〉", style=filled]; 
#3 [label="〈 2, 3 〉", style=filled]; 
#4 [label="〈 2 〉 + 〈 3 〉" , shape=box]; 
#5 [label="〈 1 〉", style=filled]; 
#6 [label="〈 1 〉", style=filled]; 
#7 [label="〈 4, 6 〉 + 〈 9 〉" , shape=box]; 
#8 [label="〈 2, 3 〉", style=filled]; 
#10 [label="〈 2 〉 + 〈 3 〉" , shape=box]; 
#11 [label="〈 1 〉", style=filled]; 
#12 [label="〈 1 〉", style=filled]; 
#9 [label="〈 1 〉", style=filled]; 
#0 -> 1; 
#1 -> 2; 
#1 -> 3; 
#3 -> 4; 
#4 -> 5; 
#4 -> 6; 
#0 -> 7; 
#7 -> 8; 
#7 -> 9; 
#8 -> 10; 
#10 -> 11; 
#10 -> 12; 
#}
#
#gap> s:=NumericalSemigroup(4,6,9);;
#gap> Print(DotOverSemigroupsNumericalSemigroup(s));
#digraph  NSGraph{rankdir = TB; edge[dir=back];
#1 [label="〈 1 〉", style=filled];
#2 [label="〈 2, 3 〉", style=filled];
#3 [label="〈 2, 5 〉", style=filled];
#4 [label="〈 2, 7 〉", style=filled];
#5 [label="〈 2, 9 〉", style=filled];
#6 [label="〈 3, 4, 5 〉", style=filled];
#7 [label="〈 3, 4 〉", style=filled];
#8 [label="〈 4, 5, 6, 7 〉"];
#9 [label="〈 4, 5, 6 〉", style=filled];
#10 [label="〈 4, 6, 7, 9 〉"];
#11 [label="〈 4, 6, 9, 11 〉"];
#12 [label="〈 4, 6, 9 〉", style=filled];
#1 -> 2;
#2 -> 3;
#2 -> 6;
#3 -> 4;
#3 -> 8;
#4 -> 5;
#4 -> 10;
#5 -> 11;
#6 -> 7;
#6 -> 8;
#7 -> 10;
#8 -> 9;
#8 -> 10;
#9 -> 11;
#10 -> 11;
#11 -> 12;
#}

gap> s:=NumericalSemigroup(4,6,9);;
gap> Print(DotRosalesGraph(15,s));
graph  NSGraph{
1 [label="6"];
2 [label="9"];
2 -- 1;
}

gap> f:=FactorizationsIntegerWRTList(20,[3,5,7]);
[ [ 5, 1, 0 ], [ 0, 4, 0 ], [ 1, 2, 1 ], [ 2, 0, 2 ] ]
gap> Print(DotFactorizationGraph(f));
graph  NSGraph{
1 [label=" (5, 1, 0)"];
2 [label=" (0, 4, 0)"];
3 [label=" (1, 2, 1)"];
4 [label=" (2, 0, 2)"];
2 -- 3[label="2", color="red"];
3 -- 4[label="2", color="red"];
1 -- 3[label="4", color="red"];
1 -- 4[label="4" ];
2 -- 4[label="4" ];
1 -- 2[label="5" ];
}

gap> f:=FactorizationsIntegerWRTList(20,[3,5,7]);
[ [ 5, 1, 0 ], [ 0, 4, 0 ], [ 1, 2, 1 ], [ 2, 0, 2 ] ]
gap> Print(DotEliahouGraph(f));
graph  NSGraph{
1 [label=" (5, 1, 0)"];
2 [label=" (0, 4, 0)"];
3 [label=" (1, 2, 1)"];
4 [label=" (2, 0, 2)"];
2 -- 3[label="2" ];
3 -- 4[label="2" ];
1 -- 3[label="4" ];
1 -- 4[label="4" ];
1 -- 2[label="5" ];
}

gap> SetDotNSEngine("circo");
true


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

gap> IsListOfIntegersNS([1,1/2]);  
false

##random.xml

# RandomNumericalSemigroup(3,9,55);;
# RandomListForNS(13,1,79);;
# RandomModularNumericalSemigroup(9);;
# RandomModularNumericalSemigroup(10,25);;
# RandomProportionallyModularNumericalSemigroup(9);;
# RandomProportionallyModularNumericalSemigroup(10,25);;
# RandomListRepresentingSubAdditiveFunction(7,9);;
# RepresentsPeriodicSubAdditiveFunction(last);
# ns := NumericalSemigroupWithRandomElementsAndFrobenius(5,10,50);;
# MinimalGeneratingSystem(ns);;
# SmallElements(ns);;
# ns2 := NumericalSemigroupWithRandomElementsAndFrobenius(5,10,9);;
# ns3 := NumericalSemigroupWithRandomElementsAndFrobenius(5,10,10);;
# MinimalGeneratingSystem(ns3);;

#############################################################################
#############################################################################
# Simple examples aiming for a better code coverage

##obsolet
#gap> NumericalSemigroupByMinimalGenerators(3,5,6,7);
#I  The list [ 3, 5, 6, 7 ] can not be the minimal generating set. The list [ 3, 5, 7 ] will be used instead.
#<Numerical semigroup with 3 generators>

## irreducibles
gap> ns := NumericalSemigroup(5,7);;
gap> RemoveMinimalGeneratorFromNumericalSemigroup(5,ns);
<Numerical semigroup with 7 generators>
gap> SmallElements(last);
[ 0, 7, 10, 12, 14, 15, 17, 19, 20, 21, 22, 24 ]

gap> IsFreeNumericalSemigroup(NumericalSemigroup(7,8,9,12,13));
false
gap> IsFreeNumericalSemigroup(NumericalSemigroup(8, 12, 18, 19));
true

## ideals
gap> ns := NumericalSemigroup(5,7,9);;
gap> i := 3+ns;
<Ideal of numerical semigroup>
gap> Display(i);
[ [ 3 ], [ 8 ], [ 10 ], [ 12, 13 ], [ 15 ], [ 17, "->" ] ]
gap> j := 2+ns;
<Ideal of numerical semigroup>
gap> i<j;
false
gap> j<i;
true
gap> Generators(i);
[ 3 ]
gap> it := Iterator(i);
<iterator>
gap> l:= [];; for i in [1..20] do Add( l, NextIterator( it ) ); od; l;
[ 3, 8, 10, 12, 13, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
  30 ]

##good
gap> e:=88+s;;
gap> dup:=NumericalSemigroupDuplication(s,e);;
gap> [ 110, 109 ] in dup;
true
gap> [87,109] in dup;
false



## get info level to the original state
gap> SetInfoLevel( InfoNumSgps, INFO_NSGPS);

gap> STOP_TEST( "testall.tst", 10000 );
## The first argument of STOP_TEST should be the name of the test file.
## The number is a proportionality factor that is used to output a
## "GAPstone" speed ranking after the file has been completely processed.
## For the files provided with the distribution this scaling is roughly
## equalized to yield the same numbers as produced by the test file
## tst/combinat.tst. For package tests, you may leave it unchnaged.

#############################################################################
##
