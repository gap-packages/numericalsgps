#############################################################################
##
#A  testsome.tst        NumericalSgps package                   Manuel Delgado
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


gap> a := AffineSemigroupByGenerators([5,3,1],[2,7,4],[3,1,5]);
<Affine semigroup in 3 dimensional space, with 3 generators>
gap> MinimalPresentationOfAffineSemigroup(a);
[  ]
gap> BettiElementsOfAffineSemigroup(a);
[  ]

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


gap> a:=AffineSemigroupByEquations([[1,-1]],[]);;
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
gap> Generators(a);
[ [ 0, 1 ], [ 1, 2 ], [ 2, 0 ], [ 3, 1 ], [ 5, 0 ] ]


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


gap> s:=FiniteComplementIdealExtension([0,2],[1,1],[3,0]);;
gap> MinimalGenerators(s);
[ [ 0, 3 ], [ 1, 1 ], [ 0, 2 ], [ 3, 0 ], [ 2, 1 ], [ 1, 2 ], [ 3, 1 ], [ 5, 0 ], [ 4, 0 ] ]

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


gap> s := NumericalSemigroup( 10, 15, 16 );
<Numerical semigroup with 3 generators>
gap> IsCompleteIntersection(s);
true
gap> s := NumericalSemigroup( 18, 24, 34, 46, 51, 61, 74, 8 );
<Numerical semigroup with 8 generators>
gap> IsACompleteIntersectionNumericalSemigroup(s);
false

gap> s:=NumericalSemigroup(10,15,18);;
gap> IsUniversallyFree(s);
true
gap> s:=NumericalSemigroup(4,6,9);;
gap> IsUniversallyFree(s);
false

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
99

gap> l:=IrreducibleNumericalSemigroupsWithFrobeniusNumber(31);;
gap> Length(l);
109
gap> Length(Filtered(l,IsSuperSymmetricNumericalSemigroup));
64

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


##affine.xml

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

gap> s1 := AffineSemigroup("equations",[[[-2,1]],[3]]);;
gap> TameDegreeOfAffineSemigroup(s1);
3
gap> OmegaPrimalityOfElementInAffineSemigroup([0,3],s1);
3

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
