gap> START_TEST("NumericalSgps package: singular.tst");

#
gap> x:=X(Rationals,"x");; y:=X(Rationals,"y");;
gap> f:= y^4-2*x^3*y^2-4*x^5*y+x^6-x^7;
-x^7+x^6-4*x^5*y-2*x^3*y^2+y^4
gap> SemigroupOfValuesOfPlaneCurve(f);
<Numerical semigroup with 3 generators>
gap> MinimalGenerators(last);
[ 4, 6, 13 ]

# FIXME: the next example is broken, no method found for MinimalGenerators
# gap> f:=(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)*(y^2-x^3);;
# gap> SemigroupOfValuesOfPlaneCurve(f);
# <Good semigroup>
# gap> MinimalGenerators(last);
# [ [ 4, 2 ], [ 6, 3 ], [ 13, 15 ], [ 29, 13 ] ]

#
gap> STOP_TEST( "singular.tst" );
