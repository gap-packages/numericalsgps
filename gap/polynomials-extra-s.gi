#############################################################################
##
#W  polynomials-extra-s.gi  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2016 by Manuel Delgado and Pedro Garcia-Sanchez
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#################################################################
##
# F SemigroupOfValuesOfPlaneCurve(f)
##  Computes the semigroup of values {mult(f,g) | g curve} of a plane curve
##   with one place at the infinity in the variables X(Rationals,1) and X(Rationals,2)
##  This function needs NumSgpsCanUseSingular to be enabled, either by loading the
##  package singular prior to numericalsgps, or by using NumSgpsUseSingular.
##  The function makes use of `semigroup` in the `alexpoly` singular library
##
#################################################################
InstallGlobalFunction(SemigroupOfValuesOfPlaneCurve,
  function(f)
  local l,x,y,R,T, gens, wgens, w1, w2, c, inf, GBASIStmp, Rtmp;

  inf:=function(u,v)
      return [Minimum(u[1],v[1]),Minimum(u[2],v[2])];
  end;

  if not(IsPolynomial(f)) then
    Error("The argument must be a polynomial\n");
  fi;

  R:=PolynomialRing(Rationals,[X(Rationals,1),X(Rationals,2)]);
  if not(f in R) then
          Error("The argument must be a polynomial in ", R,"\n");
  fi;

  T:=SingularBaseRing;
  SingularLibrary("alexpoly.lib");
  Rtmp:=SingularBaseRing;
  GBASIStmp:=GBASIS;
  GBASIS:=SINGULARGBASIS;
  SingularSetBaseRing( R );
  x:=R.1;;
  y:=R.2;;
  l:=SingularInterface("semigroup",[f],"list");
  SingularSetBaseRing( T );
  gens:=l[1];
  wgens:=l[2];
  c:=l[3];
  if IsInt(gens[1]) then #numerical semigroup
    SingularSetBaseRing(Rtmp);
    GBASIS:=GBASIStmp;
    return NumericalSemigroup(gens);
  fi;

  if Length(gens[1])=2 then # good sem dim two
    if not(ForAll(Union(gens,wgens), x->x[1]<=c[1] and x[2]<=c[2])) then
      #c:=Reversed(c);
      #wgens:=Reversed(wgens);
      Info(InfoNumSgps,2,"Warning: w-generators or generators not smaller than the conductor");
      gens:=List(gens, x->inf(x,c));
      wgens:=List(wgens,x->inf(x,c));
    fi;
    w1:=wgens[1];
    gens:=Union(gens,List([w1[1]..c[1]], i->[i,w1[2]]));
    w2:=wgens[2];
    gens:=Union(gens,List([w2[2]..c[2]], i->[w2[1],i]));
    Info(InfoNumSgps,2,"Generators ", gens);
    Info(InfoNumSgps,2,"Conductor ", c);
    SingularSetBaseRing(Rtmp);
    GBASIS:=GBASIStmp;
    return GoodSemigroup(gens,c);
  fi;
  SingularSetBaseRing(Rtmp);
  GBASIS:=GBASIStmp;
  return l;
end);
