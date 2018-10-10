#############################################################################
##
#W  basics2.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#O  IsSubsemigroupOfNumericalSemigroup(S,T)
##
##  Test whether the numerical semigroup T is contained in the
##  numerical semigroup S
##
#############################################################################

InstallMethod( IsSubsemigroupOfNumericalSemigroup,
      "method for numerical semigroups",
      true,
      [IsNumericalSemigroup and HasGenerators,
       IsNumericalSemigroup and HasGenerators],0,
      function(S,T)
      if IsSubset(GeneratorsOfNumericalSemigroup(S),GeneratorsOfNumericalSemigroup(T)) then
           return true;
      else
          TryNextMethod();
      fi;
end);


InstallMethod( IsSubsemigroupOfNumericalSemigroup,
        "method for numerical semigroups",
        true,
        [IsNumericalSemigroup,IsNumericalSemigroup],0,
        function(S,T)
    return IsSubset(GapsOfNumericalSemigroup(T),GapsOfNumericalSemigroup(S));
end);
#############################################################################
##
#O  IsSubset(S,T)
##
##  A synonym of IsSubsemigroupOfNumericalSemigroup
##
#############################################################################
InstallMethod( IsSubset,
"method for numerical semigroups",
        true,
        [IsNumericalSemigroup,IsNumericalSemigroup],0,
        function(S,T)
   return IsSubsemigroupOfNumericalSemigroup(S,T);
end);
######
#############################################################################
##
#F DifferenceOfOfNumericalSemigroups(S,T)
##
## returns the set difference S\T
#############################################################################
InstallOtherMethod(Difference, [IsNumericalSemigroup, IsNumericalSemigroup], function(S, T)
  return DifferenceOfNumericalSemigroups(S,T);
end);

InstallGlobalFunction(DifferenceOfNumericalSemigroups, function(S, T)
  local  sS, sT, MS, MT, M, SS, ST;

  if not (IsNumericalSemigroup(S) and IsNumericalSemigroup(T)) then
     Error("The arguments must be numerical semigroups.");
  fi;
  sS := SmallElementsOfNumericalSemigroup(S);
  sT := SmallElementsOfNumericalSemigroup(T);
  MS := Maximum(sS);
  MT := Maximum(sT);
  M := Maximum(MS,MT);
  SS := Union(sS,[MS..M]);
  ST := Union(sT,[MT..M]);
  return Difference(SS,ST);
end);
#############################################################################
##
#F  IntersectionOfNumericalSemigroups(S,T)
##
##  Returns the intersection of the numerical
##  semigroups S and T.
##
#############################################################################
InstallOtherMethod(Intersection2, [IsNumericalSemigroup, IsNumericalSemigroup], function(S,T)
  return IntersectionOfNumericalSemigroups(S,T);
end);

InstallGlobalFunction(IntersectionOfNumericalSemigroups, function(S,T)
    local   gs,  gt,  R,  D,  g;

    if not (IsNumericalSemigroup(S) and IsNumericalSemigroup(T)) then
        Error("The arguments of IntersectionOfNumericalSemigroups must be numerical semigroups");
    fi;
    gs := FrobeniusNumberOfNumericalSemigroup(S);
    if gs = -1 then
        return T;
    fi;
    gt := FrobeniusNumberOfNumericalSemigroup(T);
    if gt = -1 then
        return S;
    fi;
    S := Union(SmallElementsOfNumericalSemigroup(S),[gs+1..gt+1]);
    T := Union(SmallElementsOfNumericalSemigroup(T),[gt+1..gs+1]);
    R := Intersection(S,T);
    while Gcd(R) <> 1 do
        Add(R,R[Length(R)]+1);
    od;
    D := Difference([0..R[Length(R)]], R);
    g := D[Length(D)];
    return NumericalSemigroupBySmallElements(Intersection(R,[0..g+1]));
end);


#############################################################################
##
#F  RepresentsGapsOfNumericalSemigroup(L)
##
##  Tests if the given list L represents the gaps of
##  some numerical semigroup.
##
#############################################################################
InstallGlobalFunction(RepresentsGapsOfNumericalSemigroup, function(L)
    local ld, ns;

    if not (IsListOfIntegersNS(L) and ForAll(L, i -> IsPosInt(i))) then
        Error("The argument must be a list of positive integers");
    fi;
    ld := Union(List(L,DivisorsInt));
    if ld <> L then
      return false;
    else
      ns := Difference([0..ld[Length(ld)]+1],ld);
      return RepresentsSmallElementsOfNumericalSemigroup(ns);
    fi;
end);


#############################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumber(g)
##
##  Computes the set of numerical semigroups with Frobenius number g.
##  The algorithm is based on
##  "Fundamental gaps in numerical semigroup".
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumber, function(g)
    local fg, fundamentalGapsRepresentingNSGenerator;

    if(not(IsInt(g))) then
        Error("the argument must be an integer.\n");
    fi;
    if(g=0 or g<-1) then
        return [];
    fi;
    if(g=-1) then
        return [NumericalSemigroup("generators",[1])];
    fi;



    fundamentalGapsRepresentingNSGenerator := function(g)
        local   ll,  initial,  final,  i,  list,  ld,  listNodivs,  x;

        ll:=[[g]];
        initial:=1;
        final:=Length(ll);

        repeat
            for i in [initial..final] do
                list:=ll[i];


                ld := Union(List(ll[i],DivisorsInt));
                if not RepresentsSmallElementsOfNumericalSemigroup(
                           Difference([0..ld[Length(ld)]+1],ld)) then
                    Unbind(ll[i]);
                fi;
                listNodivs:=Filtered([1..(list[1]-1)],x-> not ForAny(list,i -> i mod x = 0) );
                for x in listNodivs do
                    Append(ll,[Union([x],list)]);
                od;
            od;

            initial:=final+1;
            final:=Length(ll);
        until initial >= final;

        return Compacted(ll);
    end;

    fg := fundamentalGapsRepresentingNSGenerator(g);

    if not RepresentsGapsOfNumericalSemigroup(Union(List(fg[1],DivisorsInt))) then
        fg := fg{[2..Length(fg)]};
    fi;
    return List(fg,x->NumericalSemigroupByFundamentalGaps(x));
end);


##############################################################################
##
#F NumericalSemigroupsWithGenus
##computes the set of numerical semigroups with genus g,
# that is, numerical semigroups with exactly g gaps
#
#
# numerical semigroups are encoded in lists containing the apery set with
# respect to the multiplicity removing the zero element. The multiplicity
# is thus the lenght of the list plus one. In this way deciding membership
# to a numerical semigroup is straightforward (belongs). The computation of
# the Frobenius number is performed using Selmer's idea (frob). Removing a new
# generator is easy (removegen), as well as computing those minimal generators
# greater than the Frobenius number (minimalgeneratorsf).
# Given a numerical semigroup of genus g, removing minimal generators, one
# obtains numerical semigroups of genus g+1. In order to avoid repetitions,
# we only remove minimal generators greater than the frobenius number of
# the numerical semigroup (this is accomplished with the local function sons).
# References:
# -J. C. Rosales, P. A. Garc�a-S�nchez, J. I. Garc�a-Garc�a and
#  J. A. Jimenez-Madrid, The oversemigroups of a numerical semigroup.
#  Semigroup Forum 67 (2003), 145--158.
# -M. Bras-Amor�s, Fibonacci-like behavior of the number of numerical
#  semigroups of a given genus. Semigroup Forum 76 (2008), 379--384.
##

InstallGlobalFunction(NumericalSemigroupsWithGenus,function(g)
    local   mult,  frob,  removegen,  belongs,  minimalgeneratorsf,  sons,
            l,  i;

    mult:=function(l)
        return Length(l)+1;
    end;

    frob:=function(l)
        if l=[] then
            return -1;
        fi;
        return Maximum(l)-Length(l)-1;
    end;

    removegen:=function(n,l)
        local ll,len;
        len:=mult(l);
        if (n=len) then
            return [(n+2)..(2*n+1)];
        fi;
        ll:=ShallowCopy(l);
    ll[n mod len]:=n+len;
    return ll;
    end;

    belongs:=function(n,l)
        local m;
            m:=mult(l);
            if n<0 then
            return false;
        fi;
        if (n mod m)=0 then
            return true;
        fi;
        return l[n mod m]<=n;
    end;

    minimalgeneratorsf:=function(l)
        local i,j,len,genmf,minimal,f;

        f:=frob(l);
        len:=Length(l);
        if(f<=len) then
            return [len+1..2*len+1];
        fi;

        genmf:=[];
        for i in [1..len] do
            if (l[i]>f) then
            minimal:=true;
            j:=1;
            while (j<=len and minimal) do
                if (i<>j) then
                    if belongs(l[i]-l[j],l) then
                        minimal:=false;
                    fi;
                fi;
                j:=j+1;
            od;
            if minimal then
                Append(genmf,[l[i]]);
            fi;
            fi;
        od;

        return genmf;
    end;

    sons:=function(l)
        return List(minimalgeneratorsf(l),x->removegen(x,l));
    end;

    if(not(IsInt(g)) or g<-1) then
        Error("the argument must be a positive integer.\n");
    fi;

    if g=0 then
        return [NumericalSemigroup(1)];
    fi;
    l:=[[3]];
    i:=1;

    while i<g do
        i:=i+1;
        l:=Concatenation(List(l,sons));
    od;
    return List(l,s->NumericalSemigroup(Concatenation([mult(s)],s)));
end);
