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
#O + for numerical semigroups
##
#############################################################################
InstallMethod(\+,"for numerical semigroups",[IsNumericalSemigroup,IsNumericalSemigroup],
    function(S,T)
    local msgS,msgT;

    msgS:=MinimalGenerators(S);
    msgT:=MinimalGenerators(T);
    return NumericalSemigroup(Union(msgS,msgT));
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
    
    if L = [] then # represents the gaps of N
      return true;
    fi;
    
    if not (IsListOfIntegersNS(L) and ForAll(L, i -> IsPosInt(i))) then
      return false; # Error("The argument must be a list of positive integers");
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
#F  NumericalSemigroupsWithFrobeniusNumberFG(g)
##
##  Computes the set of numerical semigroups with Frobenius number g.
##  The algorithm is based on
##  "Fundamental gaps in numerical semigroup".
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumberFG, function(g)
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


#############################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumberAndMultiplicity(F,m)
##
##  Computes the set of numerical semigroups with multipliciy m and Frobenius
##  number F. The algorithm is based on "The set of numerical semigroups of a
##  given multiplicity and Frobenius number" arXiv:1904.05551 [math.GR]
##
############################################################################# 
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumberAndMultiplicity,

function(F,m)
    local G,IrrmF,Lmf,S,small,small2,genZ,smallZ,SZ,D,pow,A,B,b,TB,TBD,bS;
      
    if (not(IsInt(F))) or (not(IsInt(m))) then
        Error("The arguments must be two integers.\n");
    fi;

    if F<-1 or F=0 then
        return [];#Error(f," is not a valid Frobenius number.\n");
    fi;

    if m<=0 then
        return [];#Error(m," is not a valid Multiplicity.\n");
    fi;

    if F=-1 and m = 1 then
        return [NumericalSemigroup(1)];
    fi;

    if F < m-1 or RemInt(F,m) = 0 then 
        return [];
    fi;

    if m=2 then 
#        return [NumericalSemigroupByMinimalGenerators([m,F+m])];
        return [NumericalSemigroup([m,F+m])];
    fi;

    if F=m-1 then 
        return [NumericalSemigroupByGaps([1 .. F])];
    fi;

    if (F in [(m+1) .. (2*m-1)]) then 
        Lmf:=[];
        G:=Difference([1 .. F],[m]);
        pow:=Combinations([(m+1) .. (F-1)]);
        for A in pow do
            Append(Lmf,[NumericalSemigroupByGaps(Difference(G,A))]);
        od;
    else 
        IrrmF:=IrreducibleNumericalSemigroupsWithFrobeniusNumberAndMultiplicity(F,m);
        Lmf:=ShallowCopy(IrrmF);
        for S in IrrmF do 
            small:=SmallElementsOfNumericalSemigroup(S);
            small2:=Intersection(small,[m .. Int(F/2)]);
            genZ:=Union(small2,[F+1 .. (F+m)]);
            SZ:=NumericalSemigroupByGenerators(genZ);
            smallZ:=SmallElements(SZ);
            D:=Difference(small,smallZ); 
            pow:=Combinations(D);
            for B in pow do
                TB:=[];
                for b in B do
                    Append(TB,b+smallZ);
                od;
                TBD:=Intersection(TB,D);
                bS:=NumericalSemigroupByGenerators(Concatenation(genZ,TBD));
                AddSet(Lmf,bS);
            od;
        od;
    fi;
    return Set(Lmf);
end);
#############################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumber(g)
##
##  Making use of NumericalSemigroupsWithFrobeniusNumberAndMultiplicity, computes 
## the set of numerical semigroups with Frobenius number g.
##  
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumber, function(F)
  local  L, m;
  L:=[];
  for m in [1 .. F+1] do 
    Append(L,NumericalSemigroupsWithFrobeniusNumberAndMultiplicity(F,m));;
  od;
  return(L);
end);

#############################################################################

#############################################################################
##
#F  NumericalSemigroupsWithMaxPrimitiveAndMultiplicity(M,m)
##
##  Computes the set of numerical semigroups with multipliciy m and maximum
## primitive M. The algorithm is based on ongoinw work by M. Delgado and Neeraj Kumar
##
############################################################################# 
InstallGlobalFunction(NumericalSemigroupsWithMaxPrimitiveAndMultiplicity,function(M,m)
    local NSgpsWithMultiplicityRatioAndMaximumPrimitive, LIST, coprime_mM, Mmodm, nonPrims, PossiblePrims, iter, Y, prims, r, coprime_mrM, s, lcombs, allowable, Mmodr, listsprim_mr, aux;

    if M < m then
       Error("The multiplicity must not exceed the maximum primitive");
    elif M=1 and m=1 then
#       return [[1]];   
       return [NumericalSemigroup([1])];   
    fi;

    Info(InfoMaxPrim,1,"multiplicity=",m);
    ##
    ##
    #### local function
    ## NSgpsWithMultiplicityRatioAndMaximumPrimitive computes the numerical semigroups with fixed multiplicity (m), second largest primitive (r) and maximum primitive (M)
    ## (Note that the embedding dimension of the computed numerical semigroups is between 4 and m (the dimension 3 case was already treated). The strategy used is to loop through the possible embedding dimensions)
    ##
    NSgpsWithMultiplicityRatioAndMaximumPrimitive := function() # m, r and M are used; it is also used a set of allowable primitives (other than m, r and M)
        local listsprim, k, A, union, mingens, num;
    
        Info(InfoMaxPrim,2,"ratio=",r);
        listsprim := []; # list used to store the semigroups (through its minimal generating sets)
        if coprime_mrM then
#        if Gcd(m,r,M)=1 then
            for k in [1..m-3] do # the embedding dimension of a numerical semigroup does not exceed the multiplicity, thus the sets of primitives (other than m, r and M) are among the subsets of 'allowable' with at most m-3 elements.
            #subsets := Combinations(allowable,k);
                iter := IteratorOfCombinations(allowable,k); # the direct use of 'Combinations' (rather than the use of an iterator) would require too much memory for large M (M > 50, say)
                for A in iter do
                    if Length(Set(A,x->x mod m)) = k then # there is no more than one minimal generator per residue class mod m, thus A must have some element in exactly k residue classes
                        union := Union([m,r,M],A);
                         #if NrRestrictedPartitions(M,union)=1 then # guarantees M to be a primitive (slightly slower than testing below)
                        s := NumericalSemigroup(union);
                        mingens := MinimalGenerators(s);
                        if M in mingens then
                            AddSet(listsprim,mingens); # this step is used to discard repetitions
                        fi;
                    fi;
                od;
            od;
        else
            for k in [1..m-3] do # the embedding dimension of a numerical semigroup does not exceed the multiplicity, thus the sets of primitives (other than m, r and M) are among the subsets of 'allowable' with at most m-3 elements.
            #subsets := Combinations(allowable,k);
                iter := IteratorOfCombinations(allowable,k); # the direct use of 'Combinations' (rather than the use of an iterator) would require too much memory for large M (M > 50, say)
                for A in iter do
                    if Length(Set(A,x->x mod m)) = k then # there is no more than one minimal generator per residue class mod m, thus A must have some element in exactly k residue classes
                        union := Union([m,r,M],A);
                        # if NrRestrictedPartitions(M,union)=1 then # guarantees M to be a primitive (slightly slower than testing below)
                        if Gcd(union) = 1 then
                            s := NumericalSemigroup(union);
                            mingens := MinimalGenerators(s);
                            if M in mingens then                            
                                AddSet(listsprim,mingens); # this step is used to discard repetitions
                            fi;
                        fi;
                    fi;
                od;
            od;
        fi;

#     return List(listsprim, gens -> NumericalSemigroup(gens));
       return listsprim;
    end;
    ##
    # end of local function
    ######
    ##
    LIST := [];
    coprime_mM := false;
    ##
    #####
    ## Some trivial cases
    if (M = 2) or (m > M) or (M mod m = 0) then # some impossible cases
  #      return LIST;
     return List(LIST, gens -> NumericalSemigroup(gens));
   fi;
    #
    if (M = 1) and (m = 1) then # The maxprim 1 case
        Add(LIST,[1]);
 #       return LIST;
      return List(LIST, gens -> NumericalSemigroup(gens));
  fi;
    if m = 2 then # the multiplicity 2 case
        if (M mod m = 1) then 
            Add(LIST,[m,M]);
        fi;
      return List(LIST, gens -> NumericalSemigroup(gens));
#      return LIST;
    fi;
    ######
    ## Computing a list of possible primitives of some numerical semigroup with multiplicity m and maximum primitive M. It is obtained by discarding some integers in [m+1..M-1] that are "easily" seen no be primitives of any such semigroup.
 
    Mmodm := M mod m; #remainder of the integer division of M by m
    
    ## Note that m and M are the only primitives in their residue classes mod m. Furthermore, no divisor of M is primitive.
    nonPrims := Filtered(Difference([m+1..M-1],DivisorsInt(M)), g -> (g mod m = 0) or (g mod m = Mmodm));
    PossiblePrims := Difference([m+1..M-1],nonPrims); # preliminary list of integers (other than m and M) that can be primitives
    ## Cleaning up the preliminary list of possible primitives:
    PossiblePrims := Filtered(PossiblePrims, g -> NrRestrictedPartitions(M,[m,g])=0); # Recall that M can not be partitioned into a sum of generators, since it is primitive
    ######
    ##
    #####
    ## The boolean variable coprime_mM aims to avoid the systematic need to compute gcds
    if Gcd(m,M) = 1 then # note that <m,M> is a numerical semigroup with multiplicity m and maximum primitive M
        coprime_mM := true;
    else
        coprime_mM := false;
    fi;
    #####

    ## The various multiplicities present some specificities. In practice, some need considerably more computations than others. The numerical semigroups of max primitive depth 2 and those of multiplicity equal to Int(M/2) or M/2 - 1 can be quickly computed (in practice). The computations for the remaining cases are much slower (unless the multiplicities are very small). These correspond roughly to semigroups of max primitive depth greater than 3.
    ####
    ## "large" multiplicity 
    ####
    if m > M/2 then # the max primitive depth 2 case 
        # As the double of m does not belong to [m+1..M-1], any subset Y of [m+1..M-1] consists of primitives (provided that Gcd(Y\cup [m,M])=1)
        iter := IteratorOfCombinations([m+1..M-1]);
        if coprime_mM then
            for Y in iter do
                prims := Union([m,M],Y);
                Add(LIST,prims);
            od;
        else
            for Y in iter do
                prims := Union([m,M],Y);
                if Gcd(prims) = 1 then
                    Add(LIST,prims);
                fi;
            od;
        fi;
    elif m = Int(M/2) then # this case only occurs when M is odd, since otherwise M/2 is a divisor of M
        # 2m = M-1 is not a primitive; m+1 is not a primitive (observe that Int(M/2)+Int(M/2)+1 = M). Any other integer in [m+1..M-1] is a primitive of some semigroup in A(m,M). In fact, the sum of any two of them is greater than M, and 2m is the only multiple of m in the interval.
        iter := IteratorOfCombinations([m+2..M-2]);
        for Y in iter do# # note that m = (M-1)/2, thus gcd(m,M)=1
            prims := Union([m,M],Y);
            Add(LIST,prims);
        od;
    elif m = M/2 - 1 then # this case only occurs when M is even, since otherwise M/2 is not an integer
        # 2m=M-2 is not a primitive; m+2 is not a primitive (observe that (m+2 + m) = (M/2+1 +M/2-1 = M); also m+1 is not a primitive (observe that m+1 + m+1 = M/2 + M/2 = M). Any other integer in [m+1..M-1] is a primitive of some semigroup in A(m,M). In fact, the sum of any two of them is greater than M, and 2m is the only multiple of m in the interval.
        iter := IteratorOfCombinations(Difference([m+3..M-1],[M-2]));
        if coprime_mM then
            for Y in iter do
                prims := Union([m,M],Y);
                Add(LIST,prims);
           od;
        else
            for Y in iter do
                if Gcd(Union([m,M],Y)) = 1 then
                    prims := Union([m,M],Y);
                    Add(LIST,prims);
               fi;
            od;
        fi;
    else # semigroups of small multiplicity
        if coprime_mM then 
            Add(LIST,[m,M]);
        fi;
        ##
        for r in PossiblePrims do # loop through the possible primitives (other than m and M)
            if Gcd(m,r,M) = 1 then ## The boolean variable coprime_mrM aims to avoid the need to compute gcds systematically
                coprime_mrM := true;
            else
                coprime_mrM := false;
            fi;
            ##### The cases where m+r >= M-1 can be treated separately in a more efficient way (experimental: efficiency increases ~ 8%)
            if m+r = M-1 then
                iter := IteratorOfCombinations(Intersection([r+1..M-2],PossiblePrims));
                if coprime_mrM then
                    for Y in iter do
                        prims := Union([m,r,M],Y);
                        Add(LIST,prims);
                    od;
                else
                    for Y in iter do
                        if Gcd(Union([m,r,M],Y)) = 1 then
##                            prims := Union([m,M],Y);
                            prims := Union([m,r,M],Y);
                            Add(LIST,prims);
                        fi;
                    od;
                fi;
            elif m+r > M-1 then
                iter := IteratorOfCombinations(Intersection([r+1..M-1],PossiblePrims));
                if coprime_mrM then
                    for Y in iter do
                        prims := Union([m,r,M],Y);
                        Add(LIST,prims);
                    od;
                else
                    for Y in iter do
                        if Gcd(Union([m,r,M],Y)) = 1 then
##                            prims := Union([m,M],Y);
                            prims := Union([m,r,M],Y);
                            Add(LIST,prims);
                        fi;
                    od;
                fi;
            else
            #####
                Mmodr := M mod r; #remainder of the integer division of M by m
                ## Producing a list of integers that (besides m, r and M) are allowed to be primitves
                s := NumericalSemigroup(Union([m,r],[M+1..M+m]));
                lcombs := ElementsUpTo(s,M+1); #the set of linear combinations with nonnegative integer coefficients of m and r, up to M+1
                # note that M must not be a linear combination of m and r (otherwise M would not be primitive), but this is guaranteed by the way 'PossiblePrims' have been constructed
                allowable := Intersection([r+1..M-1],PossiblePrims);
                allowable := Difference(allowable,Union(lcombs,M-lcombs)); # list of possible primitives other than m, r and M. # Note that linear combinations of m and r can not be primitives. # Note also that M minus such a linear combination were a primitive, then M would not be a primitive.
                ##
                if Gcd(m,r,M) = 1 then
                    Add(LIST,[m,r,M]); 
                fi;
                ##
                if allowable <> [] then
                    aux := NSgpsWithMultiplicityRatioAndMaximumPrimitive();
                    listsprim_mr := aux;
                    Append(LIST,listsprim_mr);
                fi;
            fi;            
        od;
    fi;
#    return LIST;
    return List(LIST, gens -> NumericalSemigroup(gens));
end);
#############################################################################
##
#F  NumericalSemigroupsWithMaxPrimitive(M)
##
##  Making use of NumericalSemigroupsWithMaxPrimitiveAndMultiplicity, computes 
## the set of numerical semigroups with maximum primitiveFrobenius number M.
##  
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithMaxPrimitive,function(M)
  local  L, m;
  L:=[];
  for m in [1 .. M] do 
    Append(L,NumericalSemigroupsWithMaxPrimitiveAndMultiplicity(M,m));;
  od;
  return L;
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
