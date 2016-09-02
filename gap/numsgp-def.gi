#############################################################################
##
#W  numsgp-def.gi           Manuel Delgado <mdelgado@fc.up.pt>
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
#F  NumericalSemigroupByGenerators(arg)
##
##  Returns the numerical semigroup generated by arg.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByGenerators, function(arg)
    local   L,  M,  a,  b,  fr,  u,  small,  i;

    if IsList(arg[1]) then
        L := Difference(Set(arg[1]),[0]);
    else
        L := Difference(Set(arg),[0]);
    fi;

	if L=[] then
		Error("There should be at list one generator.\n");
	fi;

    if not ForAll(L, x -> IsPosInt(x)) then
        Error("The arguments should be positive integers.\n");
    fi;
    if Gcd(L) <> 1 then
        Error("The greatest common divisor is not 1.\n");
    fi;
    M:= Objectify( NumericalSemigroupsType, rec());
    SetGenerators(M,L);
    if 1 in L then
        SetMinimalGenerators(M,[1]);
        SetModularConditionNS(M,[1,2]);
        SetGaps(M,[]);
        SetSmallElements(M,[0]);
    elif Length(L) = 2 then # a non-trivial numerical semigroup has at least 2 generators
        SetMinimalGenerators(M, L);
        a := L[1];
        b := L[2];
        fr := a*b - a - b; #(Sylvester)
        SetFrobeniusNumberOfNumericalSemigroup(M,fr);
        # M is modular...
        u:=PowerModInt(b,-1,a);#the inverse ou b mod a
        SetModularConditionNS(M,[b*u,a*b]);
    fi;
    #when the set of generators is an interval, using Rosales and Garcia-Sanchez (pacific), the  Frobenius number and the "small elements" are easily computed
    if not (1 in L) and (Length(L) = Maximum(L) - Minimum(L)+1) then
        a := L[1];
        b := L[Length(L)]-a;
        fr := CeilingOfRational((a-1)/b)*a-1;
        small := [0];
        i := 1;
        while i*(a+b) < fr+1 do
            Append(small,[i*a..i*(a+b)]);
            i := i+1;
        od;
        Append(small,[fr+1]);

        SetSmallElements(M,small);

        SetFrobeniusNumber(M,fr);

        SetMinimalGenerators(M,[a..Minimum(2*a-1,a+b)]);

        # M is proportionaly modular...
        SetProportionallyModularConditionNS(M, [a+b,a*(a+b),b]);
         SetClosedIntervalNS(M,[L[1],L[Length(L)]]);

    fi;

    return M;

end);



#############################################################################
##
#F  NumericalSemigroupByMinimalGenerators(arg)
##
##  Returns the numerical semigroup minimally generated by arg.
##  If the generators given are not minimal, the minimal ones
##  are computed and used.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByMinimalGenerators, function(arg)
    local L, S, M, l,
          minimalGSNS, sumNS;


    #####################################################
    # Computes the sum of subsets of numerical semigroups
    sumNS := function(S,T)
        local mm, s, t, R;
        R := [];
        mm := Minimum(Maximum(S),Maximum(T));
        for s in S do
            for t in T do
                if s+t > mm then
                    break;
                else
                    AddSet(R,s+t);
                fi;
            od;
        od;
        return R;
    end;
    ##  End of sumNS(S,T)  --


    minimalGSNS := function(L)
        local res, gen, ss, sss, ssss, i, ngen;

        if 1 in L then
            return [1];
        fi;
        gen := ShallowCopy(L);
        # a naive reduction of the number of generators
        ss := sumNS(gen,gen);
        gen := Difference(gen,ss);
        if ss <> [] then
            sss := sumNS(ss,gen);
            gen := Difference(gen,sss);
            if sss <> [] then
                ssss := sumNS(sss,gen);
                gen := Difference(gen,ssss);
            fi;
        fi;
        if Length(gen) = 2 then
            return gen;
        fi;
        i := Length(gen);
        while i >= 2 do
            ngen := Difference(gen, [gen[i]]);
            if Gcd(ngen) = 1 and
               BelongsToNumericalSemigroup(gen[i], NumericalSemigroup(ngen)) then
                gen := ngen;
                i := i - 1;
            else

                i := i - 1;
            fi;
        od;
        return gen;
    end;
    ##  End of minimalGSNS(L)  --


    if IsList(arg[1]) then
        L := Difference(Set(arg[1]),[0]);
    else
        L := Difference(Set(arg),[0]);
    fi;
	if L=[] then
		Error("There should be at least one generator.\n");
	fi;
    if not ForAll(L, x -> IsPosInt(x)) then
        Error("The arguments should be positive integers.\n");
    fi;
    if Gcd(L) <> 1 then
        Error("The greatest common divisor is not 1");
    fi;


    l := minimalGSNS(L);
    if not Length(L) = Length(l) then
        Info(InfoWarning,1,"The list ", L, " can not be the minimal generating set. The list ", l, " will be used instead.");
        L := l;
    fi;
    M:= Objectify( NumericalSemigroupsType, rec() );
    #    Setter(GeneratorsOfNumericalSemigroup)( M, AsList( L ) );
    SetMinimalGenerators(M,L);
    SetGenerators(M,L);
    return M;

end);



#############################################################################
##
#F  NumericalSemigroupByMinimalGeneratorsNC(arg)
##
##  Returns the numerical semigroup minimally generated by arg.
##  No test is made about args' minimality.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByMinimalGeneratorsNC, function(arg)
    local L, S, M;


    if IsList(arg[1]) then
        L := Difference(Set(arg[1]),[0]);
    else
        L := Difference(Set(arg),[0]);
    fi;
    if not ForAll(L, x -> IsPosInt(x)) then
        Error("The arguments should be positive integers");
    fi;
    if Gcd(L) <> 1 then
        Error("The greatest common divisor is not 1");
    fi;


    M:= Objectify( NumericalSemigroupsType, rec() );
    #    Setter(GeneratorsOfNumericalSemigroup)( M, AsList( L ) );
    SetMinimalGenerators(M,L);
    SetGenerators(M,L);
    return M;

end);



#############################################################################
##
#F  ModularNumericalSemigroup(a,b)
##
##  Returns the modular numerical semigroup satisfying ax mod b <= x
##
#############################################################################
InstallGlobalFunction(ModularNumericalSemigroup, function(a,b)
    local M;
    if not IsPosInt(a) or not IsPosInt(b) then
        Error("ModularNumericalSemigroup has 2 positive integers as arguments");
    fi;
    M:= Objectify( NumericalSemigroupsType, rec() );
    SetModularConditionNS(M,[a,b]);
    if (a = 1) or (b = 1) then #the semigroup is the entire N
        SetMinimalGenerators(M,[1]);
    fi;
    # a modular semigroup is also a proportionally modular semigroup
    SetProportionallyModularConditionNS(M, [a,b,1]);
    # a modular semigroup is generated by an interval
    if a > 1 then
        SetClosedIntervalNS(M,[b/a,b/(a-1)]);
    fi;
    return M;
end);




#############################################################################
##
#F  ProportionallyModularNumericalSemigroup(a,b,c)
##
##  Returns the proportionally modular numerical semigroup
##  satisfying ax mod b <= cx
##
#############################################################################
InstallGlobalFunction(ProportionallyModularNumericalSemigroup, function(a,b,c)
    local   M,  m;

    if not IsPosInt(a) or not IsPosInt(b) or not IsPosInt(c) then
        Error("ProportionallyModularNumericalSemigroup has 3 positive integers as arguments");
    fi;
    M:= Objectify( NumericalSemigroupsType, rec() );
    SetProportionallyModularConditionNS(M,[a,b,c]);
    if a > c then
      SetClosedIntervalNS(M, [b/a,b/(a-c)]);
    else
        SetClosedIntervalNS(M, [1,2]);
    fi;

    m := CeilingOfRational(b/a);
    if a <> c and b/(a-c) > 2*m -1 then #the semigroup is a halfline
        SetSmallElementsOfNumericalSemigroup(M,[0,m]);
        SetFrobeniusNumberOfNumericalSemigroup(M,m-1);
#        SetMinimalGeneratorsNS(M, [m,2*m-1]); # bug pointed out by Ilya Frolov (fixed from version 1.0.1)
        SetMinimalGenerators(M, [m..2*m-1]);
    fi;
    if b = 1 then #the semigroup is the entire N
        SetMinimalGenerators(M, [1]);
    fi;
    if c = 1 then
        SetModularConditionNS(M, [a,b]);
    fi;

    return M;
end);




#############################################################################
##
#F  NumericalSemigroupByInterval(arg)
##
##  Returns the numerical semigroup
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByInterval, function(arg)
    local   r,  s,  M,  b1,  a1,  b2,  a2,  m;

    if IsList(arg[1]) and Length(arg[1])=2 then
        r := arg[1][1];
        s := arg[1][2];
    elif Length(arg)=2 then
        r := arg[1];
        s := arg[2];
    else
        Error("The argument of NumericalSemigroupByInterval must consist of a pair of rational numbers, <r> and <s> and with r < s");
    fi;
    if IsInt(r) and IsInt(s) and r < s then
        return NumericalSemigroupByGenerators([r..s]);
    fi;
    if not (IsRat(r) and IsRat(s) and r < s) then
        Error("The argument of NumericalSemigroupByInterval must consist of a pair of rational numbers, <r> and <s> and with r < s");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetClosedIntervalNS(M,[r,s]);
## the semigroup is proportionally modular (Lemma 4.12 [Rosales & Garcia-Sanchez - book])
    b1 := NumeratorRat(r);
    a1 := DenominatorRat(r);
    b2 := NumeratorRat(s);
    a2 := DenominatorRat(s);
    SetProportionallyModularConditionNS(M, [a1*b2,b1*b2,a1*b2-a2*b1]);
    if b1*b2 = 1 then #the semigroup is the entire N
        SetMinimalGenerators(M,[1]);
    fi;

    if a1*b2-a2*b1 = 1 then
      SetModularConditionNS(M, [a1*b2,b1*b2]);
    fi;

    m := CeilingOfRational(r);
    if s > 2*m -1 then #the semigroup is a halfline
        SetSmallElements(M,[0,m]);
        SetFrobeniusNumberOfNumericalSemigroup(M,m-1);
        SetMinimalGenerators(M, [m,2*m-1]);
    fi;

    return M;
end);



#############################################################################
##
#F  NumericalSemigroupByOpenInterval(arg)
##
##  Returns the numerical semigroup
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByOpenInterval, function(arg)
    local M, r, s;
    if IsList(arg[1]) and Length(arg[1])=2 then
        r := arg[1][1];
        s := arg[1][2];
    elif Length(arg)=2 then
        r := arg[1];
        s := arg[2];
    else
        Error("The argument of NumericalSemigroupByOpenInterval must consist of a pair of rational numbers");
    fi;
    if not (IsRat(r) and IsRat(s) and r < s) then
        Error("The argument of NumericalSemigroupByOpenInterval must consist of a pair of rational numbers, <r> and <s> and with r < s");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetOpenIntervalNS(M,[r,s]);
    return M;
end);



#############################################################################
##
#F  NumericalSemigroupBySubAdditiveFunction(L)
##
##  Returns the numerical semigroup specified by the subadditive
##  function L.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupBySubAdditiveFunction, function(L)
    local M;

    if not RepresentsPeriodicSubAdditiveFunction(L) then
        Error("The argument of NumericalSemigroupBySubAdditiveFunction must be a subadditive function");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetSubAdditiveFunctionNS(M,L);
    return M;
end);



#############################################################################
##
#F  NumericalSemigroupByAperyList(L)
##
##  Returns the numerical semigroup specified by the Apery list L.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByAperyList, function(L)
    local i, M;

    if not IsAperyListOfNumericalSemigroup(L) then
        Error("The argument of NumericalSemigroupByAperyList must be The Apery List of some numerical semigroup");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetAperyList(M,L);
    return M;
end);




#############################################################################
##
#F  NumericalSemigroupBySmallElements(L)
##
##  Returns the numerical semigroup specified by L,
##  which must be the list of elements of a numerical semigroup,
##  not greater than the Frobenius number + 1.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupBySmallElements, function(L)
    local i, M, K, R;

    if not RepresentsSmallElementsOfNumericalSemigroup(L) then
        Error("The argument does not represent a numerical semigroup");
    fi;
    if L = [0] or 1 in L then
        return NumericalSemigroup(1);
    fi;
    K := Difference([1..L[Length(L)]],L);
    R := Intersection([0..K[Length(K)]+1],L);

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetGaps(M,Difference([1..R[Length(R)]], R));
    SetSmallElements(M,R);
    return M;
end);

#############################################################################
##
#F  NumericalSemigroupBySmallElementsNC(L)
##
## NC version of NumericalSemigroupBySmallElements
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupBySmallElementsNC, function(L)
    local i, M, K, R;

   if L = [0] or 1 in L then
        return NumericalSemigroup(1);
    fi;
    K := Difference([1..L[Length(L)]],L);
    R := Intersection([0..K[Length(K)]+1],L);

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetGaps(M,Difference([1..R[Length(R)]], R));
    SetSmallElements(M,R);
    return M;
end);



#############################################################################
##
#F  NumericalSemigroupByGaps(L)
##
##  Returns the numerical semigroup specified by L,
##  which must be the list of gaps of a numerical semigroup.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByGaps, function(L)
    local i, M, K;
    K := Difference([0..L[Length(L)]+1],L);
    if not RepresentsSmallElementsOfNumericalSemigroup(K) then
        Error("The argument does not represent the gaps of  a numerical semigroup");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetGaps(M,L);
    SetSmallElements(M,K);
    return M;
end);




#############################################################################
##
#F  NumericalSemigroupByFundamentalGaps(L)
##
##  Returns the numerical semigroup specified by L,
##  which must be the list of fundamental gaps of a numerical semigroup.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupByFundamentalGaps, function(L)
    local i, M, K, G;
    G := Set(Flat(List(L,i->DivisorsInt(i))));
    K := Difference([0..G[Length(G)]+1],G);
    if not RepresentsSmallElementsOfNumericalSemigroup(K) then
        Error("The argument does not represent the fundamental gaps of  a numerical semigroup");
    fi;

    M:= Objectify( NumericalSemigroupsType, rec() );
    SetFundamentalGaps(M,L);
    SetGaps(M,G);
    SetSmallElements(M,K);
    return M;
end);


#############################################################################
##
#F NumericalSemigroupByAffineMap(a,b,c)
## Computes the smallest numerical semigroup
## containing c and closed under x->ax+b
## see http://arxiv.org/pdf/1505.06580v4.pdf
#############################################################################
InstallGlobalFunction(NumericalSemigroupByAffineMap,function(a,b,c)
    local gs,gen, t ,sk;
    if not(ForAll([a,b,c],x-> IsInt(x) and x>=0)) then
        Error("The arguments must be integers");
    fi;

    if not(Gcd(b,c)=1)then
        Error("The second and third arguments must be coprime");
    fi;

    if not(a>0) then
        Error("The first argument must be a positive integer");
    fi;
    if c=1 then
        return NumericalSemigroup(1);
    fi;

    t:=function(x)
        return a*x+b;
    end;
    gs:=[c];
    gen:=c;
    sk:=0;
    while sk<c do
        gen:=t(gen);
        Add(gs,gen);
        sk:=a*sk+1;
    od;
    return NumericalSemigroup(gs);
end);

#############################################################################
##
#F  NumericalSemigroup(arg)
##
##  This function's first argument may be one of:
##  "generators", "minimalgenerators", "modular",
##  "propmodular", "elements", "gaps",
##  "fundamentalgaps", "subadditive" or "apery" according to
##  how the semigroup is being defined.
##  The following arguments must conform to the arguments of
##  the corresponding function defined above.
##  By default, the option "generators" is used, so,
##  gap> NumericalSemigroup(3,7);
##  <Numerical semigroup with 2 generators>
##
#############################################################################
InstallGlobalFunction(NumericalSemigroup, function(arg)
    local L, S, M;

    if IsString(arg[1]) then
        if arg[1] = "modular" then
            if Length(arg) = 3 then
                return ModularNumericalSemigroup(arg[2],arg[3]);
            else
                Error("For the modular case NumericalSemigroup must have 3 arguments");
            fi;
        elif arg[1] = "propmodular" then
            if Length(arg) = 4 then
                return ProportionallyModularNumericalSemigroup(arg[2],arg[3],arg[4]);
            else
                Error("For the proportionally modular case NumericalSemigroup must have 4 arguments");
            fi;
        elif arg[1] = "interval" then
            if Length(arg) = 3 then
                return NumericalSemigroupByInterval(arg[2],arg[3]);
            elif Length(arg) = 2 then
                return NumericalSemigroupByInterval(arg[2]);
            else
                Error("For the interval case NumericalSemigroup must have 2 or 3 arguments");
            fi;
        elif arg[1] = "openinterval" then
            if Length(arg) = 3 then
                return NumericalSemigroupByOpenInterval(arg[2],arg[3]);
            elif Length(arg) = 2 then
                return NumericalSemigroupByOpenInterval(arg[2]);
            else
                Error("For the open interval case NumericalSemigroup must have 2 or 3 arguments");
            fi;
        elif arg[1] = "fundamentalgaps" then
            return NumericalSemigroupByFundamentalGaps(arg[2]);
        elif arg[1] = "gaps" then
            return NumericalSemigroupByGaps(arg[2]);
        elif arg[1] = "elements" then
            return NumericalSemigroupBySmallElements(arg[2]);
        elif arg[1] = "subadditive" then
            return NumericalSemigroupBySubAdditiveFunction(arg[2]);
        elif arg[1] = "apery" then
            return NumericalSemigroupByAperyList(arg[2]);
        elif arg[1] = "generators" then
            return NumericalSemigroupByGenerators(Flat(arg{[2..Length(arg)]}));
        elif arg[1] = "minimalgenerators" then
            return NumericalSemigroupByMinimalGenerators(Flat(arg{[2..Length(arg)]}));
        else
            Error("Invalid first argument, it should be one of: \"modular\", \"propmodular\", \"interval\", \"openinterval\", \"fundamentalgaps\", \"gaps\", \"elements\", \"subadditive\", \"apery\", \"generators\", \"minimalgenerators\" ");
       fi;


    else
        if Length(arg) = 1 and IsList(arg[1]) then
            return NumericalSemigroupByGenerators(arg[1]);
        else
            return NumericalSemigroupByGenerators(arg{[1..Length(arg)]});
        fi;
    fi;
 end);


 ########################################################################
 ########################################################################

 #############################################################################
 ##
 #M  PrintObj(S)
 ##
 ##  This method for numerical semigroups.
 ##
 #############################################################################
 InstallMethod( PrintObj,
         "prints a Numerical Semigroup",
         [ IsNumericalSemigroup],
         function( S )
     if HasModularConditionNS(S) then
         Print("ModularNumericalSemigroup( ", ModularConditionNS(S), " )\n");
     elif HasProportionallyModularConditionNS(S) then
         Print("ProportionallyModularNumericalSemigroup( ", ProportionallyModularConditionNS(S), " )\n");
     elif HasGenerators(S) then
         Print("NumericalSemigroup( ", Generators(S), " )\n");
     else
         Print("NumericalSemigroup( ", GeneratorsOfNumericalSemigroup(S), " )\n");
     fi;
 end);

 #############################################################################
 ##
 #M  ViewString(S)
 ##
 ##  This method for numerical semigroups.
 ##
 #############################################################################
 InstallMethod( ViewString,
         "displays a Numerical Semigroup",
         [IsNumericalSemigroup],
         function( S )
     if HasMinimalGenerators(S) and 1 in MinimalGenerators(S) then
         return ("The numerical semigroup N");
     elif HasMinimalGenerators(S) then
         return Concatenation("Numerical semigroup with ", String(Length(MinimalGenerators(S))), " generators");
     elif HasGenerators(S) then
         return Concatenation("Numerical semigroup with ", String(Length(Generators(S))), " generators");
     elif HasModularConditionNS(S) then
         return Concatenation("Modular numerical semigroup satisfying ", String(ModularConditionNS(S)[1]),"x mod ",String(ModularConditionNS(S)[2]), " <= x");
     elif HasProportionallyModularConditionNS(S) then
         return Concatenation("Proportionally modular numerical semigroup satisfying ", String(ProportionallyModularConditionNS(S)[1]),"x mod ",String(ProportionallyModularConditionNS(S)[2]), " <= ",String(ProportionallyModularConditionNS(S)[3]),"x");
     else
         return ("Numerical semigroup");
     fi;
 end);


 #############################################################################
 ##
 #M  ViewObj(S)
 ##
 ##  This method for numerical semigroups.
 ##
 #############################################################################
 InstallMethod( ViewObj,
         "displays a Numerical Semigroup",
         [IsNumericalSemigroup],
         function( S )
     if HasMinimalGenerators(S) and 1 in MinimalGenerators(S) then
         Print("<The numerical semigroup N>");
     elif HasMinimalGenerators(S) then
         Print("<Numerical semigroup with ", Length(MinimalGenerators(S)), " generators>");
     elif HasGenerators(S) then
         Print("<Numerical semigroup with ", Length(Generators(S)), " generators>");
     elif HasModularConditionNS(S) then
         Print("<Modular numerical semigroup satisfying ", ModularConditionNS(S)[1],"x mod ",ModularConditionNS(S)[2], " <= x >");
     elif HasProportionallyModularConditionNS(S) then
         Print("<Proportionally modular numerical semigroup satisfying ", ProportionallyModularConditionNS(S)[1],"x mod ",ProportionallyModularConditionNS(S)[2], " <= ",ProportionallyModularConditionNS(S)[3],"x >");
     else
         Print("<Numerical semigroup>");
     fi;
 end);



 #############################################################################
 ##
 #M  Display(S)
 ##
 ##  This method for numerical semigroups.
 ##
 #############################################################################
 InstallMethod( Display,
         "displays a Numerical Semigroup",
         [IsNumericalSemigroup],
         function( S )
     local M, u, L, condensed;

     condensed := function(L)
         local c, C,j, n, search, bool;
         C := [];
         bool := true;
         j := 0;
         c := L[1];
         search := function(n) # searches the greatest subinterval starting in n
             local i, k;
             k := 0;
             for i in [Position(L,n).. Length(L)-1] do
                 if not (L[i]+1 = L[i+1]) then
                     c := L[i+1];
                     return [n..n+k];
                 fi;
                 k := k+1;
             od;
             bool := false;
             return [n..L[Length(L)]];
         end;
         while bool do
             Add(C,search(c));
         od;
         return C;
     end;
     ##  End of condensed()  --

     L := SmallElementsOfNumericalSemigroup(S);
     M := condensed(L);
     u := [M[Length(M)][1],"->"];
     M[Length(M)] := u;
     return M;
 end);


 ####################################################
 ####################################################
 ##
 #P  IsProportionallyModularNumericalSemigroup(S)
 ##
 ##  Tests if a numerical semigroup is proportionally modular.
 ##
 #############################################################################
 # this implementation is based in Theorem 5.35 of [RGbook]
 #############################################################################
 InstallMethod(IsProportionallyModularNumericalSemigroup,
         "Tests if a Numerical Semigroup is proportionally modular",
         [IsNumericalSemigroup],
         function( S )
     local   gens,  arrangements,  gs,  k,  addgenerator,  g,  arrangement,
             b1,  a1,  b2,  a2;

     if HasProportionallyModularConditionNS(S) then
       return true;
     fi;

    gens := MinimalGeneratingSystemOfNumericalSemigroup(S);
#    if Length(gens) <= 2 then
#        return true;
#    fi;
    if Length(gens) = 1 then
        SetClosedIntervalNS(S, [1,2]);
        SetProportionallyModularConditionNS(S, [1,1,1]);
        SetModularConditionNS(S, [1,1]);
        return true;
    else
        arrangements := [[gens[1],gens[2]]];
        gs := gens{[3..Length(gens)]};
    fi;

    k := 2;

    addgenerator := function(h)
        local   newarrangement,  a,  left,  right;
        k:=k+1;
        newarrangement := [];
        for a in arrangements do
            left := Concatenation([h],a);
            right := Concatenation(a,[h]);

            if (left[1]+left[3]) mod left[2] = 0 then
                Add(newarrangement, left);
            fi;
            if (right[k]+right[k-2]) mod right[k-1] = 0 then
                Add(newarrangement, right);
            fi;
        od;
        return newarrangement;
    end;

    for g in gs do
        arrangements := addgenerator(g);
        if arrangements = [] then
            return false;
        fi;
    od;
    #    Print(arrangement,"\n");
    arrangement := arrangements[1];
    b1 := arrangement[1];
    a1 := (arrangement[2])^-1 mod b1;
    b2 := arrangement[k];
    a2 := (-arrangement[k-1])^-1 mod b2;

    SetClosedIntervalNS(S, [b1/a1,b2/a2]);
    SetProportionallyModularConditionNS(S, [a1*b2,b1*b2,a1*b2-a2*b1]);
    if b1*b2 = 1 then #the semigroup is the entire N
        SetMinimalGenerators(S,[1]);
    fi;
    if a1*b2-a2*b1 = 1 then
        SetModularConditionNS(S, [a1*b2,b1*b2]);
    fi;

    return true;
end);

 #############################################################################
 # this function has been replaced by the above one in version 0.971.
 #############################################################################
# InstallMethod(IsProportionallyModularNumericalSemigroup,
#         "Tests if a Numerical Semigroup is proportionally modular",
#         [IsNumericalSemigroup],
#         function( S )
#     local   pm,  gens,  bzs,  gg,  r,  s,  b1,  a1,  b2,  a2;
#
#     ############# local function ##########
#     # returns a Bezout sequence in case S is proportionally modular and false otherwise
#     pm:=function(bs,gs) #bs contains a bezout sequence, gs contains the set of generators not added to it
#
#         local   generators,  a,  b,  first,  inverse,  bezoutseq,  last;
#
#         if gs=[] then
#             return bs;
#         fi;
#
#         generators:=Set(ShallowCopy(gs));
#
#         if bs = [] then
#             a:=Minimum(generators);
#             RemoveSet(generators,a);
#             b:=Minimum(generators);
#             RemoveSet(generators,b);
#             if GcdInt(a,b) <> 1 then
#                 return false;
#             fi;
#             return pm([[a,PowerMod(b,-1,a)], [b,b-PowerMod(a,-1,b)]], generators);
#         fi;
#
#
#         a:=Minimum(generators);
#         first:=bs[1];
#
#         inverse:=(1+a*first[2])/first[1];
#
#         if IsInt(inverse) then
#             RemoveSet(generators,a);
#             bezoutseq:=Concatenation([[a,inverse]],bs);
#             return pm(bezoutseq,generators);
#         fi;
#
#         a:=Minimum(generators);
#         last:=bs[Length(bs)];
#
#         inverse:=(-1+a*last[2])/last[1];
#
#         if IsInt(inverse) then
#             RemoveSet(generators,a);
#             bezoutseq:=Concatenation(bs,[[a,inverse]]);
#             return pm(bezoutseq,generators);
#         fi;
#
#         return false;
#     end;
#     ######### end of local function #########
#
#     gens := MinimalGeneratingSystemOfNumericalSemigroup(S);
#     if Length(gens) = 1 then
#        S!.closedinterval := [1,2];
#        Setter(IsNumericalSemigroupByInterval)(S,true);
#        S!.proportionallymodularcondition := [1,1,1];
#        Setter(IsProportionallyModularNumericalSemigroup)(S,true);
#        S!.modularcondition := [1,1];
#        Setter(IsModularNumericalSemigroup)(S,true);
#        return true;
#     else
#         bzs := pm([],gens); # the test...
#     fi;
#
#     if bzs = false then
#         return false;
#     else
#         gg := List(bzs, l -> l[1]/l[2]);
#         r := Minimum(gg);
#         s := Maximum(gg);
#         S!.closedinterval := [r,s];
#         Setter(IsNumericalSemigroupByInterval)(S,true);
#         b1 := NumeratorRat(r);
#         a1 := DenominatorRat(r);
#         b2 := NumeratorRat(s);
#         a2 := DenominatorRat(s);
#         Setter(IsProportionallyModularNumericalSemigroup)(S,true);
#         S!.proportionallymodularcondition := [a1*b2,b1*b2,a1*b2-a2*b1];
#         if b1*b2 = 1 then #the semigroup is the entire N
#             S!.minimalgenerators := [1];
#             Setter(IsNumericalSemigroupByMinimalGenerators)(S,true);
#         fi;
#         if a1*b2-a2*b1 = 1 then
#             Setter(IsModularNumericalSemigroup)(S,true);
#             S!.modularcondition := [a1*b2,b1*b2];
#         fi;
#
#         return true;
#     fi;
# end);
 #
 #############################################################################
 ##
 #P  IsModularNumericalSemigroup(S)
 ##
 ##  Tests if a numerical semigroup is modular.
 ##
 #############################################################################
 InstallMethod(IsModularNumericalSemigroup,
         "Tests if a Numerical Semigroup is modular",
         [IsNumericalSemigroup],
         function( S )
     local   lhs,  gs,  ms,  b,  A,  a,  C,  c,  Ac;

     if HasModularConditionNS(S) then
       return true;
     fi;

     lhs := Length(GapsOfNumericalSemigroup(S));
     gs := FrobeniusNumberOfNumericalSemigroup(S);
     ms := MultiplicityOfNumericalSemigroup(S);     #the least positive integer in S
     b := gs + ms;
     A := [];
     for a in [2..Int((b+1)/2)] do
         if b = 2*lhs + GcdInt(a,b) + GcdInt(a-1,b)-1 and
            ms < Minimum(b/GcdInt(a,b),b/GcdInt(a-1,b)) then
             Add(A,a);
         fi;
     od;
     for a in A do
         if Display(S) = Display(ModularNumericalSemigroup(a,b)) then
             SetModularConditionNS(S, [a,b]);
             SetProportionallyModularConditionNS(S, [a,b,1]);
             return(true);
         fi;
     od;
     C := [];
     for c in [2 * lhs +1 .. 12 * lhs -6] do
         if RemInt(c,ms) = 0 then
             Add(C,c);
         fi;
     od;
     for c in C do
         Ac := [];
         for a in [2..Int((c+1)/2)] do
             if c = 2*lhs + GcdInt(a,c) + GcdInt(a-1,c)-1 and
                ms = Minimum(c/GcdInt(a,c),c/GcdInt(a-1,c)) then
                 Add(Ac,a);
             fi;
         od;
         for a in Ac do
             if Display(S)
                = Display(ModularNumericalSemigroup(a,c)) then
                 SetModularConditionNS(S, [a,c]);
                 SetProportionallyModularConditionNS(S, [a,c,1]);
                 return(true);
             fi;
         od;
     od;
     return(false);
 end);



 InstallOtherMethod( One,
         "partial method for a Numerical Semigroup",
         true,
         [ IsMagmaWithOne and IsNumericalSemigroup ], 100,
         #T high priority?
         function( M )
     return(0);
 end );




 InstallMethod(MultiplicativeNeutralElement,
         "for a magma-with-one",
         true,
         [HasMultiplicativeNeutralElement and IsMagmaWithOne and IsNumericalSemigroup], GETTER_FLAGS+1,
         function(S)
     return(0);
 end);




 InstallMethod( Size,
         "for a collection",
         [ IsCollection and IsNumericalSemigroup ],
         function(S)
     return(infinity);
     return(Length(AsSSortedList(S)));
 end);

 ############################################################################
 ##
 #M Methods for the comparison of numerical semigroups.
 ##
 InstallMethod( \=,
         "for two numerical semigroups",
         [IsNumericalSemigroup and IsNumericalSemigroupRep,
          IsNumericalSemigroup and IsNumericalSemigroupRep],
         function(x, y )

     if Set(GeneratorsOfNumericalSemigroup(x)) =
        Set(GeneratorsOfNumericalSemigroup(y)) then
         return(true);
     elif HasMinimalGenerators(x) and HasMinimalGenerators(y) then
         return  MinimalGenerators(x)=MinimalGenerators(y);

     elif HasModularConditionNS(x) and HasModularConditionNS(y) and
       ModularConditionNS(x) = ModularConditionNS(y) then
         return  true;

     elif  HasProportionallyModularConditionNS(x) and HasProportionallyModularConditionNS(y) and
       ProportionallyModularConditionNS(x) = ProportionallyModularConditionNS(y) then
         return true;

     elif HasAperyList(x) and HasAperyList(y) then
         return  AperyList(x) = AperyList(y);

     elif HasGaps(x) and HasGaps(y) then
         return  Gaps(x) = Gaps(y);

     elif HasFundamentalGaps(x) and HasFundamentalGaps(y) then
         return FundamentalGaps(x) = FundamentalGaps(y);

     elif HasSmallElements(x) and HasSmallElements(y) then
         return SmallElements(x) = SmallElements(y);

     else
         return SmallElementsOfNumericalSemigroup(x) = SmallElementsOfNumericalSemigroup(y);
     fi;
 end);

 InstallMethod( \<,
         "for two numerical semigroups",
         [IsNumericalSemigroup,IsNumericalSemigroup],
         function(x, y )
     return(SmallElementsOfNumericalSemigroup(x) < SmallElementsOfNumericalSemigroup(y));
 end );
