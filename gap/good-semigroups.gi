#############################################################################
##
#W  good-semigroups.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2016-- Centro de Matemática da Universidade do Porto, Portugal and IEMath-GR, Universidad de Granada, Spain
#############################################################################

####################################################
##
#F NumericalDuplication(S,E,b)
## returns 2S\cup(2E+b)
####################################################
InstallGlobalFunction(NumericalDuplication, function(S,E,b)
    local smallS, doubS, smallE, f, small, mgsE, mgsS;

    if not(IsNumericalSemigroup(S)) or not(IsIdealOfNumericalSemigroup(E)) then
      Error("The first argument must be a numerical semigroup, and the second an ideal");
    fi;

    if not(IsInt(b)) then
      Error("The third argument must be an integer");
    fi;

    if not(b mod 2=1) then
      Error("The third argument must be an odd integer");
    fi;

    # if not(b in S) then
    #   Error("The third argument must belong to the first argument");
    # fi;

    mgsE:=MinimalGeneratingSystem(E);
    # if not(ForAll(mgsE, x -> x in S)) then
    #   Error("The second argument must be a integral ideal of the first");
    # fi;

    mgsS:=MinimalGenerators(S);

    if not(ForAll(Cartesian(mgsE,mgsE), p->Sum(p)+b in S)) then
      Error("The arguments do not define a semigroup (E+E+b is not included in S)");
    fi;

    return NumericalSemigroup(Union(2*mgsS, 2*mgsE+b));
end);

####################################################
##
#F AsNumericalDublication(T)
## Detects whether a numerical semigroup T can be obtained 
## as a numerical duplication (with a proper ideal). 
## It returns fail or the list [S,I,b], such that 
## T=NumericalDuplication(S,I,b)
####################################################
InstallGlobalFunction(AsNumericalDuplication,
function(T)
    local G, Ev, O, j, S, x, Sm, B, b, H1, N, k, I;
    if ((Multiplicity(T) mod 2)=0) then
        G:=MinimalGenerators(T);
        O:=[];
        Ev:=[];
        for j in [1..Length(G)] do
            if ((G[j] mod 2)=1) then
                Append(O,[G[j]]);
            fi;
            if ((G[j] mod 2)=0) then
                Append(Ev,[G[j]]);
            fi;
        od;
        if Gcd(Ev/2)=1 then
            S:=NumericalSemigroup(Ev/2);
            Sm:=SmallElements(S);
            B:=[];
            x:=Minimum(O)-Multiplicity(T);
            for j in [1..Length(Sm)] do
                if ((Sm[j] mod 2)=1) and (Sm[j]<=x) then
                    Append(B,[Sm[j]]);
                fi;
            od;
            for j in [Sm[Length(Sm)]..x] do
                if ((j mod 2)=1) then
                    Append(B,[j]);
                fi;
            od;
            for b in [1..Length(B)] do
                if B[b]<=Minimum(O) then
                    H1:=[];
                    H1:=(O-B[b])/2;
                    N:=0;
                    for k in [1..Length(H1)] do
                        if (H1[k] in S) then 
                            N:=N+1;
                        fi;
                    od;
                    if (N=Length(H1)) then	
                        I:=H1+S;
                        return([S,I,B[b]]);
                    fi;
                fi;
            od;
        fi;
    fi;
    return(fail);
end);


###################################################
##
#F NumericalSemigroupDuplication(S,E)
## returns S\bowtie E
###################################################
InstallGlobalFunction(NumericalSemigroupDuplication,function(S,E)
    local M, mgsE;


    if not(IsNumericalSemigroup(S)) or not(IsIdealOfNumericalSemigroup(E)) then
      Error("The first argument must be a numerical semigroup, and the second an ideal");
    fi;

    mgsE:=MinimalGeneratingSystem(E);
    if not(ForAll(mgsE, x -> x in S)) then
      Error("The second argument must be an integral ideal of the first");
    fi;

    M:=Objectify(GoodSemigroupsType, rec());
    SetNumericalSemigroupGS(M,S);
    SetIdealGS(M,E);
    SetDefinedByDuplication(M,true);

    return M;
end);


###################################################
##
#F AmalgamationOfNumericalSemigroups(S,E,c)
## returns S\bowtie^f E, f multiplication by c
###################################################
InstallGlobalFunction(AmalgamationOfNumericalSemigroups, function(S,E,c)
    local M, T, msg;

    if not(IsNumericalSemigroup(S)) or not(IsIdealOfNumericalSemigroup(E)) then
      Error("The first argument must be a numerical semigroup, and the second an ideal");
    fi;

    if not(IsPosInt(c)) then
      Error("The third argument must be a positive integer");
    fi;

    T:=AmbientNumericalSemigroupOfIdeal(E);
    msg:=MinimalGeneratingSystem(S);
    if not(ForAll(msg, x-> c*x in T)) then
      Error("Multiplication by the third argument must be a morphism from the first argument to the ambient semigroup of the second");
    fi;

    M:=Objectify(GoodSemigroupsType, rec());
    SetNumericalSemigroupGS(M,S);
    SetIdealGS(M,E);
    SetMorphismGS(M,c);
    SetDefinedByAmalgamation(M,true);

    return M;
end);

###################################################
##
#F CartesianProductOfNumericalSemigroups(S1,S2)
## Computes the cartesian product of S1 and S2, which
## is a good semigroup
###################################################
InstallGlobalFunction(CartesianProductOfNumericalSemigroups, function(S1,S2)
    local M;

    if not(IsNumericalSemigroup(S1)) or not(IsNumericalSemigroup(S2)) then
      Error("The arguments must be numerical semigroups");
    fi;

    M:=Objectify(GoodSemigroupsType, rec());
    SetNumericalSemigroupListGS(M,[S1,S2]);
    SetDefinedByCartesianProduct(M,true);

    return M;
end);



###################################################
##
#F RepresentsSmallElementsOfGoodSemigroup(X)
## detects if X is a good semiring
###################################################
InstallGlobalFunction(RepresentsSmallElementsOfGoodSemigroup, function(X)
    local c, inf, conG2, C;

    inf:=function(x,y)
        return [Minimum(x[1],y[1]), Minimum(x[2],y[2])];
    end;

    if not(IsRectangularTable(X)) and ForAll(X, x->Length(x)=2) then
      Error("The argument must be a list of pairs of positive integers");
    fi;

    if not(ForAll(X, x->IsInt(x[1]) and IsInt(x[2]) and x[1]>=0 and x[2]>=0)) then
      Error("The argument must be a list of pairs of positive integers");
    fi;

    C:=[0,0];
    if not(C in X) then
        Info(InfoNumSgps,2,"Zero is not in the set.");
        return false;
    fi;

    C[1]:=Maximum(List(X,x->x[1]));
    C[2]:=Maximum(List(X,x->x[2]));
    if not(C in X) then
        Info(InfoNumSgps,2,"The maximum is not in the set.");
        return false;
    fi;

    c:=Cartesian(X,X);
    if ForAny(c, p->not(inf(p[1]+p[2],C) in X)) then
        Info(InfoNumSgps,2,"The set is not closed under addition modulo inf the maximum.");
        return false;
    fi;

    if ForAny(c, p->not(inf(p[1], p[2]) in X)) then
        Info(InfoNumSgps,2,"The set is not closed under infimums.");
        return false;
    fi;
    conG2:=First(X, x->x[1]<C[1] and ForAny(X,y-> x[1]=y[1] and x[2]<y[2] and not(ForAny(X, z->x[2]=z[2] and z[1]>x[1]))));
    if conG2<>fail then
        Info(InfoNumSgps,2,"The set is not a good semiring: ",conG2);
        return false;
    fi;
    conG2:=First(X, x->x[2]<C[2] and ForAny(X,y-> x[2]=y[2] and x[1]<y[1] and not(ForAny(X, z->x[1]=z[1] and z[2]>x[2]))));
    if conG2<>fail then
        Info(InfoNumSgps,2,"The set is not a good semiring: ",conG2);
        return false;
    fi;
    return true;
end);


###################################################
##
#F GoodSemigroup(X,C)
## define the good semigroup from the set of points G
## with conductor C
###################################################
InstallGlobalFunction(GoodSemigroup, function(Gn,C)
    local M, p1, p2, c, CC, sm, SemiRing_NS, gen, inf, G;

    if not(IsRectangularTable(Gn)) and ForAll(Gn, x->IsList(x) and Length(x)=2) then
      Error("The first argument must be a list of pairs of positive integers");
    fi;

    if not(ForAll(Gn, x->IsInt(x[1]) and IsInt(x[2]) and x[1]>=0 and x[2]>=0)) then
      Error("The first argument must be a list of pairs of positive integers");
    fi;

    if not(IsList(C)) and Length(C)=2 then
      Error("The second argument must be a list (pair) of nonnegative integers");
    fi;

    if not(IsInt(C[1]) and IsInt(C[2]) and C[1]>=0 and C[2]>=0) then
      Error("The second argument must be a list (pair) of nonnegative integers");
    fi;

    inf:=function(u,v)
        return [Minimum(u[1],v[1]),Minimum(u[2],v[2])];
    end;

      ###############################################################
      ##
      #F SemiRing_NS(G,C)
      ## G is a set of points;
      ## computes the saturation wrt sum (cutted by C) and inf
      ################################################################
      SemiRing_NS:=function(G,C)
          local O, OO, new,i,j, o;


          O:=G;
          # sum saturation
          repeat
              new:=[];
              for i in [1..Length(O)] do
                  for j in [i.. Length(O)] do
                      o:=inf(C,O[i]+O[j]);

                      if not(o in O) then
                          Add(new, o);
                      fi;
                  od;
              od;
              O:=Union(O,new);
          until new=[];

          O:=Union(O,[[0,0]]);
          #inf saturation
          repeat
              new:=[];
              for i in [1..Length(O)] do
                  for j in [i.. Length(O)] do
                      o:=inf(O[i],O[j]);
                      if not(o in O) then
                          Add(new, o);
                      fi;
                  od;
              od;
              O:=Union(O,new);
          until new=[];

          # #good saturation x coordinate
          # repeat
          #     new:=First(O, x->x[1]<C[1] and ForAny(O,y-> x[1]=y[1] and x[2]<y[2] and not(ForAny(O, z->x[2]=z[2] and z[1]>x[1]))));
          #     if new<>fail then
          #         Add(O,[C[1],new[2]]);
          #     fi;

          # until new=fail;
          # #good saturation y coordinate
          # repeat
          #     new:=First(O, x->x[2]<C[2] and ForAny(O,y-> x[2]=y[2] and x[1]<y[1] and not(ForAny(O, z->x[1]=z[1] and z[2]>x[2]))));
          #     if new<>fail then
          #         Add(O,[new[1],C[2]]);
          #     fi;

          # until new=fail;
          return O;
      end; # of SemiRing_NS

    G:=ShallowCopy(Gn);
    p1:=List(G,x->x[1]);
    p2:=List(G,x->x[2]);

    CC:=[Maximum(p1), Maximum(p2)];
    if not(C[1]>=CC[1] and C[2]>=CC[2]) then
        Info(InfoNumSgps, 2, "The conductor is not larger than the maximum of the given set");
        G:=List(G, x->inf(x,C));
    fi;


    sm:=Union(SemiRing_NS(G,C),[C]);
    if not(RepresentsSmallElementsOfGoodSemigroup(sm)) then
      Error("The given set does not generate a good semigroup");
    fi;

    c:=C;
    while ((c-[0,1]) in sm) or ((c-[1,0]) in sm) do
      if ((c-[0,1]) in sm) and ((c-[1,0]) in sm) then #c-[1,1] also in sm
        c:=c-[1,1];
      elif c-[0,1] in sm then
        c:=c-[0,1];
      else
        c:=c-[1,0];
      fi;
    od;
    if C<>c then #sm must be redefined, and gens
      Info(InfoNumSgps,2,"Conductor redefined");
      sm:=Filtered(sm, x->x[1]<=c[1] and x[2]<=c[2]);
      gen:=List(G, x->inf(x,c));#x[1]<=c[1] and x[2]<=c[2]);
    else
      gen:=ShallowCopy(G);
    fi;

    M:=Objectify(GoodSemigroupsType, rec());
    SetGenerators(M,gen);
    SetConductor(M,c);
    SetSmallElements(M,sm);
    return M;
end);

###################################################
##
#M ConductorOfGoodSemigroup(M)
#M Conductor(M)
## returns the conductor of M
##################################################
InstallMethod(ConductorOfGoodSemigroup,
        "Calculates the conductor of the semigroup",
        [IsGoodSemigroup ],50,
        function(M)
    local e,s,c,ce,cs,c1,c2;

    if HasConductor(M) then
        return(Conductor(M));
    fi;

    if HasSmallElements(M) then
      s:=SmallElements(M);
      c:=s[Length(s)];
      SetConductor(M,c);
      return c;
    fi;

    if IsGoodSemigroupByDuplication(M) then
        Info(InfoNumSgps,2,"Using semigroup duplication formula");
        e:=IdealGS(M);
        ce:=Conductor(e);
        SetConductor(M,[ce,ce]);
        return [ce,ce];
    fi;

    if IsGoodSemigroupByAmalgamation(M) then
        Info(InfoNumSgps,2,"Using amalgamation formula");
        e:=IdealGS(M);
        ce:=Conductor(e);
        s:=NumericalSemigroupGS(M);
        c:=MorphismGS(M);
        cs:=CeilingOfRational(ce/c);
        while true do
            cs:=cs-1;
            if (cs in s) and not(2*cs in e) then
                SetConductor(M,[cs+1,ce]);
                return [cs+1,ce];
            fi;
            if cs<0 then
                SetConductor(M,[0,ce]);
                return [0,ce];
            fi;
        od;
    fi;

    if IsGoodSemigroupByCartesianProduct(M) then
      Info(InfoNumSgps,2,"This is a cartesian product, so the two conductors in a list");
      return([Conductor(NumericalSemigroupListGS(M)[1]),
              Conductor(NumericalSemigroupListGS(M)[2])]);
    fi;

    return fail;

end);

###################################################
##
#A SmallElements(M)
#A SmallElementsOfGoodSemigroup(M)
## returns de small elements of M, that is,
## the elements below the conductor
##################################################
InstallMethod(SmallElementsOfGoodSemigroup,
        "Calculates the small elements of the semigroup",
        [IsGoodSemigroup ],50,
        function(M)
    local C,box, sm;

    if HasSmallElements(M) then
        return SmallElements(M);
    fi;

    if IsGoodSemigroupByCartesianProduct(M) then
      sm:=Cartesian(SmallElements(NumericalSemigroupListGS(M)[1]),
                    SmallElements(NumericalSemigroupListGS(M)[2]));
      SetSmallElements(M,sm);
      return sm;
    fi;
    C:=Conductor(M);
    box:=Cartesian([0..C[1]],[0..C[2]]);
    sm:=Intersection(box,M);
    SetSmallElements(M,sm);
    return sm;
end);


###############################################################
##
#A IsSymmetric(M)
## Determines if M is symmetric
###############################################################
InstallMethod(IsSymmetricGoodSemigroup,
"Determines if the good semigroup is symmetric",
[IsGoodSemigroup], function(M)
  local sm, w1, w2, b1,b2, c;
  c:=Conductor(M);
  sm:=SmallElementsOfGoodSemigroup(M);
  w1:=Length(Set(sm,x->x[1]));
  w2:=Length(Set(sm,x->x[2]));
  b1:=Length(Filtered(sm, x-> x[1]=c[1] and x[2]<c[2]));
  b2:=Length(Filtered(sm, x-> x[2]=c[2] and x[1]<c[1]));
  return Sum(c)=w1+w2+b1+b2-2;
end);



###################################################
##
#A MinimalGenerators(M)
#A MinimalGoodGeneratingSystemOfGoodSemigroup(M)
## returns the unique minimal good generating of the
## good semigroup M
###################################################
InstallMethod(MinimalGoodGeneratingSystemOfGoodSemigroup,
       "Calculates the minimal generating system of the semigroup",
        [IsGoodSemigroup ],50,
        function(M)
  local filter,mingen,C;

  ## G is a given set of small elements
  ## filter outputs a subset that generates all
  filter:=function(G,C)

      local member1, member2,  gen, g, gg, visited, left;


      member1:=function(X,x)
          if x[1]*x[2]=0 then
              return x[2]<=0 and x[1]=0;
          fi;
          if x[1]<0 or x[2]< 0 then
              return false;
          fi;

          return ForAny(X, y->member1(X,x-y));

      end;

      member2:=function(X,x)
          if x[1]*x[2]=0 then
              return x[1]<=0 and x[2]=0;
          fi;
          if x[1]<0 or x[2]< 0 then
              return false;
          fi;

          return ForAny(X, y->member2(X,x-y));

      end;

      gen:=Set(ShallowCopy(G));
      RemoveSet(gen,[0,0]);

      # removing those that can be infimums of two others to make faster
      # the next test
      for g in G do
          if g[1]<C[1] then
              if First(gen, x->x[1]=g[1] and x[2]>g[2])<>fail then
                  RemoveSet(gen,g);
                  continue;
              fi;
          fi;

          if g[2]<C[2] then
              if First(gen, x->x[2]=g[2] and x[2]>g[2])<>fail then
                  RemoveSet(gen,g);
                  continue;
              fi;
          fi;
      od;

      if gen=[] then
        return [];
      fi;

      visited:=[];
      left:=gen;
      while left<>[] and gen<>[] do
          g:=left[1];
          AddSet(visited,g);
          left:=Difference(gen,visited);
          gg:=Difference(gen,[g]);
          if g[1]=C[1] then
              if member2(gg,g) then
                  RemoveSet(gen,g);
              fi;
          elif g[2]=C[2] then
              if member1(gg,g) then
                  RemoveSet(gen,g);
              fi;
          else
              if member1(gg,g) and member2(gg,g) then
                  RemoveSet(gen,g);
              fi;
          fi;
      od;
      return gen;
  end;

  if IsGoodSemigroupByCartesianProduct(M) then
    Print("ToDo\n");
    return fail;
  fi;

  C:=Conductor(M);
  if HasGenerators(M) then
    mingen:=filter(Generators(M),C);
    #SetGenerators(M,mingen);
    return mingen;
  fi;

  mingen:=filter(SmallElementsOfGoodSemigroup(M),C);
  SetGenerators(M,mingen);
  return mingen;
end);

###############################################################
##
#F GoodSemigroupBySmallElements(M)
## Constructs good semigroup from a set of small elements
###############################################################
InstallGlobalFunction(GoodSemigroupBySmallElements, function(X)
  local M, C, c, sm;
  if not(IsRectangularTable(X)) and ForAll(X, x->Length(x)=2) then
    Error("The argument must be a list of pairs of positive integers");
  fi;

  if not(ForAll(X, x->IsInt(x[1]) and IsInt(x[2]) and x[1]>=0 and x[2]>=0)) then
    Error("The argument must be a list of pairs of positive integers");
  fi;

  if not(RepresentsSmallElementsOfGoodSemigroup(X)) then
    Error("This set is not the set of small elements of a good semigroup");
  fi;

  C:=[Maximum(List(X,x->x[1])),Maximum(List(X,x->x[2]))];

  #now we see if C is the minimum conductor
  c:=C;
  sm:=ShallowCopy(X);
  while ((c-[0,1]) in sm) or ((c-[1,0]) in sm) do
    if ((c-[0,1]) in sm) and ((c-[1,0]) in sm) then #c-[1,1] also in sm
      c:=c-[1,1];
    elif c-[0,1] in sm then
      c:=c-[0,1];
    else
      c:=c-[1,0];
    fi;
  od;
  if C<>c then #small elements must be redefined
    Info(InfoNumSgps,2,"Conductor redefined");
    sm:=Filtered(X, x->x[1]<=c[1] and x[2]<=c[2]);
  fi;

  M:=Objectify(GoodSemigroupsType, rec());
  #SetGenerators(M,gen);
  SetConductor(M,c);
  SetSmallElements(M,sm);
  return M;
end);

###############################################################
##
#F ArfGoodSemigroupClosure(M)
## Constructs Arf good semigroup closure of M
###############################################################
InstallGlobalFunction(ArfGoodSemigroupClosure,function(s)
   local CompatibilityLevelOfMultiplicitySequences, s1, s2, t1, t2, sm, sma, c, included, i, cand, ca, c1, c2, k, seq1, seq2, tail1, tail2, car;
    
  CompatibilityLevelOfMultiplicitySequences:=function(M)
            local ismultseq, k, s, max, D, i,j, inarf;
            # tests whether x is in the Arf semigroup with multiplicity
            # sequence j
            inarf:=function(x,j)
                local l;
                if x>Sum(j) then
                return true;
                fi;
                if x=0 then
                return true;
                fi;
                if x<j[1] then
                return false;
                fi;
                l:=List([1..Length(j)], i-> Sum(j{[1..i]}));
                return x in l;
            end;

            # tests if m is a multiplicity sequence
            ismultseq := function(m)
                local n;
                n:=Length(m);
                return ForAll([1..n-1], i-> inarf(m[i], m{[i+1..n]}));
            end;

            if not(IsTable(M)) then
                Error("The first argument must be a list of multiplicity sequences");
            fi;

            if Length(M)<>2 then
                Error("We are so far only considering Arf good semigroups in N^2");
            fi;

            if not(ForAll(Union(M), IsPosInt)) then
                Error("The first argument must be a list of multiplicity sequences");
            fi;

            if not(ForAll(M, ismultseq)) then
                Error("The first argument must be a list of multiplicity sequences");
            fi;

            s:=[];
            max:= Maximum(List(M, Length));

            for i in [1..2] do
                s[i]:=[];
                for j in [1..Length(M[i])] do
                s[i][j]:=First([j+1..Length(M[i])], k-> M[i][j]=Sum(M[i]{[j+1..k]}));
                if s[i][j]=fail then
                    s[i][j]:=M[i][j]-Sum(M[i]{[j+1..Length(M[i])]})+Length(M[i])-j;
                else
                    s[i][j]:=s[i][j]-j;
                fi;
                od;
            od;
            for i in [1..2] do
                s[i]:=Concatenation(s[i],List([Length(s[i])+1..max],_->1));
            od;

            D:=Filtered([1..max], j->s[1][j]<>s[2][j]);
            k:=[];
            if D=[] then
                k:=infinity;
            else
                k:=Minimum(Set(D, j->j+Minimum(s[1][j],s[2][j])));
            fi;
            return k;
  end;

  if not(IsGoodSemigroup(s)) then
    Error("The argument must be a good semigroup");
  fi;


  
  sm := SmallElementsOfGoodSemigroup(s);
  c:= Conductor(s);
  s1:=Set(sm, x->x[1]);
  s2:=Set(sm, x->x[2]);
  s1:=NumericalSemigroupBySmallElements(s1);
  s2:=NumericalSemigroupBySmallElements(s2);
  t1:=ArfNumericalSemigroupClosure(s1);
  t2:=ArfNumericalSemigroupClosure(s2);
  c1:=Conductor(t1);
  c2:=Conductor(t2);
  seq1:=MultiplicitySequenceOfNumericalSemigroup(t1);
  seq2:=MultiplicitySequenceOfNumericalSemigroup(t2);
  k:=CompatibilityLevelOfMultiplicitySequences([seq1,seq2]);
  
  t1:=Intersection([0..c[1]],t1);
  t2:=Intersection([0..c[2]],t2);
  ca:=[c1,c2];
  i:=1;
  included:=true;
  while included do
    if i>Length(t1) or i>Length(t2) then
      i:=i-1;
      sma:=List([1..i], i->[t1[i],t2[i]]);
      return GoodSemigroupBySmallElements(sma);
    fi;
    if i>k+1 then
      i:=i-1;
      sma:=List([1..i], i->[t1[i],t2[i]]);
      return GoodSemigroupBySmallElements(sma);
    fi;
    sma:=Union(List([1..i], i->[t1[i],t2[i]]), Cartesian(t1{[i+1..Length(t1)]}, t2{[i+1..Length(t2)]}));
    if First(sm, x->not(x in sma))<> fail then
      included:=false;
    else
        i:=i+1;
    fi;
  od;
  i:=i-1;
  tail1:=Intersection(t1{[i+1..Length(t1)]},[0..c[1]]);
  tail2:=Intersection(t2{[i+1..Length(t2)]},[0..c[2]]);
  car:=Cartesian(tail1,tail2);
  sma:=Union(List([1..i], i->[t1[i],t2[i]]), car);

  return GoodSemigroupBySmallElements(sma);
end);

InstallMethod(ArfClosure,
"Computes the Arf closure of a good semigroup",
[IsGoodSemigroup],
  ArfGoodSemigroupClosure
);

###############################################################
##
#F MaximalElementsOfGoodSemigroup(M)
## returns the set of maximal elements of M
###############################################################
InstallGlobalFunction(MaximalElementsOfGoodSemigroup,function(g)
  local sm;
  if not(IsGoodSemigroup(g)) then
    Error("The argument must be a good semigroup");
  fi;

  sm:=SmallElements(g);
  return Filtered(Difference(sm,[Conductor(g)]), x->not(ForAny(sm,
      y->((y[1]=x[1] and y[2]>x[2]) or (y[1]>x[1] and y[2]=x[2])))));
end);

###############################################################
##
#F IrreducibleMaximalElementsOfGoodSemigroup(M)
## returns the set of irreducible maximal elements of M
###############################################################
InstallGlobalFunction(IrreducibleMaximalElementsOfGoodSemigroup,
function(g)
  local mx;
  mx:=MaximalElementsOfGoodSemigroup(g);
  if Length(mx)=1 then
    return mx;
  fi;
  return Filtered(Difference(mx,[[0,0]]), x->not(ForAny(Difference(mx,[[0,0]]), y->y<>x and (y[1]<=x[1]) and (y[2]<=x[2]) and ((x-y) in mx))));
end);

###############################################################
##
#F GoodSemigroupByMaximalElements(S1,S2,mx,c)
## returns the good semigroup determined by removing from
## S1 x S2 the set of points "above" a maximal element; c is
## the conductor
###############################################################
InstallGlobalFunction(GoodSemigroupByMaximalElements,
function(s1,s2,mx,c)
  local l1,l2, m1,m2,c1,c2,q, v, cc, g1,g2;

  if not(IsNumericalSemigroup(s1)) then
    Error("The first argument must be a numerical semigroup");
  fi;
  if not(IsNumericalSemigroup(s2)) then
    Error("The second argument must be a numerical semigroup");
  fi;

  l1:=List(mx,x->x[1]);
  l2:=List(mx,x->x[2]);
  q:=c;
  # removed because this is true only for curves
  #if ForAny(mx, x-> not(q-x in mx)) then
  #  Error("There is no symmetry in the third argument");
  #fi;
  g1:=Intersection([0..q[1]+1],s1);
  g2:=Intersection([0..q[2]+1],s2);
  cc:=Cartesian(g1,g2);
  return GoodSemigroupBySmallElements(Difference(cc,
    Filtered(cc, x->ForAny(mx,
      y->((y[1]=x[1] and x[2]>y[2]) or (x[1]>y[1] and y[2]=x[2]))))
    ));
end);


###################################################
##
#M BelongsToGoodSemigroup
## decides if a vector is in the semigroup
##################################################
InstallMethod(BelongsToGoodSemigroup,
         "Tests if the vector is in the semigroup",
         [IsHomogeneousList, IsGoodSemigroup], 50,
  function(v, a)
    local S,T,E,c,s,t,sprime, X, saturation, C,edge1,edge2, sm, edge;


    # G is a set of points;
    # computes the saturation wrt sum (cutted by the edges) and inf
    saturation:=function(G)
        local inf, O, OO, new,i,j, o;

        inf:=function(u,v)
            return [Minimum(u[1],v[1]),Minimum(u[2],v[2])];
        end;

        O:=G;
        # sum saturation
        repeat
            new:=[];
            for i in [1..Length(O)] do
                for j in [i.. Length(O)] do
                    o:=inf(C,O[i]+O[j]);

                    if not(o in O) then
                        Add(new, o);
                    fi;
                od;
            od;
            O:=Union(O,new);
        until new=[];

        O:=Union(O,[[0,0]]);
        #inf saturation
        repeat
            new:=[];
            for i in [1..Length(O)] do
                for j in [i.. Length(O)] do
                    o:=inf(O[i],O[j]);
                    if not(o in O) then
                        Add(new, o);
                    fi;
                od;
            od;
            O:=Union(O,new);
        until new=[];

        # #good saturation x coordinate
        # repeat
        #     new:=First(O, x->x[1]<C[1] and ForAny(O,y-> x[1]=y[1] and x[2]<y[2] and not(ForAny(O, z->x[2]=z[2] and z[1]>x[1]))));
        #     if new<>fail then
        #         Add(O,[C[1],new[2]]);
        #     fi;

        # until new=fail;
        # #good saturation y coordinate
        # repeat
        #     new:=First(O, x->x[2]<C[2] and ForAny(O,y-> x[2]=y[2] and x[1]<y[1] and not(ForAny(O, z->x[1]=z[1] and z[2]>x[2]))));
        #     if new<>fail then
        #         Add(O,[new[1],C[2]]);
        #     fi;

        # until new=fail;
        return O;
    end;

    if Length(v)<>2 then
      Error("The first argument must be a list with two integers (a pair)");
    fi;
    if not(ForAll(v, IsInt)) then
      Error("The first argument must be a list with two integers (a pair)");
    fi;
    if IsGoodSemigroupByDuplication(a) then
        S:=NumericalSemigroupGS(a);
        E:=IdealGS(a);
        if v[1]=v[2] then
            return v[1] in S;
        fi;

        if v[1]<v[2] then
            return (v[1] in E) and (v[2] in S);
        fi;

        if v[2]<v[1] then
            return (v[2] in E) and (v[1] in S);
        fi;
    fi;
    if IsGoodSemigroupByAmalgamation(a) then
        S:=NumericalSemigroupGS(a);
        E:=IdealGS(a);
        c:=MorphismGS(a);
        T:=UnderlyingNSIdeal(E);
        s:=v[1];
        t:=v[2];
        if not(s in S) then
            return false;
        fi;
        if not(t in T) then
            return false;
        fi;

        if t=c*s then
            return true;
        fi;

        if (c*s in E) and (t in E) then
            return true;
        fi;

        if t< c*s then
            return t in E;
        fi;
        if t>c*s then
            if not(c*s in E) then
                return false;
            fi;
            if t in E then
                return true;
            fi;
            if not(IsInt(t/c)) then
                return false;
            fi;
            return t/c in S;
        fi;

    fi;

    if IsGoodSemigroupByCartesianProduct(a) then
      return v[1] in NumericalSemigroupListGS(a)[1] and
              v[2] in NumericalSemigroupListGS(a)[2];
    fi;

    if HasSmallElements(a) then
        C:=Conductor(a);

        if v[1]>=C[1] and v[2]>=C[2] then
            return true;
        fi;

        sm:=SmallElements(a);

        if v[1]>C[1] then
            edge:=Filtered(sm,x->x[1]=C[1]);
            return ForAny(edge, x->x[2]=v[2]);
        fi;

        if v[2]>C[2] then
            edge:=Filtered(sm,x->x[2]=C[2]);
            return ForAny(edge, x->x[1]=v[1]);
        fi;

        return v in sm;

    fi;


    if HasGenerators(a) then
        X:=Generators(a);
        C:=Conductor(a);

        if v[1]>=C[1] and v[2]>=C[2] then
            return true;
        fi;

        if not(HasSmallElements(a)) then
            SetSmallElements(a,saturation(X));
        fi;
        sm:=SmallElements(a);

        if v[1]>C[1] then
            edge:=Filtered(sm,x->x[1]=C[1]);
            return ForAny(edge, x->x[2]=v[2]);
        fi;

        if v[2]>C[2] then
            edge:=Filtered(sm,x->x[2]=C[2]);
            return ForAny(edge, x->x[1]=v[1]);
        fi;

        return v in sm;
    fi;

    return false;

end);


###################################################
##
#M BelongsToGoodSemigroup
## decides if a vector is in the semigroup
##################################################
InstallMethod( \in,
        "for good semigroups",
        [ IsHomogeneousList, IsGoodSemigroup],
        function( v, a )
    return BelongsToGoodSemigroup(v,a);
end);

###################################################
##
#M Equality of good semigroups
## decides if the two good semigroups are equal
##################################################
InstallMethod( \=,
        "for good semigroups",
        [ IsGoodSemigroup, IsGoodSemigroup],
        function( a, b )
    return Conductor(a)=Conductor(b) and SmallElements(a)=SmallElements(b);
end);


 #############################################################################
 ##
 #M  ViewObj(S)
 ##
 ##  This method for good semigroups.
 ##
 #############################################################################
 InstallMethod( ViewObj,
         "Displays an Affine Semigroup",
         [IsGoodSemigroup],
         function( S )
         Print("<Good semigroup>");

 end);

 #############################################################################
 ##
 #M  ViewString(S)
 ##
 ##  This method for good semigroups.
 ##
 #############################################################################
 InstallMethod( ViewString,
         "String of an Affine Semigroup",
         [IsGoodSemigroup],
         function( S )
         return ("Good semigroup");

 end);


 #############################################################################
 ##
 #M  Display(S)
 ##
 ##  This method for good  semigroups. ## under construction... (= View)
 ##
 #############################################################################
InstallMethod( Display,
         "Displays an Affine Semigroup",
         [IsGoodSemigroup],
         function( S )
         Print("<Good semigroup>");
 end);











#####################################################
##
#F ProjectionOfGoodSemigroup:=function(S,num)
## Given a good semigroup S it returns the num-th numerical semigroup projection
#####################################################

InstallGlobalFunction(ProjectionOfGoodSemigroup,
function(S,num)
  local small,S1,S2;
    if not(IsGoodSemigroup(S)) then
        Error("The first argument must be a good semigroup");
    fi;

    if not(num=1 or num=2)  then
        Error("The second argument must be 1 or 2");
    fi;
    small:=SmallElements(S);

    if num=1 then
    return NumericalSemigroupBySmallElements(Set([1..Length(small)],i->small[i][1]));
    fi;
    if num=2 then
    return NumericalSemigroupBySmallElements(Set([1..Length(small)],i->small[i][2]));
    fi;
end);


#####################################################
##
#F GenusOfGoodSemigroup:=function(S)
## Given a good semigroup S it returns its genus
#####################################################
InstallGlobalFunction(GenusOfGoodSemigroup,
function(S)
    if not(IsGoodSemigroup(S)) then
        Error("The argument must be a good semigroup");
    fi;
    return Length(MaximalElementsOfGoodSemigroup(S))+Genus(ProjectionOfGoodSemigroup(S,1))+Genus(ProjectionOfGoodSemigroup(S,2));
end);


InstallMethod(Genus,"Genus for a good semigroup",[IsGoodSemigroup], GenusOfGoodSemigroup );

#####################################################
##
#F LengthOfGoodSemigroup:=function(S)
## Given a good semigroup S it returns its length
#####################################################
InstallGlobalFunction(LengthOfGoodSemigroup,
function(S)
    local c;
    if not(IsGoodSemigroup(S)) then
        Error("The argument must be a good semigroup");
    fi;
    c:=Conductor(S);
    return c[1]+c[2]-GenusOfGoodSemigroup(S);
end);

InstallMethod(Length,"for a good semigroup",[IsGoodSemigroup], LengthOfGoodSemigroup );
#####################################################
#F AperySetOfGoodSemigroup:=function(S)
## Given a good semigroup S it returns a list with the elements of the Apery Set
#####################################################
InstallGlobalFunction(AperySetOfGoodSemigroup,
function(S)
    local c,small,i,j,AddElementsOverTheConductor;

    AddElementsOverTheConductor:=function(v,w)
        local ags,i,j;

        if Length(v)=1 then
            ags:=[];
            for i in [v[1]..w[1]] do
                ags:=Union(ags,[[i]]);
            od;  
        
        else 
            ags:=[];
            for i in AddElementsOverTheConductor(v{[1..Length(v)-1]},w{[1..Length(v)-1]}) do
                for j in [v[Length(v)]..w[Length(v)]] do
                    ags:=Union(ags,[Concatenation(i,[j])]);
                od;
            od;
        fi;
        
        return ags;
    end;

    if not(IsGoodSemigroup(S)) then
        Error("The argument must be a good semigroup");
    fi;

    c:=Conductor(S);
    small:=SmallElements(S);

    #First we add to the small elements the elements on the infinite lines that can to become elements of the AperySet.

    for i in Filtered(small,j->j[1]=c[1]) do
        for j in [c[1]+1..c[1]+small[2][1]] do
            small:=Union(small,[[j,i[2]]]); 
        od; 
    od;

    for i in Filtered(small,j->j[2]=c[2]) do
        for j in [c[2]+1..c[2]+small[2][2]] do
            small:=Union(small,[[i[1],j]]);
        od; 
    od;

    return  Filtered(Union(small,AddElementsOverTheConductor(c,c+small[2])),i-> not i-small[2] in small);
end);

#####################################################
#F StratifiedAperySetOfGoodSemigroup:=function(S)
## Given a good semigroup S, it returns a list
#  where the elements are the levels of the AperySet
#####################################################
InstallGlobalFunction(StratifiedAperySetOfGoodSemigroup,
function(S)
local Dominance,ce,small,A,ags,temp,temp2;

    Dominance:=function(v,w,cond)
        return v=w or ForAll([1..Length(v)], i->v[i]<w[i] or w[i]=cond[i]);
    end;
    
    if not(IsGoodSemigroup(S)) then
        Error("The argument must be a good semigroup");
    fi;

    small:=SmallElements(S);
    ce:=Conductor(S)+small[2];
    A:=AperySetOfGoodSemigroup(S);
    ags:=[];
    while A<>[] do
        temp:=Filtered(A,i->Length(Filtered(A,j->Dominance(i,j,ce)))=1);
        temp2:=Filtered(temp,i->Filtered(temp,j->j[1]=i[1] and j[2]>i[2])=[] or Filtered(temp,j->j[2]=i[2] and j[1]>i[1])=[]);
        ags:=Union(ags,[temp2]);
        A:=Filtered(A,i->not i in temp2);
    od;
    Info(InfoNumSgps,2,"Number of levels ", Length(ags));
    return ags;
end);


#####################################################
#F AbsoluteIrreduciblesOfGoodSemigroup:=function(S)
## Given a good semigroup S, the function returns the irreducible absolutes of S.
#  These are the elements that generates S as semiring.
#####################################################
InstallGlobalFunction(AbsoluteIrreduciblesOfGoodSemigroup,
function(S)
local ElementsOnTheEdge,TransformToInf,c,small,irrabsf,irrabsi,infi,edge,i;

  if not(IsGoodSemigroup(S)) then
    Error("The argument must be a good semigroup");
  fi;
  
  #This function check if an elements has a coordinate equal to the conductor.
  ElementsOnTheEdge:=function(vs,c)
    return vs[1]=c[1] or vs[2]=c[2];
  end;

  #This function transforms the elements with a coordinate equal to the conductor in infinities.
  TransformToInf:=function(vs,c)
    local a,i;
    a:=ShallowCopy(vs);
      for i in Filtered([1..2],j->vs[j]=c[j]) do
        a[i]:=infinity;
      od;
    return a;
  end;

  c:=Conductor(S);
  small:=Difference(SmallElements(S),[[0,0]]);
  #Computation of finite irreducible absolutes.
  irrabsf:=IrreducibleMaximalElementsOfGoodSemigroup(S);

  #I take the elements of S different from the conductor but with a coordinate equal to this one.
  edge:=Filtered(small,k->k<>c and ElementsOnTheEdge(k,c));

  infi:=[];

  #Here I add to Infi the infinities in the square over the conductor (built considering the multiplicity)
  for i in [0..small[1][1]-1] do
    infi:=Union(infi,[[c[1]+i,infinity]]);
  od;

  for i in [0..small[1][2]-1] do
    infi:=Union(infi,[[infinity,c[2]+i]]);
  od;

  #Here I add the infinities under the conductor
  for i in edge do
    infi:=Union(infi,[TransformToInf(i,c)]);
  od;

  #Computation of infinite irreducible absolutes
  irrabsi:=Filtered(infi,i->Filtered(small,j->i-j in infi)=[]);

  return  Union(irrabsi,irrabsf);
end);


#####################################################
#F TracksOfGoodSemigroup:=function(S)
## Given a good semigroup S, the function returns the tracks of S.
#####################################################
InstallGlobalFunction(TracksOfGoodSemigroup,
function(S)
local CompareGS,MinimumGS,I,RemoveLabels,GluePieceOfTrack,ComputePieceOfTrack,T,temp;
  
  if not(IsGoodSemigroup(S)) then
    Error("The argument must be a good semigroup");
  fi;

  CompareGS:=function(v,w)

    return ForAll([1..Length(v)], i->v[i]<=w[i]);
  end;

  MinimumGS:=function(v,w)
    return List([1..Length(v)],i->Minimum(v[i],w[i]));
  end;

  #It glues a new piece of track to an existing track. T is the list of all piece of track. V is a list of two elements, 
  # V[1] represents not complete tracks and V[2] the complete tracks.
  #The function add a new piece to the incomplete tracks and returns the updated V.
  GluePieceOfTrack:=function(T,V)
    local ags,temp,i,j;
    ags:=[[],[]];
    ags[2]:=ShallowCopy(V[2]);
    for i in V[1] do
      temp:=i[Length(i)];
      if temp="last" then
        ags[2]:=Union(ags[2],[i]);
      else
        for j in Filtered(T,k->k[1]=temp) do
          ags[1]:=Union(ags[1],[Concatenation(i,[j[2]])]);
        od;
      fi;
    od;
    return ags;
  end;

  #This funcion compute all possibles piece of track of a good semigroups having irreducible absolutes I
  ComputePieceOfTrack:=function(I)
    local IsAPOT,MaximalRed,ags,first,last,i;

    #It check if between two irreducible absolutes there is a piece of track. It check if  their minimum overcome
    #the maximum  in both direction or is equal to this one in entrambe le direzioni o coincide
    IsAPOT:=function(a,b)
        local min;
        if CompareGS(a[1],b[1]) or CompareGS(b[1],a[1]) then return false; else if a[1][1]>b[1][1] then return false; else

        min:=MinimumGS(a[1],b[1]); return min[2]>=a[3] and min[1]>=b[2];
        fi; fi;
    end;

    #If (x,y) is an irreducible absolute it returns (x1,y1), where (x,y1) is the greatest irr. abs. under (x,y)
    # and (x1,y) is the greatest irr. abs. on the left of (x,y).
    MaximalRed:=function(v,I)
      local Factorize,ind,temp,temp2,temp3;

      Factorize:=function(n,l)
          local a,b,ags,i,c,j,a1;

          if l=[] then
            return [];

          else
            a1:=Filtered([1..Length(l)],i->l[i]<>infinity);
            a:=List(a1,k->l[k]);
            b:=FactorizationsIntegerWRTList(n,a);
            ags:=[];
              for i in b do
                c:=List([1..Length(l)],o->0);
                j:=1;
                while j<=Length(a1) do
                  c[a1[j]]:=i[j]; j:=j+1;
                od;
                ags:=Union(ags,[c]);
              od;
            return ags;
          fi;

      end;

      if not v in I then
      return [0,0];

      else
        if infinity in v then
        temp3:=[0,0];
        ind:=First([1,2],i->v[i]<>infinity);
        temp3[3-ind]:=0;
        temp:=Filtered(I,i->i[ind]<v[ind]);
        temp2:=List(List(Factorize(v[ind],List(temp,i->i[ind])),j->Sum(List([1..Length(j)],k->j[k]*temp[k]))),k1->k1[3-ind]);

          if temp2=[] then
          temp3[ind]:=0;
          return Reversed(temp3);

          else
          temp3[ind]:=Maximum(temp2);
          return Reversed(temp3);
          fi;
        else
        temp3:=[0,0];

          for ind in [1,2] do
          temp:=Filtered(I,i->i[ind]<v[ind]);
          temp2:=List(List(Factorize(v[ind],List(temp,i->i[ind])),j->Sum(List([1..Length(j)],k->j[k]*temp[k]))),k1->k1[3-ind]);

            if temp2=[] then
            temp3[ind]:=0;
            else
            temp3[ind]:=Maximum(temp2);
            fi;
          od;
        return Reversed(temp3);
        fi;
      fi;
    end;


    #If (x,y) is an irreducible absolute it returns ((x,y),x1,y1), where (x,y1) is the greatest irr. abs. under (x,y)
    # and (x1,y) is the greatest irr. abs. on the left of (x,y).
    I:=List(I,i->Concatenation([i],MaximalRed(i,I)));

    #I add all the Piece Of Track
    ags:=List(Filtered(Cartesian(I,I),i->IsAPOT(i[1],i[2])),k->[k[1][1],k[2][1]]);

    #I add the point of start and the point of end
    first:=Filtered(I,i->i[2]=0);
    last:=Filtered(I,i->i[3]=0);

    for i in first do
    ags:=Union(ags,[["first",i[1]]]);
    od;

    for i in last do
    ags:=Union(ags,[[i[1],"last"]]);
    od;

    return ags;
  end;

  #This function removes the label "first" and "last" in the tracks.
  RemoveLabels:=function(T)
    return List(T,i->Filtered(i,j->j<>"last" and j<>"first"));
  end;

  I:=AbsoluteIrreduciblesOfGoodSemigroup(S);
  T:=ComputePieceOfTrack(I);

  #The idea is to create the list of all tracks, adding one by one the piece of tracks in all possible way, reccalling
  #GluePieceOfTrack until all possible track are completed (V[1]=[])
  temp:=[[["first"]],[]];
  temp:=GluePieceOfTrack(T,temp);

  while temp[1]<>[] do
  temp:=GluePieceOfTrack(T,temp);
  od;

  return RemoveLabels(temp[2]);

end);

###############################################################
##
#P IsLocal(S)
## Determines if S is local
###############################################################

InstallMethod(IsLocal,
"Determines if the good semigroup is local",
[IsGoodSemigroup], function(S)
local small;
small:=Difference(SmallElements(S),[[0,0]]);
return ForAll([1..Length(small)],i->small[i][1]<>0);
end);

###############################################################
##
#A Multiplicity(S)
## Determines the multiplicity of S
###############################################################

InstallMethod(Multiplicity,
"Returns the multiplicity of a local good semigroup",
[IsGoodSemigroup], function(S)
local small;
if not(IsLocal(S)) then
  Error("The good semigroup must be local");
fi;

small:=SmallElements(S);
return small[2];
end);


