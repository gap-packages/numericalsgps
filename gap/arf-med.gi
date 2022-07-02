#############################################################################
##
#W  arf-med.gi              Manuel Delgado <mdelgado@fc.up.pt>
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



#####################################################################
##                        ARF
## See [RGGB04]
#####################################################################
##
#F ArfNumericalSemigroupClosure(arg)
##
## The argument may be a numerical semigroup or a list of relatively prime
## positive integers
## The output is the Arf-closure of arg (the smallest Arf-semigroup
## containing arg)
##
#####################################################################
InstallGlobalFunction(ArfNumericalSemigroupClosure, function(arg)
    local   set,  min,  A,  i,  MIN,  small, ac;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    set := Set(set);
    min := Minimum(set);
    if min <= 0 then
        Error("The elements of the list must be positive integers");
    fi;

    A := [set];
    i := 1;
    while true do
        if (1 in A[i]) then
            MIN := List(A, x -> Minimum(x));
            small := List([0..Length(MIN)], i -> Sum(MIN{[1..i]}));
            ac:=NumericalSemigroupBySmallElements(small);
            Setter(IsArfNumericalSemigroup)(ac,true);
            return ac;
        fi;

        A[i+1] := Union([min],Difference(Set(A[i], x -> x-min),[0]));
        i := i+1;
        min := Minimum(A[i]);
    od;
end);

InstallMethod(ArfClosure,
"Computes the Arf closure of a numerical semigroup",
[IsNumericalSemigroup],
  ArfNumericalSemigroupClosure
);

#####################################################################
##
#P IsArfNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is an Arf-semigroup and false otherwise
##
#####################################################################
InstallMethod(IsArfNumericalSemigroup,
        "Tests if a Numerical Semigroup is an Arf-semigroup",
        [IsNumericalSemigroup],
        function(s)
    return (s = ArfNumericalSemigroupClosure(s));
end);

InstallTrueMethod(IsMEDNumericalSemigroup,IsArfNumericalSemigroup);
InstallTrueMethod(IsAcuteNumericalSemigroup,IsArfNumericalSemigroup);

#####################################################################
##
#A MinimalArfGeneratingSystemOfArfNumericalSemigroup(s)
##
## The argument s is an Arf numerical semigroup
## returns the minimal Arf-generating system of s.
## Implemented with G. Zito
#############################################################################
InstallMethod(MinimalArfGeneratingSystemOfArfNumericalSemigroup,
  "Returns the minimal Arf-generating system of an Arf-semigroup",
  [IsNumericalSemigroup],
  function(s)
    local char,  ms,i,j,  m, r, b;


    if not(IsArfNumericalSemigroup(s)) then
      Error("The argument must be an Arf numerical semigroup");
    fi;

    ms:=Concatenation(MultiplicitySequenceOfNumericalSemigroup(s),[1]);
    r:=List(ms,_->0);
    for i in [1..Length(ms)] do
      b:=First([i+1..Length(ms)], j->ms[i]=Sum(ms{[i+1..j]}));
      if b=fail then
        b:=Length(ms);
      fi;
      for j in [i+1..b] do
        r[j]:=r[j]+1;
      od;
    od;
    i:=Filtered([1..Length(ms)-1], j->r[j]<r[j+1]);
    return List(i, j->Sum(ms{[1..j]}));

end);

#####################################################################
##
#F ArfNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## Returns the set of Arf numerical semigroups with Frobenius number f
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
## This version is due to Giuseppe Zito
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithFrobeniusNumber, function(f)
  local n, T, Cond, i,j,k, inarf, filt, al, s;

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

  if(not(IsInt(f))) then
    Error("The argument must be an integer");
  fi;

  n:=f+1;
  if (n<0) or (n=1) then
    return [];
  fi;

  if n=0 then
    s:=NumericalSemigroup(1);
    Setter(IsArfNumericalSemigroup)(s,true);
    return [s];
  fi;

  Cond:=List([[n]]);
  T:=[];
  for i in [2..n-2] do
    T[i]:=[[i]];
  od;

  for i in [2..n-2] do
    for j in T[i] do
      if inarf(n-i,j) then
          Add(Cond, Concatenation([n-i],j));
      fi;
      filt:= Filtered([j[1]..Int((n-i)/2)], x->inarf(x,j));
      for k in filt do
        Add(T[i+k],Concatenation([k],j));
      od;
    od;

  od;
  al := List(Cond, j-> NumericalSemigroupBySmallElementsNC(Concatenation([0],List([1..Length(j)], i-> Sum(j{[1..i]})))));
  for s in al do
    Setter(IsArfNumericalSemigroup)(s,true);
  od;
  return al;
end);

#####################################################################
##
#F ArfNumericalSemigroupsWithGenus(g)
##
## Returns the set of Arf numerical semigroups with genus g,
## This version is due to Giuseppe Zito
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithGenus, function(g)
  local n, T, Gen, i,j,k, inarf, filt, al, s;

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

  n:=g;

  if(not(IsInt(g))) then
    Error("The argument must be an integer");
  fi;

  if (g<0) then
    return [];
  fi;

  if n=0 then
    s:=NumericalSemigroup(1);
    Setter(IsArfNumericalSemigroup)(s,true);
    return [s];
  fi;

  Gen:=List([[n+1]]);
  T:=[];
  for i in [1..n-1] do
    T[i]:=[[i+1]];
  od;

  for i in [1..n-1] do
    for j in T[i] do
      if inarf(n-i+1,j) then
          Add(Gen, Concatenation([n-i+1],j));
      fi;
      filt:= Filtered([j[1]..Int((n-i+2)/2)], x->inarf(x,j));
      for k in filt do
        Add(T[i+k-1],Concatenation([k],j));
      od;
    od;

  od;
  al := List(Gen, j-> NumericalSemigroupBySmallElementsNC(Concatenation([0],List([1..Length(j)], i-> Sum(j{[1..i]})))));
  for s in al do
    Setter(IsArfNumericalSemigroup)(s,true);
  od;
  return al;
end);

#####################################################################
##
#F ArfNumericalSemigroupsWithGenusUpTo(g)
##
## Returns the set of Arf numerical semigroups with genus less than
## or equal to g, as explained in
## -Rosales et al., Arf numerical semigroups with given genus and
##  Frobenius number
## New version by Giuseppe Zito (U Catania)
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithGenusUpTo,function(g)
  local n, T, i,j,k, inarf, filt, al, s;

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

  n:=g;

  if(not(IsInt(g))) then
    Error("The argument must be an integer");
  fi;

  if (g<0) then
    return [];
  fi;

  if n=0 then
    s:=NumericalSemigroup(1);
    Setter(IsArfNumericalSemigroup)(s,true);
    return [s];
  fi;

  T:=[];
  for i in [1..n] do
    T[i]:=[[i+1]];
  od;
  T[n+1]:=[[1]];

  for i in [1..n-1] do
    for j in T[i] do
      filt:= Filtered([j[1]..n-i+1], x->inarf(x,j));
      for k in filt do
        Add(T[i+k-1],Concatenation([k],j));
      od;
    od;

  od;
  al :=List(Union(T),j-> NumericalSemigroupBySmallElementsNC(Concatenation([0],List([1..Length(j)], i-> Sum(j{[1..i]})))));
  for s in al do
    Setter(IsArfNumericalSemigroup)(s,true);
  od;

  return al;
end);


#####################################################################
##
#F ArfNumericalSemigroupsWithFrobeniusNumberUpTo(f)
##
## Returns the set of Arf numerical semigroups with Frobenius number
## less than or equal to f, as explained in
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
## New version by Giuseppe Zito (U Catania)
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithFrobeniusNumberUpTo,function(f)
  local n, T, i,j,k, inarf, filt, al, s;


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

  if(not(IsInt(f))) then
    Error("The argument must be an integer");
  fi;

  n:=f+1;
  if (n<0) or (n=1) then
    return [];
  fi;

  if n=0 then
    s:=NumericalSemigroup(1);
    Setter(IsArfNumericalSemigroup)(s,true);
    return [s];
  fi;

  T:=[];
  for i in [1..n] do
    T[i]:=[[i]];
  od;

  for i in [2..n-2] do
    for j in T[i] do
      filt:= Filtered([j[1]..n-i], x->inarf(x,j));
      for k in filt do
        Add(T[i+k],Concatenation([k],j));
      od;
    od;

  od;
  al:=List(Union(T),j-> NumericalSemigroupBySmallElementsNC(Concatenation([0],List([1..Length(j)], i-> Sum(j{[1..i]})))));
  for s in al do
    Setter(IsArfNumericalSemigroup)(s,true);
  od;
  return al;
end);

#####################################################################
##
#F ArfNumericalSemigroupsWithGenusAndFrobeniusNumber(g,f)
##
## Returns the set of Arf numerical semigroups with genus g and
## Frobenius number f, as explained in
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithGenusAndFrobeniusNumber,function(g,f)
	local par2sem, testArfSeq, arfsequences, n, al, s;

	#transforms a partition list of an element to the set of sums
	# which will correspond with the set of small elements of the semigroup
	par2sem:=function(l)
            local rl, n,sm, i;

            n:=Length(l);
            rl:=Reversed(l);
            sm:=[0];
            for i in [1..n] do;
                Add(sm,Sum(rl{[1..i]}));
            od;

            return sm;

	end;

	# computes all Arf sequences with sumset equal f+1
	arfsequences:=function(n)
            local l, k, lk, lkm1,bound,cand,s;

            l:=[List([2..Int(f+1)], x-> [x])];
            for k in [2..n] do
                lk:=[];
                lkm1:=l[Length(l)];
                for s in lkm1 do
                    bound:=Int((f+1-Sum(s))/(n-(k-1)));
                    cand:=Intersection(Union(Difference(par2sem(s),[0]), [Sum(s)..bound]),[s[Length(s)]..bound]);
                    lk:=Concatenation(lk,List(cand,x->Concatenation(s,[x])));
                od;

                if lk<>[] then
                    l:=Concatenation(l,[lk]);
                fi;
            od;
            l:=Filtered(l[n], x->Sum(x)=f+1);

            return l;
	end;

	if not(IsInt(g)) or not(IsInt(f)) then
		Error("The arguments must be an integers");
	fi;

	if g<0 then
		return [];
	fi;

	if g=0 and f=-1 then
    s:=NumericalSemigroup(1);
    Setter(IsArfNumericalSemigroup)(s,true);
    return [s];
	fi;

	if not(g<=f and f<=2*g-1) then
		return [];
	fi;

	n:=f+1-g;

	al:=List(List(arfsequences(n), par2sem),NumericalSemigroupBySmallElementsNC);
  for s in al do
    Setter(IsArfNumericalSemigroup)(s,true);
  od;
  return al;

end);


#####################################################################
##
## ArfSpecialGaps(s)
##
## returns the set of gaps g of s such that s cup {g} is again Arf
##
#####################################################################
InstallMethod(ArfSpecialGaps,
  "Returns Arf special gaps",
  [IsNumericalSemigroup],
function(s)
    local ag, sg, g, se, rse, e1, e2;

    if not(IsArf(s)) then
        Error("The argument must be an Arf numerical semigroup");
    fi;

    ag:=[];

    sg:=SpecialGaps(s);
    se:=SmallElements(s);
    rse:=Reversed(se);
    for g in sg do
        e1:=First(rse, e->g-e>0);
        e2:=First(se, e->g-e<0);
        if (2*g-e1 in s) and (2*e2-g in s) then
            Add(ag,g);
        fi;
    od;    
    return ag;
end);

#####################################################################
##
## ArfOverSemigroups(s)
##
## returns the set of Arf oversemigroups of s
##
#####################################################################
InstallMethod(ArfOverSemigroups,
  "Returns the set of Arf oversemigroups of the given numerical semigroup",
  [IsNumericalSemigroup],
  function(s)
    local   t,  sg,  A,  O, g, a;

    if(not(IsArf(s))) then
        Error("The argument must be an Arf numerical semigroup");
    fi;

    if(s=NumericalSemigroup(1)) then
        return [s];
    fi;

    t:=s;
    sg:=ArfSpecialGaps(t); #which must be different from [-1]
    A:=[NumericalSemigroup(1),t];
    O:=[];
    for g in sg do #List(sg,g->AddSpecialGapOfNumericalSemigroup(g,t));
        a:=AddSpecialGapOfNumericalSemigroup(g,t);
        Setter(IsArf)(a,true);
        AddSet(O,a);
    od;
    
    while(not(O=[])) do
        t:=O[1];
        O:=O{[2..Length(O)]};
        if(not(t in A)) then
            A:=Union(A,[t]);
            sg:=ArfSpecialGaps(t);
            for g in sg do #O:=Union(O,List(sg,g->AddSpecialGapOfNumericalSemigroup(g,t)));
                a:=AddSpecialGapOfNumericalSemigroup(g,t);
                Setter(IsArf)(a,true);
                AddSet(O,a);
            od;
        fi;
    od;
    return A;
end);

#####################################################################
##
## IsArfIrreducible(s)
##
## detects it s can be written as the intersection of two or more 
## Arf semigroups containing it
##
#####################################################################
InstallMethod(IsArfIrreducible,
  "Tests wether the semigroup is Arf irreducible",
  [IsNumericalSemigroup],
  function(s)
     if not(IsArf(s)) then
        Error("The argument must be an Arf numerical semigroup");
    fi;
    return Length(ArfSpecialGaps(s))<=1;
end);

InstallTrueMethod(IsArfIrreducible, IsArf and IsArfIrreducible);

#####################################################################
##
## DecomposeIntoArfIrreducibles(s)
##
## retuns a set of Arf irreducible numerical semigroups whose
## intersection is s; this representation is minimal in the sense
## that no semigroup can be removed
##
#####################################################################
InstallMethod(DecomposeIntoArfIrreducibles,
  "retuns the Arf irreducible numerical semigroups whose intersection is the arguement",
  [IsNumericalSemigroup],
  function(s)

    local   sg,  caux,  I,  C,  B,  pair,  i,  I2,  j,  l, dec, si;

    #sg contains the special gaps of s
    #B auxiliar ser used to construct C and I
    #I will include Arf irreducibles containing s
    #C non irreducibles
    #caux(t) is to compute those elements in gs that are not in t
    #II auxiliar set of Arf irreducibles

    if(not(IsNumericalSemigroup(s))) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    if(not(IsArf(s))) then
        Error("The argument must be an Arf numerical semigroup.\n");
    fi;


    if(s=NumericalSemigroup(1)) then
        Setter(IsIrreducibleNumericalSemigroup)(s,true);
        return [s];
    fi;

    sg:=ArfSpecialGaps(s);
    if (Length(sg)=1) then
        #Setter(IsIrreducibleNumericalSemigroup)(s,true);
        return [s];
    fi;

    caux:=function(t)
        return Filtered(sg,g->not(BelongsToNumericalSemigroup(g,t)));
    end;

    I:=[];
    C:=[s];
    B:=[];
    while(not(C=[])) do
        B:=Union(List(C,sp->List(ArfSpecialGaps(sp),g->AddSpecialGapOfNumericalSemigroup(g,sp))));
        B:=Filtered(B,b->not(caux(b)=[]));
        B:=Filtered(B,b->Filtered(I,i->IsSubsemigroupOfNumericalSemigroup(i,b))=[]);
        C:=Filtered(B,b->not(Length(ArfSpecialGaps(b))=1));
        I:=Union(I,Difference(B,C));
    od;
    I:=List(I,i->[i,caux(i)]);
    while(true) do
        pair := fail;
        for i in I do
            I2 := [];
            for j in I do
                if not j = i then
                    Add(I2, j);
                fi;
            od;

            l := [];
            for j in I2 do
                Add(l, j[2]);
            od;
            if Union(l) = sg then
                pair := i;
                break;
            fi;
        od;

        if(pair=fail) then
            dec:=List(I, x -> x[1]);
            for si in dec do
               Setter(IsArfIrreducible)(si,true);
            od;
            return dec;
        fi;

        I2 := [];
        for j in I do
            if not j = pair then
                Add(I2, j);
            fi;
        od;

        I := I2;
        Unbind(I2);
    od;
    dec:=List(I, x -> x[1]);
    for si in dec do
      Setter(IsArfIrreducible)(si,true);
    od;
    return dec;

end);

#####################################################################
##                        MED
## See [RGGB03]
#####################################################################
##
#A IsMEDNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is a MED-semigroup and false otherwise
##
#####################################################################
InstallMethod(IsMEDNumericalSemigroup,
        "Tests if a numerical semigroup is a MED-semigroup",
        [IsNumericalSemigroup],
        function(s)
    local e, m;
    m := MultiplicityOfNumericalSemigroup(s);
    e := Length(MinimalGeneratingSystemOfNumericalSemigroup(s));
    return e = m;
end);


#####################################################################
##
#F MEDNumericalSemigroupClosure(arg)
##
## The argument may be a numerical semigroup or a list of relatively prime
## positive integers
## The output is the MED-closure of arg (the smallest MED-semigroup
## containing arg)
##
#####################################################################
InstallGlobalFunction(MEDNumericalSemigroupClosure, function(arg)
    local   set,  min,  A,  small, s;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    set := Set(set);
    min := Minimum(set);
    if min <= 0 then
        Error("The elements of the list must be positive integers");
    fi;

    A := set - min;
    A[1] := min;
    small := Union([0], min +
                   SmallElementsOfNumericalSemigroup(NumericalSemigroup(A)));
    s:= NumericalSemigroupBySmallElements(small);
    Setter(IsMEDNumericalSemigroup)(s,true);
    return s;
end);

InstallMethod(MEDClosure,
"Computes the MED closure of a numerical semigroup",
[IsNumericalSemigroup],
  MEDNumericalSemigroupClosure
);

#####################################################################
##
#A MinimalMEDGeneratingSystemOfMEDNumericalSemigroup(s)
##
## The argument s is a MED numerical semigroup
## returns the minimal MED-generating system of s.
##
#############################################################################
InstallMethod(MinimalMEDGeneratingSystemOfMEDNumericalSemigroup,
        "Returns the minimal MED-generating system of a MED numerical semigroup",
        [IsNumericalSemigroup],
        function(s)
    local   gen,  len,  ngen;

    if not IsMEDNumericalSemigroup(s) then
        Error("s must be a MED numerical semigroup");
    fi;
    gen := MinimalGeneratingSystemOfNumericalSemigroup(s);

    len := Length(gen);
    if len = 2 then
        return gen;
    fi;

    while len >= 2 do
        ngen  := Difference(gen, [gen[len]]);
        if Gcd(ngen) = 1 and s = MEDNumericalSemigroupClosure(ngen) then
            gen := ngen;
            len := len - 1;
        else
            len := len -1;
        fi;
    od;
    return gen;
end);

#####################################################################
##                        Saturated
## See [book]
#####################################################################
##
#F SaturatedfNumericalSemigroupClosure(arg)
##
## The argument may be a numerical semigroup or a list of relatively prime
## positive integers
## The output is the saturated-closure of arg (the smallest saturated-semigroup
## containing arg)
##
#####################################################################
InstallGlobalFunction(SaturatedNumericalSemigroupClosure, function(arg)
    local   set,  gen,  min,  ne,  edim,  dis,  small,  i,  kjs, s;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    gen := Set(set);
    min := gen[1];
    ne := Maximum(gen);
    if not IsPosInt(min) then
        Error("The elements of the list must be positive integers");
    fi;
    edim := Length(gen); #embedding dimension
    dis := List([1..edim], i -> Gcd(gen{[1..i]}));

    small := Set([0]);
    for i in [1..edim-1] do
        kjs := 0;
        if (dis[i] = 1) then
            return NumericalSemigroupBySmallElements(Union(small,[gen[i]]));
        fi;
        while(gen[i] + kjs*dis[i] < gen[i+1]) do
            AddSet(small,gen[i]+kjs*dis[i]);
            kjs := kjs+1;
        od;
    od;
    s:=NumericalSemigroupBySmallElements(Union(small,[ne]));
    Setter(IsSaturatedNumericalSemigroup)(s,true);
    return s;
end);

InstallMethod(SaturatedClosure,
"Computes the Saturated closure of a numerical semigroup",
[IsNumericalSemigroup],
  SaturatedNumericalSemigroupClosure
);


#####################################################################
##
#P IsSaturatedNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is a saturated-semigroup and false otherwise
##
#####################################################################
InstallMethod(IsSaturated,
        "Tests if a Numerical Semigroup is a saturated semigroup",
        [IsNumericalSemigroup],
        function(s)
    return (s = SaturatedNumericalSemigroupClosure(s));
end);

# InstallMethod(IsSaturated,
#   "Tests if a Numerical Semigroup is a saturated semigroup",
#   [IsNumericalSemigroup],IsSaturatedNumericalSemigroup);
#solution by Sebastian Gutsche to

InstallTrueMethod(IsArfNumericalSemigroup, IsSaturatedNumericalSemigroup);

#####################################################################
##
#F SaturatedNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## returns the the set of saturated numerical semigroups with Frobenius number f
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
#############################################################################
InstallGlobalFunction(SaturatedNumericalSemigroupsWithFrobeniusNumber,function(f)
	local alg23, alg24, satpart, listsatseq, L, C, Ll, l, t, satsystem, i, ls, s;

	if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

	if f=0 or f<-1 then
		return [];
	fi;
	if f=-1 then
    s:=NumericalSemigroup(1);
    Setter(IsSaturatedNumericalSemigroup)(s,true);
    return [s];
	fi;

	#returns the set nonnegative integer of solutions x
	# with l.x=c; l is a (saturated) sequence
	# l[i+1]|l[i], l[i+1]<>l[i], l[Length(l)]=1
	satpart:=function(l,c)
		local sols, len, i, j, sol, next, cand;

		len:=Length(l);
	 	sol:=[];
		for i in [1..len-1] do Add(sol, 0); od;
		sol[len]:=c;
		sols:=[sol];

		while true do
			j:=First(Reversed([1..len-1]), i-> sol{[i+1..len]}*l{[i+1..len]}>=l[i]);
			if j=fail then
				return sols;
			fi;
			next:=[];
			for i in [1..j-1] do next[i]:=sol[i]; od;
			next[j]:=sol[j]+1;
			for i in [j+1..len-1] do next[i]:=0; od;
			next[len]:=sol{[j+1..len]}*l{[j+1..len]}-l[j];
			sol:=next;#sol:=ShallowCopy(next);
			Add(sols, sol);
		od;

	end;

	#returns saturated semigroups associated to a saturaded sequence
	# with Frobenius number f
	listsatseq:=function(d, f)
		local c, l, ok, sum, len, ones, i;

		l:=[];
		ok:=false;
		sum:=Sum(d);
		len:=Length(d);

		if (f+1>=sum) and (Gcd(f+1,d[len-1])=1) and (((f+1) mod d[len-1])<>1) then
			ok:=true;
			c:=f+1-sum;
		fi;

		if (f+2>=sum) and (((f+2) mod d[len-1])=1) then
			ok:=true;
			c:=f+2-sum;
		fi;

		if not(ok) then
			return [];
		fi;
		ones:=[];
		for i in [1..len] do ones[i]:=1; od;

		l:=satpart(d,c);
		l:=l+ones;
		l:=Filtered(l, t-> ForAll([1..len-1], i-> Gcd(d[i]/d[i+1],t[i+1])=1));
		return l;

	end;

	#implements Algorithm 23
	alg23:=function(f)
		local tot, A, AA, cand, d, x, dd;

		cand:=Filtered([2..f], x-> Gcd(f+1,x)=1 and f mod x<> 0);
		A:=List(cand, x-> [x,1]);
		tot:=A;

		while (A<>[]) do
			AA:=[];
			for d in A do
				for x in [2..Int((f+1-Sum(d))/d[1])] do
					dd:=ShallowCopy(d);
					dd[1]:=x*d[1];
					dd{[2..Length(d)+1]}:=d;
					Add(AA, dd);
				od;
			od;
			A:=AA;
			tot:=Union(tot,A);
		od;

		return tot;

	end;

	#implements Algorithm 24
	alg24:=function(f)
		local tot, A, AA, cand, d, x, dd;

		cand:=Difference(DivisorsInt(f+1),[1]);
		A:=List(cand, x-> [x,1]);
		tot:=A;

		while (A<>[]) do
			AA:=[];
			for d in A do
				for x in [2..Int((f+2-Sum(d))/d[1])] do
					dd:=ShallowCopy(d);
					dd[1]:=x*d[1];
					dd{[2..Length(d)+1]}:=d;
					Add(AA, dd);
				od;
			od;
			A:=AA;
			tot:=Union(tot,A);
		od;

		return tot;

	end;
	#main
	L:=Union(alg23(f),alg24(f));
	C:=[];
	for l in L do
		Ll:=listsatseq(l,f);
		for t in Ll do
			satsystem:=[];
			satsystem[1]:=l[1];
			for i in [2..Length(l)] do
				satsystem[i]:=l{[1..i]}*t{[1..i]};
			od;
			Add(C,satsystem);
		od;
	od;
  ls:=Set(C, SaturatedNumericalSemigroupClosure);
  for s in ls do
    Setter(IsSaturatedNumericalSemigroup)(s,true);
  od;
  return ls;
end);
