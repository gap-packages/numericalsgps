#############################################################################
##
#W  catenary-tame.gi        Manuel Delgado <mdelgado@fc.up.pt>
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
#F  NSGPfactorizationsNC(n,l)
##
##  <n> is a nonnegative integer and <l> is a list of positive integers.
##  Returns a list with the different factorizations of n as a linear
##  combination with elements in l.
##
#############################################################################
InstallGlobalFunction( NSGPfactorizationsNC, function(n,l)
    local k,e1;

    k:=Length(l);

    if(n<0) then
        return [];
    fi;

    if(k=1) then
        if ((n mod l[1])=0) then
            return [[n /(l[1])]];
        else
            return [];
        fi;
    fi;

    e1:=List([1..k],n->0);
    if(n=0) then
        return [e1];
    fi;
    e1[1]:=1;

    return Union(List(NSGPfactorizationsNC(n-l[1],l),n-> n+e1),
            List(NSGPfactorizationsNC(n,l{[2..k]}),n->Concatenation([0],n)));

end);


#############################################################################
##
#F  CatenaryDegreeOfNumericalSemigroup(s)
##
##  Computes the catenary degree of the numerical semigroup <s>.
##
##  The definition of catenary degree can be found in
##  the book:
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. Garcia-Sanchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
InstallGlobalFunction( CatenaryDegreeOfNumericalSemigroup, function(s)
    local    msg,  ap,  candidates,  rclasses;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
    	return 0;  #Error("The catenary degree does not make sense for ",s,"\n");
    fi;

    ap:=AperyListOfNumericalSemigroupWRTElement(s,msg[1]);
    ap:=ap{[2..Length(ap)]};    # I remove the zero,
                                #   minimal generators yield conneted graphs
    candidates:=Union(List(msg,n->List(ap,m->m+n)));
                                # Gn not conneted implies n=wi+minimalgenerator
                                #    thus these are the candidates
    rclasses:=List(candidates,n->RClassesOfSetOfFactorizations(FactorizationsIntegerWRTList(n,msg)));
                                # from every n y obtain the connected components
                                #   they will give me the expressions of n
                                #   that yield minimal generators
    rclasses:=Filtered(rclasses, n->Length(n)>1);

    return Maximum(List(rclasses,l->Maximum(List(l,r->Minimum(List(r,Sum))))));
end);

InstallMethod(CatenaryDegree, 
    "Computes the catenary degree of a numerical semigroup",
    [IsNumericalSemigroup],    
    CatenaryDegreeOfNumericalSemigroup);


##
#F This function returns the catenary cegree in a numerical semigroup S of
## a positive integer n
##
#------------------------------------------------------------------------
##
InstallGlobalFunction(CatenaryDegreeOfElementInNumericalSemigroup, function(n,S)

    #---- Tests on the arguments ------------------------------
    if not (IsNumericalSemigroup(S) and IsPosInt(n+1)) then
        Error(" The arguments of CatenaryDegreeOfElementInNumericalSemigroup are a nonnegativeinteger and a numerical semigroup.\n");
    fi;

    #---- End of Tests on the arguments -----------------------


    #==========================================================
    #-----------      MAIN CODE       -------------------------
    #----------------------------------------------------------
    if (not n in S) or (n=0) or (n in MinimalGeneratingSystemOfNumericalSemigroup(S)) then
        return 0;
    fi;
    return CatenaryDegreeOfSetOfFactorizations(FactorizationsElementWRTNumericalSemigroup(n,S));
end);
## ----  End of CatenaryDegreeOfElementInNumericalSemigroup()  ----
##
#========================================================================

InstallMethod(CatenaryDegree, 
    "Computes the catenary degree of an element in a numerical semigroup",
    [IsInt, IsNumericalSemigroup],    
    CatenaryDegreeOfElementInNumericalSemigroup);

InstallMethod(CatenaryDegree, 
    "Computes the catenary degree of an element in a numerical semigroup",
    [IsNumericalSemigroup,IsInt],
    function(s,n)    
        return CatenaryDegreeOfElementInNumericalSemigroup(n,s);
end);


#############################################################################
##
#F  TameDegreeOfElementInNumericalSemigroup(n,s)
##
##  Computes the tame degre of the element <n> of the numerical semigroup <s>.
##  Used for the computation of the tame degree of s, but can
##  be used separately.
##
##  The definition of tame degree appears in
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. Garc�a-S�nchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
InstallGlobalFunction( TameDegreeOfElementInNumericalSemigroup, function(n,s)
    local msg, i, max, fact, mtemp, candidates, rest, distance;

    # distance between two factorizations
    distance:=function(x,y)
        local p,n,i,z;

        p:=0; n:=0;
        z:=x-y;
        for i in [1..Length(z)] do
            if z[i]>0 then
                p:=p+z[i];
            else
                n:=n+z[i];
            fi;
        od;

        return Maximum(p,-n);
    end;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
     Error("The tame degree does not make sense for ",s,"\n");
    fi;

    max:=0;
    fact:=FactorizationsIntegerWRTList(n,msg);

    for i in [1..Length(msg)] do
        candidates:=Filtered(fact, x->x[i]=0);
        rest:=Filtered(fact,x->x[i]<>0);
        if (rest=[] or candidates=[]) then
            mtemp:=0;
        else
            mtemp:=Maximum(List(candidates,x->Minimum(List(rest, z->distance(x,z)))));
        fi;
        if mtemp>max then
            max:=mtemp;
        fi;
    od;
    return max;
end);


InstallMethod(TameDegree,
    "Tame degree for element in numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    TameDegreeOfElementInNumericalSemigroup);


InstallMethod(TameDegree,
    "Tame degree for element in numerical semigroup",
    [IsNumericalSemigroup,IsInt],
    function(s,n)
        return TameDegreeOfElementInNumericalSemigroup(n,s);
    end);


#############################################################################
##
#F  TameDegreeOfNumericalSemigroup(s)
##
##  Computes the tame degree of a numerical semigroup <s>.
##
##  The definition of tame degree appears in
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. Garc�a-S�nchez,
##    D. Llena,  The catenary and tame degree of numerical
##    monoids, Forum Math. 2007 1--13.
##
##  Improved by Alfredo S�nchez-R. Navarro and P. A. Garc�a-S�nchez
##  for Alfredo S�nchez-R. Navarro's PhD Thesis
##
#############################################################################
InstallGlobalFunction( TameDegreeOfNumericalSemigroup, function(s)
   local msg, ap, candidates, rp, facts, translate;


    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    translate:=function(l) #translates partitions to factorizations
		return List(msg, x-> Length(Positions(l,x)));
    end;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    #Print(msg);
    if(msg[1]=1) then
     return 0;
    fi;

    ap:=Difference(Union(Set(msg,n->AperyListOfNumericalSemigroupWRTElement(s,n))),[0]);

    candidates:=Set(Cartesian(ap,msg),Sum);

	# remove elements having in all its factorizations a common atom
    rp:=List(candidates, x->RestrictedPartitions(x, msg));
    rp:=Filtered(rp, x->Intersection(x)=[]);
    facts:=List(rp, x->List(x, translate));
    if facts=[] then
      return 0;
    fi;
    return Maximum(Set(facts,n->TameDegreeOfSetOfFactorizations(n)));
end);

InstallMethod(TameDegree,
    "Tame degree for a numerical semigroup",
    [IsNumericalSemigroup],
    TameDegreeOfNumericalSemigroup);

#############################################################################
##
#F  FactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the set of factorizations
##  of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
InstallGlobalFunction(FactorizationsElementWRTNumericalSemigroup, function(n,s)
    local gen;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.\n");
    fi; #this ensures that the lenghts won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return FactorizationsIntegerWRTList(n,gen);
end);

InstallMethod(Factorizations,
    "for an element in a numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    FactorizationsElementWRTNumericalSemigroup);

InstallMethod(Factorizations,
    "for an element in a numerical semigroup",
    [IsNumericalSemigroup,IsInt],
    function(s,n)
        return FactorizationsElementWRTNumericalSemigroup(n,s);
        end);


#############################################################################
##
#F  LengthsOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the lengths of the set of
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
InstallGlobalFunction(LengthsOfFactorizationsElementWRTNumericalSemigroup, function(n,s)
    local gen;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.\n");
    fi; #this ensures that the lenghts won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return LengthsOfFactorizationsIntegerWRTList(n,gen);
end);



#############################################################################
##
#F  ElasticityOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the quotient (maximum length)/(minimum lenght) of the
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
InstallGlobalFunction(ElasticityOfFactorizationsElementWRTNumericalSemigroup, function(n,s)
    local gen,max,min,lenfact;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if not IsPosInt(n) then
        Error("The first argument must be a positive integer.\n");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.\n");
    fi; #this ensures that the lengths won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    lenfact:=Set(LengthsOfFactorizationsIntegerWRTList(n,gen));
    min:=Minimum(lenfact);
    max:=Maximum(lenfact);

    return max/min;
end);

InstallMethod(Elasticity,
    "Elasticity of the factorizations of an element in a numerical semigroup", 
    [IsPosInt,IsNumericalSemigroup],
    ElasticityOfFactorizationsElementWRTNumericalSemigroup);

InstallMethod(Elasticity,
    "Elasticity of the factorizations in a numerical semigroup of one of its elements", 
    [IsNumericalSemigroup, IsPosInt],
    function(a,v)
        return  ElasticityOfFactorizationsElementWRTNumericalSemigroup(v,a);
    end);


#############################################################################
##
#F  ElasticityOfNumericalSemigroup(s)
##
##  Computes the supremum of the elasticities of the
##  factorizations of the elements of <s>.
##  From [CHM06, GHKb] this is precisely np/n1
##  with n1 the multiplicity of <s> and np the greatest
##  generator.
##
#############################################################################
InstallGlobalFunction(ElasticityOfNumericalSemigroup, function(s)
    local gen,max,min;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;


    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    min:=Minimum(gen);
    max:=Maximum(gen);

    return max/min;
end);

InstallMethod(Elasticity,
    "Computes the elasticity of a numerical semigroup",
    [IsNumericalSemigroup],
    ElasticityOfNumericalSemigroup);


#############################################################################
##
#F  DeltaSetOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the set of differences between
##  two consecutive lengths of factorizations of
##  an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
InstallGlobalFunction(DeltaSetOfFactorizationsElementWRTNumericalSemigroup, function(n,s)
    local gen,max,min,lenfact;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if n=0 then
        return [];
    fi;


    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.\n");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.\n");
    fi; #this ensures that the lenghts won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    lenfact:=LengthsOfFactorizationsIntegerWRTList(n,gen);
    return Set([1..(Length(lenfact)-1)], i->lenfact[i+1]-lenfact[i]);

end);

InstallMethod(DeltaSet,
    "for the factorizations of an element in a numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    DeltaSetOfFactorizationsElementWRTNumericalSemigroup);

InstallMethod(DeltaSet,
    "for the factorizations of an element in a numerical semigroup",
    [IsNumericalSemigroup,IsInt],
    function(s,n)
        return DeltaSetOfFactorizationsElementWRTNumericalSemigroup(n,s);
    end);


#############################################################################
##
#F  MaximumDegreeOfElementWRTNumericalSemigroup(n,s)
##
##  Computes the maximum length of the
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
InstallGlobalFunction(MaximumDegreeOfElementWRTNumericalSemigroup, function(n,s)
    local gen;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if n=0 then
        return 0;
    fi;

    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.\n");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.\n");
    fi; #this ensures that the lenghts won't be empty

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return Maximum(LengthsOfFactorizationsIntegerWRTList(n,gen));
end);

InstallMethod(MaximumDegree,
    "for an element in a numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    MaximumDegreeOfElementWRTNumericalSemigroup);

InstallMethod(MaximumDegree,
    "for a numerical semigroup and one of its elements",
    [IsNumericalSemigroup,IsInt],
    function(s,n) 
        return MaximumDegreeOfElementWRTNumericalSemigroup(n,s);
    end);


#############################################################################
##
#F  OmegaPrimalityOfElementInNumericalSemigroup(n,s)
##
##  Computes the omega primality of an elmenent n in S, as explained in
##  V. Blanco, P. A. Garc\'{\i}a-S\'anchez, A. Geroldinger,
##  Semigroup-theoretical characterizations of arithmetical invariants with
##  applications to numerical monoids and Krull monoids, {arXiv}:1006.4222v1.
##  Current implementation optimized by C. O'Neill based on a work in progress
##  by O'Neill, Pelayo and Thomas and uses
##  OmegaPrimalityOfElemtListInNumericalSemgiroup
#############################################################################
InstallGlobalFunction(OmegaPrimalityOfElementInNumericalSemigroup, function(n,s)
    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup");
    fi;
    if not (n in s) then
        Error("The first argument must be an element of the second");
    fi;
  
    return OmegaPrimalityOfElementListInNumericalSemigroup([n],s)[1];

end);

InstallMethod(OmegaPrimality,
    "for an element in a numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    OmegaPrimalityOfElementInNumericalSemigroup);

InstallMethod(OmegaPrimality,
    "for an element in a numerical semigroup",
    [IsNumericalSemigroup,IsInt],
    function(s,n)
        return OmegaPrimalityOfElementInNumericalSemigroup(n,s);
    end);

#############################################################################
##
#F  OmegaPrimalityOfNumericalSemigroup(s)
##
##  Computes the maximum of omega primality of the minimal generators of S.
##
#############################################################################
InstallGlobalFunction(OmegaPrimalityOfNumericalSemigroup, function(s)

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

      return Maximum(Set(MinimalGeneratingSystemOfNumericalSemigroup(s),
	n->OmegaPrimalityOfElementInNumericalSemigroup(n,s)));

end);

InstallMethod(OmegaPrimality,
    "for numerical semigroups",
    [IsNumericalSemigroup],
    OmegaPrimalityOfNumericalSemigroup);


#############################################################################
##
#F  FactorizationsIntegerWRTList(n,ls)
##
##  Computes the set of factorizations
##  of an integer n as linear combinations
##  with nonnegative coefficients of the elements in the list of positive integers ls
##  Makes use of RestrictedPartitions
#############################################################################
InstallGlobalFunction(FactorizationsIntegerWRTList,function(n,ls)
	local translate;

	if not(IsListOfIntegersNS(ls) and ForAll(ls, x->IsPosInt(x))) then
		Error("The list must be a list of positive integers.\n");
	fi;

	if Length(ls)<>Length(Set(ls)) then #repeated elements
		return NSGPfactorizationsNC(n,ls);
	fi;

	translate:=function(l)

		return List(ls, x-> Length(Positions(l,x)));

	end;

	return List(RestrictedPartitions(n,ls), translate);
end);

#############################################################################
##
#F  LengthsOfFactorizationsIntegerWRTList(n,ls)
##
##  Computes the lengths of the set of
##  factorizations of an  integer <n> as linear combinations
##  with nonnegative coefficients of the elements in the list of positive integers <ls>
##
#############################################################################
InstallGlobalFunction(LengthsOfFactorizationsIntegerWRTList,function(n,ls)

	if not(IsListOfIntegersNS(ls) and ForAll(ls, x->IsPosInt(x))) then
		Error("The list must be a list of positive integers.\n");
	fi;

	return Set(RestrictedPartitions(n,ls), Length);
end);


#############################################################################
##
#F  DeltaSetOfSetOfIntegers(ls)
##
##  Computes the set of differences between
##  consecutive elements in the list <ls>
##
#############################################################################
InstallGlobalFunction(DeltaSetOfSetOfIntegers,function(ls)
	local lenfact;

	if not(IsListOfIntegersNS(ls)) then
		Error("The argument must be a nonempty list of integers.\n");
	fi;

	lenfact:=Set(ls);

    return Set([1..(Length(lenfact)-1)], i->lenfact[i+1]-lenfact[i]);

end);
InstallMethod(DeltaSet,
    "for a list of integers",
    [IsHomogeneousList],
    DeltaSetOfSetOfIntegers);

#############################################################################
##
#F  CatenaryDegreeOfSetOfFactorizations(fs)
##
##  Computes the catenary degree of the set of factorizations
##
#############################################################################
InstallGlobalFunction(CatenaryDegreeOfSetOfFactorizations,
function(fs)
  local cart, i , j, nfs, distance, Kruskal;

  # edges will be [u,w] with u,w vertices
  Kruskal := function(V, E)
    local trees, needed, v, e, i,j, nv;

    trees := List(V, v-> [v]);
    needed := [];
    nv:=Length(V);
    for e in E do
      i:=First([1..Length(trees)], k-> e[1] in trees[k]);
      j:=First([1..Length(trees)], k-> e[2] in trees[k]);
      if i<>j then
        trees[i]:=Union(trees[i], trees[j]);
        trees[j]:=[];
        Add(needed,e);
      fi;
      if Length(needed)=nv-1 then
        break;
      fi;
    od;
    return needed;
  end;

  distance:=function(e)
        local p,n,i,z,x,y;
        x:=e[1]; y:=e[2];
        p:=0; n:=0;
        z:=x-y;
        for i in [1..Length(z)] do
            if z[i]>0 then
                p:=p+z[i];
            else
                n:=n+z[i];
            fi;
        od;

        return Maximum(p,-n);
    end;

    if not(IsRectangularTable(fs) and ForAll(fs,IsListOfIntegersNS)) then
      Error("The argument is not a list of factorizations.\n");
    fi;
    if Minimum(Flat(fs))<0 then
      Error("Coefficients must be nonnegative integers.\n");
    fi;

  nfs:=Length(fs);

  if nfs<2 then
    return 0;
  fi;

  cart:=[];
  for i in [1..nfs] do
    for j in [i+1 .. nfs] do
      Add(cart, [fs[i], fs[j]]);
    od;
  od;

  Sort(cart,function(e,ee) return distance(e)<distance(ee); end);
  return Maximum(Set(Kruskal(fs,cart), distance));
end);

InstallMethod(CatenaryDegree, 
    "Computes the catenary degree of a set of factorizations",
    [IsHomogeneousList],
    CatenaryDegreeOfSetOfFactorizations
    );

#############################################################################
##
#F  TameDegreeOfSetOfFactorizations(fact)
##
##  Computes the tame degree of the set of factorizations
##
#############################################################################
InstallGlobalFunction(TameDegreeOfSetOfFactorizations,function(fact)
    local distance, i, max, mtemp, candidates, rest, len;

	if not(IsRectangularTable(fact) and ForAll(fact, IsListOfIntegersNS)) then
		Error("The argument is not a list of factorizations.\n");
	fi;
	if Minimum(Flat(fact))<0 then
		Error("Coefficients must be nonnegative integers.\n");
	fi;

    # distance between two factorizations
    distance:=function(x,y)
        local p,n,i,z;

        p:=0; n:=0;
        z:=x-y;
        for i in [1..Length(z)] do
            if z[i]>0 then
                p:=p+z[i];
            else
                n:=n+z[i];
            fi;
        od;

        return Maximum(p,-n);
    end;

    if Length(fact) <= 1 then
      return 0;
    fi;

    max:=0;
    len := Length(fact[1]);
    for i in [1..len] do
        candidates:=Filtered(fact, x->x[i]=0);
        rest:=Filtered(fact,x->x[i]<>0);
        if (rest=[] or candidates=[]) then
            mtemp:=0;
        else
            mtemp:=Maximum(List(candidates,x->Minimum(List(rest, z->distance(x,z)))));
        fi;
        if mtemp>max then
            max:=mtemp;
        fi;
    od;
    return max;
end);

InstallMethod(TameDegree,
    "Tame degree for a set of factorizations",
    [IsHomogeneousList],
    TameDegreeOfSetOfFactorizations);

#############################################################################
##
#F  RClassesOfSetsOfFactorizations(l)
##
##  Determine the set of R-classes (Chapter 7 [RGBook] of a set of factorizations
##
#############################################################################
InstallGlobalFunction(RClassesOfSetOfFactorizations, function(l)
    local current, pos, len, colisionan, cola;

	if not(IsRectangularTable(l) and ForAll(l,IsListOfIntegersNS)) then
		Error("The argument is not a list of factorizations.\n");
	fi;

	if Minimum(Flat(l))<0 then
		Error("Coefficients must be nonnegative integers.\n");
	fi;

    current:=List(l,n->[n]);
    pos :=1;
    len:=Length(current);
    while (pos<len) do
        colisionan:=Filtered(current{[(pos+1)..len]},k->First(Cartesian(k,current[pos]),n->n[1]*n[2]<>0)<>fail);
        if(colisionan<>[]) then
            current[pos]:=Union(current[pos],Union(colisionan));
            cola:=Difference(current{[(pos+1)..len]},colisionan);
            current:=Concatenation(current{[1..pos]},cola);
            len:=Length(current);
            pos:=0;
        fi;
        pos:=pos+1;
    od;

    return(current);

end);

########################################################
#  MaximalDenumerantOfElementInNumericalSemigroup(x,s)
#  returns the number of factorizations of maximal length of x in
#  the numerical semigroup s
########################################################
InstallGlobalFunction(MaximalDenumerantOfElementInNumericalSemigroup,
function(x,s)
	local max, fact;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if not ( x in s ) then
        Error("The first argument must be an element of the second.\n");
    fi;

	fact:=FactorizationsElementWRTNumericalSemigroup(x,s);
	max:=Maximum(Set(fact,Sum));

	return Length(Filtered(fact, x->Sum(x)=max));
end);
InstallMethod(MaximalDenumerant,
    "for an element in a numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    MaximalDenumerantOfElementInNumericalSemigroup);

InstallMethod(MaximalDenumerant,
    "for a numerical semigroup and one if its elements",
    [IsNumericalSemigroup,IsInt],
    function(s,n)
        return MaximalDenumerantOfElementInNumericalSemigroup(n,s);
    end);

########################################################
#  MaximalDenumerantOfSetOfFactorizations(ls)
#  returns the number of factorizations of maximal length in ls
########################################################
InstallGlobalFunction(MaximalDenumerantOfSetOfFactorizations,
function(ls)
	local max;

	if not(IsRectangularTable(ls) and ForAll(ls,IsListOfIntegersNS)) then
		Error("The argument is not a list of factorizations.\n");
	fi;
	if Minimum(Flat(ls))<0 then
		Error("Coefficients must be nonnegative integers.\n");
	fi;

	max:=Maximum(Set(ls,Sum));

	return Length(Filtered(ls, x->Sum(x)=max));
end);


########################################################
# MaximalDenumerantOfNumericalSemigroup(s)
# computes the maximal denumerant of a numerical semigroup
# by using de algorithm given by Bryant and Hamblin
# Semigroup Forum 86 (2013), 571-582
########################################################
InstallGlobalFunction(MaximalDenumerantOfNumericalSemigroup, function(s)
	local adj, ord, minord, msg, bmsg, p, m, ap, adjSi, i, Si, apb, bi, b, x, md, j, lr, bj;

	if(not(IsNumericalSemigroup(s)))then
		Error("The argument must be anumerical semigroup.\n");
	fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	m:=MultiplicityOfNumericalSemigroup(s);
	ap:=AperyListOfNumericalSemigroupWRTElement(s,m);
	b:=BlowUpOfNumericalSemigroup(s);
	apb:=AperyListOfNumericalSemigroupWRTElement(b,m);

	bmsg:=ShallowCopy(msg-m);
	bmsg[1]:=m;

	ord:=function(x)
		return Maximum(LengthsOfFactorizationsIntegerWRTList(x,msg));
	end;

	adj:=function(x)
		return x-ord(x)*m;
	end;

	md:=0;

	for i in [0..m-1] do
		x:=ap[i+1];
		Si:=[x];
		bi:=apb[i+1]+m*Minimum(LengthsOfFactorizationsIntegerWRTList(apb[i+1],bmsg));
		while x<= bi do
			x:=x+m;
			if x in s then
				Add(Si,x);
			fi;
		od;
		adjSi:=Set(Si,adj);
		#Print(adjSi," ");
		lr:=[];
		lr[1]:=Length(FactorizationsIntegerWRTList(adjSi[1],bmsg));
		for j in [2..Length(adjSi)] do
			bj:=Minimum(LengthsOfFactorizationsIntegerWRTList(adjSi[j-1],bmsg))-(adjSi[j]-adjSi[j-1])/m;
			lr[j]:=Length(Filtered(FactorizationsIntegerWRTList(adjSi[j],bmsg), x->Sum(x)<bj));
			#Print(lr[j]," ");
		od;
		#Print(Maximum(lr),"\n");
		md:=Maximum(md,Maximum(lr));
	od;
	return md;
end);

InstallMethod(MaximalDenumerant,
    "for a numerical semigroup",
    [IsNumericalSemigroup],
    MaximalDenumerantOfNumericalSemigroup);

########################################################
# AdjustmentOfNumericalSemigroup(s)
# computes the adjustment a numerical semigroup
# by using de algorithm given by Bryant and Hamblin
# Semigroup Forum 86 (2013), 571-582
########################################################
InstallGlobalFunction(AdjustmentOfNumericalSemigroup,function(s)
	local adj, ord, minord, msg, bmsg, p, m, ap, adjSi, i, Si, apb, bi, b, x, j, bj, adjust;

	if(not(IsNumericalSemigroup(s)))then
		Error("The argument must be anumerical semigroup.\n");
	fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	m:=MultiplicityOfNumericalSemigroup(s);
	ap:=AperyListOfNumericalSemigroupWRTElement(s,m);
	b:=BlowUpOfNumericalSemigroup(s);
	apb:=AperyListOfNumericalSemigroupWRTElement(b,m);

	bmsg:=ShallowCopy(msg-m);
	bmsg[1]:=m;

	ord:=function(x)
		return Maximum(LengthsOfFactorizationsIntegerWRTList(x,msg));
	end;

	adj:=function(x)
		return x-ord(x)*m;
	end;

	adjust:=[];
	for i in [0..m-1] do
		x:=ap[i+1];
		Si:=[x];
		bi:=apb[i+1]+m*Minimum(LengthsOfFactorizationsIntegerWRTList(apb[i+1],bmsg));
		while x<= bi do
			x:=x+m;
			if x in s then
				Add(Si,x);
			fi;
		od;
		adjSi:=Set(Si,adj);
		#Print(adjSi," ");
		adjust:=Union(adjust,adjSi);
	od;
	return adjust;
end);
InstallMethod(Adjustment,
    "for numerical semigroups",
    [IsNumericalSemigroup],
    AdjustmentOfNumericalSemigroup);

##############################################################
# IsAdditiveNumericalSemigroup(s)
# Detects if s is an additive numerical semigroup, that is,
# ord(m+x)=ord(x)+1 for all x in s. For these semigroups gr_m(K[[s]]) is
# Cohen-Macaulay.
# We use Proposition 4.7 in  Semigroup Forum 86 (2013), 571-582
##############################################################
InstallGlobalFunction(IsAdditiveNumericalSemigroup, function(s)
	local b,m;

	if(not(IsNumericalSemigroup(s)))then
		Error("The argument must be anumerical semigroup.\n");
	fi;

	m:=MultiplicityOfNumericalSemigroup(s);
	b:=BlowUpOfNumericalSemigroup(s);
	return AdjustmentOfNumericalSemigroup(s)
				=AperyListOfNumericalSemigroupWRTElement(b,m);
end);


##############################################################
# IsSuperSymmetricNumericalSemigroup(s)
# Detects if s is a numerical semigroup is supersymmetric, that is,
# it is symmetric, additive and whenever w+w'=f+m
# (with m the multiplicity and f the Frobenius number) we have
# ord(w+w')=ord(w)+ord(w')
##############################################################
InstallGlobalFunction(IsSuperSymmetricNumericalSemigroup,function(s)
	local ap,m, ord, msg, f, om;

	if(not(IsNumericalSemigroup(s)))then
		Error("The argument must be anumerical semigroup.\n");
	fi;

	if not(IsSymmetricNumericalSemigroup(s)) then
		return false;
	fi;

	if not(IsAdditiveNumericalSemigroup(s)) then
		return false;
	fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	m:=MultiplicityOfNumericalSemigroup(s);
	ap:=AperyListOfNumericalSemigroupWRTElement(s,m);
	f:=FrobeniusNumberOfNumericalSemigroup(s);

	ord:=function(x)
		return Maximum(LengthsOfFactorizationsIntegerWRTList(x,msg));
	end;

	ap:=Filtered(ap, x-> x<=(f+m)/2);
	om:=ord(f+m);
	return ForAll(ap, x-> om=ord(x)+ord(f+m-x));
end);

#######################################################################
# BelongsToHomogenizationOfNumericalSemigroup(n,s)
# checks if the pair n belongs to the homogenization of s
#######################################################################
InstallGlobalFunction(BelongsToHomogenizationOfNumericalSemigroup, function(n,s)
	local msg;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

	if not IsListOfIntegersNS(n) then
		Error("The first argument must be a list of integers");
	fi;

	if n[1]<0 or n[2]<0 then
		return false;
	fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	return First(FactorizationsIntegerWRTList(n[2],msg), x-> Sum(x)<= n[1])<>fail;
end);

#######################################################################
# FactorizationsInHomogenizationOfNumericalSemigroup(n,s)
# computes the set of factorizations of  n with respect to generators of
# the homogenization of s
#######################################################################
InstallGlobalFunction(FactorizationsInHomogenizationOfNumericalSemigroup, function(n,s)
	local msg, fact, facthom, x, xhom;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

	if not IsListOfIntegersNS(n) then
		Error("The first argument must be a list of integers");
	fi;

	if n[1]<0 or n[2]<0 then
		return [];
	fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	fact:=Filtered(FactorizationsIntegerWRTList(n[2],msg), x-> Sum(x)<= n[1]);
	facthom:=[];
	for x in fact do
		xhom:=Concatenation([n[1]-Sum(x)],x);
		Add(facthom,xhom);
	od;
	return facthom;
end);

#######################################################################
# HomogeneousBettiElementsOfNumericalSemigroup(s)
#  Computes the Betti elements of the Homogenization of s
#  uses Cox-Little-O'Shea, Chapter 8, Theorem 4  [CLOS] for finding
#  a system of generators of the ideal of S^h
#######################################################################
InstallGlobalFunction(HomogeneousBettiElementsOfNumericalSemigroup,function( s )
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial,  candidates, mp;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    ed:=Length(msg);
    if NumSgpsCanUseSI or NumSgpsCanUseSingular or NumSgpsCanUse4ti2 then
        msg:=List(msg, m->[1,m]);
        msg:=Concatenation([[1,0]],msg);
        return BettiElementsOfAffineSemigroup(
                       AffineSemigroup(msg));
    fi;

    mp:=MinimalPresentationOfNumericalSemigroup(s);
    p := [];
	# list of exponents to monomial
	monomial:=function(l)
		local i;
		pol:=1;
		for i in [1..ed] do
			pol:=pol*Indeterminate(Rationals,i)^l[i];
		od;
		return pol;
	end;

    for rel in mp do
        Add( p, monomial(rel[1])-monomial(rel[2]));
    od;

    rgb := ReducedGroebnerBasis( p, MonomialGrevlexOrdering() );
    ## the homogenization of this is a system of genetators of the ideal of S^h

	 ##computes the s^h degree of a pol in the semigroup ideal
    sdegree:=function(r)
		local mon;
		mon:=LeadingMonomialOfPolynomial(r,MonomialGrlexOrdering() );
		return [Sum(List([1..ed],i->DegreeIndeterminate(mon,i))),Sum(List([1..ed],i-> msg[i]*DegreeIndeterminate(mon,i)))];
    end;

    candidates:=List(rgb, g-> sdegree(g));

    candidates:=Filtered(candidates, x-> Length(RClassesOfSetOfFactorizations(
		FactorizationsInHomogenizationOfNumericalSemigroup(x,s)))>1);
    return Set(candidates);
end);

####################################################################
#F HomogeneousCatenaryDegreeOfNumericalSemigroup(s) computes the
##  homogeneous catenary degree of the numerical semigroup s ([GSOSN])
####################################################################
InstallGlobalFunction(HomogeneousCatenaryDegreeOfNumericalSemigroup,function( s )
	local betti;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;
	betti:=HomogeneousBettiElementsOfNumericalSemigroup(s);

	return Maximum(Set(betti,b-> CatenaryDegreeOfSetOfFactorizations(
		FactorizationsInHomogenizationOfNumericalSemigroup(b,s))));
end);

########################################
#F DenumerantElementInNumericalSemigroup(n,s)
## returns the denumerant
########################################
InstallGlobalFunction(DenumerantOfElementInNumericalSemigroup, function(x,s)
    local gen;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;


    if not ( x in s ) then
		return 0;
    fi;
    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	  return NrRestrictedPartitions(x,gen);
end);

## as function
InstallMethod(DenumerantFunction,
    "denumerant function for a numerical semigroup",
    [IsNumericalSemigroup],
    function(s)
        return n->DenumerantOfElementInNumericalSemigroup(n,s);
    end);


#################################################################
## DenumerantIdeal(s,n)
## returns the ideal of s of all elements in s with denumerant 
## larger than n
#################################################################
InstallMethod(DenumerantIdeal,
    "Denumerant ideal of numerical semigroup",
    [IsNumericalSemigroup,IsInt],

    function(s,p)
    local msg, m, maxgen, factorizations, n, i, f, facts, toadd, gaps, ap, di;

    if p<0 then 
        Error("The integer argument must be non-negative");
    fi;
    if p=0 then 
        return 0+s;
    fi;
    msg:=MinimalGenerators(s);
    m:=Multiplicity(s);
    maxgen:=Maximum(msg);
    factorizations:=[];
    gaps:=[0];
    ap:=List([1..m], _->0);
    n:=0;
    while ForAny(ap,x->x=0) do
        if n>maxgen then 
            factorizations:=factorizations{[2..maxgen+1]};
        fi;
        factorizations[Minimum(n,maxgen)+1]:=[];

        for i in [1 .. Length(msg)] do
            if n-msg[i] >= 0 then
                facts:=[List(msg,x->0)];
                if n-msg[i] > 0 then
                    facts:=factorizations[Minimum(n,maxgen)+1-msg[i]];
                fi;

                for f in facts do
                    toadd:=List(f);
                    toadd[i]:=toadd[i]+1;
                    Add(factorizations[Minimum(n,maxgen)+1],toadd);
                od;
            fi;
        od;

        factorizations[Minimum(n,maxgen)+1]:=Set(factorizations[Minimum(n,maxgen)+1]);

        if Length(factorizations[Minimum(n,maxgen)+1])<=p then
            Add(gaps,n);
        else
            if ap[(n mod m) +1]=0 then 
                ap[(n mod m)+1]:=n;
            fi;
        fi;
        n:=n+1;
    od;

    di:=ap+s;
    Setter(SmallElements)(di,Difference([0..Maximum(gaps)+1],gaps));

    return di;
end);

InstallMethod(DenumerantIdeal,
    "Denumerant ideal of numerical semigroup",
    [IsInt,IsNumericalSemigroup],
    function(p,s)
        return DenumerantIdeal(s,p);
end);

####################################################################
#F MoebiusFunctionAssociatedToNumericalSemigroup(s,x)
## Computes the value in x of  Moebius function of the poset
## associated to a numerial semigroup s
## -Chappelon and Ramirez Alfonsin, Semigroup Forum 87 (2013), 313-330
####################################################################
InstallGlobalFunction(MoebiusFunctionAssociatedToNumericalSemigroup,function(s,x)
	local small, mu, msg, m, ap;

	if not(IsNumericalSemigroup(s)) then
		Error("The first argument must be a numerical semigroup.\n");
	fi;


	if not(IsInt(x)) then
		Error("The second argument must be an integer.\n");
	fi;

	if x<0 then return 0;	fi;

	if x=0 then return 1; fi;

	if not(x in s) then return 0; fi;


	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);

	if x in msg then return -1; fi;

	m:=MultiplicityOfNumericalSemigroup(s);
	ap:=Difference(AperyListOfNumericalSemigroupWRTElement(s,m),[0]);
	small:=Filtered(ap, y->y<=x);

	#Print(small,"\n");
	mu:=function(y)
		local sm;
		if y=0 then return 1; fi;
		if y in msg then return -1; fi;
		if not(y in s) then return 0;fi;
		sm:=Filtered(ap,z->z<=y);
		return -Sum(List(y-sm,mu));
	end;


	return -Sum(List(x-small,mu));

end);

### as a function
InstallMethod(MoebiusFunction,
    "of a numerical semigroup",
    [IsNumericalSemigroup],
    function(s)
        return n->MoebiusFunctionAssociatedToNumericalSemigroup(s,n);
    end);

###################################################################
#F  AdjacentCatenaryDegreeOfSetOfFactorizations(ls)
## computes the adjacent catenary degree of the set of factorizations ls
###################################################################
InstallGlobalFunction(AdjacentCatenaryDegreeOfSetOfFactorizations,function(ls)
    local distance, Fn, lenset, Zi, facti, i;


	if not(IsRectangularTable(ls) and ForAll(ls,IsListOfIntegersNS)) then
		Error("The argument is not a list of factorizations.\n");
	fi;

    # distance between two factorizations
    distance:=function(x,y)
        local p,n,i,z;

        p:=0; n:=0;
        z:=x-y;
        for i in [1..Length(z)] do
            if z[i]>0 then
                p:=p+z[i];
            else
                n:=n+z[i];
            fi;
        od;

        return Maximum(p,-n);
    end;

	Fn:=Set(ShallowCopy(ls));
    lenset:=Set( ls, Sum );
    if Length(lenset)=1 then
	return 0;
    fi;
    Zi:=[];
    for i in lenset do
        facti:=Filtered( Fn, x->Sum(x)=i );
        SubtractSet( Fn, facti );
        Add( Zi, facti );
    od;
    return Maximum( List( [2..Length( Zi )], t->Minimum( List( Zi[t-1], x->Minimum( List( Zi[t], y->distance( x, y ) ) ) ) ) ) );
end);

###################################################################
#F EqualCatenaryDegreeOfSetOfFactorizations(ls)
## computes the equal catenary degree of of the set of factorizations
###################################################################
InstallGlobalFunction(EqualCatenaryDegreeOfSetOfFactorizations,function(ls)
    local lFni;

	if not(IsRectangularTable(ls) and ForAll(ls,IsListOfIntegersNS)) then
		Error("The argument is not a list of factorizations.\n");
	fi;


    lFni:=Set( ls, t->Sum( t ) );
    return Maximum( List( lFni, y->CatenaryDegreeOfSetOfFactorizations( Filtered( ls, x->Sum( x )=y ) ) ) );
end);


###################################################################
#F MonotoneCatenaryDegreeOfSetOfFactorizations(ls)
## computes the equal catenary degree of of the set of factorizations
###################################################################
InstallGlobalFunction(MonotoneCatenaryDegreeOfSetOfFactorizations,function(ls)
    return Maximum(AdjacentCatenaryDegreeOfSetOfFactorizations(ls),
		EqualCatenaryDegreeOfSetOfFactorizations( ls ));
end);


############################################################
#F LShapesOfNumericalSemigroup(s)
## computes the set of LShapes associated to S (see [AG-GS])
##########################################################
InstallGlobalFunction(LShapesOfNumericalSemigroup,function(s)
	local ap, facts, total, totalfact,  new, w, z, n, ones,l, leq ;

	leq:=function(x,y)
		return ForAll(y-x, t->t>=0);
	end;

	l:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	n:=Length(l);
	ap:=Set(AperyListOfNumericalSemigroupWRTElement(s, l[n])) ;
	ones:=List([1..n-1],_->1);

	total:=[[]];
	for w in ap do
		facts:=FactorizationsIntegerWRTList(w,l{[1..n-1]});
		totalfact:=[];
		for z in facts do
			new:=Filtered(total, ll->Length(Filtered(ll, x->leq(x,z)))=Product(z+ones)-1);
			new:=List(new, ll->Concatenation(ll,[z]));
			totalfact:=Union(totalfact,new);
		od;
		total:=(totalfact);
	od;

	return total;
end);

InstallMethod(LShapes,
    "of a numerical semigroup",
    [IsNumericalSemigroup],
    LShapesOfNumericalSemigroup);

#############################################################################
##
#F  RFMatrices(f,s)
##
##  The integer f is a pseudo-Frobenius number of the numerical semigroup s
##  For each minimal generator n of s, it computes the factorizations of 
##  f+n in terms of the generators of s. These factorizations yield 
##  combinations of f in terms of the minimal generators of s (by substracting n).
##  The output is the cartesian product of these combinations for each of the 
##  minimal generator. This corresponds with the set of all Row Factorization 
##  matrices introduced by Moscariello (RF-Matrices)
##
#############################################################################
InstallGlobalFunction(RFMatrices,function(f,S)
    local A, B, v, F, k, pf;
    if not(IsNumericalSemigroup(S)) then 
        Error("The second argument must be a numerical semigroup");
    fi;
    A:=MinimalGenerators(S);
    if not(IsInt(f)) or (f in S) or ForAny(A, a->not(f+a in S)) then 
        Error("The first argument must be a pseudo-Frobenius number of the second");
    fi;
    v:=Length(A);
    B:=IdentityMat(v);
    F:=[];
    for k in [1..Length(A)] do
        F[k]:=Factorizations(f+A[k],S)-B[k];
    od;
    return Cartesian(F);
end);


###########################################################################
#F  DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal
## solution to msg*x-msg*y = 0, |x|<=|y| such that x,y are factorizations of s
## Used to compute the monotone catenary degree of the semigroup s
##
#############################################################################
InstallGlobalFunction(DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup,function(s)
	local l, n, facs, mat, ones, ncone, nmzcone,nmzconeproperty;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    # if not IsPackageMarkedForLoading("NormalizInterface","0.0") then
    #     Error("The package NormalizInterface is not loaded.\n");
    # fi;

    l:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    n:=Length(l);
    ones:=List([1..n],_->1);
    mat:=[];
    mat[1]:=Concatenation(l,-l,[0]);
    mat[2]:=Concatenation(ones,-ones,[1]);
    # nmzcone:=ValueGlobal("NmzCone");#Display(mat);
    # ncone:=nmzcone(["equations",mat]);
    # nmzconeproperty:=ValueGlobal("NmzConeProperty");
    # facs:=nmzconeproperty(ncone,"HilbertBasis");
    facs:=HilbertBasisOfSystemOfHomogeneousEquations(mat,[]);
    facs:=Set(facs,m->m{[1..n]});
    return Set(facs, f-> f*l);
end);

###########################################################################
#F  DegreesOfEqualPrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal
## solution to msg*x-msg*y = 0, |x|=|y| such that x,y are factorizations of s
## Used to compute the equal catenary degree of the semigroup
##
#############################################################################
InstallGlobalFunction(DegreesOfEqualPrimitiveElementsOfNumericalSemigroup,function(s)
	local l, n, facs, mat, ones, ncone, nmzcone,nmzconeproperty;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    # if not IsPackageMarkedForLoading("NormalizInterface","0.0") then
    # 	Error("The package NormalizInterface is not loaded.\n");
    # fi;

    l:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    n:=Length(l);
    ones:=List([1..n],_->1);
    mat:=[];
    mat[1]:=Concatenation(l,-l);
    mat[2]:=Concatenation(ones,-ones);
    # nmzcone:=ValueGlobal("NmzCone");#Display(mat);
    # ncone:=nmzcone(["equations",mat]);
    # nmzconeproperty:=ValueGlobal("NmzConeProperty");
    # facs:=nmzconeproperty(ncone,"HilbertBasis");
    facs:=HilbertBasisOfSystemOfHomogeneousEquations(mat,[]);
    facs:=Set(facs,m->m{[1..n]});
    return Set(facs, f-> f*l);
end);


####################################################################
#F EqualCatenaryDegreeOfNumericalSemigroup(s) computes the
##  adjacent catenary degree of the numerical semigroup s
##  the equal catenary degree is reached in the set of primitive
##  elements of s (see [PH])
####################################################################
InstallGlobalFunction(EqualCatenaryDegreeOfNumericalSemigroup,function(s)
	local prim, msg;
    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    # if not IsPackageMarkedForLoading("NormalizInterface","0.0") then
    # 	Error("The package NormalizInterface is not loaded.\n");
    # fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    prim:=DegreesOfEqualPrimitiveElementsOfNumericalSemigroup(s);

    return Maximum(Set(prim, n-> EqualCatenaryDegreeOfSetOfFactorizations(
                   FactorizationsIntegerWRTList(n,msg))));
end);

####################################################################
#F MonotoneCatenaryDegreeOfNumericalSemigroup(s) computes the
##  adjacent catenary degree of the numerical semigroup s
##  the monotone catenary degree is reached in the set of primitive
##  elements of s (see [PH])
####################################################################
InstallGlobalFunction(MonotoneCatenaryDegreeOfNumericalSemigroup,function(s)
	local prim, msg;
    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    # if not IsPackageMarkedForLoading("NormalizInterface","0.0") then
    #     Error("The package NormalizInterface is not loaded.\n");
    # fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    prim:=DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup(s);

    return Maximum(Set(prim, n-> MonotoneCatenaryDegreeOfSetOfFactorizations(
                   FactorizationsIntegerWRTList(n,msg))));
end);
#####################################################################
##
#O FengRaoDistance(NS,r,m)
##
# Computes the r-th Feng-Rao distance of the element m in the numerical semigroup NS
# function originally implemented by Benjamin Heredia
##

#####################################################################
InstallMethod(FengRaoDistance, "Feng-Rao distance of element in Numerical Semigroup",   
        [IsNumericalSemigroup,IsPosInt,IsPosInt],
 function(s,r,m)
  local  conductor, multiplicity, final, elementsUpToFinal, divisorsOfMany2, 
         addOne2, posiblesOfLen2;

  conductor := ConductorOfNumericalSemigroup(s);
  multiplicity := MultiplicityOfNumericalSemigroup(s);
  final := Maximum([m+multiplicity-1,conductor+multiplicity-1]);
  #  elementsUpToFinal := FirstElementsOfNumericalSemigroup(final,s);
  elementsUpToFinal := Intersection([0..final],FirstElementsOfNumericalSemigroup(final+r,s));
  #local functions
  # This function gives the set of divisors of several elements
  divisorsOfMany2 := function(lst)
    return Union(List(lst, x -> DivisorsOfElementInNumericalSemigroup(s,x)));
  end;
  # addOne(s,m,lst) gives back X(m;lst) as in the notes. That is the set used
  # to compute the X^r(m) from X^{r-1}(m).
  addOne2 := function(lst)
    local  prec, pos1, pos2;
    if IsEmpty(lst) then
      return Intersection([m..final],elementsUpToFinal);
    else
      prec := Reversed(lst)[1];
      if (prec<final) then
        pos1 := Intersection([prec+1..final],elementsUpToFinal);
      else
        pos1 := [];
      fi;
      #      pos2 := Filtered(List(lst, x -> x+multiplicity), y -> y>prec);      
      pos2 := Filtered(lst+multiplicity, y -> y>prec);
      return Union(pos1,pos2);
    fi;
  end;

  # This functions gives back X^r(m) recursively.
  posiblesOfLen2 := function(r)
    local lst, tot;
    if r = 1 then
      return List(addOne2([]), x -> [x]);
    else
      lst := posiblesOfLen2(r-1);
      tot := List(lst,x -> List(addOne2(x),y -> Concatenation(x,[y])));
      return Union(tot);
    fi;
  end;
  # end of local functions
  # And here it is the Feng-Rao distance

  return Minimum(List(posiblesOfLen2(r), d -> Length(divisorsOfMany2(d))));
end);

###########################################################################
##
#O FengRaoNumber(NS,r)
#O FengRaoNumber(r,NS)
# returns the r-Feng Rao number of a numerical semigroup NS
#####################################################################
InstallMethod(FengRaoNumber,"Feng-Rao number for a numerical semigroup",
        [IsNumericalSemigroup,IsPosInt],
  function(NS,r)
  local  fr, m, gens, a, ne, dm, fe, span, 
         generators_for_lists_for_Feng_Rao, LIST, RES, numbers, M, divs;


  fr := FrobeniusNumberOfNumericalSemigroup(NS);
  m := 2*fr+1;
  
  gens := MinimalGeneratingSystemOfNumericalSemigroup(NS);
  a := MultiplicityOfNumericalSemigroup(NS);
  ne := gens[Length(gens)];
  dm := DivisorsOfElementInNumericalSemigroup(NS,m);
  fe := Union(SmallElementsOfNumericalSemigroup( NS ),[fr+1..m+1+ne]);

  ###########################################################################
  ##################### local functions
  ##########################################################################
  span := function(conf)
    local   u,  i;

    u := [];
    for i in [1..Length(conf)] do
      u := Union(u,DivisorsOfElementInNumericalSemigroup(NS,conf[i]));
    od;
    return Difference(Filtered(u,n->n>0),dm);
  end;
  #################################################################
  #This function computes configurations of length r, starting in m and satisfying the property FR, among which the r-Feng Rao number of the numerical sgp NS can be computed
  # Besides the property FR, it uses also the fact that two configurations of the same length satisfying the property FR and having the same shadow (intersection with [m..m+ne-1] (the "ground")) have the same number of divisors
  #

  generators_for_lists_for_Feng_Rao:=function()
    local   rho2,  fr,  gens,  el,  elts,  elts0,  ne,  span2,  salida,  i,  
            salidan,  x,  min,  mj,  Dmj_aux,  divs,  divlist;

    rho2 := MultiplicityOfNumericalSemigroup(NS);
    fr := FrobeniusNumberOfNumericalSemigroup(NS);
    gens := MinimalGeneratingSystemOfNumericalSemigroup(NS);
    el := FirstElementsOfNumericalSemigroup(r,NS);
    elts := el{[2..r]};
    elts0 := Union([0],elts);
    elts := Intersection(elts,gens); # as the configuration is computed recursively, it suffices to work with generators

    ne := gens[Length(gens)];

    #####################
    ## computes the union of the divisors of the elements in conf that are greater than m
    span2 := function(conf)
      local   fe,  u,  i;

      u := [];
      for i in [1..Length(conf)] do
        u := Union(u,Filtered(conf[i]-elts0,n -> n>=m));
      od;
      return u;
    end;


    if r=0 then 
      return [[]]; 
    fi;
    salida := [[m]];
    for i in [2..r] do
      salidan := [];
      for x in salida do
        min := Minimum(x[Length(x)]+rho2,m+el[i]);
        divlist := span2(x);

        for mj in [x[Length(x)]+1..min] do 
          Dmj_aux := Set(mj - elts);#strict divisors of mj among which are those greater than m 
          divs := Intersection(Dmj_aux,[m..mj-rho2]);

          if IsSubset(divlist, divs) then
            Append(salidan,[Difference(Union(x,[mj]),divs)]);

            if mj > m + ne then #This ensures that the configurations constructed have different shadows
              break;
            fi;
          fi;
        od;  
      od;
      salida := salidan;
    od;     
    return salida;
  end;
  ###########################################################################
  ##################### end of local functions
  ###########################################################################

  LIST := generators_for_lists_for_Feng_Rao();
  Info(InfoNumSgps,3, "The possible configurations have been computed: ",Length(LIST),"\n");

  RES := [];
  numbers := [];

  for M in LIST do
    divs := span(M);
    AddSet(numbers,Length(divs));
  od;
  return numbers[1];
end);
#################################################
#####################################################################
InstallMethod(FengRaoNumber,"Feng-Rao number for a numerical semigroup",
        [IsPosInt,IsNumericalSemigroup],
  function(r,NS)
  return FengRaoNumber(NS,r);
end);


##############################################################################################################
##
#P  IsHomogeneousNumericalSemigroup(S)
##
##  Tests if S is homogeneous, that is, for every element a in the Apéry set of its multiplicity,
##  all the factorizations of a have the same length
##
##############################################################################################################
InstallMethod(IsHomogeneousNumericalSemigroup,"Checks if the numerical semigroup is homogeneous",
        [IsNumericalSemigroup],
function(S)
    local A;
    A:=AperyList(S);
    return ForAll(A, a->Length(LengthsOfFactorizationsElementWRTNumericalSemigroup(a,S))=1);
end);
