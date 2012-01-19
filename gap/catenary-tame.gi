#############################################################################
##
#W  catenary-tame.gi        Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: catenary-tame.gi,v 0.971 $
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
##   -S. T. Chapman, P. A. García-Sánchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
InstallGlobalFunction( CatenaryDegreeOfNumericalSemigroup, function(s)
    local   rClasses,  msg,  ap,  candidates,  rclasses;

    # used to determine the different R-classes of the
    #   factorizations of an element
    rClasses:=function(l)
        local current, pos, len, colisionan, cola;

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

    end;


    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
     Error("The catenary degree does not make sense for ",s,"\n");
    fi;

    ap:=AperyListOfNumericalSemigroupWRTElement(s,msg[1]);
    ap:=ap{[2..Length(ap)]};    # I remove the zero,
                                #   minimal generators yield conneted graphs
    candidates:=Union(List(msg,n->List(ap,m->m+n)));
                                # Gn not conneted implies n=wi+minimalgenerator
                                #    thus these are the candidates
    rclasses:=List(candidates,n->rClasses(NSGPfactorizationsNC(n,msg)));
                                # from every n y obtain the connected components
                                #   they will give me the expressions of n
                                #   that yield minimal generators
    rclasses:=Filtered(rclasses, n->Length(n)>1);

    return Maximum(List(rclasses,l->Maximum(List(l,r->Minimum(List(r,Sum))))));
end);
############################################################################
## 
#F This function returns true if the graph is connected an false otherwise
##
## It is part of the NumericalSGPS package just to avoid the need of using 
## other graph packages only to this effect. It is used in 
## CatenaryDegreeOfElementInNumericalSemigroup
##
##
InstallGlobalFunction( IsConnectedGraphNCForNumericalSemigroups, function(G)
    local i, j, fl,
          n, # number of vertices
          uG, # undirected graph (an edge of the undirected graph may be 
          #                 seen as a pair of edges of the directed graph) 
          visit,
          dfs;

    fl := Flat(G);
    if fl=[] then
        n := Length(G);
    else
        n := Maximum(Length(G),Maximum(fl));
    fi;
    uG := StructuralCopy(G);
    while Length(uG) < n do
        Add(uG,[]);
    od;
    for i in [1..Length(G)] do
        for j in G[i] do
            UniteSet(uG[j],[i]);
        od;
    od;


    visit := [];          # mark the vertices an unvisited
    for i in [1..n] do
        visit[i] := 0;
    od;

    dfs := function(v) #recursive call to Depth First Search
        local w;
        visit[v] := 1;
        for w in uG[v] do
            if visit[w] = 0 then
                dfs(w);
            fi;
        od;
    end;
    dfs(1);
    if 0 in visit then
        return false;
    fi;
    return true;
end);

#========================================================================
##
#F This function is the NC version of CatenaryDegreeOfElementInNumericalSemigroup. It works 
## well for numbers bigger than the Frobenius number
##
##------------------------------------------------------------------------
InstallGlobalFunction(CatenaryDegreeOfElementInNumericalSemigroup_NC, function(n,s)
    local   len,  distance,  Fn,  V,  underlyinggraph,  i,  weights,  
            weightedgraph,  j,  dd,  d,  w;

    #---- Local functions definitions -------------------------
    #==========================================================
    #==========================================================
    #Given two factorizations a and b of n, the distance between a 
    #and b is d(a,b)=max |a-gcd(a,b)|,|b-gcd(a,b)|, where 
    #gcd((a_1,...,a_n),(b_1,...,b_n))=(min(a_1,b_1),...,min(a_n,b_n)).


    #----------------------------------------------------------
    distance := function(a,b)
        local   k,  gcd,  i;

        k := Length(a);
        if k <> Length(b) then
            Error("The lengths of a and b are different");
        fi;


        gcd := [];
        for i in [1..k] do
            Add(gcd, Minimum(a[i],b[i]));
        od;
        return(Maximum(Sum(a-gcd),Sum(b-gcd)));

    end;
    ## ----  End of distance()  ---- 

    #==========================================================
    #---- End of Local functions definitions ------------------

    #==========================================================
    #-----------      MAIN CODE       -------------------------
    #----------------------------------------------------------

    Fn := FactorizationsElementWRTNumericalSemigroup( n, s );
    #Print("Factorizations:\n",Fn,"\n");
    V := Length(Fn);
    if V = 1 then
        return 0;
    elif V = 2 then
        return distance(Fn[1],Fn[2]);
    fi;


    # compute the directed weighted graph
    underlyinggraph := [];
    for i in [2 .. V] do
        Add(underlyinggraph, [i..V]);
    od;
    Add(underlyinggraph, []);
    weights := [];
    weightedgraph := StructuralCopy(underlyinggraph);
    for i in [1..Length(weightedgraph)] do
        for j in [1..Length(weightedgraph[i])] do
            dd := distance(Fn[i],Fn[weightedgraph[i][j]]);
            Add(weights,dd);
            weightedgraph[i][j] := [weightedgraph[i][j],dd];

        od;
    od;
    weights:=Set(weights);
    d := 0;
    while IsConnectedGraphNCForNumericalSemigroups(underlyinggraph) do
        w := weights[Length(weights)-d];
        d := d+1;
        for i in weightedgraph do
            for j in i do
                if IsBound(j[2]) and j[2]= w then
                    Unbind(i[Position(i,j)]);
                fi;
            od;
        od;
        for i in [1..Length(weightedgraph)] do
            weightedgraph[i] := Compacted(weightedgraph[i]);
        od;
        underlyinggraph := [];
        for i in weightedgraph do
            if i <> [] then
                Add(underlyinggraph, TransposedMatMutable(i)[1]);
            else
                Add(underlyinggraph, []);
            fi;
        od;
    od;


    return(weights[Length(weights)-d+1]);      

end);
## ----  End of CatenaryDegreeOfElementInNumericalSemigroup_NC()  ---- 
#========================================================================
##
#========================================================================
##
#F This function returns the catenary cegree in a numerical semigroup S of 
## a positive integer n
##
#------------------------------------------------------------------------
##
InstallGlobalFunction(CatenaryDegreeOfElementInNumericalSemigroup, function(n,S)

    #---- Tests on the arguments ------------------------------
    if not (IsNumericalSemigroup(S) and IsPosInt(n-1)) then
        Error(" The arguments of CatenaryDegreeOfElementInNumericalSemigroup are a nonnegativeinteger and a numerical semigroup");
    fi;

    #---- End of Tests on the arguments -----------------------


    #==========================================================
    #-----------      MAIN CODE       -------------------------
    #----------------------------------------------------------
    if (not n in S) or (n=0) or (n in MinimalGeneratingSystemOfNumericalSemigroup(S)) then
        return 0;
    fi;
    return CatenaryDegreeOfElementInNumericalSemigroup_NC(n,S);
end);
## ----  End of CatenaryDegreeOfElementInNumericalSemigroup()  ---- 
##
#========================================================================

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
##   -S. T. Chapman, P. A. García-Sánchez,
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
    fact:=NSGPfactorizationsNC(n,msg);

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
##   -S. T. Chapman, P. A. García-Sánchez,
##    D. Llena,  The catenary and tame degree of numerical
##    monoids, Forum Math. 2007 1--13.
##
#############################################################################
InstallGlobalFunction( TameDegreeOfNumericalSemigroup, function(s)
    local msg, ap, candidates;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    #Print(msg);
    if(msg[1]=1) then
     return 0;
    fi;

    ap:=Difference(Union(Set(msg,n->AperyListOfNumericalSemigroupWRTElement(s,n))),[0]);

    candidates:=Set(Cartesian(ap,msg),Sum);

    return Maximum(Set(candidates,n->TameDegreeOfElementInNumericalSemigroup(n,s)));
end);



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
        Error("The second argument must be a numerical semigroup.");
    fi;

    if n=0 then
        return [List(MinimalGeneratingSystemOfNumericalSemigroup(s),n->0)];
    fi;


    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.");
    fi;

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return NSGPfactorizationsNC(n,gen);
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
        Error("The second argument must be a numerical semigroup.");
    fi;


    if n=0 then
        return [0];
    fi;

    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.");
    fi;

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return Set(NSGPfactorizationsNC(n,gen),Sum);
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
        Error("The second argument must be a numerical semigroup.");
    fi;

    if not IsPosInt(n) then
        Error("The first argument must be a positive integer.");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.");
    fi; #this ensures that the lengths won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    lenfact:=Set(NSGPfactorizationsNC(n,gen),Sum);
    min:=Minimum(lenfact);
    max:=Maximum(lenfact);

    return max/min;
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
        Error("The argument must be a numerical semigroup.");
    fi;


    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    min:=Minimum(gen);
    max:=Maximum(gen);

    return max/min;
end);


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
        Error("The second argument must be a numerical semigroup.");
    fi;

    if n=0 then
        return [];
    fi;


    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.");
    fi;

    if not (n in s) then
        Error("The first argument does not belong to the second.");
    fi; #this ensures that the lenghts won't be zero

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    lenfact:=Set(NSGPfactorizationsNC(n,gen),Sum);
    return Set([1..(Length(lenfact)-1)], i->lenfact[i+1]-lenfact[i]);

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
        Error("The second argument must be a numerical semigroup.");
    fi;

    if n=0 then
        return 0;
    fi;


    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.");
    fi;

    gen:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    return Maximum(List(NSGPfactorizationsNC(n,gen),Sum));
end);


#############################################################################
##
#F  OmegaPrimalityOfElementInNumericalSemigroup(n,s)
##
##  Computes the omega primality of an elmenent n in S, as explained in 
##  V. Blanco, P. A. Garc\'{\i}a-S\'anchez, A. Geroldinger, 
##  Semigroup-theoretical characterizations of arithmetical invariants with 
##  applications to numerical monoids and Krull monoids, {arXiv}:1006.4222v1.
##
#############################################################################
InstallGlobalFunction(OmegaPrimalityOfElementInNumericalSemigroup, function(n,s)

	local candidates,msg,fact,c, le,sum;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.");
    fi;


    if not ( n in s ) then
        Error("The first argument must be an element of the second.");
    fi;

        le:=function(a,b)  #ordinary partial order
            return ForAll(b-a,x-> x>=0);
        end;

        msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);

        candidates:=Union(List(msg, x-> n + AperyListOfNumericalSemigroupWRTElement(s,x)));
        fact:=[];

        for c in candidates do
            fact:=Union(fact,FactorizationsElementWRTNumericalSemigroup(c,s));
        od;

        fact:=Filtered(fact, f-> ForAll(fact, z-> not(le(z,f)) or z=f));
        sum:=Set(fact,Sum);

        return Maximum(sum);
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
        Error("The second argument must be a numerical semigroup.");
    fi;

      return Maximum(Set(MinimalGeneratingSystemOfNumericalSemigroup(s),
	n->OmegaPrimalityOfElementInNumericalSemigroup(n,s)));

end);

