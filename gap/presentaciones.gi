#############################################################################
##
#W  presentaciones.gi       Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: presentaciones.gi,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#F  FortenTruncatedNCForNumericalSemigroups(l)
##
##  l contains the list of coefficients of a 
##  single linear equation. fortenTruncated gives a minimal generator 
##  of the affine semigroup of nonnegative solutions of this equation
##  with the first coordinate equal to one.
##
##  Used for computing minimal presentations.
##
#############################################################################
InstallGlobalFunction(FortenTruncatedNCForNumericalSemigroups, function(l)
    local   leq,  m,  solutions,  explored,  candidates,  x,  tmp;
    

    #  leq(v1,v2)
    #  Compares vectors (lists) v1 and v2, returning true if v1 is less than or 
    #  equal than v2 with the usual partial order.
    leq := function(v1,v2)
        local v;
        #one should make sure here that the lengths are the same
        v:=v2-v1;
        return (First(v,n->n<0)=fail);
    end;
    ##  End of leq()  --
    
    m:=IdentityMat(Length(l));
    solutions:=[];
    explored:=[];
    candidates:=[m[1]];
    m:=m{[2..Length(m)]};
    while (not(candidates=[])) do
        x:=candidates[1];
        explored:=Union([x],explored);
        candidates:=candidates{[2..Length(candidates)]};
        if(l*x=0) then
            return x;
        else
            tmp:=List(Filtered(m,n->((l*x)*(l*n)<0)),y->y+x);
            tmp:=Difference(tmp,explored);
            tmp:=Filtered(tmp,n->(First(solutions,y->leq(y,n))=fail));
            candidates:=Union(candidates,tmp);
        fi;
    od;
    return fail;
end);


#############################################################################
##
#F  GraphAssociatedToElementInNumericalSemigroup(n,s)
##
##  Computes the graph associated to the element n
##  the numerical semigroup s.
##  Its vertices are those minimal generators m such that
##      n-m in s
##  Its edges are those pairs (m1,m2) of minimal generators
##      such that n-(m1+m2) in s.
#############################################################################
InstallGlobalFunction(GraphAssociatedToElementInNumericalSemigroup, function(n,s)
    local vertices, edges,msg;
    
    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;
    if(not(BelongsToNumericalSemigroup(n,s))) then
        Error(n," must be an element of ",s,".\n");
    fi;
    
    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    vertices:=Filtered(msg,m->(BelongsToNumericalSemigroup(n-m,s)));
    edges:=Filtered(Filtered(Cartesian(vertices,vertices),e->e[1]<e[2]),
                   e->(BelongsToNumericalSemigroup(n-Sum(e),s)));
    
    return [vertices,edges];
end);





#############################################################################
##
#F  MinimalPresentationOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, give a minimal presentation
##  the output is a list of pairs showing the relationship
##  between the minimal generators of s
##  the algorithm is the one given in 
##  -J. C. Rosales, {\em An algorithmic method to compute a minimal
##  relation for any numerical semigroup}, Internat. J. Algebra Comput. 
##  {\bf 6} (1996), no. 4, 441--455.
#############################################################################
InstallGlobalFunction(MinimalPresentationOfNumericalSemigroup, function(s)
    local   connectedComponents,  msg,  ap,  candidates,  
            pairs,  expandexpression,  presentation,  pair,  n,  components,  
            rclasses,  ii;
    
    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");        
    fi;
    
    ##  Computes the set of connected components of the
    ##  graph with vertices v and edges e. Returns a 
    ##  list of lists, each one containing the vertices of 
    ##  a connected component.
    connectedComponents := function(g)
        local current,possible, colission, rest, tmp, components, v, e;
    
        # if(not(IsList(g))) then
        #     Error("The argument must be a list");
        # fi;
        # if((Length(g)<>2)) then
        #     Error("The argument must have two elements: vertices and edges.\n");
        # fi;
        # if(not(IsList(g[1]) and IsList(g[2]))) then
        #     Error("The two elements of the argument are lists: one with the vertices and the other with the edges.\n");
        # fi;
        
        v:=g[1]; #vertices
        e:=g[2]; #edges
        
        tmp:=List(v,n->[n]);
        possible:=Union(tmp,e);
        
        if(possible=[]) then
            return [];
        fi;
        
        components:=[];
        while(possible<>[]) do
            current:=possible[1];
            possible:=possible{[2..Length(possible)]};
            colission:=Union(Filtered(possible,s->Intersection(s,current)<>[]));
            rest:=Filtered(possible,s->Intersection(s,current)=[]); 
                #this should be optimized, this is the complement 
                #      of colission in possible before the union
            
            while(colission<>[]) do            
                current:=Union(colission,current);
                colission:=Union(Filtered(possible,s->Intersection(s,current)<>[]));
                rest:=Filtered(possible,s->Intersection(s,current)=[]);             
                possible:=rest;
            od;
            Add(components,current);
        od;
        return components;
    end;
    ##  End of connectedComponents()  --    
    
    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
     return [];
    fi;
    ap:=AperyListOfNumericalSemigroupWRTElement(s,msg[1]);
    ap:=ap{[2..Length(ap)]};    # I remove the zero, 
                                #   minimal generators yield conneted graphs
    candidates:=Union(List(msg,n->List(ap,m->m+n)));
                                # Gn not conneted implies n=wi+minimalgenerator
                                #    thus these are the candidates
    candidates:=Filtered(candidates,n->Length(connectedComponents(
                    GraphAssociatedToElementInNumericalSemigroup(n,s)))>1);
                                # choose n with nonconnected graphs

    presentation:=[];
	for n in candidates do
		rclasses:=RClassesOfSetOfFactorizations(FactorizationsIntegerWRTList(n,msg));
		pairs:=List([2..Length(rclasses)],i-> [rclasses[1][1],rclasses[i][1]]);		
		presentation:=Union(presentation,pairs);
	od;
    
    return presentation;

end);

#############################################################################
##
#F  BettiElementsOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, returns the elements whose associated graphs
##  are non-connected, or in other words, whose factorizations are used to 
##  construct any minimal presentation for s
##
#############################################################################
InstallGlobalFunction(BettiElementsOfNumericalSemigroup, function(s)
    local mgs, pre;

    if(not(IsNumericalSemigroup(s))) then
        Error("The argument must be a numerical semigroup.\n");        
    fi;
    
    mgs:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    pre:=MinimalPresentationOfNumericalSemigroup(s);
    return Set(pre,r->r[1]*mgs);
end);
