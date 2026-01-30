#############################################################################
##
#W  order.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2026 by Manuel Delgado and Pedro Garcia-Sanchez 
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
#####################        Defining posets           ######################
#############################################################################
##
#O PosetNS(l,S)
#O PosetNS(S,l)
##
## l is a list of integers and S a numerical semigroup
##
## returns the poset whose underlying set is l and the order is defined by
## a <= b if b - a in S
##
#############################################################################
InstallMethod(PosetNS, "for list of integers and numerical semigroup",
  [IsList, IsNumericalSemigroup],
function(l,S)
  local  I;
      if not (IsNumericalSemigroup(S)) then
        Error("The arguments of PosetNS must be a numerical semigroup and a nonempty list of integers.");
    fi;
    I := rec();
    ObjectifyWithAttributes(I, PosetNSType,
        UnderlyingNSPoset, S,
        GroundSet, Set(l)
        );
    return I;
end);

InstallMethod(PosetNS, "for numerical semigroup and list of integers",
        [IsNumericalSemigroup, IsList],
function(S,l)
  return PosetNS(l,S);
end);

#############################################################################
##
#M  ViewObj(p)
##
##  This method for posets defined by numerical semigroups.
##
#############################################################################
InstallMethod( ViewObj,
  "prints poset of a numerical semigroup",
  [IsPosetNS],
  function( p )
    Print("<Poset defined wrt to numerical semigroup>");
end);


#############################################################################
##
#M  ViewString(p)
##
##  This method for posets defined by numerical semigroups.
##
#############################################################################
InstallMethod( ViewString,
  "prints a poset defined by a numerical semigroup",
  [IsPosetNS],
  function( p )
    return ("Poset defined by numerical semigroup");
end);

#############################################################################
##
#M  String(p)
##
##  This method for posets defined by numerical semigroups.
##
#############################################################################
InstallMethod(String,
  "prints a poset defined by a numerical semigroup",
  [IsPosetNS],
  function( p )
    return (Concatenation(String(GroundSet(p)), " ordered wrt NumericalSemigroup([", 
        String(Generators(UnderlyingNSPoset(p))), "])"));
end);

#############################################################################
##
#A  MaximalElements(p)
##  Returns the list of maximal elements of the poset p
##
#############################################################################
InstallMethod(MaximalElements,
  "for posets defined by numerical semigroups",
  [IsPosetNS],  
  function( p )
    local s,l,maxs,max,below;    
    s := UnderlyingNSPoset(p);
    l := List(GroundSet(p));
    maxs := [];
    while l<>[] do
        max:=Remove(l);
        Add(maxs, max);
        below:=Filtered(l, x-> max - x in s);
        l:=Difference(l, below);
    od;
    return maxs;
end);

#############################################################################
##
#A  MinimalElements(p)
##  Returns the list of minimal elements of the poset p
##
#############################################################################
InstallMethod(MinimalElements,
  "for posets defined by numerical semigroups",
  [IsPosetNS],  
  function( p )
    local s,l,mins,min,above;    
    s := UnderlyingNSPoset(p);
    l := Reversed(List(GroundSet(p)));
    mins := [];
    while l<>[] do
        min:=Remove(l);
        Add(mins, min);
        above:=Filtered(l, x-> x - min in s);
        l:=Reversed(Difference(l, above));
    od;
    return mins;
end);

#############################################################################
##
#O UpSet(p,l)
##  Returns the upset of the list l in the poset p
##
#############################################################################
InstallMethod(UpSet, "for posets defined by numerical semigroups", [IsPosetNS, IsList],
function(p, l)
  local s;
  if not(IsListOfIntegersNS(l)) then
    Error("The second argument must be a list of integers.\n");
  fi;
  if not(IsSubset(GroundSet(p), Set(l))) then
    Error("The elements of the list must belong to the ground set of the poset.\n");
  fi;
  s:=UnderlyingNSPoset(p);
  return Filtered(GroundSet(p), x->ForAny(l, y-> x - y in s));
end);

#############################################################################
##
#O DownSet(p,l)
##  Returns the downset of the list l in the poset p, that is, the set of 
##  elements less than or equal to some element of l.
##
#############################################################################
InstallMethod(DownSet, "for posets defined by numerical semigroups", [IsPosetNS, IsList],
function(p, l)
  local s;
  if not(IsListOfIntegersNS(l)) then
    Error("The second argument must be a list of integers.\n");
  fi;
  if not(IsSubset(GroundSet(p), Set(l))) then
    Error("The elements of the list must belong to the ground set of the poset.\n");
  fi;
  s:=UnderlyingNSPoset(p);
  return Filtered(GroundSet(p), x->ForAny(l, y-> y - x in s));
end);

#############################################################################
##
#O Antichains(p)
##  Returns the set of antichains (sets of non comparable elements) of p.
##
#############################################################################
InstallMethod(Antichains, "for posets defined by numerical semigroups", [IsPosetNS],
function(p)
  return AntichainsOfNumericalSemigroup(UnderlyingNSPoset(p), List(GroundSet(p)));
end);

############################################################################
##
#F HasseDiagramOfNumericalSemigroup(s, A)
##  Returns a binary relation which is the Hasse diagram of A with 
##  respect to the ordering a <= b if b - a in S.
##
############################################################################
InstallGlobalFunction(HasseDiagramOfNumericalSemigroup, function(s, A)
  local rel, p, D;
  
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
  
  # Build the binary relation and returns its Hasse diagram
  D := Domain(Set(A));
  rel := Tuples(D, 2);
  rel := Filtered(rel, p -> p[2] - p[1] in s);
  rel := List(rel, p -> DirectProductElement([p[1], p[2]]));  
  rel := BinaryRelationByElements(D, rel);  
  return HasseDiagramBinaryRelation(rel);  
end);

############################################################################
##
#F HasseDiagramOfBettiElementsOfNumericalSemigroup(s)
##  Returns a binary relation which is the Hasse diagram of the Betti
##  elements of s with respect to the ordering a <= b if b - a in S.
##
############################################################################
InstallGlobalFunction(HasseDiagramOfBettiElementsOfNumericalSemigroup, function(s)
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
    
  return HasseDiagramOfNumericalSemigroup(s, BettiElementsOfNumericalSemigroup(s));    
end);

############################################################################
##
#F HasseDiagramOfAperyListOfNumericalSemigroup(s, n)
##
############################################################################
InstallGlobalFunction(HasseDiagramOfAperyListOfNumericalSemigroup, function(s, n...)
  local a;
    
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
  
  if Length(n) = 0 then
    a := MultiplicityOfNumericalSemigroup(s);
  elif Length(n) = 1 then
    a := n[1];
  else
    Error("The number of arguments must be one or two");
  fi;
    
  return HasseDiagramOfNumericalSemigroup(s, AperyListOfNumericalSemigroup(s, a));    
end);

############################################################################
##
#F AntichainsOfNumericalSemigroup(s, A)
##  Returns a the set of antichains (sets of non comparable elements) of A 
##  with respect to the ordering a <= b if b - a in S.
##  The implementation is based on the implementation of antichains_iterator
##  Peter Jipsen and Franco Saliola for the SAGE project
##  https://github.com/sagemath/sage/blob/981d7d71a2738778c9e3fb4fdf67a0fd3ce0c19c/src/sage/combinat/posets/hasse_diagram.py#L2216
##
############################################################################
InstallGlobalFunction(AntichainsOfNumericalSemigroup, function(s, l)
  local n, leq, antichain_queues, antinchain, queue, x, new_antichain, new_queue, t, pop, list_antichains;

  if not IsNumericalSemigroup(s) then
    Error("The firts argument must be a numerical semigroup.\n");
  fi;
  if not IsListOfIntegersNS(l) then
    Error("The second argument must be a list of integers.\n");
  fi;
  
  n:=Length(l);
  #leq:=List([1..n], i->Filtered(l, x->x<>l[i] and x-l[i] in s));
  #we will not use this, since we are calling it with l=Gaps(s) and then membership in s is faster
  antichain_queues:=[[[],[n,n-1..1]]];
  list_antichains:=[];
  while Length(antichain_queues)>0 do
    pop:=Remove(antichain_queues);
    antinchain:=pop[1];
    queue:=pop[2];
    Add(list_antichains, antinchain);
    while Length(queue)>0 do
      x:=Remove(queue);
      new_antichain:=Concatenation(antinchain,[x]);
      #new_queue:=Filtered(queue, t->not(x in leq[t] or t in leq[x]));
      new_queue:=Filtered(queue, t->not((l[x]-l[t] in s) or (l[t]-l[x] in s)));
      Add(antichain_queues, [new_antichain, new_queue]);
    od;
  od;

  return List(list_antichains, a->l{a});
end);