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