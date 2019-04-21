#############################################################################
##
#W  pseudoFrobenius.gi          Manuel Delgado <mdelgado@fc.up.pt>
#W                              Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################
################################################################
## The main function in this file is
#F NumericalSemigroupsWithPseudoFrobeniusNumbers
## Input: PF (a set of postive integers)
## ouput: a list of numerical semigrups S such that PF(S)=PF
#################################################################
##
## Info classes have been created. It may be used by typing: SetInfoLevel(InfoTipo,2);
##
DeclareInfoClass("InfoTipo");
#################################################################
##
########
## Several auxiliary functions are included
#############################################
#################################################################
#F NumericalSemigroupWithGivenElementsAndFrobenius(elts,frob)
##
## Returns the least numerical semigroup containing the list elts of positive integers and having the largest possible Frobenius number not greater than frob.
## This is just an "abreviation" of  NumericalSemigroup(Union(elts,[frob+1..frob+First(elts,IsPosInt)])) that is intended to turn the code more readable.
##
#################################################################
InstallGlobalFunction(NumericalSemigroupWithGivenElementsAndFrobenius, function(elts,frob)
  local  ns;
  if elts <> [] and elts <> [0] then
    ns := NumericalSemigroup(Union(elts,[frob+1..frob+Minimum(Difference(elts,[0]))]));
  else
    ns := NumericalSemigroup([frob+1..2*(frob+1)]);
  fi;
  return ns;
end);
##
#################################################################
##
#F StartingForcedGapsForPseudoFrobenius(PF)
##
## Computes the set of starting forced gaps, according to the definition in [DG-SR-P15]. 
##
#################################################################
InstallGlobalFunction(StartingForcedGapsForPseudoFrobenius, function(PF)  
  local  type, forced_gaps, closures, differences, i, x;

  type := Length(PF);  
  forced_gaps := Union([1..Length(PF)],PF); # uses the fact m(S)>=t(S)+1
  ## justification: lemma:pseudo-comb-pseudo
  closures := List([1..type-1], i->SmallElementsOfNumericalSemigroup(NumericalSemigroupWithGivenElementsAndFrobenius(PF{[1..i]},PF[i+1])));
  differences := [];
  for i in [2..type] do
    differences := Union(differences,Filtered(PF[i] - closures[i-1],IsPosInt));
  od; 
  ##
  forced_gaps := Union(forced_gaps,differences);
  ##### add divisors
  forced_gaps := Union(List(forced_gaps,DivisorsInt));
  ##### 
  Info(InfoTipo,2,"starting forced gaps:\n",forced_gaps,"\n");
  return forced_gaps;
end);
#################################################################
#################################################################
##
#F FurtherForcedElementsForPseudoFrobenius(f_gaps,f_elts,PF)
##
## computes a set of elements forced by the known forced gaps (f_gaps) and forced elements (f_elts) 
## returns the set computed if it is disjoint from f_gaps, returns fail otherwise.
##
## NOTE: "PF" is exactly the set of pseudo-frobenius numbers
##
## the computation is done in two parts (elements forced by exclusion and big elements forced by small gaps) 
## The justifications:
##  - exclusion: $x\in Z\S$ if and only if $f-x\in S$ for some $f\in PF(S)$
##  - big elements: if m is the multiplicity and 1 <= i < m, then frob-i + m \in S. So, either frob-i \in  S or frob-i is pseudo-frobenius.
## 
#################################################################
InstallGlobalFunction(FurtherForcedElementsForPseudoFrobenius, function(f_gaps,f_elts,PF)
  local  frob, ef_elts, x, filt, candidates, m, bf_elts, nf_elts, closure, 
         elts, pf_plus_elements, conflicts;
  
  frob := Maximum(PF);     
  ###exclusion
  ef_elts := [];
 #( Lemma 10 )
  for x in f_gaps do
    filt := Filtered(PF, f -> not((f-x in f_gaps) or (f-x < 0)));
    if Length(filt) = 1 then
      AddSet(ef_elts, filt[1]-x);
    fi;
  od;
  #( Lemma 11 )
  candidates := Difference([1..frob-1],Union(f_elts,ef_elts,PF));
  for x in candidates do 
    if Filtered(Difference(PF-x,f_gaps),IsPosInt) = [] then
      AddSet(ef_elts, x);
    fi;
  od;
  ###big elements (Lemma 12)
  m := First(Integers,n-> IsPosInt(n) and not(n in f_gaps)); #least integer that is not a forced gap
  bf_elts := Difference(frob - [1..m-1], PF);
  ### test
  nf_elts := Union(f_elts,ef_elts,bf_elts);  
  closure := NumericalSemigroupWithGivenElementsAndFrobenius(nf_elts,frob);
  nf_elts := SmallElementsOfNumericalSemigroup(closure);
  nf_elts := Union(nf_elts,[Maximum(nf_elts)..frob+1]);
  #############
  ##added due to an observation of Ignacio Ojeda
  elts := Difference(nf_elts,[0,frob+1]);
  #From the definition: the sum of a pseudo-Frobenius with an element is an element
  pf_plus_elements := Intersection(Union(List(PF,f->f+elts)),[0..frob+1]);
  nf_elts := Union(nf_elts,pf_plus_elements);
  ##############
  conflicts := Intersection(f_gaps,nf_elts);
  if conflicts = [] then
    return nf_elts;
  else
    Info(InfoTipo,2,"There is no numerical semigroup with the given set as set of pseudo-Frobenius numbers, since the integers ",conflicts," would have to be at the same time forced gaps and forced integers.\n");
    return fail;
  fi;
end);
#################################################################
#################################################################
##
#F FurtherForcedGapsForPseudoFrobenius(f_gaps,f_elts,PF)
##
## Returns a list of integers that must be gaps of any numerical semigroup containing elts and for which it is known that f_gaps are gaps
## Returns fail in case it finds an element that had to be a gap, which implies that no semigroup exists having the given sets of gaps and elements.
##
## Justification: note that if f is a gap and e is an element, then f-e is a gap. (Otherwise f=(f-e)+e would be an element)
##
#################################################################
InstallGlobalFunction(FurtherForcedGapsForPseudoFrobenius, function(f_gaps,f_elts,PF)
  local  frob, elts, nf_gaps, conflicts;
  
  frob := Maximum(PF);   
  elts := Difference(f_elts,[0,frob+1]);
  
  nf_gaps := Union(List(f_gaps, g -> g - elts));
  nf_gaps := Filtered(nf_gaps,IsPosInt);
  nf_gaps := Union(List(nf_gaps,DivisorsInt));
  
  ##test
  conflicts := Intersection(nf_gaps,f_elts);
  if conflicts = [] then
    return Union(f_gaps,nf_gaps);
  else
    Info(InfoTipo,2,"There is no numerical semigroup with the given set as set of pseudo-Frobenius numbers, since the integers",conflicts," would have to be at the same time forced gaps and forced integers.\n");
    return fail;
  fi;
end);  
#################################################################
##
#F SimpleForcedIntegersForPseudoFrobenius(f_gaps,f_elts,PF)
##
## The aim of this function is to compute forced gaps and forced integers for a semigroup having PF as set of pseudo-Frobenius numbers, containing f_elts and having f_gaps as some of its gaps.
##
## The function consists of a loop that makes use of the functions "FurtherForcedGapsForPseudoFrobenius" and "FurtherForcedElementsForPseudoFrobenius" to discover new forced gaps and new forced elements, respectively.
##
## If it is discovered an integers that simoultaniously had to be a gap and an element, we say that there is a conflict and "fail" is returned. Otherwise, the function returns a pair [forced_gaps, forced_elements].
#################################################################
InstallGlobalFunction(SimpleForcedIntegersForPseudoFrobenius, function(f_gaps,f_elts,PF)
  local  nf_gaps, nf_elts, changes, gaps, elts;
  
  nf_gaps := ShallowCopy(f_gaps);
  nf_elts := ShallowCopy(f_elts);
  repeat
    changes := false;
    gaps := FurtherForcedGapsForPseudoFrobenius(nf_gaps,nf_elts,PF);  
    if gaps = fail then
      return fail;
    elif gaps <> nf_gaps then
      changes := true;
      nf_gaps := gaps;      
    fi;
    elts := FurtherForcedElementsForPseudoFrobenius(nf_gaps,nf_elts,PF);
    if elts = fail then
      return fail;
    elif elts <> nf_elts then
      changes := true;
      nf_elts := elts;      
    fi;
  until not changes;
  return [nf_gaps,nf_elts];
end);
#################################################################
##
#F NonAdmissibleForPseudoFrobenius(f_gaps,f_elts,PF)
##
## determination of new forced gaps based on the concept of admissible integer
##
## input: a pair of sets of forced gaps and forced elements
## output: non-admissible integers, which (by Lemma~\ref{lemma:admissible_ints}) are new forced gaps. 
#################################################################
InstallGlobalFunction(NonAdmissibleForPseudoFrobenius, function(f_gaps,f_elts,PF)
  local  frob, admissible, totest, v, pnf_ce, non_admissible;
  frob := Maximum(PF); 
  admissible := [];
  totest := Difference([1..frob], Union(f_gaps,f_elts));
  while totest <> [] do
    v := totest[1];
    pnf_ce:= SimpleForcedIntegersForPseudoFrobenius(f_gaps,Union(f_elts,[v]),PF);
    if pnf_ce <> fail then
      admissible := Union(admissible,pnf_ce[2]);
      totest := Difference(totest,admissible);
    else
      totest := Difference(totest,[v]);
    fi;
  od;
  non_admissible := Difference([1..frob], admissible);
  Info(InfoTipo,2,"Non admissible integers\n",non_admissible,"\n"); 
  return non_admissible; 
end);
#################################################################
##
## Let PF={g_1<...<g_n} be a set of integers such that there exists a numerical semigroup S such that PF(S)=PF.
## The aim of this function is to compute forced integers (gaps or elements) for any semigroup S such that PF(S)=PF.
##
##
#F ForcedIntegersForPseudoFrobenius(PF)     
###
# Input: 
#  * one list of integers: [g_1,...,g_n] 
# or
#  * integers: g_1,...,g_n
#
###
# Output: 
## * in case there exists a numerical semigroup S such that PF(S)=PF:
##  - a list [forced_gaps,forced_elts] such that: 
##   -- forced_gaps is contained in N\S for any numerical semigroup S such that PF(S)={g_1,...,g_n}
##   -- forced_elts is contained in S for any numerical semigroup S such that PF(S)={g_1,...,g_n}
## * "fail" in case it is found some condition that fails
#################################################################
InstallGlobalFunction(ForcedIntegersForPseudoFrobenius, function(arg)
  local  PF, type, frob, sfg, f_ints, n_ad, new_gaps;

  ## arguments
  if Length(arg) = 1 and IsList(arg[1]) then
    PF := Set(arg[1]); 
  else 
    PF := Set(arg);
  fi;
  ##
  type := Length(PF);
  frob := Maximum(PF);
  ##
  if type = 1 then 
    if IsEvenInt(frob) then # frob/2 is also a pseudo-Frobenius number, thus the type can not be 1.
      return [];
    fi;
    return [DivisorsInt(frob),[0,frob+1]];
  fi;

  sfg := StartingForcedGapsForPseudoFrobenius(PF);    
  f_ints := SimpleForcedIntegersForPseudoFrobenius(sfg,[],PF);
  if f_ints = fail then
    return fail;
  elif IsRange(Union(f_ints)) then
    #a test (motivated by a problem found by Ojeda)
    if PseudoFrobeniusOfNumericalSemigroup(NumericalSemigroupByGaps(f_ints[1])) <> PF then
      Error("There is a problem in ForcedIntegersForPseudoFrobenius. Please communicate the input to the numericalsgps package authors");
    fi;    
    return f_ints;
  fi;
  n_ad := NonAdmissibleForPseudoFrobenius(f_ints[1],f_ints[2],PF);  
  new_gaps := Difference(n_ad,f_ints[1]);
  Info(InfoTipo,2,"extra forced gaps\n",new_gaps,"\n");
  return SimpleForcedIntegersForPseudoFrobenius(Union(new_gaps,f_ints[1]),f_ints[2],PF);
end);
#############################################
## a quick version
InstallGlobalFunction(ForcedIntegersForPseudoFrobenius_QV, function(arg)
  local  PF, type, frob, sfg;
  ## arguments
  if Length(arg) = 1 and IsList(arg[1]) then
    PF := Set(arg[1]); 
  else 
    PF := Set(arg);
  fi;
  ##
  type := Length(PF);
  frob := Maximum(PF);
  ##
  if type = 1 then 
    return [DivisorsInt(frob),[0,frob+1]];
  fi;
  ##
  sfg := StartingForcedGapsForPseudoFrobenius(PF);
  return SimpleForcedIntegersForPseudoFrobenius(sfg,[],PF);
end);
#################################################################
#################################################################
#################################################################
##
#F NumericalSemigroupsWithPseudoFrobeniusNumbers(PF)
## Input: PF (a set of postive integers)
## Ouput: a list of numerical semigrups S such that PF(S)=PF
## When Length(PF)=1, it makes use of the function NumericalSemigroupsWithFrobeniusNumber
##
## The option draw_tree may be useful, since it gives an output that gives some light on the way the computations are done
##
## example: NumericalSemigroupsWithPseudoFrobeniusNumbers(rec(pseudo_frobenius := pf, draw_tree := true));
#################################################################
InstallGlobalFunction(NumericalSemigroupsWithPseudoFrobeniusNumbers, function(arg)
  local  PF, type, frob, initially_forced_integers, 
         freeElementsTree_recursive, semigroups, fg, fe;

  ## arguments
  if Length(arg) = 1 and IsList(arg[1]) then
    PF := Set(arg[1]);   
  else
    PF := Set(arg);
  fi;
  ##
  type := Length(PF);
  frob := Maximum(PF);
  if type = 1 then
    if IsEvenInt(PF[1]) then # PF[1]/2 is also a pseudo-Frobenius number, thus the type can not be 1.
      return [];
    fi;
    Info(InfoTipo,2, "As the type is 1, the function IrreducibleNumericalSemigroupsWithFrobeniusNumber will be used");    
    return IrreducibleNumericalSemigroupsWithFrobeniusNumber(frob);
  elif type = 2 and 2*PF[1] = PF[2] then
    Info(InfoTipo,2, "As the type is 2 and frob is even, the function IrreducibleNumericalSemigroupsWithFrobeniusNumber will be used");
    return IrreducibleNumericalSemigroupsWithFrobeniusNumber(frob);
  fi;
  
  ## testing some simple conditions
  if PF[type] - PF[type-1] > PF[1] then
    return [];
  fi;
  initially_forced_integers := ForcedIntegersForPseudoFrobenius(PF);
  if initially_forced_integers = fail then 
    return [];
  fi;

   ##################### local recursive function #####################
  # rightmost indicates if the visiting node is the rightmost of the current free integers
  freeElementsTree_recursive :=  function(fg,fe)
    local  forced_integers, free, ending_condition, nfg, current_free, v, 
           left, right;
 
    forced_integers := Union(fg,fe);
    free := Difference([1..frob], forced_integers);
 
    ########################## local function ##########################
    ## Ending condition ##
    ## The number of free elements must be <= 1
    ending_condition := function(g,e)
      local  forced_integers, free;
 #     Error("..");
      
      forced_integers := Union(g,e);
      free := Difference([1..frob], forced_integers);
      
      if Length(free) = 0 then 
        #g are the gaps 
        #e are the small elements
         if First(Difference(g,PF), pf -> Intersection(pf + Difference(e,[0]),g) = []) = fail then
           Add(semigroups, NumericalSemigroupByGaps(g));
         fi;
         return;
       fi;
       if  Length(free) = 1 then
         if RepresentsGapsOfNumericalSemigroup(g) then
           #g are the gaps 
           #eUfree are the small elements
           if First(Difference(g,PF), pf -> Intersection(pf + Difference(Union(e,free),[0]),g) = []) = fail then
             Add(semigroups, NumericalSemigroupByGaps(g));
           fi;
         fi;
         if RepresentsGapsOfNumericalSemigroup(Union(g,free)) then
           #gUfree are the gaps 
           #e are the small elements
           if First(Difference(Union(g,free),PF), pf -> Intersection(pf + Difference(e,[0]),Union(g,free)) = []) = fail then
             Add(semigroups, NumericalSemigroupByGaps(Union(g,free)));
           fi;
         fi;
         return;
       fi;
     end;
     ### end of local function ######     
    ##
     if Length(free) <= 1 then
       ending_condition(fg,fe);
     fi;
     
    nfg := ShallowCopy(fg); #used to store gaps...
    current_free := ShallowCopy(free);
    
    ## the cycle...
    while Length(current_free) > 1 do
      v := current_free[1];
      left := SimpleForcedIntegersForPseudoFrobenius(nfg,Union(fe,[v]),PF);
      if left = fail then
        right := SimpleForcedIntegersForPseudoFrobenius(Union(nfg,[v]),fe,PF);
        if (right = fail) or (Intersection(right[1],right[2]) <> []) then
          break;     
        fi;
      else
        freeElementsTree_recursive(left[1],left[2]);
      fi;
      nfg := Union(nfg,[v]);
      current_free := Difference(current_free,[v]);
    od;
    if Length(current_free) = 1 then
       ending_condition(nfg,fe);
    fi;
       
  end;
  ########### end of local recursive function #####
  semigroups := [];
  fg := initially_forced_integers[1];
  fe := initially_forced_integers[2];
  
  freeElementsTree_recursive(fg,fe);
  return semigroups;
end);
#################################################################
##
#F ANumericalSemigroupWithPseudoFrobeniusNumbers(arg)
## Input: PF (a set of postive integers) 
# Alternativelly
#  * a record with fields "pseudo_frobenius" and "max_attempts" option
##
## ouput: A numerical semigrups S such that PF(S)=PF. Returns fail if it conludes that it does not exist and suggests to use NumericalSemigroupsWithPseudoFrobeniusNumbers if it is not able to conclude...
## When Length(PF)=1 or Length(PF)=2 and 2*PF[1] = PF[2], it makes use of the function AnIrreducibleNumericalSemigroupWithFrobeniusNumber
#################################################################
InstallGlobalFunction(ANumericalSemigroupWithPseudoFrobeniusNumbers, function(arg)
  local  m_att, PF, type, frob, of_ints, free, nspfn, i, f_ints, v, nfig, 
         nfie;
  
    ## check arguments
  m_att := fail;
  if Length(arg) = 1 then
    if IsRecord(arg[1]) then
      if IsBound(arg[1].pseudo_frobenius) then 
        PF := Set(arg[1].pseudo_frobenius);
      fi;
      if IsBound(arg[1].max_attempts) then 
        m_att := arg[1].max_attempts;
      fi;
    elif IsInt(arg[1]) then
      PF := arg;
    else
      PF := Set(arg[1]);
    fi;   
  elif Length(arg) > 1 then 
    PF := Set(arg);
  else
    Error("please check the arguments...");
  fi; 
  type := Length(PF);
  frob := Maximum(PF);
  if (type = 1) or (type = 2 and 2*PF[1] = PF[2]) then
    Info(InfoTipo,1, "As the type is 1 (or 2 and frob is even), the function AnIrreducibleNumericalSemigroupWithFrobeniusNumber will be used");    
    return AnIrreducibleNumericalSemigroupWithFrobeniusNumber(frob);
  fi;

  ## testing some simple conditions
  if PF[type] - PF[type-1] > PF[1] then
    return fail;
  fi;
  
  #maximum number of attempts
  if m_att = fail then
    m_att := Minimum(7,Int(frob/3)); #many experiments suggest that it is a reasonable number
  fi;
  
  # forced integers
  of_ints := ForcedIntegersForPseudoFrobenius(PF);

  if of_ints = fail then 
    return fail;
  fi;
  
  free := Difference([1..frob],Union(of_ints));
  if Length(free) < Minimum(10,Int(frob/5)) then #NumericalSemigroupsWithPseudoFrobeniusNumbers is reasonably fast...
    nspfn := NumericalSemigroupsWithPseudoFrobeniusNumbers(PF);
    if nspfn = [] then
      return fail;
    else
      return RandomList(nspfn);
    fi;
  fi;
  for i in [1..m_att] do
    f_ints := ShallowCopy(of_ints);
    free := Difference([1..frob],Union(f_ints));
    while free <> [] do
      v := RandomList(free);
      nfig := SimpleForcedIntegersForPseudoFrobenius(Union(f_ints[1],[v]),f_ints[2],PF);
      nfie := SimpleForcedIntegersForPseudoFrobenius(f_ints[1],Union(f_ints[2],[v]),PF);
      if nfig <> fail then
        if IsRange(Union(nfig)) then
          return NumericalSemigroupByGaps(nfig[1]);
        fi;
        f_ints := nfig;
        free := Difference([1..frob],Union(f_ints));
      elif nfie <> fail then
          if IsRange(Union(nfie)) then
            return NumericalSemigroupByGaps(nfie[1]);
          fi;
          f_ints := nfie;
          free := Difference([1..frob],Union(f_ints));       
      else
        break;
      fi;
    Info(InfoTipo,1,"Length free: ",Length(free),"\n");
    od;
    Info(InfoTipo,1,"Attempt ",i,"\n");
  od;
  Info(InfoWarning,1,"I have not been able to determine a semigroup satisfying the required conditions. Please increase the number of maximum attempts or use the function NumericalSemigroupsWithPseudoFrobeniusNumbers instead...");
  return fail;
  
end);

