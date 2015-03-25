#############################################################################
##
#W  pseudoFrobenius.gi          Manuel Delgado <mdelgado@fc.up.pt>
#W                              Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright .........
#############################################################################
################################################################
## The main function in this file is
#F NumericalSemigroupsWithPseudoFrobeniusNumbers
## Input: PF (a set of postive integers)
## ouput: a list of numerical semigrups S such that PF(S)=PF
#################################################################
##
## Info classes have been created
##
DeclareInfoClass("InfoTipo");
DeclareInfoClass("InfoTree");
#############################################
##
########
## Several auxiliary functions are included
#############################################
###################################################
#F LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(elts,frob)
##
## Returns the least numerical semigroup containing the list elts of elements and having the largest possible Frobenius number not greater than frob.
## This is just an "abreviation" of  NumericalSemigroup(Union(elts,[frob+1..frob+First(elts,IsPosInt)])) that is intended to turn the code mor readable.
##
#############################################################
InstallGlobalFunction(LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber, function(elts,frob)
  local  ns;
  if elts <> [] then
    ns := NumericalSemigroup(Union(elts,[frob+1..frob+First(elts,IsPosInt)]));
  else
    ns := NumericalSemigroup([frob+1..2*(frob+1)]);
  fi;
  return ns;
end);
##
###################################################
###################################################
#F GapsOfNumericalSemigroupForcedByGapsAndElements(f_gaps,elts)
## 
## Returns a list of integers that must be gaps of any numerical semigroup containing elts and for which it is known that f_gaps are gaps
## Returns fail in case it finds an element that had to be a gap, which implies that no semigroup exists having the given sets of gaps and elements
##
## Justification: note that if f is a gap and e is an element, then f-e is a gap. (Otherwise f=(f-e)+e would be an element)
##
InstallGlobalFunction(GapsOfNumericalSemigroupForcedByGapsAndElements, function(f_gaps,elts)
  local  ng, e, f;

  ng := Union(List(f_gaps, g -> g - elts));
  ng := Filtered(ng,IsPosInt);
  ng := Union(List(ng,DivisorsInt));

  if Intersection(elts,ng) = [] then   
    return ng;
  else
    return fail;
  fi;
end);  
###################################################
###################################################
##
#F SomeConditionsForPseudoFrobenius(arg)
##
## Test whether some necessary conditions for the given set PF to be the set of pseudo-Frobenius numbers of a numerical semigroup are fulfilled.
InstallGlobalFunction(SomeConditionsForPseudoFrobenius, function(arg)
  local  PF, type, mult, diff;

  ## check arguments
  if Length(arg) = 1 then
    if IsRecord(arg[1]) then
      if IsBound(arg[1].pseudo_frobenius) then 
        PF := Set(arg[1].pseudo_frobenius);
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
  ##
  mult := 0;

  if Length(arg) = 1 then
    if IsRecord(arg[1]) then
      if IsBound(arg[1].mult_candidate) then 
        mult := arg[1].mult_candidate;
      fi;
    fi;
  fi;
  mult := Maximum(mult,type+1);

  if type = 1 then
    return true;
  fi;
  diff := PF[type] - PF[type-1];  
  if diff > PF[1] then
    return false; #justification: cor:naive_condition_g1
  fi;
  
  ### wrong!! it aparenty has to do with the fact that mult may not be the multiplicity
  # if PF[1] - diff <> 0 then
  #   if PF[1] - diff < mult then 
  #     return false; #justification: cor:naive_condition_multiplicity
  #   fi;
  # else
  #   if PF[2] - diff < mult then
  #     return false;
  #   fi;
  # fi;
  return true;
end);
###################################################
###################################################
##
#F ConditionsForPseudoFrobeniusBasedOnForcedIntegers(arg)
##
## Test whether some necessary conditions for the given set PF to be the set of pseudo-Frobenius numbers of a numerical semigroup are fulfilled. 
## The input is a record with fields "forced_elts" and "forced_gaps" and "pseudo_frobenius". 
##
## Optionally, the option "quick" (which is "true" or "false") may be given.
## In case the conditions tested are not fulfilled, returns fail.
## 
InstallGlobalFunction(ConditionsForPseudoFrobeniusBasedOnForcedIntegers, function(arg)
  local  f_g, f_e, PF, ifg, frob, free, f, fe0, es, opt_quick, x, filt;

  f_g := arg[1].forced_gaps;
  f_e := arg[1].forced_elts;
  PF := arg[1].pseudo_frobenius;
  ##

  ## gaps can not be elements of the semigroup 
  if Intersection(f_g, f_e) <> [] then
    Info(InfoTipo,2,"There is no numerical semigroup with the given set as set of pseudo-Frobenius numbers and given forced integers, since\n", Intersection(f_g, f_e),"\n are forced gaps that are at the same time forced elements\n");
    return fail;
  fi;
  ##
  frob := Maximum(PF);
  free := Difference([1..frob],Union(f_g,f_e));

  # test whether there is a gap (not in PF) that would have to be a pseudo-Frobenius number of the numerical semigroup
  fe0 := Difference(f_e,[0]);
  es := Union(free,fe0); # a set containing all the small elements (except 0)
  #
  ifg := Difference(f_g,PF);  #can this be improved by using the obviously forced??
  #
  for f in ifg do    
    if ForAll(f+es,e -> not(e in f_g)) then       
      Info(InfoTipo,2,"There is no numerical semigroup with the given set as set of pseudo-Frobenius numbers and given forced integers, since ",f," would have to be a pseudo-Frobenius number of that numerical semigroup.\n");
      return fail;
    fi;
  od;
  ###################################################  
  opt_quick := true;
  if IsBound(arg[1].quick) then 
    opt_quick := arg[1].quick;
  fi;
  if opt_quick then 
    return true;
  fi;
  ###################################################
  ## the not so quick version
  ###################################################
  ## Another test. 
  ## Warning: the time used in this test is not negligible
  #
  for x in ifg do
    filt := List(PF-x, IsPosInt);
    ## uses: if not($f-x\not \in S$ for all $f\in PF(S))$ then $x \in S$
    if First(filt, s -> not(s in f_g)) <> fail then #contradiction: there exists x \in S such that x\in G(S)
      Info(InfoTipo,2,"There is no numerical semigroup with the given set as set of pseudo-Frobenius numbers and given forced integers, since the difference between ",x," and any element of PF is either a forced gap or negative (thus can not belong to that numerical semigroup).\n");
      return fail;
    fi;
  od;
  return true;
end);
###################################################
## Some auxiliary functions to be called by the functions below
###################################################
##
#F StartingForcedGaps(PF)
##
InstallGlobalFunction(StartingForcedGaps, function(PF)  
  local  type, forced_gaps, closures, differences, i;

  type := Length(PF);  
  forced_gaps := Union([1..Length(PF)],PF); # uses the fact m(S)>=t(S)+1
  ## justification: lemma:pseudo-comb-pseudo
  closures := List([1..type-1], i->SmallElementsOfNumericalSemigroup(LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(PF{[1..i]},PF[i+1])));
  differences := [];
  for i in [2..type] do
    differences := Union(differences,Filtered(PF[i] - closures[i-1],IsPosInt));
  od; 
  ##
  forced_gaps := Union(forced_gaps,differences);
  ##### add divisors
  forced_gaps := Union(List(forced_gaps,DivisorsInt));
  Info(InfoTipo,2,"starting forced gaps:\n",forced_gaps,"\n");
  return forced_gaps;
end);

#######################################
##
#F NewBigElements
##
## returns a list of NEW elements forced by the small gaps
#
# the jusification is 
# if m is the multiplicity and 1 <= i < m, then frob-i + m \in S. So, either frob-i \in  S or frob-i is pseudo-frobenius.
#
# note the usage of the fact that "PF" is exactly the set of pseudo-frobenius numbers
InstallGlobalFunction(NewBigElements, function(f_gaps,f_elts,PF)
  local  frob, m, nf_elts;
  frob := Maximum(PF);   
  m := First(Integers,n-> IsPosInt(n) and not(n in f_gaps)); #least integer that is not a forced gap
  nf_elts := Difference(frob - [1..m-1], PF); ## "PF" is exactly the set of pseudo-frobenius numbers
  return Difference(nf_elts,f_elts);
end);
######################################
##
#F NewElementsByExclusion(f_gaps,f_elts,PF)
##
## returns a list of NEW forced elements
##
# the ingredient used in this function to obtain new forced elements is
#$x\in Z\S$ if and only if $f-x\in S$ for some $f\in PF(S)$
# or equivalently
#$x \in S$ iff not($f-x\not \in S$ for all $f\in PF(S))$
## note the usage of the fact that "PF" is exactly the set of pseudo-frobenius numbers
##
InstallGlobalFunction(NewElementsByExclusion, function(f_gaps,f_elts,PF)
  local  frob, nf_elts, x, filt, candidates;

  frob := Maximum(PF);   
  nf_elts := [];
  for x in f_gaps do
    filt := Filtered(PF, f -> not((f-x in f_gaps) or (f-x < 0)));
    if Length(filt) = 1 and not(filt[1]-x in nf_elts) then
      AddSet(nf_elts, filt[1]-x);
    fi;
  od;
  candidates := Difference([1..frob-1],Union(f_gaps,Union(f_elts,nf_elts)));
  for x in candidates do 
    if Filtered(Difference(PF-x,f_gaps),IsPosInt) = [] then
      AddSet(nf_elts, x);
    fi;
  od;
  return Difference(nf_elts,f_elts);
end);
######################################
##
#F SimpleForcedIntegersForPseudoFrobenius(f_gaps,f_elts,PF)
##
## consists of a loop that makes use of the functions "NewBigElements" and "NewElementsByExclusion" to discover new elements. When some are discovered, then GapsOfNumericalSemigroupForcedByGapsAndElements is also used to possibly discover new gaps.
##
## Returns a pair [forced_gaps, forced_elements] or "fail", when it finds to be not possible for a numerical semigroup having PF as set of pseudo-Frobenius numbers to have f_gaps as gaps and to contain f_elts.
InstallGlobalFunction(SimpleForcedIntegersForPseudoFrobenius, function(f_gaps,f_elts,PF)
  local  frob, iteration, f_g, closure, f_e, changes_excl, changes_big, 
         possibly_new_elts, changes;
  frob := Maximum(PF); 
  iteration := 0; # a counter used exclusively to provide information
  f_g := Union(List(f_gaps,DivisorsInt));
  closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(f_elts,frob);
  f_e := SmallElementsOfNumericalSemigroup(closure);
  f_e := Union(f_e,[Maximum(f_e)..frob+1]);
  f_g := GapsOfNumericalSemigroupForcedByGapsAndElements(f_g,f_e);
  if f_g = fail then
    return fail;
  fi;
  Info(InfoTipo,2,"gaps forced... (iteration ",iteration,"):\n",f_g,"\n");
  repeat
    ## a quick test
    if Intersection(f_g,f_e) <> [] then
      return fail;
    fi;
    ##
    changes_excl := false;
    changes_big := false;
    iteration := iteration + 1; 
    ##
    ############ elements vs gaps forced by NewElementsByExclusion #############
    # elements
    possibly_new_elts := NewElementsByExclusion(f_g,f_e,PF);
    if possibly_new_elts <> [] then
      changes_excl := true;
      closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(f_e,possibly_new_elts),frob);
      f_e := SmallElementsOfNumericalSemigroup(closure);
      f_e := Union(f_e,[Maximum(f_e)..frob+1]);
      Info(InfoTipo,2,"forced elements by NewElementsByExclusion (iteration ",iteration,"):\n",f_e,"\n");
      ##
      #gaps
      f_g := GapsOfNumericalSemigroupForcedByGapsAndElements(f_g,f_e);
      if f_g = fail then
        return fail;
      fi;
      Info(InfoTipo,2,"gaps forced by NewElementsByExclusion (iteration ",iteration,"):\n",f_g,"\n");
    fi;
    ##

    ############ big_elg elements versus small gaps #############
    # elements
    possibly_new_elts := NewBigElements(f_g,f_e,PF);
    if possibly_new_elts <> [] then
      changes_big := true;
      closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(f_e,possibly_new_elts),frob);
      f_e := SmallElementsOfNumericalSemigroup(closure);
      f_e := Union(f_e,[Maximum(f_e)..frob+1]);
      Info(InfoTipo,2,"forced big elements (iteration ",iteration,"):\n",f_e,"\n");
      ##
      #gaps
      f_g := GapsOfNumericalSemigroupForcedByGapsAndElements(f_g,f_e);
      if f_g = fail then
        return fail;
      fi;
      Info(InfoTipo,2,"gaps forced by big elements (iteration ",iteration,"):\n",f_g,"\n");
    fi;
    changes := changes_excl or changes_big;      
  until not changes;
  ##
  if ConditionsForPseudoFrobeniusBasedOnForcedIntegers(rec(pseudo_frobenius := PF,forced_gaps := f_g,forced_elts := f_e)) = fail then
    return fail;
  fi;
  return [f_g,Intersection(f_e,[0..frob+1])];
end);
###################################################
###################################################
##
## Let PF={g_1<...<g_n} be a set of integers such that there exists a numerical semigroup S such that PF(S)=PF.
## The aim of this function is to compute forced integers (gaps or elements) for any semigroup S such that  PF(S)=PF.
##
#F ForcedIntegersForPseudoFrobenius
##
########
# Input: 
#  * one list of integers: [g_1,...,g_n] 
#or
#  * integers: g_1,...,g_n
#
###
# Output: 
## * in case there exists a numerical semigroup S such that PF(S)=PF:
##  - a list [forced_gaps,forced_elts] such that: 
##   -- forced_gaps is contained in N\S for any numerical semigroup S such that PF(S)={g_1,...,g_n}
##   -- forced_elts is contained in S for any numerical semigroup S such that PF(S)={g_1,...,g_n}
## * "fail" in case it is found some condition that fails
#############################################
## admissible
## determination of forced gaps based on the concept of admissible integer
InstallGlobalFunction(ForcedIntegersForPseudoFrobenius, function(arg)
  local  PF, type, frob, non_admissible, f_ints, n_ad, new_gaps;
  ## arguments
  if Length(arg) = 1 then
    PF := Set(arg[1]);   
  elif Length(arg) > 1 then 
    PF := Set(arg);
  fi;
  ##
  type := Length(PF);
  frob := Maximum(PF);
  ##
  if type = 1 then 
    return [DivisorsInt(frob),[0,frob+1]];
  fi;
  ####################### local function ############
  ############## non admissible  ####################
  non_admissible := function(f_g,f_e)
    local  admissible, totest, v, pnf_ce;

    admissible := [];
    totest := Difference([1..frob], Union(f_g,f_e));
    while totest <> [] do
      v := totest[1];
      pnf_ce:= SimpleForcedIntegersForPseudoFrobenius(f_g,Union(f_e,[v]),PF);
      if pnf_ce <> fail then
#        Error("--");
              
        admissible := Union(admissible,pnf_ce[2]);
        totest := Difference(totest,admissible);
      else
        totest := Difference(totest,[v]);
      fi;
    od;
    return Difference([1..frob], admissible); 
  end;
  ################ end of local function ###########
  ##################
    f_ints := SimpleForcedIntegersForPseudoFrobenius(StartingForcedGaps(PF),[],PF);
    if f_ints = fail then
      return fail;
    elif IsRange(Union(f_ints)) then
      return f_ints;
    fi;
    n_ad := non_admissible(f_ints[1],f_ints[2]);   
    new_gaps := Difference(n_ad,f_ints[1]);
    Info(InfoTipo,1,"extra forced gaps\n",new_gaps,"\n");
    return SimpleForcedIntegersForPseudoFrobenius(Union(new_gaps,f_ints[1]),f_ints[2],PF);
end);
#############################################
## a quick version
InstallGlobalFunction(ForcedIntegersForPseudoFrobenius_QV, function(arg)
  local  PF, type, frob, non_admissible, f_ints, n_ad, new_gaps;
  ## arguments
  if Length(arg) = 1 then
    PF := Set(arg[1]);   
  elif Length(arg) > 1 then 
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
  return SimpleForcedIntegersForPseudoFrobenius(StartingForcedGaps(PF),[],PF);
end);

#############################################
## The original version is slower...
##
InstallGlobalFunction(ForcedIntegersForPseudoFrobenius_original, function(arg)
  local  PF, type, frob, forced_gaps, aux, aux2, fe_aux, big_g, g, closure, 
         forced_elts, intersec, gfbyge, special_forced_elts, 
         extra_forced_gaps, f_gaps, f_elts, changes_special, changes_non_elt, 
         forced_integers, sfe, efg;

  ## arguments
  if Length(arg) = 1 then
    PF := Set(arg[1]);   
  elif Length(arg) > 1 then 
    PF := Set(arg);
  fi;
  ##
  type := Length(PF);
  frob := Maximum(PF);
  ##
  if type = 1 then 
    return [DivisorsInt(frob),[0,frob+1]];
  fi;
  ##################
  forced_gaps := StartingForcedGaps(PF);
  ##################
  # elements forced by:
  # * being in the semigroup generated by PF
  # * big gaps (note that PF-g must contain an element of the semigroup)
  # * small gaps (uses the function new_big_elements)
  ##
  aux := SmallElementsOfNumericalSemigroup(LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(PF,frob));
  aux2 := Union(aux,[Maximum(aux)..frob]);
  fe_aux := Difference(aux2,PF);
  Info(InfoTipo,2,"closure of pseudo-Frobenius forced elements:\n",fe_aux,"\n");
  ### note that type > 1, since the case type = 1 has been treated
  big_g := Intersection(forced_gaps,[PF[type-1]+1..frob-1]);
  for g in big_g do
    AddSet(fe_aux,frob-g);
  od;
  Info(InfoTipo,2,"elements forced by big gaps:\n",fe_aux,"\n");
  ##
  closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(fe_aux,NewBigElements(forced_gaps,fe_aux,PF)),frob);
  forced_elts := SmallElementsOfNumericalSemigroup(closure);
  forced_elts := Union(forced_elts,[Maximum(forced_elts)..frob+1]);
  Info(InfoTipo,2,"elements forced by \"obvious\" gaps\n",forced_elts,"\n");
  ## a quick test
  intersec := Intersection(forced_gaps, forced_elts);
  if intersec <> [] then
    Info(InfoTipo,2,"The integers ",intersec," had to be elements and gaps at the same time\n");
    return fail;
  fi;
  ##################
  gfbyge := GapsOfNumericalSemigroupForcedByGapsAndElements(forced_gaps,forced_elts);
  if gfbyge = fail then 
    return fail;
  else
    forced_gaps := Union(forced_gaps,gfbyge);
    Info(InfoTipo,2,"gaps forced by initial forced elements:\n",forced_gaps,"\n");
  fi;
  ##################

  ###################################################
  ####################### local functions ###########
  ###################################################

  #########################################################
  ############ Extra elements and gaps  #################
  #########################################################
  ## Some special situations that may give rise to extra forced elements or gaps
  ##
  ## As the analysis of these situations requires some time, it shall be avoided in recursive functions
  #######################################################
  ############## special reasons ####################
  ##
  ## A special situation may occur when the first integer that is not a forced gap is isolated. In this case we consider the two possible cases (unless the first integer that is not a forced gap is a forced element (in which case it is the multiplicity)) for the first integer that is not a forced gap apply both of the above procedures:
  ### * case 1. assume that the first "non forced gap" is an element. New forced elements would then be obtained by closure.
  ### * case 2. assume that the first "non forced gap" is in fact a gap.

  special_forced_elts := function(f_ints)
    local  nf_elts, f_g, f_e, fng, possibly_new_elts, closure, f_e_excl_aux, 
           f_e_big_aux;

    nf_elts := [];
    f_g := f_ints[1];
    f_e := f_ints[2];
    fng := First(Integers,n-> IsPosInt(n) and not(n in f_g)); #first integer that is not a forced gap

    if not(fng in f_e) and (fng+1 in f_g) then # the first integer that is not a forced gap is isolated
      possibly_new_elts := NewElementsByExclusion(f_g,Union(f_e,[fng]),PF);
      # case 1.
      if possibly_new_elts <> [] then
        closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(f_e,possibly_new_elts),frob);
        f_e_excl_aux := SmallElementsOfNumericalSemigroup(closure);
      else
        f_e_excl_aux := f_e;        
      fi;
      # case 2.
      possibly_new_elts := NewBigElements(Union(f_g,[fng]),f_e,PF);
      if possibly_new_elts <> [] then
        closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(f_e,possibly_new_elts),frob);
        f_e_big_aux := SmallElementsOfNumericalSemigroup(closure);
      else
        f_e_big_aux := f_e;        
      fi;
      nf_elts := Difference(Intersection(f_e_excl_aux,f_e_big_aux),f_e);   
      #      Error("..");

      Info(InfoTipo,2,"elements forced by special reasons \n",nf_elts,"\n");
           Print("elements forced by special reasons \n",nf_elts,"\n");
    fi;

    return nf_elts;
  end;
  ######################
  ######################

  extra_forced_gaps := function(f_ints)
    local  nf_gaps, f_g, f_e, free, u, closure, aux, sfree, v, left;

    nf_gaps := [];
    f_g := f_ints[1];
    f_e := f_ints[2];

    free := Difference([1..frob], Union(f_ints));
    if free = [] then
      return nf_gaps;
    fi;

    sfree := ShallowCopy(free);
    for v in free do
      if v in sfree then
        left := SimpleForcedIntegersForPseudoFrobenius(f_g,Union(f_e,[v]),PF);
        if left = fail then
          Info(InfoTipo,2,v, " can not be an element of any semigoup S with PF(S)=PF, thus it is a forced gap","\n");
          nf_gaps := Union(nf_gaps,DivisorsInt(v));
        else
          closure := LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(Union(f_e,[v]),frob);
          aux := SmallElementsOfNumericalSemigroup(closure);
          sfree := Difference(sfree,aux); # if v is admissible, then any element in the semigroup containing v and the forced elements is admissible (thus does not have to be tested)
        fi;
      fi;
    od;       
      Info(InfoTipo,2,"extra forced gaps\n",nf_gaps,"\n");
           Print("extra forced gaps \n",nf_gaps,"\n");
    return nf_gaps;
  end;
  ####################################################
  ################# end of local functions ###########
  ####################################################
  f_gaps := forced_gaps;
  f_elts := forced_elts;
  #########################################################
  ############ Further elements and gaps  #################
  #########################################################
  ############ a loop...
  #########################################################
  repeat
    changes_special := false;
    changes_non_elt := false;
    #    Error("..");

    forced_integers := SimpleForcedIntegersForPseudoFrobenius(f_gaps,f_elts,PF);
    if forced_integers = fail then
      return fail;
    fi;
    f_gaps := forced_integers[1];
    f_elts := forced_integers[2];

    sfe := special_forced_elts([f_gaps,f_elts]);
    if sfe <> [] then
      changes_special := true;
      f_elts := Union(sfe,f_elts);
    fi;
    efg := extra_forced_gaps([f_gaps,f_elts]);
    if efg <> [] then
      changes_non_elt := true;
      f_gaps := Union(efg,f_gaps);
    fi;
  until not (changes_non_elt or changes_special);
  if ConditionsForPseudoFrobeniusBasedOnForcedIntegers(rec(pseudo_frobenius := PF,forced_gaps := f_gaps,forced_elts := f_elts)) = fail then
    Info(InfoTipo,1,"The conditions are not fulfilled... More than simple checks are needed\n");
    return fail;
  fi;
  return [f_gaps,f_elts];
end);

###################################################
###################################################
##
#F NumericalSemigroupsWithPseudoFrobeniusNumbers(PF)
## Input: PF (a set of postive integers)
## Ouput: a list of numerical semigrups S such that PF(S)=PF
## When Length(PF)=1, it makes use of the function NumericalSemigroupsWithFrobeniusNumber
##
## The option draw_tree may be useful, since it gives an output that gives some light on the way the computations are done
##
## example: NumericalSemigroupsWithPseudoFrobeniusNumbers(rec(pseudo_frobenius := pf, draw_tree := true));

InstallGlobalFunction(NumericalSemigroupsWithPseudoFrobeniusNumbers, function(arg)
  local  tree, path, nodes_visited, dead_nodes, opt_draw_tree, opt_quick, PF, 
         type, frob, initially_forced_integers, freeElementsTree_recursive, 
         freeElementsTree_recursive_draw_tree, semigroups, fg, fe, 
         forced_integers, initial_free;

  tree := [];
  path := [];
  nodes_visited := 0;   # just to provide some information
  dead_nodes := 0;   
  ## defaults#
  opt_draw_tree := false;
  opt_quick := false;
  ## check arguments
  if Length(arg) = 1 then
    if IsRecord(arg[1]) then
      if IsBound(arg[1].pseudo_frobenius) then 
        PF := Set(arg[1].pseudo_frobenius);
      fi;
      if IsBound(arg[1].quick) then 
        opt_quick := arg[1].quick;
      fi;
      if IsBound(arg[1].draw_tree) then 
        opt_draw_tree := arg[1].draw_tree;
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
  ##
  type := Length(PF);
  frob := Maximum(PF);
  if type = 1 then
    Info(InfoTipo,1, "As the type is 1, the function NumericalSemigroupsWithFrobeniusNumber will be used");    
    return NumericalSemigroupsWithFrobeniusNumber(frob);
  fi;

  ## testing some simple conditions
  if not SomeConditionsForPseudoFrobenius(rec(pseudo_frobenius := PF)) then
    Info(InfoTipo,1,"The initial conditions fail");
    return [];
  fi;

  # forced integers
  if opt_quick then
    initially_forced_integers := SimpleForcedIntegersForPseudoFrobenius(StartingForcedGaps(PF),[],PF);
  else
    initially_forced_integers := ForcedIntegersForPseudoFrobenius(PF);
  fi;
  if initially_forced_integers = fail then 
    Info(InfoTipo,1,"There is some failure when determining the initial forced integers");
    return [];
  fi;

  ########### local recursive function ###########
  # rightmost indicates if the visiting node is the rightmost of the current free integers
  freeElementsTree_recursive :=  function(fg,fe)
    local  forced_integers, free, rightmostfreenode, ending_condition, v, 
           nfg, current_free, left, right;

    forced_integers := Union(fg,fe);
    free := Difference([1..frob], forced_integers);
    Info(InfoTipo,2,"free elements:\n",free,"\n");
#    rightmostfreenode := false;
    ### local function ######
    ## Ending condition ##
    ending_condition := function(g,e)
      if IsRange(Union(g,e)) then
        if First(Difference(g,PF), pf -> Intersection(pf + Difference(e,[0]),g) = []) = fail then
          Add(semigroups, NumericalSemigroupByGaps(g));
          Info(InfoTipo,2,"A new semigroup is found at node ",v, ". Its minimal generating system:\n",MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroupByGaps(g)),"\n");
          nodes_visited := nodes_visited + 1;
          return true;   
        else
          Info(InfoTipo,2,"a semigroup having a different set of pseudo-Frobenius numbers:\n",MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroupByGaps(g)),"\n");
          return false;
        fi;
      fi;
      return false;
    end;
    ### end of local function ######     
    ##
    if IsRange(Union(fg,fe)) then
      v := "?"; #just to provide information that no node is visited      
      ending_condition(fg,fe);
      return;
    fi;
    ##
    nfg := ShallowCopy(fg); #used to store gaps...

    current_free := ShallowCopy(free);
    while Length(current_free) > 1 do
      nodes_visited := nodes_visited + 1;
      v := current_free[1];
      left := SimpleForcedIntegersForPseudoFrobenius(nfg,Union(fe,[v]),PF);
      if left = fail then
        right := SimpleForcedIntegersForPseudoFrobenius(Union(nfg,[v]),fe,PF);
        if (right = fail) or (Intersection(right[1],right[2]) <> []) then
          dead_nodes := dead_nodes + 1;
          break;     
        fi;

      else
        freeElementsTree_recursive(left[1],left[2]);
      fi;
      nfg := Union(nfg,[v]);
      current_free := Difference(current_free,[v]);
    od;
    if Length(current_free) = 1 then
      nodes_visited := nodes_visited + 1;
      v := current_free[1];
      ##
      left := SimpleForcedIntegersForPseudoFrobenius(nfg,Union(fe,[v]),PF);
#      rightmostfreenode := true;
      if not((left = fail) or (Intersection(left[1],left[2]) <> [])) then
        ending_condition(left[1],left[2]);
      fi;
      right := SimpleForcedIntegersForPseudoFrobenius(Union(nfg,[v]),fe,PF);
#      rightmostfreenode := false;
      if not((right = fail) or (Intersection(right[1],right[2]) <> [])) then
        ##        Error("a possible example....");
        ending_condition(right[1],right[2]);#goes to the ending condition
      fi;
    fi;
    Info(InfoTipo,2,"number of sgps already discovered: ",Length(semigroups),", nodes visited: ",nodes_visited, ", number of dead nodes: ",dead_nodes,"\n");
  end;
  ############# the same function than freeElementsTree, but it is to be used when the option to draw the tree is set to true
# rightmost indicates if the visiting node is the rightmost of the current free integers
   freeElementsTree_recursive_draw_tree :=  function(fg,fe)
    local  forced_integers, free, rightmostfreenode, add_leaf, 
           ending_condition_draw, v, nfg, current_free, left, 
           right;

    forced_integers := Union(fg,fe);
    free := Difference([1..frob], forced_integers);
    Info(InfoTipo,2,"free elements:\n",free,"\n");
    rightmostfreenode := false;
    ### local functions ######
    ## an auxiliary function which is called several times when opt_draw_tree is set to true: it is used to add the leafs
    add_leaf := function(leaf)
      local newpath;
      newpath := ShallowCopy(path);        
      Append(path,[leaf]);
      Append(tree, [path]);
      if Length(newpath) >= 1 then
        if rightmostfreenode then
          path := newpath;
        else
          path := newpath{[1..Length(newpath)-1]};
        fi;
      else
        path := [];
      fi;
    end;
    ## Ending condition  when the drawing tree option is set to true##
    ending_condition_draw := function(g,e)
      local f_int;
      f_int := Union(g,e);
      if IsRange(f_int) then
        if First(Difference(g,PF), pf -> Intersection(pf + Difference(e,[0]),g) = []) = fail then
          Add(semigroups, NumericalSemigroupByGaps(g));
          Info(InfoTipo,2,"A new semigroup is found at node ",v, ". Its minimal generating system:\n",MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroupByGaps(g)),"\n");
          add_leaf(NumericalSemigroupByGaps(g));
          nodes_visited := nodes_visited + 1;
          return true;   
        else
          Info(InfoTipo,2,"a semigroup having a different set of pseudo-Frobenius numbers:\n",MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroupByGaps(g)),"\n");
          return false;
        fi;
      fi;
      return false;
    end;

    ### end of local functions ######     
    ##   
    if IsRange(Union(fg,fe)) then
      v := "?"; #just to provide information that no node is visited      
      ending_condition_draw(fg,fe);
      return;
    fi;
    ##
    nfg := ShallowCopy(fg); #used to store gaps...

    current_free := ShallowCopy(free);
    while Length(current_free) > 1 do
      nodes_visited := nodes_visited + 1;
      v := current_free[1];
      Append(path,[v]);#for the drawing of the tree
      left := SimpleForcedIntegersForPseudoFrobenius(nfg,Union(fe,[v]),PF);
      if left = fail then
        Info(InfoTree,1,"There is no semigroup to the left of node ",v, "\n");
        add_leaf("leftnonsgp");#for the drawing of the tree
        right := SimpleForcedIntegersForPseudoFrobenius(Union(nfg,[v]),fe,PF);
        if (right = fail) or (Intersection(right[1],right[2]) <> []) then
          Info(InfoTree,1,"There is no semigroup to the right of node ",v,"\n");
          add_leaf("rightnonsgp");#for the drawing of the tree
          dead_nodes := dead_nodes + 1;
          break;     
        fi;
      else
        freeElementsTree_recursive_draw_tree(left[1],left[2]);
      fi;
      nfg := Union(nfg,[v]);
      current_free := Difference(current_free,[v]);
    od;
    if Length(current_free) = 1 then
      nodes_visited := nodes_visited + 1;
      v := current_free[1];
      Append(path,[v]);#for the drawing of the tree
      ##
      left := SimpleForcedIntegersForPseudoFrobenius(nfg,Union(fe,[v]),PF);
      rightmostfreenode := true;
      if not((left = fail) or (Intersection(left[1],left[2]) <> [])) then
        ending_condition_draw(left[1],left[2]);
      else
        Info(InfoTree,1,"There is no semigroup to the left of node ",v,"\n");
        add_leaf("leftnonsgp");#for the drawing of the tree        
      fi;
      right := SimpleForcedIntegersForPseudoFrobenius(Union(nfg,[v]),fe,PF);
      rightmostfreenode := false;
      if (right = fail) or (Intersection(right[1],right[2]) <> []) then
        Info(InfoTree,1,"There is no semigroup to the right of node ",v,"\n");
        add_leaf("rightnonsgp");#for the drawing of the tree
      else
        ##        Error("a possible example....");
        ending_condition_draw(right[1],right[2]);#goes to the ending condition
      fi;
    fi;
    Info(InfoTipo,2,"number of sgps already discovered: ",Length(semigroups),", nodes visited: ",nodes_visited, ", number of dead nodes: ",dead_nodes,"\n");
    if path <> [] then
      Remove(path,Length(path));
    fi;
  end;

  ########### end of local recursive function #####

  semigroups := [];
  fg := initially_forced_integers[1];
  fe := initially_forced_integers[2];
  forced_integers := Union(fg,fe);
  initial_free := Difference([1..frob], forced_integers);
  Info(InfoTipo,1,"number of free elements: ",Length(initial_free),"\n");

  if IsRange(forced_integers) then #all integers are forced and therefore there is only one numerical semigroup
    # to guarantee that PF is the set of pseudo-Frobenius numbers:
    if First(Difference(fg,PF), pf -> Intersection(pf + Difference(fe,[0]),fg) = []) = fail then
      Add(semigroups, NumericalSemigroupByGaps(fg));
      Info(InfoTipo,2,"a new semigroup\n");
      Info(InfoTipo,3,"minimal generating system: \n",MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroupByGaps(fg)),"\n");
    else
      Info(InfoTipo,2,"It is obtained a semigroup having a different set of pseudo-Frobenius numbers\n");
      return [];
    fi;
  elif opt_draw_tree then
    freeElementsTree_recursive_draw_tree(fg,fe);
  else
    freeElementsTree_recursive(fg,fe);
  fi;
  Info(InfoTipo,1,"number of semigroups: ", Length(semigroups),"\n");
  Info(InfoTipo,1,"number of nodes visited: ", nodes_visited,"\n");
  Info(InfoTipo,3,"Minimal generators of the semigroups obtained by making choices for the free integers:\n",List(semigroups,MinimalGeneratingSystemOfNumericalSemigroup),"\n");
  #### tests ### can be done by the user outside the function...
  # if Difference(Difference([1..frob],Union(initially_forced_integers)),Union(List(semigroups,SmallElementsOfNumericalSemigroup))) <> [] then
  #   Info(InfoTipo,1, Difference(Difference([1..frob],Union(initially_forced_integers)),Union(List(semigroups,SmallElementsOfNumericalSemigroup))), " are gaps of all semigroups, but do not appear as forced gaps\n");
  # fi; 
  ##
  # if First(semigroups,s -> PseudoFrobeniusOfNumericalSemigroup(s)<>PF) <> fail then
  #   Error("the list contains a semigroup whith a non expected set of pseudo-Frobenius numbers\n");
  # fi;
  ####
  if opt_draw_tree then
    return [tree,semigroups];
  fi;
  return semigroups;

end);

###################################################
###################################################
##
#F RandomNumericalSemigroupWithPseudoFrobeniusNumbers(arg)
## Input: PF (a set of postive integers) 
# Alternativelly
#  * a record with fields "pseudo_frobenius" and "max_attempts" option
##
## ouput: A numerical semigrups S such that PF(S)=PF, at random. Returns fail if it conludes that it exists and suggets to use NumericalSemigroupsWithPseudoFrobeniusNumbers if it is not able to conclude...
## When Length(PF)=1, it makes use of the function AnIrreducibleNumericalSemigroupWithFrobeniusNumber

InstallGlobalFunction(RandomNumericalSemigroupWithPseudoFrobeniusNumbers, function(arg)
  local  PF, m_att, type, frob, f_ints, newForcedIntegers, free, i, v, nfig, 
         nfie;

  ## check arguments
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
  if type = 1 then
    Info(InfoTipo,1, "As the type is 1, the function AnIrreducibleNumericalSemigroupWithFrobeniusNumber will be used");    
    return AnIrreducibleNumericalSemigroupWithFrobeniusNumber(frob);
  fi;

  ## testing some simple conditions
  if not SomeConditionsForPseudoFrobenius(rec(pseudo_frobenius := PF)) then
    Info(InfoTipo,1,"The initial conditions fail");
    return [];
  fi;

  m_att := Minimum(7,Int(frob/3)); #many experiments suggest that it is a reasonable number

  # forced integers
  f_ints := ForcedIntegersForPseudoFrobenius(PF);

  if f_ints = fail then 
    return fail;
  fi;
  free := Difference([1..frob],Union(f_ints));
  for i in [1..m_att] do
    while free <> [] do
      v := RandomList(free);
      nfig := SimpleForcedIntegersForPseudoFrobenius(Union(f_ints[1],[v]),f_ints[2],PF);
      if nfig <> fail then
        if IsRange(Union(nfig)) then
          return NumericalSemigroupByGaps(nfig[1]);
        fi;
      fi;
      nfie := SimpleForcedIntegersForPseudoFrobenius(f_ints[1],Union(f_ints[2],[v]),PF);
      if nfie <> fail then
        if IsRange(Union(nfie)) then
          return NumericalSemigroupByGaps(nfie[1]);
        fi;
      fi;
      if nfig <> fail then
        f_ints := nfig;
        free := Difference([1..frob],Union(f_ints));
      elif  nfie <> fail then
        f_ints := nfie;
        free := Difference([1..frob],Union(f_ints));
      fi;
    od;
    Info(InfoTipo,1,"Attempt ",i,"\n");
  od;
  Info(InfoWarning,1,"I have not been able to determine a semigroup satisfying the required conditions. Please increase the number of maximum attempts or use the function NumericalSemigroupsWithPseudoFrobeniusNumbers instead...");

end);
