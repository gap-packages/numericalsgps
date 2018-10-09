#############################################################################
# This file contains obsolet functions which are to be kept during a while for
# compatibility
# WARNING: the manual must be updated before removing the functions
#############################################################################
##
#F  GeneratorsOfNumericalSemigroupNC(S)
##
##  Returns a set of generators of the numerical
##  semigroup S.
##
#############################################################################
# InstallGlobalFunction( GeneratorsOfNumericalSemigroupNC, function(S)
#     if not IsNumericalSemigroup(S) then
#         Error("The argument must be a numerical semigroup");
#     fi;
#     if HasGenerators(S) then
#         return(Generators(S));
#     fi;
#     return(MinimalGeneratingSystemOfNumericalSemigroup(S));
# end);
#############################################################################
##
#F  ReducedSetOfGeneratorsOfNumericalSemigroup(arg)
##
##  Returns a set with possibly fewer generators than those recorded in <C>S!.generators</C>. It changes <C>S!.generators</C> to the set returned.
##The function has 1 to 3 arguments. One of them a numerical semigroup. Then an argument is a boolean (<E>true</E> means that all the elements not belonging to the Apery set with respect to the multiplicity are removed; the default is "false") and another argument is a positive integer <M>n</M> (meaning that generators that can be written as the sum of <n> or less generators are removed; the default is "2"). The boolean or the integer may not be present. If a minimal generating set for <M>S</M> is known or no generating set is known, then the minimal generating system is returned.
##
# InstallGlobalFunction( ReducedSetOfGeneratorsOfNumericalSemigroup, function(arg)
#     local   sumNS,  S,  apery,  n,  generators,  m,  aux,  i,  g,  gen,  ss;

#     #####################################################
#     # Computes the sum of subsets of numerical semigroups
#     # WARNING: the arguments have to be non empty sets, not just lists
#     sumNS := function(S,T)
#         local mm, s, t, R;
#         R := [];
#         mm := Maximum(Maximum(S),Maximum(T));
#         for s in S do
#             for t in T do
#                 if s+t > mm then
#                     break;
#                 else
#                     AddSet(R,s+t);
#                 fi;
#             od;
#         od;
#         return R;
#     end;
#     ##
#     S := First(arg, s -> IsNumericalSemigroup(s));
#     if S = fail then
#         Error("Please check the arguments of ReducedSetOfGeneratorsOfNumericalSemigroup");
#     fi;
#     apery := First(arg, s -> IsBool(s));
#     if apery = fail then
#         apery := false;
#     fi;
#     n := First(arg, s -> IsInt(s));
#     if n = fail then
#         n := 2;
#     fi;

#     if not IsBound(S!.generators) then
#         S!.generators := MinimalGeneratingSystemOfNumericalSemigroup(S);
#         return S!.generators;
#     else
#         if IsBound(S!.minimalgenerators) then
#             #S!.generators := MinimalGeneratingSystemOfNumericalSemigroup(S);
#             return S!.minimalgenerators;
#         fi;
#         generators := S!.generators;
#         m := Minimum(generators); # the multiplicity
#         if m = 1 then
#             S!.generators := [1];
#             return S!.generators;
#         elif m = 2 then
#             S!.generators := [2,First(generators, g -> g mod 2 = 1)];
#             return S!.generators;
#         fi;
#         if apery then
#             aux := [m];
#             for i in [1..m-1] do
#                 g := First(generators, g -> g mod m = i);
#                 if g <> fail then
#                     Append(aux,[g]);
#                 fi;
#             od;
#             gen := Set(aux);
#         else
#             gen := ShallowCopy(generators);
#         fi;
#         ss := sumNS(gen,gen);
#         i := 1;
#         while i < n and ss <> [] do
#             gen :=  Difference(gen,ss);
#             ss := sumNS(gen,ss);
#             i := i+1;
#         od;
#     fi;

#     S!.generators := gen;
#     return S!.generators;
# end);

##
#############################################################################


