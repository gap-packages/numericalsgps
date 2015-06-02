#############################################################################
##
#W  arf-med.gd              Manuel Delgado <mdelgado@fc.up.pt>
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
DeclareGlobalFunction("ArfNumericalSemigroupClosure");



#####################################################################
##
#A IsArfNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is an Arf-semigroup and false otherwise
## 
#####################################################################
DeclareAttribute("IsArfNumericalSemigroup", IsNumericalSemigroup);



#####################################################################
##
#A MinimalArfGeneratingSystemOfArfNumericalSemigroup(s)
##
## The argument s is an Arf numerical semigroup
## returns the minimal Arf-generating system of s. 
## 
#############################################################################
DeclareAttribute("MinimalArfGeneratingSystemOfArfNumericalSemigroup", IsNumericalSemigroup);


#####################################################################
##
#F ArfNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## Returns the set of Arf numerical semigroups with Frobenius number f 
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
#############################################################################
DeclareGlobalFunction("ArfNumericalSemigroupsWithFrobeniusNumber");

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
DeclareAttribute("IsMEDNumericalSemigroup", IsNumericalSemigroup);



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
DeclareGlobalFunction("MEDNumericalSemigroupClosure");



#####################################################################
##
#A MinimalMEDGeneratingSystemOfMEDNumericalSemigroup(s)
##
## The argument s is a MED numerical semigroup
## returns the minimal MED-generating system of s. 
## 
#############################################################################
DeclareAttribute("MinimalMEDGeneratingSystemOfMEDNumericalSemigroup", IsNumericalSemigroup);

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
DeclareGlobalFunction("SaturatedNumericalSemigroupClosure");

#####################################################################
##
#A IsSaturatedNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is a saturated-semigroup and false otherwise
## 
#####################################################################
DeclareAttribute("IsSaturatedNumericalSemigroup", IsNumericalSemigroup);

#####################################################################
##
#F SaturatedNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## returns the the set of saturated numerical semigroups with Frobenius number f
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
########################################################################
DeclareGlobalFunction("SaturatedNumericalSemigroupsWithFrobeniusNumber");
