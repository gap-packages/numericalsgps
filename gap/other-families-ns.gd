#############################################################################
##
#W  other-families-ns.gd
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2017-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################

#############################################################################
##
#P  IsAcuteNumericalSemigroup(s)
##
##  Checks whether or not the nuemrical semigroup s is acute.
##
#############################################################################
DeclareProperty("IsAcuteNumericalSemigroup", IsNumericalSemigroup);
DeclareSynonymAttr("IsAcute", IsAcuteNumericalSemigroup);
#############################################################################
##
#P  IsOrdinaryNumericalSemigroup(s)
##
##  Checks whether or not the nuemrical semigroup s is acute.
##
#############################################################################
DeclareProperty("IsOrdinaryNumericalSemigroup", IsNumericalSemigroup);
DeclareSynonymAttr("IsOrdinary",IsOrdinaryNumericalSemigroup);
