#############################################################################
##
#W  dot.gd                  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Andrés Herrera-Poyatos <andreshp9@gmail.com>
##
##
#Y  Copyright 2018 by Manuel Delgado and Pedro Garcia-Sanchez
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

############################################################################
##
#F DotSplash(dots...)
##  Launches a browser and visualizes the dots diagrams 
##  provided as arguments.
##
############################################################################
DeclareGlobalFunction("DotSplash");

############################################################################
##
#F DotBinaryRelation(br)
##  Returns a GraphViz dot which represents the binary relation br.
##  The set of vertices of the resulting graph is the source of br.
##  Edges join those elements which are related in br.
##
############################################################################
DeclareGlobalFunction("DotBinaryRelation");

############################################################################
##
#F HasseDiagramOfNumericalSemigroup(s, A)
##  Returns a binary relation which is the Hasse diagram of A with 
##  respect to the ordering a <= b if b - a in S.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfNumericalSemigroup");

############################################################################
##
#F HasseDiagramOfBettiElementsOfNumericalSemigroup(s)
##  Returns a binary relation which is the Hasse diagram of the Betti
##  elements of s with respect to the ordering a <= b if b - a in S.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfBettiElementsOfNumericalSemigroup");

############################################################################
##
#F HasseDiagramOfAperyListOfNumericalSemigroup(s, n)
##  Returns a binary relation which is the Hasse diagram of the 
##  set Ap(s; n) with respect to the ordering a <= b if b - a in S.
##  The argument n is optional and its default value is the multiplicity 
##  of s.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfAperyListOfNumericalSemigroup");

############################################################################
##
#F DotTreeOfGluingsOfNumericalSemigroup(s, depth...)
##  Returns a GraphViz dot that represents the tree of gluings of the
##  numerical semigroup s.
##  The tree is truncated at the given depth. If the depth is not provided,
##  then the tree is fully built.
##
############################################################################
DeclareGlobalFunction("DotTreeOfGluingsOfNumericalSemigroup");

############################################################################
##
#F DotOverSemigroupsNumericalSemigroup(s)
##  Returns a GraphViz dot that represents the Hasse diagram of 
##  oversemigroupstree of the numerical semigroup s.
##  Irreducible numerical semigroups are highlighted.
##
############################################################################
DeclareGlobalFunction("DotOverSemigroupsNumericalSemigroup");
DeclareOperation("DotOverSemigroups", [IsNumericalSemigroup]);

############################################################################
##
#O DotRosalesGraph(n,s)
## s is either a numerical semigroup or an affine semigroup, and n is an
## element of s
## returns the graph associated to n in s in dot.
##
#############################################################################
DeclareOperation("DotRosalesGraph",[IsHomogeneousList,IsAffineSemigroup]);
DeclareOperation("DotRosalesGraph",[IsInt,IsNumericalSemigroup]);

############################################################################
##
#O DotFactorizationGraph(f)
##
## f is a set of factorizations 
## returns the graph of factorizations associated to f: a complete graph 
## whose vertices are the elements of f. Edges are labelled with
## distances between nodes they join. Kruskal algorithm is used to 
## draw in red a spannin tree with minimal distances. Thus the catenary
## degree is reached in the edges of the tree.
##
#############################################################################
DeclareOperation("DotFactorizationGraph",[IsHomogeneousList]);

############################################################################
##
#O DotEliahouGraph(f)
##
## f is a set of factorizations 
## returns the Eliahou graph of factorizations associated to f: a graph 
## whose vertices are the elements of f, and there is an edge between
## two vertices if they have common support. Edges are labelled with
## distances between nodes they join.
##
#############################################################################
DeclareOperation("DotEliahouGraph",[IsHomogeneousList]);

############################################################################
##
#V DotNSEngine
##
## This variable stores the engine vizjs will use
##
############################################################################
BindGlobal("DotNSEngine", "dot");

############################################################################
##
#F SetDotNSEngine(engine)
##
## This sets de value of DotNSEngine to engine, which must be any of 
## the following "circo","dot","fdp","neato","osage","twopi".
##
############################################################################
DeclareGlobalFunction("SetDotNSEngine");