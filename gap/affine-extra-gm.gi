#############################################################################
##
#W  affine-extra-gm.gi            
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################
BindGlobal("NumSgpsRationals",HomalgFieldOfRationalsInSingular());


############################################################
# computes a set of generators of the kernel congruence 
# of the monoid morphism associated to the matrix m with 
# nonnegative integer coefficients
############################################################
############################################################
# computes a set of generators of the kernel congruence 
# of the monoid morphism associated to the matrix m with 
# nonnegative integer coefficients
############################################################
# Uses GradedModules

InstallOtherMethod(GeneratorsOfKernelCongruence,
        "Computes a set of generators of the kernel congruence of the monoid morphism associated to a matrix", 
        [IsHomogeneousList],5,
        function(msg)
    
    
    local Q,R,S, vR, vS, phi, ker, degree, bintopair, sdegree, 
          p, ed, dim;
    
    
    bintopair:=function(pp)
        local m1,m2, d1, d2;
        m1:=LeadingMonomial(pp);
        m2:=m1-pp;
        d1:=degree(m1);
        d2:=degree(m2);
        
        return [d1,d2];
    end;
      
    if not(IsRectangularTable(msg)) then 
        Error("The argument must be a matrix of nonnegative integers.");
    fi;
    
    if not(ForAll(msg, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
        Error("The argument must be a matrix of nonnegative integers.");
    fi;
    
    ed:=Length(msg);
    if ed=0 then 
        return [];
    fi;
    dim:=Length(msg[1]);
    Q:= NumSgpsRationals;
    vR:=Concatenation("x1..",String(ed));
    R:=Q*vR;
    vR:=Indeterminates(R);
    
    degree:=DegreeOfRingElementFunction(R,IdentityMat(Length(Indeterminates(R))));
    
    vS:=Concatenation("y1..",String(dim));
    S:=Q*vS;
    vS:=Indeterminates(S);
    p:=List([1..ed], i->Product(List([1..dim], j->vS[j]^msg[i][j])));
    phi:=RingMap(p,R,S);
    Display(phi);
    ker:=KernelSubobject(phi); #ByASmallRepresentation will choose a minimal generating system
    p:=EntriesOfHomalgMatrix(MatrixOfSubobjectGenerators(ker));
    return List(p, bintopair);
    
end);
