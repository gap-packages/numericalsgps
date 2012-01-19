#############################################################################
##
#W  irreducibles.gi         Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: irreducibles.gi,v 0.971 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#F  RemoveMinimalGeneratorFromNumericalSemigroup(n,s)
##
##  Computes the numerical semigroup obtained from s after removing from s 
##  its minimal generator n: s\{n}.
##
#############################################################################
InstallGlobalFunction(RemoveMinimalGeneratorFromNumericalSemigroup, function(n,s)
    local   msg,  multiplicity,  ap,  position;

    if(not(IsInt(n))) then
        Error("The first argument must be an integer.\n");
    fi;

    if(not(IsNumericalSemigroup(s))) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    multiplicity:=MultiplicityOfNumericalSemigroup(s);


    if (not(n in msg)) then
        Error(n," must be a minimal generator of ",s,".\n");
    fi;


    if (n<>multiplicity) then 
        ap:=AperyListOfNumericalSemigroupWRTElement(s,multiplicity);
        ap[1]:=multiplicity;
        position:=Position(ap,n);
        ap[position]:=n+multiplicity;
        return NumericalSemigroup(ap);
    else
        if(msg=[1]) then
            return(NumericalSemigroup([2,3]));
        fi;
        ap:=AperyListOfNumericalSemigroupWRTElement(s,msg[2]);
        ap[1]:=msg[2];
        position:=Position(ap,n);
        ap[position]:=n+msg[2];
        return NumericalSemigroup(ap);    
    fi;

end);


#############################################################################
##
#F  AddSpecialGapOfNumericalSemigroup(g,s)
##
##  Adds the special gap g to the numerical semigroup s and
##  returns the resulting numerical semigroup: s\cup {g}.
##
#############################################################################
InstallGlobalFunction(AddSpecialGapOfNumericalSemigroup, function(g,s)
    local   eh,  primedivisorsg,  candidates,  fh,  divisorsfh;

    if(not(IsInt(g))) then
        Error("The first argument must be an integer.\n");
    fi;

    if(not(IsNumericalSemigroup(s))) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;


    eh:=SpecialGapsOfNumericalSemigroup(s);

    if(not(g in eh)) then
        Error(g," must be a special gap of ",s,".\n");
    fi;
    if(g=1) then
        return NumericalSemigroup(1);
    fi;

    primedivisorsg:=Set(FactorsInt(g));
    candidates:=List(primedivisorsg,x->g/x);
    fh:=ShallowCopy(FundamentalGapsOfNumericalSemigroup(s));
    RemoveSet(fh,g);
    divisorsfh:=Union(List(fh,x->Set(DivisorsInt(x)))); 
    ##0.97 divisorsfh:=Union(List(fh,x->Set(FactorsInt(x))));
    candidates:=Filtered(candidates,x->not(x in divisorsfh));
    fh:=Union(fh,candidates);

    return NumericalSemigroupByFundamentalGaps(fh);    
end);



#############################################################################
##
#F  AnIrreducibleNumericalSemigroupWithFrobeniusNumber(f)
##
##  Produces an irreducible numerical semigroup by using
##  "Every positive integer is the Frobenius number of an irreducible...".
##
#############################################################################
InstallGlobalFunction(AnIrreducibleNumericalSemigroupWithFrobeniusNumber, function(f)
    local alpha,q;

    if(not(IsInt(f))) then
        Error("The argument must be an integer.\n");
    fi;

    if(f=0 or f<-1) then
        Print(f," is not a suitable integer.\n");
        return fail;
    fi;
    if(f mod 2<>0) then 
        return NumericalSemigroup([2,f+2]);
    fi;
    if(f mod 3<>0) then
        return NumericalSemigroup([3,f/2+3,f+3]);
    fi;
    if(f mod 4<>0) then
        return NumericalSemigroup([4,f/2+2,f/2+4]);
    fi;
    #if we reach this point, then 2|f, 3|f and 4|f
    alpha:=First([1..((f-(f mod 9))/9+1)],n->(f mod 3^(n+1)<>0));
    q:=f/3^alpha;
    return NumericalSemigroup([3^(alpha+1),q/2+3,3^alpha*q/4+q/2+3,3^alpha*q/2+q/2+3]);
end);


#############################################################################
##
#F  IrreducibleNumericalSemigroupsWithFrobeniusNumber(f)
##
##  Computes the set of irreducible numerical semigroups with given
##  Frobenius number f.
##
#############################################################################
InstallGlobalFunction(IrreducibleNumericalSemigroupsWithFrobeniusNumber, function(f)
    local   s,  A,  L,  B;

    #A the set of irreducibles
    #B the set of candidates for a given irreducible
    #s current numerical semigroup
    #L temporal list of semigroups

    if(not(IsInt(f))) then
        Error("The argument must be an integer.\n");
    fi;

    s:=AnIrreducibleNumericalSemigroupWithFrobeniusNumber(f);
    A:=[s];

    L:=Filtered(MinimalGeneratingSystemOfNumericalSemigroup(s),
               n->(f-n)in SpecialGapsOfNumericalSemigroup(RemoveMinimalGeneratorFromNumericalSemigroup(n,s)));

    B:=List(L,n->AddSpecialGapOfNumericalSemigroup(f-n,
               RemoveMinimalGeneratorFromNumericalSemigroup(n,s)));

    while(B<>[]) do
        s:=B[1];
        B:=B{[2..Length(B)]};
        if (not(s in A)) then 
            A:=Union(A,[s]);
            L:=Filtered(MinimalGeneratingSystemOfNumericalSemigroup(s),
                       n->(f-n) in (SpecialGapsOfNumericalSemigroup(RemoveMinimalGeneratorFromNumericalSemigroup(n,s))));
            B:=Union(B,List(L,n->AddSpecialGapOfNumericalSemigroup(f-n,
                       RemoveMinimalGeneratorFromNumericalSemigroup(n,s))));
        fi;
    od;
    return A;
end);


#############################################################################
##
#F  OverSemigroupsNumericalSemigroup(s)
##
##  Computes the set of numerical semigroups containing s.
##  The algorithm is based on 
##  "The oversemigroups of a numerical semigroup", Semigroup Forum.
##
#############################################################################
InstallGlobalFunction(OverSemigroupsNumericalSemigroup, function(s)
    local   t,  sg,  A,  O;

    #A the output
    #t current semigroup for which we compute unitary extensions
    #O list of over semigroups of t
    #sg special gaps of t

    if(not(IsNumericalSemigroup(s))) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    if(s=NumericalSemigroup(1)) then
        return [s];
    fi;

    t:=s;
    sg:=SpecialGapsOfNumericalSemigroup(t); #which must be different from [-1]
    A:=[NumericalSemigroup(1),t];
    O:=List(sg,g->AddSpecialGapOfNumericalSemigroup(g,t));
    while(not(O)=[]) do
        t:=O[1];
        O:=O{[2..Length(O)]};
        if(not(t in A)) then
            A:=Union(A,[t]);
            sg:=SpecialGapsOfNumericalSemigroup(t);
            O:=Union(O,List(sg,g->AddSpecialGapOfNumericalSemigroup(g,t)));
        fi;
    od;
    return A;
end);



#############################################################################
##
#F  DecomposeIntoIrreducibles(s)
##
##  Returns a list of irreducible numerical semigroups
##  such that its intersection is s.
##  This decomposition is minimal, and is inspired in 
##  Algorithm 26 of "The oversemigroups of a numerical semigroup".
##
#############################################################################
InstallGlobalFunction(DecomposeIntoIrreducibles, function(s)
    local   sg,  caux,  I,  C,  B,  pair,  i,  I2,  j,  l;

    #sg contains the special gaps of s
    #B auxiliar ser used to construct C and I
    #I will include irreducibles containing s
    #C non irreducibles
    #caux(t) is to compute those elements in gs that are not in t
    #II auxiliar set of irreducibles

    if(not(IsNumericalSemigroup(s))) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    if(s=NumericalSemigroup(1)) then
        return [s];
    fi;

    sg:=SpecialGapsOfNumericalSemigroup(s);
    if (Length(sg)=1) then
        return [s];
    fi;

    caux:=function(t)
        return Filtered(sg,g->not(BelongsToNumericalSemigroup(g,t)));
    end;

    I:=[];
    C:=[s];
    B:=[];
    while(not(C=[])) do
        B:=Union(List(C,sp->List(SpecialGapsOfNumericalSemigroup(sp),g->AddSpecialGapOfNumericalSemigroup(g,sp))));
        B:=Filtered(B,b->not(caux(b)=[]));
        B:=Filtered(B,b->Filtered(I,i->IsSubsemigroupOfNumericalSemigroup(i,b))=[]);
        C:=Filtered(B,b->not(Length(SpecialGapsOfNumericalSemigroup(b))=1));
        I:=Union(I,Difference(B,C));
    od;
    I:=List(I,i->[i,caux(i)]);
    while(true) do 
        pair := fail;
        for i in I do
            I2 := [];
            for j in I do
                if not j = i then
                    Add(I2, j);
                fi;
            od;

            l := [];
            for j in I2 do
                Add(l, j[2]);
            od;
            if Union(l) = sg then
                pair := i;
                break;
            fi;
        od;

        if(pair=fail) then
            return List(I,i->i[1]);
        fi;

        I2 := [];
        for j in I do
            if not j = pair then
                Add(I2, j);
            fi;
        od;

        I := I2;
        Unbind(I2);
    od;
    return(List(I, x -> x[1]));
end);


#############################################################################
##
#F  IsIrreducibleNumericalSemigroup(s)
##
##  Checks whether or not s is an irreducible numerical semigroup.
##
#############################################################################
InstallGlobalFunction(IsIrreducibleNumericalSemigroup, function(s)
    if(not(IsNumericalSemigroup(s))) then 
        Error("The argument must be a numerical semigroup.\n");
    fi;
    return Length(SpecialGapsOfNumericalSemigroup(s))=1;
end);



#############################################################################
##
#F  IsSymmetricNumericalSemigroup(s)
##
##  Checks whether or not s is a symmetric numerical semigroup.
##
#############################################################################
InstallGlobalFunction(IsSymmetricNumericalSemigroup, function(s)
    local sg;

    if(not(IsNumericalSemigroup(s))) then 
        Error("The argument must be a numerical semigroup.\n");
    fi;
    sg:=SpecialGapsOfNumericalSemigroup(s);
    if(not(Length(sg)=1)) then
        return (false);
    fi;

    return (sg[1] mod 2)=1;
end);



#############################################################################
##
#F  IsPseudoSymmetricNumericalSemigroup(s)
##
##  Checks whether or not s is a pseudosymmetric numerical semigroup.
##
#############################################################################
InstallGlobalFunction(IsPseudoSymmetricNumericalSemigroup,function(s)
    local sg;

    if(not(IsNumericalSemigroup(s))) then 
        Error("The argument must be a numerical semigroup.\n");
    fi;
    sg:=SpecialGapsOfNumericalSemigroup(s);
    if(not(Length(sg)=1)) then
        return (false);
    fi;

    return (sg[1] mod 2)=0;
end);

#############################################################################
##
#F AsGluingOfNumericalSemigroups
##
## returns all partitions {A1,A2} of the minimal generating set of s such
## that s is a gluing of <A1> and <A2> by gcd(A1)gcd(A2) 
##
#############################################################################
InstallGlobalFunction(AsGluingOfNumericalSemigroups,function(s)
	local msg,partitions, gluing;
	
	gluing:=function(l1,l2) #this function checks if for  a couple of  
				#	lists of integers, the numerical semigroup 
				# 	generated by [l1,l2] is the gluing of those
				# 	generated by l1/d1 and l2/d2
		local d1, d2, t1, t2, s1, s2;

		d1:=Gcd(l1);
		d2:=Gcd(l2);

		if (not(Gcd(d1,d2)=1)) then 
			return false;
		fi;

		s1:=NumericalSemigroup(l1/d1);
		s2:=NumericalSemigroup(l2/d2);

		return (not(d1 in l2) and not(d2 in l1)) and ((d1 in s2) and (d2 in s1));
	end;


	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);	

	partitions:=PartitionsSet(msg,2);

	return Filtered(partitions, p->gluing(p[1],p[2]));
end);


#############################################################################
##
#F IsACompleteIntersectionNumericalSemigroup
##
##returns true if the numerical semigroup is a complete intersection, 
## that is, the cardinality of a (any) minimal presentation equals 
## its embedding dimension minus one
##
#############################################################################
InstallGlobalFunction(IsACompleteIntersectionNumericalSemigroup,function(s)
	
	return Length(MinimalPresentationOfNumericalSemigroup(s))=EmbeddingDimensionOfNumericalSemigroup(s)-1;

end);
