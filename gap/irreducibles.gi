#############################################################################
##
#W  irreducibles.gi         Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
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
##  Frobenius number f, following Theorem 2.9 in [BR13]
##  -V. Blanco, J.C. Rosales, The tree of irreducible numerical semigroups with fixed 
##   Frobenius number, Forum Math.
#############################################################################
InstallGlobalFunction(IrreducibleNumericalSemigroupsWithFrobeniusNumber, function(f)
	local cf, irr, B, s, lsons, sons;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

	if f<-1 or f =0 then 
		return [];#Error(f," is not a valid Frobenius number.\n");
	fi;

	if f=-1 then 
		return [NumericalSemigroup(1)];
	fi;

	sons:=function(s) #implements Th. 2.9, the sons of an  
			  # irreducible numerical semigroup	 
		local small,candidates, f, minimal, m;


		small:=SmallElementsOfNumericalSemigroup(s);	
		f:=FrobeniusNumber(s);
	 	m:=MultiplicityOfNumericalSemigroup(s);

		minimal:=function(x) #checks if x is a minimal generator 
				     #	smaller than or equal to f+1
			return Filtered(small, y-> (y<x) and (x-y) in small)=[0];
		end;
		#canditadates are the elements fulfilling (i)-(v) in Th. 2.9
		candidates:=Filtered(small, x-> (x>f/2) and (x<f) and 
			not(2*x-f in small) and not(3*x=2*f) and not(4*x=3*f) 
			and (f-x<m) and (minimal(x)));
	
		return List(candidates, x-> 
		NumericalSemigroupBySmallElements(Set(Concatenation(Difference(small,[x]),[f-x]))));	
	
	end;

	#cf is the root of the tree of irreducible numerical semigroups with Frobenius number f 
	# as stated in Th. 2.9
	if IsEvenInt(f) then 
		cf:=NumericalSemigroupBySmallElements(Concatenation([0],[((f/2+1))..(f-1)],[f+1]));
	else
		cf:=NumericalSemigroupBySmallElements(Concatenation([0],[((f+1)/2)..(f-1)],[f+1]));
	fi;

	B:=[cf];
	irr:=[cf];
	#we keep computing the sons in the tree for every new irreducible numerical semigroup
	while B<>[] do
		s:=B[1];
		B:=B{[2..Length(B)]};
		lsons:=sons(s);
		irr:=Union(irr,lsons);
		B:=Union(B,lsons);
	od;
	return irr;
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
    return Length(GapsOfNumericalSemigroup(s))<=(FrobeniusNumberOfNumericalSemigroup(s)+2)/2;
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
      return Length(GapsOfNumericalSemigroup(s))=(FrobeniusNumberOfNumericalSemigroup(s)+1)/2;
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
    return Length(GapsOfNumericalSemigroup(s))=(FrobeniusNumberOfNumericalSemigroup(s)+2)/2;
end);


#####################################################################
##                        Almost-symmetric numerical semigroups
## See [BF97] and [RGS13]
#  -J. C. Rosales, P. A. García-Sánchez, Constructing almost symmetric numerical 
#   semigroups from almost irreducible numerical semigroups, Comm. Algebra.
#####################################################################
##
#F IsAlmostSymmetricNumericalSemigroup(arg)
##
## The argument is a numerical semigroup. The output is True or False depending
## on if the semigroup is almost symmetric or not, see [BF97]
## 
#####################################################################
InstallGlobalFunction(IsAlmostSymmetricNumericalSemigroup,function(s)

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

	return 2*GenusOfNumericalSemigroup(s)=FrobeniusNumber(s)+TypeOfNumericalSemigroup(s);
end);


#####################################################################
##
#F AlmostSymmetricNumericalSemigrupsFromIrreducible(s)
##
## The argument is an irreducible numerical semigroup. The output is the set of
## almost-symmetric numerical semigroups obtained from s, as explained in 
## Theorem 3 in [RGS13]
## 
#####################################################################

InstallGlobalFunction(AlmostSymmetricNumericalSemigroupsFromIrreducible,function(s)
	local msg, pow, conditionb, f,cand, small;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    if not IsIrreducibleNumericalSemigroup(s) then
        Error("The argument must be an irreducible numerical semigroup.\n");
    fi;


	#implements Condition (b) in Proposition 1 of [RGS13]
	conditionb:=function(l)
		local cart;
		cart:=Filtered(Cartesian(l,l),p->p[1]<=p[2]);
		return ForAll(cart, p-> (p[1]+p[2]-f in l) or not(p[1]+p[2]-f in s));
	end;
	
	f:=FrobeniusNumber(s);
	small:=SmallElementsOfNumericalSemigroup(s);

	#chooses minimal generators between f/2 and f, and then tests Condition (b)
	msg:=Filtered(MinimalGeneratingSystemOfNumericalSemigroup(s), x-> (f/2<x) and (x<f));
	pow:=Combinations(msg);
	cand:=Filtered(pow, conditionb); 

	return Set(cand,l->NumericalSemigroupBySmallElementsNC(Difference(small,l)));
end);

#####################################################################
##
#F AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument is an integer. The output is the set of all almost-symmetric 
## numerical semigroups with Frobenius number f ([RGS13])
## 
#####################################################################
InstallGlobalFunction(AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber,function(f)

    if(not(IsInt(f))) then
        	Error("The argument must be an integer.\n");
    fi;


	if f<-1 or f =0 then 
		return []; #Error(f," is not a valid Frobenius number.\n");
	fi;

	if f=-1 then 
		return [NumericalSemigroup(1)];
	fi;

	return Union(Set(IrreducibleNumericalSemigroupsWithFrobeniusNumber(f),
								AlmostSymmetricNumericalSemigroupsFromIrreducible));
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


    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;
	
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

		return (not(d1 in l2/d2) and not(d2 in l1/d1)) and ((d1 in s2) and (d2 in s1));
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

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
      fi;
      ## if the semigroup is no symmetric, then it not a complete intersection
    if not IsSymmetricNumericalSemigroup(s) then
        return false;
    fi;
      
    return Length(MinimalPresentationOfNumericalSemigroup(s))=EmbeddingDimensionOfNumericalSemigroup(s)-1;

end);

#############################################################################
##
#F IsFreeNumericalSemigroup
##
# # returns true if the numerical semigroup is a free semigroup, in the sense of 
# # Bertin and Carbonne [BC77]
##
#############################################################################
InstallGlobalFunction(IsFreeNumericalSemigroup,function(s)
	local gluing, msg;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;
      ## if the semigroup is no symmetric, then it not free
    if not IsSymmetricNumericalSemigroup(s) then
        return false;
    fi;

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

	if Length(msg)<=2 then
		return true;
	fi;

	return ForAny(msg, m-> gluing([m],Difference(msg,[m])) and 
	IsFreeNumericalSemigroup(NumericalSemigroup(Difference(msg,[m])/Gcd(Difference(msg,[m])))));
end);

#############################################################################
##
#F IsTelescopicNumericalSemigroup
##
## returns true if the numerical semigroup is telescopic [KP95], 
##  that is, free for the ordering n_1<...<n_e, with n_i the minimal generators
##
#############################################################################
InstallGlobalFunction(IsTelescopicNumericalSemigroup,function(s)
	local gluing, msg, max;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;
      ## if the semigroup is no symmetric, then it not telescopic
    if not IsSymmetricNumericalSemigroup(s) then
        return false;
    fi;

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
	max:=Maximum(msg);

	if Length(msg)<=2 then
		return true;
	fi;

	return gluing([max],Difference(msg,[max])) and 
	IsTelescopicNumericalSemigroup(NumericalSemigroup(Difference(msg,[max])/Gcd(Difference(msg,[max]))));

end);

#############################################################################
##
#F IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity
##
## returns true if the numerical semigroup is a telescopic numerical semigroup,
##  and in addition for all i, d_i n_i < d_{i+1}n_{i+1}, con d_i=gcd{n_j | j<i} [Z86]
##
#############################################################################
InstallGlobalFunction(IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity,function(s)
	local gluing, msg, max, di, dip1, rest, maxrest;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;
      ## if the semigroup is no symmetric, then it not planar
    if not IsSymmetricNumericalSemigroup(s) then
        return false;
    fi;

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
		
	if Length(msg)<=2 then
		return true;
	fi;

	max:=Maximum(msg);
	rest:=Difference(msg,[max]);
	maxrest:=Maximum(rest);
	di:=Gcd(Difference(rest,[maxrest]));
	dip1:=Gcd(rest);

	return gluing([max],rest) and (di*maxrest< dip1*max) and
		IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity( NumericalSemigroup(rest/Gcd(rest)) );
 
end);

#############################################################################
##
#F NumericalSemigroupsAssociatedIrreduciblePlanarCurveSingularityWithFrobeniusNumber
##
## returns the set of numerical semigroups associated to irreducible
## planar curves with Frobenius number given, as explained in [AGS13]
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupsAssociatedIrreduciblePlanarCurveSingularityWithFrobeniusNumber, function(f)

	local pcs, out, i, s;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

 	pcs:=function(f) #we first compute the list of all possible minimal generating systems
		local dkcand, dk, rk, fp, candr, bound, total, pcscond;

		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then 
			return [[2,3]];
		fi;
	
		#irreducible planar curve conditition
		pcscond:=function(l,d,r)
			local rest, max, dl;

			if Length(l)=1 then
				return true;
			fi;
			max:=Maximum(l);
			rest:=Difference(l,[max]);
			dl:=Gcd(rest);
			return max*d*dl < r;
		end;

		total:=[[2,f+2]]; #this one is always 
		for rk in [4..f-1] do
			bound:=Minimum(rk-1, Int((f+1)/(rk-1)+1), RootInt(f+1)+1);
			dkcand:=Filtered([2..bound],d->(Gcd(d,rk)=1)and((f+rk) mod d=0));
			for dk in dkcand do
				fp:=(f+rk*(1-dk))/dk;
				candr:=Filtered(pcs(fp), l-> (rk> Maximum(l)*dk) and pcscond(l,dk,rk) and (rk in NumericalSemigroup(l)));
				#Print(fp,candr,"\n");
				candr:=List(candr, l-> Concatenation(l*dk, [rk]));
				total:=Union(total,candr);	
			od;		
		od;
		return total;
	end;

	out:=[];
	for i in pcs(f) do #from the construction we know that the sets of generators are minimal
		s:=NumericalSemigroupByMinimalGenerators(i);
		Add(out,s);
	od;
	return out;
end);

#############################################################################
##
#F TelescopicNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of telescopic numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
InstallGlobalFunction(TelescopicNumericalSemigroupsWithFrobeniusNumber,function(f)
	local tel, s, i, out;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

	tel:=function(f) #we first compute the list of all possible minimal generating systems
		local dkcand, dk, rk, fp, candr, bound, total;
	 

		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then 
			return [[2,3]];
		fi;
	
		total:=[[2,f+2]]; #this one is always
		for rk in [4..f-1] do
			bound:=Minimum(rk-1, Int((f+1)/(rk-1)+1), RootInt(f+1)+1);
			dkcand:=Filtered([2..bound],d->(Gcd(d,rk)=1)and((f+rk) mod d=0));
			for dk in dkcand do
				fp:=(f+rk*(1-dk))/dk;
				candr:=Filtered(tel(fp), l-> (rk> Maximum(l)*dk) and (rk in NumericalSemigroup(l)));
				candr:=List(candr, l-> Concatenation(l*dk, [rk]));
				total:=Union(total,candr);	
			od;		
		od;
		return total;
	end;

	out:=[];
	for i in tel(f) do #from the construction we know that the sets of generators are minimal
		s:=NumericalSemigroupByMinimalGenerators(i);
		Add(out,s);
	od;
	return out;
end);

#############################################################################
##
#F FreeNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of free numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
InstallGlobalFunction(FreeNumericalSemigroupsWithFrobeniusNumber,function(f)
	local free, s, i, out;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

	free:=function(f)#we first compute the list of all possible minimal generating systems
		local dkcand, dk, rk, fp, candr, bound, total;
	 
		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then 
			return [[2,3]];
		fi;
	
		total:=[[2,f+2]];
		for rk in [4..f-1] do
			bound:=(Int((f+1)/(rk-1)+1));
			dkcand:=Filtered([2..bound],d->(Gcd(d,rk)=1)and((f+rk) mod d=0));
			for dk in dkcand do
				fp:=(f+rk*(1-dk))/dk;
				candr:=Filtered(free(fp), l-> not(rk in l) and (rk in NumericalSemigroup(l)));
				#Print(fp,candr,"\n");
				candr:=List(candr, l-> Union(l*dk, [rk]));
				total:=Union(total,candr);	
			od;		
		od;
		return total;
	end;

	out:=[];
	for i in free(f) do #from the construction we know that the sets of generators are minimal
		s:=NumericalSemigroupByMinimalGenerators(i);
		Add(out,s);
	od;
	return out;
end);

#############################################################################
##
#F CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of comple intersection numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
InstallGlobalFunction(CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber,function(f)
	local ci, s, i, out;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;

	ci:=function(f)
		local d2cand, d1, d2, pairfcand, fcand1, fcand2, cand1, cand2, bound, total, partial, p;
	 
		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then 
			return [[2,3]];
		fi;
	
		total:=[[2,f+2]];
		for d1 in [3..f-1] do
			bound:=Minimum(d1-1,Int((f+1)/(d1-1)+1)); #we consider d2<d1
			d2cand:=Filtered([2..bound],d->(Gcd(d,d1)=1));
	#		Print(f,":",d1,"->",d2cand,"\n");
			for d2 in d2cand do
				if((f+d1) mod d2=0) then 
					cand2:=Filtered(ci((f+d1*(1-d2))/d2), l-> not(d1 in l) and (d1 in NumericalSemigroup(l)));
					partial:=List(cand2, l-> Union([d1],d2*l));
					total:=Union(total,partial); cand2:=[]; partial:=[];				
				elif((f+d2) mod d1=0) then 
					cand1:=Filtered(ci((f+d2*(1-d1))/d1), l-> not(d2 in l) and (d2 in NumericalSemigroup(l)));
					partial:=List(cand1, l-> Union(d1*l,[d2]));
					total:=Union(total,partial); cand1:=[]; partial:=[];					
				fi;	
				if(f-d1*d2>0) then
					pairfcand:=FactorizationsIntegerWRTList(f-d1*d2,[d2,d1]);
					pairfcand:=Filtered(pairfcand, p-> (p[1] mod 2=1) and (p[2] mod 2=1));
					for p in pairfcand do
						cand1:=Filtered(ci(p[2]), l-> not(d2 in l) and (d2 in NumericalSemigroup(l)));
						cand2:=Filtered(ci(p[1]), l-> not(d1 in l) and (d1 in NumericalSemigroup(l)));
						partial:=List(Cartesian(cand1,cand2), pp-> Union(d1*pp[1],d2*pp[2]));
						total:=Union(total,partial);
					od;
				fi;
			od;		
		od;
		return total;
	end;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;


	out:=[];
	for i in ci(f) do #from the construction we know that the sets of generators are minimal
		s:=NumericalSemigroupByMinimalGenerators(i);
		Add(out,s);
	od;
	return out;
end);


