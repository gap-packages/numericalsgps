#############################################################################
##
#W  arf-med.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2005 by Manuel Delgado and  Pedro Garcia-Sanchez 
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################

#################################################
## 
#F NumericalSemigroupPolynomial(s,x)
## s is a numerical semigroup, and x a variable (or a value)
## returns the polynomial (1-x)\sum_{s\in S}x^s
##
##################################################
InstallGlobalFunction(NumericalSemigroupPolynomial, function(s,x)
	local gaps, p;

    if not IsNumericalSemigroup(s) then
        Error("The first argument must be a numerical semigroup.\n");
    fi;
	
	gaps:=GapsOfNumericalSemigroup(s);
	p:=List(gaps, g-> x^g);
	return 1+(x-1)*Sum(p);
end);

###################################################
#F HilbertSeriesOfNumericalSemigroup(s,x)
## Computes the Hilber series of s in x : \sum_{s\in S}x^s
###################################################
InstallGlobalFunction(HilbertSeriesOfNumericalSemigroup,function(s,x)
	local m,ap;
    if not IsNumericalSemigroup(s) then
        Error("The first argument must be a numerical semigroup.\n");
    fi;

	if HasAperyListNS(s) then #uses J.Ramirez-Alfonsin trick
		m:=MultiplicityOfNumericalSemigroup(s);
		ap:=AperyListOfNumericalSemigroup(s);
		return 1/(1-x^m)*Sum(List(ap, w->x^w));
	fi;
	return NumericalSemigroupPolynomial(s,x)/(1-x);
end);


######################################################
##
#F Computes the Graeffe polynomial of p
##  see for instance [BD-cyclotomic]
##
######################################################
InstallGlobalFunction(GraeffePolynomial,function(p)
	local coef, h, g, i, f1,x;

	if not(IsUnivariatePolynomial(p)) then 
		Error("The argument must be a univariate polynomial");
	fi;
	x:=IndeterminateOfUnivariateRationalFunction(p);
	coef:=CoefficientsOfUnivariatePolynomial(p);
	h:=[]; g:=[];
	for i in [1..Length(coef)] do
		if (i-1) mod 2 = 0 then
			Add(g,coef[i]);
		else
			Add(h,coef[i]);
		fi;
	od;
	#Print(g," ,",h,"\n");
	g:=UnivariatePolynomial(Rationals,g);
	h:=UnivariatePolynomial(Rationals,h);
	f1:=Value(g,x)^2-x*Value(h,x)^2;
	return f1/LeadingCoefficient(f1);
end);


#####################################################
##
#F IsCyclotomicPolynomial(f) detects 
## if f is a cyclotomic polynomial using the method explained in 
## BD-cyclotomic
#####################################################
InstallGlobalFunction(IsCyclotomicPolynomial,function(f)
	local f1, x, f2, mf;

	if not(IsUnivariatePolynomial(f)) then 
		Error("The argument must be a univariate polynomial");
	fi;
	
	if not(IsListOfIntegersNS(CoefficientsOfUnivariatePolynomial(f))) then 
		Error("The polynomial has not integer coefficients");
	fi;

	if not(IsOne(LeadingCoefficient(f))) then 
		return false;
	fi;

	if not(IsIrreducible(f)) then
		return false;
	fi;

	x:=IndeterminateOfUnivariateRationalFunction(f);
	f1:=GraeffePolynomial(f);
	if (f=f1) then
		return true;
	fi;

	mf:=Value(f,-x);# Print(f1, ", ",f, ",", mf,"\n");

	if ((f1=mf) or (f1=-mf)) and IsCyclotomicPolynomial(mf/LeadingCoefficient(mf)) then
		return true;
	fi;

	f2:=Set(Factors(f1))[1];
	if f1=f2^2 and IsCyclotomicPolynomial(f2/LeadingCoefficient(f2)) then
		return true;
	fi;
	return false;
end);

########################################################################
##
#F IsKroneckerPolynomial(f) decides if 
##   f is a Kronecker polynomial, that is,   a monic polynomial with integer coefficients 
##   having all its roots in the unit circunference, equivalently, is a product of 
##   cyclotomic polynomials
#########################################################################
InstallGlobalFunction(IsKroneckerPolynomial,function(f)
	if LeadingCoefficient(f)<>1 then 
		return false;
	fi;
	return ForAll(Factors(f),IsCyclotomicPolynomial);	
end);


###########################################
##
#F IsCyclotomicNumericalSemigroup(s)
## Checks if the polynomial fo s is Kronecker
###########################################
InstallGlobalFunction(IsCyclotomicNumericalSemigroup,function(s)
	local x, f;
	x:=X(Rationals,"x");
	f:=NumericalSemigroupPolynomial(s,x);
	return IsKroneckerPolynomial(f);# ForAll(Factors(f),IsCyclotomicPolynomial);
end);

#####################################################
## 
#F IsSelfReciprocalUnivariatePolynomial(p)
## Checks if the univariate polynomial p is selfreciprocal
#####################################################
InstallGlobalFunction(IsSelfReciprocalUnivariatePolynomial,function(p)
	local x, d;

	if not(IsUnivariatePolynomial(p)) then 
		Error("The argument must be a polynomial\n");
	fi;
	
	x:=IndeterminateOfLaurentPolynomial(p);
	d:=DegreeOfUnivariateLaurentPolynomial(p);
	return p=x^d*Value(p,1/x);

end);

#################################################################
##
# F SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f)
##  Computes the semigroup of values {mult(f,g) | g curve} of a plane curve 
##   with one place at the infinity in the variables X(Rationals,1) and X(Rationals,2)
##  f must be monic on X(Rationals(2))
##  SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f,"all")
##    The same as above, but the output are the approximate roots and  
##    delta-sequence
##
#################################################################
InstallGlobalFunction(SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity, function(arg)
	local monomials, degree, degreepol, polexprc, polexprs, polexpr, app, tt1, sv;
	#returns the set of monomials of a polynomial
	monomials:=function(f)
		local term, out,temp;
		out:=[];
		if IsZero(f) then 
			return ([0]);
		fi;
		temp:=f;
		while not(IsZero(temp)) do
			term:=LeadingTermOfPolynomial(temp,MonomialLexOrdering());
			Add(out,term);
			temp:=temp-term;
		od;
		return out;

	end;

	#returns the total weighted degree of a monomial wrt l
	degree:=function(mon,l)
		local n;
		n:=Length(l);
		if IsRat(mon) then
			return 0;
		fi;
		return Sum(List([1..n], i->DegreeIndeterminate(mon,i)*l[i]));
	end;

	#returns the total weighted degree of a polynomial wrt l
	degreepol:=function(pol,l)
		return Maximum(List(monomials(pol), t->degree(t,l)));
	end;
	#finds an expression of f in terms of p, and outputs it as a polynomial in x, var is the list of variables in f and p
	polexpr:=function(f,p,var,x)
		local rem, quo, pr;

		pr:=PolynomialDivisionAlgorithm(f,[p],MonomialLexOrdering(var));
		rem:=[pr[1]];
		quo:=pr[2][1];
		while not(IsZero(quo)) do
			pr:=PolynomialDivisionAlgorithm(quo,[p],MonomialLexOrdering(var));
			Add(rem,pr[1]);
			quo:=pr[2][1];
		od;
		return Sum(List([1..Length(rem)],i->rem[i]*x^(i-1)));
	end;
	#uses the previous function to apply recurrently the procedure for all polynomials in ps and variables in xs
	polexprs:=function(f,ps,var,xs)
		local i, len, out;

		len:=Length(ps);
		out:=f;
		for i in [1..len] do
			out:=polexpr(out,ps[i],var,xs[i]);
		od;
		return out;
	end;
	#expression of f in terms of p; both in variables x and y...
	polexprc:=function(f,p)
		local rem, quo, pr;

		pr:=PolynomialDivisionAlgorithm(f,[p],MonomialLexOrdering([X(Rationals,2),X(Rationals,1)]));
		rem:=[pr[1]];
		quo:=Value(pr[2][1],[],[]);
		while not(IsZero(quo)) do
			pr:=PolynomialDivisionAlgorithm(quo,[p],MonomialLexOrdering([X(Rationals,2),X(Rationals,1)]));
			Add(rem,pr[1]);
			quo:=pr[2][1];
		od;
		return Reversed(rem);
	end;
	##TT1
	tt1:=function(f,g,y)
		local d;
		d:=DegreeIndeterminate(f,y)/DegreeIndeterminate(g,y);
		if d>0 then
			return g+polexprc(f,g)[2]/d;
		fi;
		return f;
	end;
	#approximate root
	app:=function(f,y,d)
		local G,S,F,P,e,t,coef;

		e:=DegreeIndeterminate(f,y)/d;
		G:=tt1(f,y^e,y);
		S:=polexprc(f,G);
		P:=S[2];
		#tschirnhausen transform
		while not(IsZero(P)) do
			F:=G+P/e;
			G:=tt1(f,F,y);
			S:=polexprc(f,G);
			P:=S[2];
		od;
		return G;
	end;
	#semigroup of values
	sv:=function(f)
		local  m, d,dd, n,g,it, max, lt, e, coef, deg, tsti,red,rd, var, gred, id, R, aroots;

		if not(IsPolynomial(f)) then
			Error("The argument must be a polynomial\n");
		fi;

		R:=PolynomialRing(Rationals,[X(Rationals,1),X(Rationals,2)]);
		if not(f in R) then
						Error("The argument must be a polynomial in ", R,"\n");
		fi;
		
		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,1));
		if not(IsConstantRationalFunction(coef[Length(coef)])) then
			Error("The polynomial does not have a single place at infinity or the leading coefficient in ", X(Rationals,1)," is not a rational number\n");
		fi;
		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,2));
		if not(IsConstantRationalFunction(coef[Length(coef)])) then
			Error("The polynomial does not have a single place at infinity or or the leading coefficient in ", X(Rationals,2)," is not a rational number\n");
		fi;
		f:=f/coef[Length(coef)];
		m:=[]; 
		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,2));
		Info(InfoNumSgps,2,"f ",f);
		Info(InfoNumSgps,2,"f ",coef);
		it:=coef[1];
		if IsZero(it) then
			Error("The polynomial is not irreducible\n");
		fi;

		m[1]:=DegreeIndeterminate(f,X(Rationals,2));
		m[2]:=DegreeIndeterminate(PolynomialCoefficientsOfPolynomial(f,X(Rationals,2))[1],X(Rationals,1));
		n:=2;
		d:=Gcd(m); 
		e:=m[1]/d;
		g:=f; 
		red:=[];
		var:=[X(Rationals,2),X(Rationals,1)];
		aroots:=[X(Rationals,2)];
		while d>1  do
			Add(var,X(Rationals,n+1));
			rd:=app(f,X(Rationals,2),d);
			Add(aroots,rd);
			g:=polexprs(f,Reversed(aroots),var,Reversed(List([2..n+1],i->X(Rationals,i))));
			coef:=PolynomialCoefficientsOfPolynomial(g, X(Rationals,n+1));
			Info(InfoNumSgps,2,"We obtain coefficients: ",coef);
			it:=coef[1];
			Info(InfoNumSgps,2,"Independent term ", it);
			max:=degreepol(it,m/Gcd(m));
			if (max=0) then #or (d=Gcd(d,max)) or (max in m) then
				Error("Error the polynomial is not irreductible or it has not a single place at infinity (deg 0)\n");
			fi;
			#irreducibility test
			tsti:=First(coef{[2..Length(coef)]}, t->degreepol(t,m/d)>=max);
			if tsti<>fail then
				Info(InfoNumSgps,1,"Term ", tsti," produces error");
				Error("Error the polynomial is not irreductible", "\n");
			fi;
			dd:=Gcd(d,max);
			Info(InfoNumSgps,2,"New candidate: ",max);
			if (d=Gcd(d,max)) or (max in m) then
				Error("Error the polynomial is not irreductible or it has not a single place at infinity\n");
			fi;
			m[n+1]:=max; 
			if(m[n]*Gcd(m{[1..n-1]})<=max*d)then
				Error("Error the polynomial is not irreductible (not subdescending sequence)", m,"\n");
			fi;
			e:=d/Gcd(d,max);
			d:=dd; 
			n:=n+1;
		od;
		return [m,aroots];
	end;

	if Length(arg)=1 then
		return NumericalSemigroup(sv(arg[1])[1]);
	fi;
	if Length(arg)=2  and arg[2] = "all" then
		return sv(arg[1]);
	fi;
	Error("Wrong number of arguments\n");
end);


#########################################################################
## 
#F IsDeltaSequence(l)
## tests whether or not l is a \delta-sequence (see for instancd [AGS14])
#########################################################################
InstallGlobalFunction(IsDeltaSequence,function(l)
	local d,e,h;

	if not(IsListOfIntegersNS(l)) then
		Error("The argument must be a list of positive integers\n");
	fi;
	if Gcd(l)<>1 then
		Info(InfoNumSgps,2,"The gcd of the list is not one");	
		return false;
	fi;
	h:=Length(l)-1;
	d:=List([1..h+1], i->Gcd(l{[1..i]}));
	e:=List([1..h], i->d[i]/d[i+1]);
	if Length(l)=1 then 
		return true;
	fi;
	if (l[1]<=l[2]) or (d[2]=l[2]) then 
		Info(InfoNumSgps,2,"Either the first element is smaller than or equal to the second, or the gcd of the first to equals the second");	
		return false;
	fi;
	if Length(Set(d))<h+1 then
		Info(InfoNumSgps,2,"The list of gcds is not strctitly decreasing");	
		return false;
	fi;
	
	return ForAll([1..h], i->l[i+1]/Gcd(l{[1..i+1]}) in NumericalSemigroup(l{[1..i]}/Gcd(l{[1..i]})))
			  and  
			  ForAll([2..h], i->l[i]*Gcd(l{[1..i-1]}) >l[i+1]*Gcd(l{[1..i]})); 
end);


#########################################################
##
#F DeltaSequencesWithFrobeniusNumber(f)
##   Computes  the list of delta-sequences with Frobenius number f
##
#########################################################
InstallGlobalFunction(DeltaSequencesWithFrobeniusNumber,function(f)
	local abs, test;

	if not(IsInt(f)) then
		Error("The argument must be an integer\n");
	fi;

	if (f<-1) or IsEvenInt(f) then
		return [];
	fi;

	if f=-1 then
		return [[1]];
	fi;
	
	abs:=function(f)
		local dkcand, dk, rk, fp, candr, bound, total;
	 
		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then 
			return [[2,3],[3,2]];
		fi;

		total:=[[f+2,2],[2,f+2]];
		for rk in Reversed([2..f-1]) do
			bound:=(Int((f+1)/(rk-1)+1));
			dkcand:=Filtered([2..bound],d->(Gcd(d,rk)=1)and((f+rk) mod d=0));
			for dk in dkcand do
				fp:=(f+rk*(1-dk))/dk;
				candr:=Filtered(abs(fp), l->  rk in NumericalSemigroup(l)); 
				candr:=List(candr, l-> Concatenation(l*dk, [rk]));
				candr:=Filtered(candr, test);
				candr:=Filtered(candr,l->l[1]>l[2]);
				total:=Union(total,candr);
			od;
		od;
		return total;
	end;

	test:=function(l)
		local len;
		len:=Length(l);
		return ForAll([3..len], k-> l[k-1]*Gcd(l{[1..k-2]})>l[k]*Gcd(l{[1..k-1]})); 
	end;

	return Difference(abs(f),[[2,f+2]]);
end);

#####################################################
##
#F CurveAssociatedToDeltaSequence(l)
##  computes the curve associated to a delta-sequence l in 
##  the variables X(Rationals,1) and X(Rationals,2)
##  as explained in [AGS14]
##
#####################################################
InstallGlobalFunction(CurveAssociatedToDeltaSequence,function(l)
	local n, d, f, fact,facts, g, e, k, ll, len, dd, x,y;

	if not(IsDeltaSequence(l)) then 
		Error("The sequence is not valid\n");
	fi; 
	x:=X(Rationals,1);
	y:=X(Rationals,2);
	len:=Length(l);
	if len<2 then 
		Error("The sequence must have at least two elements\n");
	fi;
	if Gcd(l)<>1 then 
		Error("The sequence must have gcd equal to one\n");
	fi;
	if len=2 then
		return y^(l[1])-x^(l[2]);
	fi;

	e:=List([1..len-1],k->Gcd(l{[1..k]})/Gcd(l{[1..k+1]}));
	g:=[]; g[1]:=x; g[2]:=y; 
	for k in [2..len] do
		d:=Gcd(l{[1..k-1]});
		dd:=Gcd(l{[1..k]});
		ll:=l{[1..k-1]}/d;
		fact:=First(FactorizationsIntegerWRTList(l[k]/dd, ll),ff->ForAll([2..k-1], i->ff[i]<e[i-1]));

		if fact=fail then 
			Error("The sequence is not valid\n");
		fi; 
		if fact=fail then
			Error("The sequence is not valid\n");
		fi;
		g[k+1]:=g[k]^e[k-1]-Product(List([1..k-1], i->g[i]^fact[i]));
		Info(InfoNumSgps,2,"Temporal curve: ",g[k+1]);
	od;
	return g[Length(g)];
end);

#################################################################
##
#F SemigroupOfValuesOfCurve_Local(arg)
## Computes the semigroup of values of R=K[pols],
## that is, the set of possible order of series in this ring
## pols is a set of polynomials in a single variable
## The semigroup of values is a numerical semigroup if l(K[[x]]/R) is finite
## If this length is not finite, the output is fail
## If the second argument "basis" is given, then the output is a basis B of
## R such that o(B) minimally generates o(R), and it is reduced
## If the second argument is an integer, then the output is a polynomial f in R
## with o(f) that value (if there is none, then the output is fail)
## Implementation based in [AGSM14]
###########################################################
InstallGlobalFunction(SemigroupOfValuesOfCurve_Local, function(arg)
    local G, F, n, T, max, s, d, newG, c, kernel,var, tomoniclocal, order, subduction, t,tt, TT, narg,val,facts,pols, msg,reduce;

    ### the order of the series (polynomial)
    order:=function(p)
        local cl;

        if IsInt(p) or IsZero(p) then
            return 0;
        fi;

        cl:=CoefficientsOfUnivariatePolynomial(p);
        return First([1..Length(cl)], i->cl[i]<>0)-1;
    end;

    #### transforms to monic
    tomoniclocal:=function(p)
        local cl;
        if IsZero(p) then
            return 0*X(Rationals,var);
        fi;
        if IsConstantRationalFunction(p) or IsRat(p) then
            return 1;
        fi;
        cl:=CoefficientsOfUnivariatePolynomial(p);
        return p/First(cl,i->i<>0);
    end;

    #### subduction: tries to express p as a polynomial in pols
    #### pols are supposed to be monic
    subduction:=function(p)
        local initial, facts, ord;

        ord:=order(p);

        if d<>1 and (ord mod d)<>0 then
          return p;
        fi;

        if order(p)=0 then
            return 0*X(Rationals,var);
        fi;

        if order(p)>c then
          if d=1 then
            return 0*X(Rationals,var);
          else
            return p;
          fi;
        fi;

        initial:=List(F,order);

        facts:=FactorizationsIntegerWRTList(order(p), initial);
        if facts=[] then
            return p;
        fi;

        Info(InfoNumSgps,2,"Reducing ",p, " to ",
             tomoniclocal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));

        return subduction(tomoniclocal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));
    end;

    #### reduces the elements in the minimal basis F, p is monic

    reduce:=function(p)
        local ords, cfs, cf, fact, reduced, head, tail;

        head:=X(Rationals,var)^order(p);
        tail:=p-head;

        ords:=List(F,order);

        repeat
            reduced:=false;
            #Print(p," ");

            cfs:=CoefficientsOfUnivariatePolynomial(tail);
            cf:=First([1..Length(cfs)], i->(cfs[i]<>0) and ((i-1) in s));


            if cf<>fail then
                fact:=FactorizationsIntegerWRTList(cf-1,ords)[1];
                tail:=(tail-cfs[cf]*Product(List([1..Length(F)],i->F[i]^(fact[i])))) mod X(Rationals,var)^c;
                cfs:=CoefficientsOfUnivariatePolynomial(tail);
            else
                reduced:=true;
            fi;
        until reduced;
        return head+tail;


    end;


    #### computes the relations among the polynomials in pols
    kernel:=function(pols)
        local  p,  msg,  ed,  mp, minp, eval, candidates, c, pres, rclass, exps;


        eval:=function(pair)
            local m1,m2;
            m1:=tomoniclocal(Product(List([1..ed],i-> pols[i]^pair[1][i])));
            m2:=tomoniclocal(Product(List([1..ed],i-> pols[i]^pair[2][i])));
            return tomoniclocal(-m1+m2);
        end;

        msg:=List(pols,order);
        ed:=Length(msg);
        if ed=0 then
            return [];
        fi;
        minp:=GeneratorsOfKernelCongruence(List(msg,x->[x]));
        Info(InfoNumSgps,2,"The exponents of the binomials of the kernel are ",minp);
        # Contrary to the global case, here it is faster not to compute a minimal presentation
        # candidates:=Set(minp, x->x[1]*msg);
        # pres:=[];
        # for c in candidates do
        #     exps:=FactorizationsIntegerWRTList(c,msg);
        #     rclass:=RClassesOfSetOfFactorizations(exps);
        #     if Length(rclass)>1 then
        #         pres:=Concatenation(pres,List([2..Length(rclass)], 
        #                       i->[rclass[1][1],rclass[i][1]]));
        #     fi;
        # od;
        return List(minp,p->eval(p));
    end; #end of kernel

    narg:=Length(arg);

    if narg=2 then
        pols:=arg[1];
        val:=arg[2];
        if not(IsInt(val)) and not(val="basis") then
           Error("The second argument must be an integer or the string 'basis'");
        fi;
    fi;
    if narg=1 then
        pols:=arg[1];
    fi;

    if narg>2 then
        Error("Wrong number of arguments (two or one)");
    fi;



    if not(IsHomogeneousList(pols)) then
        Error("The argument must be a list of polynomials.");
    fi;

    if not(ForAll(pols, IsUnivariatePolynomial)) then
        Error("The argument must be a list of polynomials.");
    fi;
    if Length(Set(pols, IndeterminateOfLaurentPolynomial))<>1 then
        Error("The arguments must be polynomials in the same variable; constants not allowed nor the empty list.");
    fi;

    var:=IndeterminateNumberOfLaurentPolynomial(pols[1]);

    F:=ShallowCopy(pols);
    Sort(F,function(a,b) return order(a)< order(b); end);
    F:=List(F,tomoniclocal);
    G:=List(F,order);
    n:=0;

    while true do
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        c:=d*ConductorOfNumericalSemigroup(s);
        T:=kernel(F);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"The kernel evaluated in the polynomials is ",T);
        T:=Set(T,subduction);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"After subduction: ",T);
        if Gcd(G) = 1 then
            T:=Filtered(T, t->order(t)<c);
        fi;

        if T=[] or F=Union(F,T) then
            d:=Gcd(G);
            if d=1 then
                s:=NumericalSemigroup(G);
                if narg=1 then
                    return s;
                fi;
                msg:=MinimalGeneratingSystem(s);
                F:=Filtered(F,f->order(f) in msg);
                if val="basis" then
                    return List(F,reduce);
                fi;

                if IsInt(val) then
                    Info(InfoNumSgps,2,"The generators of the algebra are ",F);
                    facts:=FactorizationsIntegerWRTList(val,List(F,order));
                    if facts=[] then
                        return fail;
                    fi;
                    return reduce(Product(List([1..Length(facts[1])],i->F[i]^facts[1][i])));
                fi;
            fi;
            Info(InfoNumSgps,2,"The monoid is not a numerical semigroup and it is generated by ",
                 G);
            #d*MinimalGeneratingSystem(NumericalSemigroup(G/d)));
            #return fail;
        fi;
        Info(InfoNumSgps,2,"Adding ",T," to my list of polynomials");
        F:=Union(F,T);
        newG:=Set(T,order);
        G:=Union(G,newG);
        Info(InfoNumSgps,2,"The set of possible values uptates to ",G);
        n:=n+1;
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        Info(InfoNumSgps,2,"Small elements ",d*SmallElements(s));
    od;

    return fail;
end);
        
#################################################################
##
#F SemigroupOfValuesOfCurve_Global(arg)
## Computes the semigroup of values of R=K[pols],
## that is, the set of possible degrees of polynomials in this ring
## pols is a set of polynomials in a single variable
## The semigroup of values is a numerical semigroup if l(K[x]/R) is finite
## If this length is not finite, the output is fail
## If the second argument "basis" is given, then the output is a basis B of
## R such that deg(B) minimally generates deg(R), and it is reduced
## If the second argument is an integer, then the output is a polynomial f in R
## with deg(f) that value (if there is none, then the output is fail)
## Implementation based in [AGSM14]
###########################################################
InstallGlobalFunction(SemigroupOfValuesOfCurve_Global, function(arg)
    local G, F, n, T, max, s, d, newG, c, kernel,var, tomonicglobal, degree, subduction, pols, val, narg, degs, facts, reduce, msg;

    ### the degree of the polynomial
    degree:=function(p)
        return DegreeOfUnivariateLaurentPolynomial(p);

    end;

    #### transforms to monic
    tomonicglobal:=function(p)
        if IsZero(p) then
            return 0*X(Rationals,1);
        fi;
        return p/LeadingCoefficient(p);
    end;

    #### subduction: tries to express p as a polynomial in pols
    #### pols are supposed to be monic
    subduction:=function(p)
        local initial, facts;


        if IsZero(p) then
            return p;
        fi;

        if degree(p)=0 then
            return 0*X(Rationals,1);
        fi;

        initial:=List(F,degree);

        facts:=FactorizationsIntegerWRTList(degree(p), initial);
        if facts=[] then
            return p;
        fi;

        Info(InfoNumSgps,2,"Reducing ",p, " to ",
             tomonicglobal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));

        return subduction(tomonicglobal(
          p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));
    end;

    #### reduces the elements in the minimal basis F, p is monic

    reduce:=function(p)
    local degs, cfs, cf, fact, reduced, head, tail;

    head:=X(Rationals,var)^degree(p);
    tail:=p-head;

    degs:=List(F,degree);

    repeat
      reduced:=false;
      #Print(p," ");

      cfs:=CoefficientsOfUnivariatePolynomial(tail);
      cf:=First([1..Length(cfs)], i->(cfs[i]<>0) and ((i-1) in s));
      #Error("blabla");
      #Print("cf",cf,"\n");
      if cf<>fail then
        fact:=FactorizationsIntegerWRTList(cf-1,degs)[1];
        tail:=(tail-cfs[cf]*Product(List([1..Length(F)],i->F[i]^(fact[i]))));
        cfs:=CoefficientsOfUnivariatePolynomial(tail);
      else
        reduced:=true;
      fi;
    until reduced;

    return head+tail;
    end;


    #### computes the relations among the polynomials in pols
    kernel:=function(pols)
        local  p,  msg,  ed,  mp, minp, eval, candidates, c, pres, rclass, exps;

        eval:=function(pair)
            local m1,m2;
            m1:=tomonicglobal(Product(List([1..ed],i-> pols[i]^pair[1][i])));
            m2:=tomonicglobal(Product(List([1..ed],i-> pols[i]^pair[2][i])));
            return tomonicglobal(-m1+m2);
        end;

        msg:=List(pols,degree);
        ed:=Length(msg);
        if ed=0 then
            return [];
        fi;

        minp:=GeneratorsOfKernelCongruence(List(msg,x->[x]));
        Info(InfoNumSgps,2,"The exponents of the binomials of the kernel are ",minp);
        candidates:=Set(minp, x->x[1]*msg);
        pres:=[];
        for c in candidates do
            exps:=FactorizationsIntegerWRTList(c,msg);
            rclass:=RClassesOfSetOfFactorizations(exps);
            if Length(rclass)>1 then
                pres:=Concatenation(pres,List([2..Length(rclass)], 
                              i->[rclass[1][1],rclass[i][1]]));
            fi;
        od;
        return List(pres,p->tomonicglobal(eval(p)));

    
    end; #end of kernel

    narg:=Length(arg);

    if narg>2 then
        Error("Wrong number of arguments (two or one)");
    fi;
    if narg=1 then
        pols:=arg[1];
    fi;

    if narg=2 then
      pols:=arg[1];
      val:=arg[2];
      if not(IsInt(val)) and not(val="basis") then
        Error("The second argument must be an integer or the string 'basis'");
      fi;
    fi;

    if not(IsHomogeneousList(pols)) then
        Error("The argument must be a list of polynomials.");
    fi;

    if not(ForAll(pols, IsUnivariatePolynomial)) then
        Error("The argument must be a list of polynomials.");
    fi;
    if Length(Set(pols, IndeterminateOfLaurentPolynomial))<>1 then
        Error("The arguments must be polynomials in the same variable; constants not allowed nor the empty list.");
    fi;

    var:=IndeterminateNumberOfLaurentPolynomial(pols[1]);

    F:=ShallowCopy(pols);
    Sort(F,function(a,b) return degree(a)< degree(b); end);
    F:=List(F,tomonicglobal);
    G:=List(F,degree);
    n:=0;

    while true do
        T:=kernel(F);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"The kernel evaluated in the polynomials is ",T);
        T:=Set(T,subduction);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"After subduction: ",T);
        if Gcd(G) = 1 then
            s:=NumericalSemigroup(G);
            c:=ConductorOfNumericalSemigroup(s);
            T:=Filtered(T, t->degree(t)<c);
        fi;

        if T=[] or F=Union(F,T) then
            d:=Gcd(G);
            if d=1 then
              s:=NumericalSemigroup(G);
              if narg=1 then
              return s;
              fi;
              msg:=MinimalGeneratingSystem(s);
              F:=Filtered(F,f->degree(f) in msg);
              if val="basis" then
                return List(F,reduce);
              fi;

              if IsInt(val) then
                Info(InfoNumSgps,2,"The generators of the algebra are ",F);
                facts:=FactorizationsIntegerWRTList(val,List(F,degree));
                if facts=[] then
                  return fail;
                fi;
                return reduce(Product(List([1..Length(facts[1])],i->F[i]^facts[1][i])));
              fi;
            fi;
            Info(InfoNumSgps,2,"The monoid is not a numerical semigroup and it is generated by ",
                 G);
            #d*MinimalGeneratingSystem(NumericalSemigroup(G/d)));
            return fail;
        fi;
        Info(InfoNumSgps,2,"Adding ",T," to my list of polynomials");
        F:=Union(F,T);
        newG:=Set(T,degree);
        G:=Union(G,newG);
        if narg=2 and val in G then
            return First(F,f->degree(f)=val);
        fi;

        Info(InfoNumSgps,1,"The set of possible values uptates to ",G);
        n:=n+1;
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        Info(InfoNumSgps,2,"Small elements ",d*SmallElements(s));
    od;
    return fail;
end);
    
