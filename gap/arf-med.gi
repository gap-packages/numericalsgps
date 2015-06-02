#############################################################################
##
#W  arf-med.gi              Manuel Delgado <mdelgado@fc.up.pt>
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
InstallGlobalFunction(ArfNumericalSemigroupClosure, function(arg)
    local   set,  min,  A,  i,  MIN,  small;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else 
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    set := Set(set);    
    min := Minimum(set);
    if min <= 0 then
        Error("The elements of the list must be positive integers");
    fi;

    A := [set];
    i := 1;
    while true do
        if (1 in A[i]) then
            MIN := List(A, x -> Minimum(x));
            small := List([0..Length(MIN)], i -> Sum(MIN{[1..i]}));
            return NumericalSemigroupBySmallElements(small);
        fi;

        A[i+1] := Union([min],Difference(Set(A[i], x -> x-min),[0]));
        i := i+1;
        min := Minimum(A[i]);
    od;
end);

#####################################################################
##
#A IsArfNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is an Arf-semigroup and false otherwise
## 
#####################################################################
InstallMethod(IsArfNumericalSemigroup,
        "Tests if a Numerical Semigroup is an Arf-semigroup",
        [IsNumericalSemigroup],
        function(s)
    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup");
    fi;
    return (s = ArfNumericalSemigroupClosure(s));
end);



#####################################################################
##
#A MinimalArfGeneratingSystemOfArfNumericalSemigroup(s)
##
## The argument s is an Arf numerical semigroup
## returns the minimal Arf-generating system of s. 
## 
#############################################################################
InstallMethod(MinimalArfGeneratingSystemOfArfNumericalSemigroup,
        "Returns the minimal Arf-generating system of an Arf-semigroup",
        [IsNumericalSemigroup],
        function(s)
    local   gen,  len,  ngen;

    if not IsArfNumericalSemigroup(s) then
        Error("s must be an Arf numerical semigroup");
    fi;
    gen := MinimalGeneratingSystemOfNumericalSemigroup(s);

    len := Length(gen);
    if len = 2 then
        return gen;
    fi;

    while len >= 2 do
        ngen  := Difference(gen, [gen[len]]);
        if Gcd(ngen) = 1 and s = ArfNumericalSemigroupClosure(ngen) then
            gen := ngen;
            len := len - 1;
        else
            len := len -1;
        fi;
    od;
    return gen;
end);

#####################################################################
##
#F ArfNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## Returns the set of Arf numerical semigroups with Frobenius number f 
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
#############################################################################
InstallGlobalFunction(ArfNumericalSemigroupsWithFrobeniusNumber, function(f)
	local par2sem, testArfSeq, arfsequences;

    if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;
	
	if f=0 or f<-1 then
		return [];
	fi;
	if f=-1 then 
		return [NumericalSemigroup(1)];
	fi;
	#transforms a partition list of an element to the set of sums 
	# which will correspond with the set of small elements of the semigroup
	par2sem:=function(l)
		local n, sm, i;

		sm:=[0];
		n:=0;
		for i in l do;
			n:=n+i;
			Add(sm,n);
		od;

		return sm;

	end;

	#this function tests if a partition l is an Arf sequence
	testArfSeq:=function(l)
		local i, lt, pl, size;

		if 1 in l then
			return false;
		fi;

		size:=Length(l);

		for i in [1..Length(l)-1] do
			lt:=l{[i+1..size]};
			pl:=par2sem(lt);
			#Print(l[i]," ",pl,"\n");
			if (l[i]<Maximum(pl)) and not(l[i] in pl) then
				return false;
			fi;
		od;
		return true;
	end;

	# computes all Arf sequences with sumset equal n+1 (the conductor)
	# and translate them to small elements in the semigroup
	# to this end we compute  all partitions inspired in 
	# Algorithm 3.1 of Jerome Kelleher, Barry O'Sullivan, Generating All Partitions: A 
	# Comparison Of Two Encodings  arXiv:0909.2331
	arfsequences:=function(n)
		local x,y,k, a, ra, l; 

		l:=Set([]);
		k:=2; a:=[0,n+1];
		while k<>1 do
			y:=a[k]-1;
			k:=k-1;
			x:=a[k]+1;
			if x=1 then  #to avoid ones
				x:=2;
				y:=y-1;
			fi;
			while (x<=y)  do
				a[k]:=x;
				y:=y-x;
				k:=k+1;
			od;
			a[k]:=x+y;
			ra:=Reversed(a{[1..k]});
			if testArfSeq(ra) then 
				Add(l,par2sem(ra));
			fi;
		od;
	
		return l;
	end;
	
	return List(arfsequences(f),NumericalSemigroupBySmallElementsNC);
end);


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
InstallMethod(IsMEDNumericalSemigroup,
        "Tests if a numerical semigroup is a MED-semigroup",
        [IsNumericalSemigroup],
        function(s)
    local e, m;
    m := MultiplicityOfNumericalSemigroup(s);
    e := Length(MinimalGeneratingSystemOfNumericalSemigroup(s));
    return e = m;
end);


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
InstallGlobalFunction(MEDNumericalSemigroupClosure, function(arg)
    local   set,  min,  A,  small;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else 
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    set := Set(set);
    min := Minimum(set);
    if min <= 0 then
        Error("The elements of the list must be positive integers");
    fi;

    A := set - min;
    A[1] := min;
    small := Union([0], min + 
                   SmallElementsOfNumericalSemigroup(NumericalSemigroup(A)));
    return NumericalSemigroupBySmallElements(small);
end);

#####################################################################
##
#A MinimalMEDGeneratingSystemOfMEDNumericalSemigroup(s)
##
## The argument s is a MED numerical semigroup
## returns the minimal MED-generating system of s. 
## 
#############################################################################
InstallMethod(MinimalMEDGeneratingSystemOfMEDNumericalSemigroup,
        "Returns the minimal MED-generating system of a MED numerical semigroup",
        [IsNumericalSemigroup],
        function(s)
    local   gen,  len,  ngen;

    if not IsMEDNumericalSemigroup(s) then
        Error("s must be a MED numerical semigroup");
    fi;
    gen := MinimalGeneratingSystemOfNumericalSemigroup(s);

    len := Length(gen);
    if len = 2 then
        return gen;
    fi;

    while len >= 2 do
        ngen  := Difference(gen, [gen[len]]);
        if Gcd(ngen) = 1 and s = MEDNumericalSemigroupClosure(ngen) then
            gen := ngen;
            len := len - 1;
        else
            len := len -1;
        fi;
    od;
    return gen;
end);

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
InstallGlobalFunction(SaturatedNumericalSemigroupClosure, function(arg)
    local   set,  gen,  min,  ne,  edim,  dis,  small,  i,  kjs;

    if Length(arg) = 1 and IsNumericalSemigroup(arg[1]) then
        if HasMinimalGeneratingSystemOfNumericalSemigroup(arg[1]) then
            set := MinimalGeneratingSystemOfNumericalSemigroup(arg[1]);
        else
            set := GeneratorsOfNumericalSemigroup(arg[1]);
        fi;
    elif Length(arg) = 1 then
        if  Gcd(arg[1]) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg[1];
    else 
        if  Gcd(arg) <> 1 then
            Error("The greatest common divisor is not 1");
        fi;
        set := arg;
    fi;
    gen := Set(set);    
    min := gen[1];
    ne := Maximum(gen);
    if not IsPosInt(min) then
        Error("The elements of the list must be positive integers");
    fi;
    edim := Length(gen); #embedding dimension
    dis := List([1..edim], i -> Gcd(gen{[1..i]}));

    small := Set([0]);
    for i in [1..edim-1] do
        kjs := 0;
        if (dis[i] = 1) then
            return NumericalSemigroupBySmallElements(Union(small,[gen[i]]));
        fi;
        while(gen[i] + kjs*dis[i] < gen[i+1]) do
            AddSet(small,gen[i]+kjs*dis[i]);
            kjs := kjs+1;
        od;
    od;
    return NumericalSemigroupBySmallElements(Union(small,[ne]));
end);

#####################################################################
##
#A IsSaturatedNumericalSemigroup(s)
##
## The argument s is a numerical semigroup
## returns true if s is a saturated-semigroup and false otherwise
## 
#####################################################################
InstallMethod(IsSaturatedNumericalSemigroup,
        "Tests if a Numerical Semigroup is a saturated semigroup",
        [IsNumericalSemigroup],
        function(s)
    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup");
    fi;
    return (s = SaturatedNumericalSemigroupClosure(s));
    
end);
 
#####################################################################
##
#F SaturatedNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument f is an integer
## returns the the set of saturated numerical semigroups with Frobenius number f
## as explained in the preprint
##    Rosales et al., Arf numerical semigroups with given genus and Frobenius number
#############################################################################
InstallGlobalFunction(SaturatedNumericalSemigroupsWithFrobeniusNumber,function(f)
	local alg23, alg24, satpart, listsatseq, L, C, Ll, l, t, satsystem, i;

	if(not(IsInt(f))) then
		Error("The argument must be an integer.\n");
    fi;
	
	if f=0 or f<-1 then
		return [];
	fi;
	if f=-1 then 
		return [NumericalSemigroup(1)];
	fi;

	#returns the set nonnegative integer of solutions x
	# with l.x=c; l is a (saturated) sequence 
	# l[i+1]|l[i], l[i+1]<>l[i], l[Length(l)]=1
	satpart:=function(l,c)
		local sols, len, i, j, sol, next, cand;

		len:=Length(l);
	 	sol:=[];
		for i in [1..len-1] do Add(sol, 0); od;
		sol[len]:=c;
		sols:=[sol];

		while true do
			j:=First(Reversed([1..len-1]), i-> sol{[i+1..len]}*l{[i+1..len]}>=l[i]);
			if j=fail then
				return sols;
			fi;
			next:=[];
			for i in [1..j-1] do next[i]:=sol[i]; od;
			next[j]:=sol[j]+1;
			for i in [j+1..len-1] do next[i]:=0; od;
			next[len]:=sol{[j+1..len]}*l{[j+1..len]}-l[j];
			sol:=next;#sol:=ShallowCopy(next);
			Add(sols, sol);
		od; 	
	
	end;

	#returns saturated semigroups associated to a saturaded sequence
	# with Frobenius number f
	listsatseq:=function(d, f)
		local c, l, ok, sum, len, ones, i;

		l:=[];
		ok:=false;
		sum:=Sum(d);
		len:=Length(d);

		if (f+1>=sum) and (Gcd(f+1,d[len-1])=1) and (((f+1) mod d[len-1])<>1) then 
			ok:=true;
			c:=f+1-sum;
		fi;
	
		if (f+2>=sum) and (((f+2) mod d[len-1])=1) then 
			ok:=true;
			c:=f+2-sum;
		fi;

		if not(ok) then 
			return [];
		fi;
		ones:=[];
		for i in [1..len] do ones[i]:=1; od;

		l:=satpart(d,c);
		l:=l+ones;
		l:=Filtered(l, t-> ForAll([1..len-1], i-> Gcd(d[i]/d[i+1],t[i+1])=1));	
		return l;

	end;

	#implements Algorithm 23
	alg23:=function(f)
		local tot, A, AA, cand, d, x, dd;

		cand:=Filtered([2..f], x-> Gcd(f+1,x)=1 and f mod x<> 0);
		A:=List(cand, x-> [x,1]);
		tot:=A;
	
		while (A<>[]) do
			AA:=[];
			for d in A do
				for x in [2..Int((f+1-Sum(d))/d[1])] do
					dd:=ShallowCopy(d);
					dd[1]:=x*d[1];
					dd{[2..Length(d)+1]}:=d;	
					Add(AA, dd);
				od;
			od;
			A:=AA;
			tot:=Union(tot,A);
		od;

		return tot; 

	end;

	#implements Algorithm 24
	alg24:=function(f)
		local tot, A, AA, cand, d, x, dd;

		cand:=Difference(DivisorsInt(f+1),[1]);
		A:=List(cand, x-> [x,1]);
		tot:=A;
	
		while (A<>[]) do
			AA:=[];
			for d in A do
				for x in [2..Int((f+2-Sum(d))/d[1])] do
					dd:=ShallowCopy(d);
					dd[1]:=x*d[1];
					dd{[2..Length(d)+1]}:=d;	
					Add(AA, dd);
				od;
			od;
			A:=AA;
			tot:=Union(tot,A);
		od;

		return tot; 

	end;
	#main 
	L:=Union(alg23(f),alg24(f));
	C:=[];
	for l in L do 
		Ll:=listsatseq(l,f);
		for t in Ll do 
			satsystem:=[];
			satsystem[1]:=l[1];
			for i in [2..Length(l)] do
				satsystem[i]:=l{[1..i]}*t{[1..i]};
			od;			
			Add(C,satsystem);
		od;		
	od;

	return Set(C, SaturatedNumericalSemigroupClosure);
end);


