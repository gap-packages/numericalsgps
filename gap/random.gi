#############################################################################
##
#W  random.gi               Manuel Delgado <mdelgado@fc.up.pt>
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


#############################################################################
##
#F  RandomListForNS(n,a[,b])
##
##  Returns a set of length not greater than n of random integers in [a..b] 
##  whose GCD is 1.
##  It is used to create "random" numerical semigroups.
##  If b is not present, the generators are taken randomly in [1..a]
##
#############################################################################
InstallGlobalFunction(RandomListForNS, function(arg)
    local   d,  n,  a,  b,  r;
    d := 0;
    n := arg[1];
    if Length(arg) = 2 then 
        a := 1;
        b := arg[2];
    elif Length(arg) = 3 then 
        a := arg[2];
        b := arg[3];
    fi;;
    while d<>1 do    
        r := SSortedList(List([1 .. Random([1 .. n])], j -> Random([a..b])));
        d := Gcd(r);
    od;
    return r;    
end);



#############################################################################
##
#F  RandomNumericalSemigroup(n,a[,b])
##
##  Returns a "random" numerical semigroup  with no more 
##  than n generators in [a..b]. If b is not present, the generators are taken 
##  randomly in [1..a]
##
#############################################################################
InstallGlobalFunction(RandomNumericalSemigroup, function(arg)
    local   d,  n,  a,  b,  r;
    d := 0;
    n := arg[1];
    if Length(arg) = 2 then 
        a := 1;
        b := arg[2];
    elif Length(arg) = 3 then 
        a := arg[2];
        b := arg[3];
    fi;;
    return(NumericalSemigroup(RandomListForNS(n,a,b)));
end);


#############################################################################
##
#F  RandomListRepresentingSubAdditiveFunction(m, a)
##
##  Produces a list representing a subadditive function which is periodic
##  with period m (or less). When possible, the images are in [a..20*a].
##  (Otherwise, the list of possible images is enlarged.)
##
#############################################################################
InstallGlobalFunction(RandomListRepresentingSubAdditiveFunction, function(m, a)
    local i, L, r, R;
    
    if not (IsPosInt(m) and IsPosInt(a)) then
        Error("This function takes two positive integers as arguments.");
    fi;

    R := [];
    L := [a..20*a];
    while Length(R) < m - 1 and L <> [] do
        r := RandomList(L);
        if ForAll(R, i-> not i mod m = r mod m) then
            if RepresentsPeriodicSubAdditiveFunction(Concatenation(R,[r,0])) then
                Add(R,r);
            fi;
        fi;
        L := Difference(L,[r]);
        if L = [] then
            L := [20*a+1..2*(a+1)];
            a := a+1;
        fi;
    od;
    Add(R,0);
    return(R);
end);



#############################################################################
##
#F  RandomProportionallyModularNumericalSemigroup(k[,m])
##
##  Produces a "random" proportionally modular semigroup with a <= k and multiplicity at least m.
##
#############################################################################
InstallGlobalFunction(RandomProportionallyModularNumericalSemigroup, function(arg)
    local   k,  m,  a,  b,  c,  l;
    k := arg[1];
    m := 1;
    if Length(arg) = 2 then
        m := arg[2];
    fi;
        
    a := Random([2..k]);
    l := 0;
    while l <> 1 do 
        b := Random([m*a+1..5*m*a]);
        l := Gcd(a,b);
    od;
    c := Random([1..a-1]);

    return(NumericalSemigroup("propmodular",a,b,c));
end);




#############################################################################
##
#F  RandomModularNumericalSemigroup(k[,m])
##
##  Produces a "random" modular semigroup with a <= k and multiplicity at least m.
##
#############################################################################
InstallGlobalFunction(RandomModularNumericalSemigroup, function(arg)
    local   k,  m,  a,  b,  l;
    k := arg[1];
    m := 1;
    if Length(arg) = 2 then
        m := arg[2];
    fi;
        
    a := Random([2..k]);
    l := 0;
    while l <> 1 do 
        b := Random([m*a+1..5*m*a]);
        l := Gcd(a,b);
    od;
    l := Gcd(a,b);

    return(NumericalSemigroup("modular",a/l,b/l));
end);
