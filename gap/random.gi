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
#############################################################################
##
#F  NumericalSemigroupWithRandomElementsAndFrobenius(n,mult,frob)
##
##  Produces a "random" semigroup containing (at least) <n> elements greater than or equal to <mult> and less than <frob>, choosen at random. The semigroup returned has multiplicity choosen at random but no smaller than <mult> and having Frobenius number choosen at random but not greater than <frob>.
##
#############################################################################
InstallGlobalFunction(NumericalSemigroupWithRandomElementsAndFrobenius, function(n,mult,frob)
  local  fr, elts;
  
  if mult > frob then
    Info(InfoWarning,1,"The third argument must not be smaller than the second");
    
    return fail;
  fi;
  
  fr := Random([mult..frob]); 
  elts := SSortedList(List([1 .. Random([1 .. n])], j -> Random([mult..frob])));
  return NumericalSemigroupWithGivenElementsAndFrobenius(elts,fr);
end);
#############################################################################
##
#F RandomNumericalSemigroupWithGenus(g)
##
## Produces a pseudo-random numerical semigroup with genus g 
#############################################################################
InstallGlobalFunction(RandomNumericalSemigroupWithGenus, function(g)
  local  s, gaps, i, mingens, x;
  
  s := NumericalSemigroup(1);
  gaps := [];
  for i in [1..g] do
    mingens := MinimalGenerators(s);
    x := RandomList(mingens);
    AddSet(gaps,x);
    s := RemoveMinimalGeneratorFromNumericalSemigroup(x,s);
  od;
  SetGaps(s,gaps);
  return s;
end);

###############################################################################
# Random functions for affine semigroups
#############################################################################
##
#F RandomAffineSemigroupWithGenusAndDimension(g,d)
##
## Produces a pseudo-random affine semigroup with genus g in dimension d
#############################################################################
InstallGlobalFunction(RandomAffineSemigroupWithGenusAndDimension, function(g,d)
  local  s, i, mingens;
  
  s := AffineSemigroup(IdentityMat(d));
  SetGaps(s,[]); #this will force 'RemoveMinimal...' to set the gaps of the semigroups computed.
  for i in [1..g] do
    mingens := MinimalGenerators(s);
    s := RemoveMinimalGeneratorFromAffineSemigroup(RandomList(mingens),s);
  od;
  return s;
end);
###############################################################################
###############################################################################
##
#F RandomAffineSemigroup(n,d[,m])
# Returns an affine semigroup generated by a n'*d' matrix where d' (the dimension) is randomly choosen from [1..d] and n' (the number of generators) is randomly choosen from [1..n]. The entries of the matrix are randomly choosen from [0..m] (when the third argument is not present, m is taken as n'*d')
###########################################################################
InstallGlobalFunction(RandomAffineSemigroup,function(arg)
  local  rn, rd, max;

  rn := Random([1..arg[1]]);
  rd := Random([1..arg[2]]);
  if Length(arg) = 3 then
    max := arg[3];
  else
    max := rn*rd;
  fi;

  return AffineSemigroup("generators",RandomMat(rn,rd,[0..max]));

end);

###############################################################################
##
#F RandomFullAffineSemigroup(n,d[,m, string])
# Returns a full affine semigroup either given by equations or inequalities (when no string is given, one is choosen at random). The matrix is an n'*d' matrix where d' (the dimension) is randomly choosen from [1..d] and n' is randomly choosen from [1..n]. When it is given by equations, the moduli are choosen at random. The entries of the matrix (and moduli) are randomly choosen from [0..m] (when the third integer is not present, m is taken as n'*d')
###########################################################################
InstallGlobalFunction(RandomFullAffineSemigroup,function(arg)
  local  type, nums, rn, rd, max;

  if First(arg, IsString) <> fail then
    type := First(arg, IsString);
  else
    type := RandomList(["equations","inequalities"]);
  fi;

  nums := Filtered(arg,IsInt);

  rn := Random([1..nums[1]]);
  rd := Random([1..nums[2]]);
  if Length(nums) = 3 then
    max := nums[3];
  else
    max := rn*rd;
  fi;


  if type = "equations" then
    return AffineSemigroup(type,[RandomMat(rn,rd,[0..max]),RandomMat(1,rd,[0..max])[1]]);
  fi;
  return AffineSemigroup(type,RandomMat(rn,rd,[0..max]));
end);


###############################################################################
# Random functions for good semigroups
#############################################################################
##
#####################################################
#F RandomGoodSemigroupWithFixedMultiplicity:=function(m,cond)
# It produces a Good Semigroup with multiplicity m and conductor bounded by cond
#####################################################

InstallGlobalFunction(RandomGoodSemigroupWithFixedMultiplicity,
function(m,cond)
local SemiringGeneratedBy, ThirdP, AddVectors, AdmissibleElementsToAdd, Inverti, RandomVectorsInteger, CompareGS, MinimumGS, n, L, G, S, temp;

  CompareGS:=function(v,w)

    return ForAll([1..Length(v)], i->v[i]<=w[i]);
  end;

  MinimumGS:=function(v,w)
    return List([1..Length(v)],i->Minimum(v[i],w[i]));
  end;


  if CompareGS(m,cond)=false then
  Error("The first argument must to be smaller then the second one respect the partial order in N^2");
  fi;

  if Length(m)<>2 then
  Error("The first argument must to have length equal to 2");
  fi;

  if Length(cond)<>2 then
  Error("The second argument must to have length equal to 2");
  fi;

  AddVectors:=function(vs,u)
        local ScaleCond,ClosureRespectSumAndMinimimuminUnion,ags,agsc,u1,cond,condS,temp;

        ScaleCond:=function(vs,v)
          local i,ags;
          ags:=[];
          for i in v do
          ags:=Union(ags,Filtered(vs,j->Sum(i)-1=Sum(j) and CompareGS(j,i)));
          od;
          return ags;
        end;

        ClosureRespectSumAndMinimimuminUnion:=function(vs,u,cond)
            local ags,i,j,u1;
            ags:=ShallowCopy(vs);
              for i in ags do
                for j in u do
                  if not MinimumGS(i+j,cond) in ags then
                    ags:=Union(ags,[MinimumGS(i+j,cond)]);
                  fi;
                  if not MinimumGS(i,j) in ags then
                    ags:=Union(ags,[MinimumGS(i,j)]);
                  fi;
                od;
              od;
            return ags;
        end;

        ags:=ShallowCopy(Union(vs,u));
        condS:=vs[Length(vs)];
        agsc:=ClosureRespectSumAndMinimimuminUnion(ags,u,condS);
        while ags<>agsc do
          u1:=Filtered(agsc,i-> not i in ags);
          ags:=ShallowCopy(agsc);
          agsc:=ClosureRespectSumAndMinimimuminUnion(agsc,Union(u,u1),condS);
        od;
        temp:=[condS];
        while temp<>[] do
          condS:=ShallowCopy(temp);
          temp:=ScaleCond(agsc,temp);
        od;
        return Filtered(agsc,i->CompareGS(i,condS[1]));
  end;

  SemiringGeneratedBy:=function(vs,cond)
    local SemiringGeneratedByNaive,ScaleCond,S,i;


    #This function starting to the greatest element of the list vs, produce the list of possible "real conductor" decreasing by one each component of the list cond.
    #Ex: If we have ...[35,35],[35,36],[36,34],[36,35],[36,36],[37,36]. With v:=[37,36], the function return the list [[36,35],[36,36]]. With v:=[[36,35],[36,36]]
    #it returns [[35,35],[35,36],[36,34]] and so on.

    ScaleCond:=function(vs,v)
          local i,ags;
          ags:=[];
          for i in v do
          ags:=Union(ags,Filtered(vs,j->Sum(i)-1=Sum(j) and CompareGS(j,i)));
          od;
          return ags;
    end;
    

    #####################################################
    #F SemiringGeneratedByNaive:=function(vs,cond)
    ## vs is a set of vector, cond is a couple [v1,v2]
    ## The ouput is the semiring generated by vs, with conductor cond
    #####################################################

    SemiringGeneratedByNaive:=function(vs,cond)
      local Scalecond,ClosureRespectSumAndMinimimum,ags,agsc,temp;

      #This function add to vs, all the sums of two elements of vs, and all minimums of elements of vs under the conductor.
      ClosureRespectSumAndMinimimum:=function(vs,cond)
        local ags,i,j;
          ags:=ShallowCopy(vs);
          for i in [1..Length(ags)] do
            for j in [i..Length(ags)] do
              if not MinimumGS(ags[i]+ags[j],cond) in ags then
              ags:=Concatenation(ags,[MinimumGS(ags[i]+ags[j],cond)]);
              fi;
              if not MinimumGS(ags[i],ags[j]) in ags then
              ags:=Concatenation(ags,[MinimumGS(ags[i],ags[j])]);
              fi;
            od;
          od;
        return Set(ags);
      end;

      ags:=Union(ShallowCopy(vs),[cond]);
      agsc:=ClosureRespectSumAndMinimimum(ags,cond);

      while ags<>agsc do
        ags:=ShallowCopy(agsc);
        agsc:=ClosureRespectSumAndMinimimum(agsc,cond);
      od;

      temp:=[cond];
      while temp<>[] do
        cond:=ShallowCopy(temp);
        temp:=ScaleCond(agsc,temp);
      od;

      return Filtered(agsc,i->CompareGS(i,cond[1]));
    end;



    S:=SemiringGeneratedByNaive([vs[1]],cond);
    for i in [2..Length(vs)] do
    S:=AddVectors(S,[vs[i]]);
    od;
    return Union(S,[[0,0]]);
  end;

 #  ThirdP:=function(A) internal function that, given a semiring of N^2, 
 #  it finds the couples of vectors which don't satisfy the third property.
 #  The output is in the form [ [ v1, v2 , i ], where i is the index that v1 and v2 share.

  ThirdP:=function(A)
    local RemoveUnnecessary,cond,car,C,ags,ind,indd,F,i;
     
    RemoveUnnecessary:=function(v)
      local a1,a2,i;
      a1:=ShallowCopy(v[1][1]);
      a2:=ShallowCopy(v[1][2]);
      for i in [1..2] do
        if i<>v[2] then
        a1[i]:=Minimum(a1[i],a2[i]); 
        a2[i]:=Minimum(a1[i],a2[i]); 
        fi; 
      od;
      return [[a1,a2],v[2]];
    end;

    cond:=(A[Length(A)]);
    #We do the Cartesian product considering the couples of B only one time.
    car:=List(List(Filtered(Cartesian([1..Length(A)],[1..Length(A)]),i->i[1]<i[2])),k->[A[k[1]],A[k[2]]]);
    #We take the couple of different elements with a common component
    C:=Filtered(car,i->Product(i[1]-i[2])=0 and Sum(i[1]-i[2])<>0);
    ags:=[];
    
    for i in C do
      #Find the index of equal component
      ind:=Filtered([1..2],j->i[1][j]=i[2][j])[1];
        if i[1][ind]<>cond[ind] then
          indd:= 3-ind;
          #Check in in B, there are elements which satisfy third property for the couple i
          F:=Filtered(A,k1->k1[indd]=Minimum(i[1][indd],i[2][indd]) and k1[ind]>i[1][ind] and CompareGS(MinimumGS(i[1],i[2]),k1));
          if F=[] then
            ags:=Union(ags,[[i,ind]]);
          fi;
        fi;
    od;

    # If in ags there are for example, elements of the form [ [ [ 16, 18 ], [ 16, 19 ] ], 1 ],
    #[ [ [ 16, 18 ], [ 16, 20 ] ], 1 ], the second one is unnecessary because it gives the same condition of the first one
    return List(Set(List(ags,i->RemoveUnnecessary(i))),i1->First(ags,k->RemoveUnnecessary(k)=i1));
  end;

  AdmissibleElementsToAdd:=function(v,A)
      local conf,m,k,inter,l,temp;

      inter:=function(v,w)
        local ags,i,j,temp;
        if Length(v)=1 then
          ags:=[];
          for i in [v[1]..w[1]] do
            ags:=Union(ags,[[i]]);
          od;  
        else ags:=[];
          temp:=inter([v[1]],[w[1]]);
          for i in temp do
            for j in [v[2]..w[2]] do
              ags:=Union(ags,[Concatenation(i,[j])]);
            od;
          od;
        fi;
        return ags;
      end;

      #Check if t is strictly greater on the component k(the equal componet) and greater then the minimum on the different
      #component of v and w.
      conf:=function(v1,v2,s,ind)
        local a,i;
        a:=true;
        for i in [1..2] do
          if i=ind then
            a:=a and s[i]>=v1[i]+1;
          else
            a:=a and s[i]>=Minimum(v1[i],[i]);
          fi;
        od;
        return a;
      end;

      temp:=Filtered(A,k->conf(v[1][1],v[1][2],k,v[2]));
      m:=temp[1];
      for k in temp do
        m:=MinimumGS(m,k);
      od;
      l:=MinimumGS(v[1][1],v[1][2]);
      l[v[2]]:=l[v[2]]+1;
    return Filtered(inter(l,m),k->k[3-v[2]]=Minimum(v[1][1][3-v[2]],v[1][2][3-v[2]]));
  end;

  #This function ordered the output of ThirdP with the goal to optimize the speed of ...
  Inverti:=function(b)
    local k,ags,i;
    k:=List(Set(List(b,i->i[1][1][i[2]])),j->Reversed(Filtered(b,k->k[1][1][k[2]]=j)));
    ags:=[];
    for i in [1 .. Length(k)] do
    ags:=Concatenation(ags,k[i]);
    od;
    return ags;
  end;

  RandomVectorsInteger:=function(n,L)
    local a,i ;
    a:=List([1..n],i->[]);
    for i in [1..n] do
    a[i]:=List([1..Length(L)],i->Random(L[i]));
    od;
    return a;
  end;

  n:=Random([1..m[1]+m[2]-1]);
  L:=List([1..Length(cond)],i->[m[i]..cond[i]]);
  G:=Union(RandomVectorsInteger(n,L),[m]);
  S:=SemiringGeneratedBy(G,cond);
  temp:=ThirdP(S);
  while temp<>[] do
    S:=AddVectors(S,[RandomList(AdmissibleElementsToAdd(Inverti(temp)[1],S))]);
    temp:=ThirdP(S);
  od;

  return GoodSemigroupBySmallElements(S);
end);