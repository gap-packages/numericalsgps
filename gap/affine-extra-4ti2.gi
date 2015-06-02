#############################################################################
##
#W  affine-extra-4ti2.gi            
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################
InstallOtherMethod(PrimitiveElementsOfAffineSemigroup,
        "Computes the set of primitive elements of an affine semigroup",
        [IsAffineSemigroup],4, 
        function(a)
    local dir, filename, exec, filestream, matrix,
				 facs, mat, trunc, ls;
    
    ls:=GeneratorsOfAffineSemigroup(a);
    
    dir := DirectoryTemporary();
    filename := Filename( dir, "gap_4ti2_temp_matrix" );

	mat:=TransposedMat(ls);
    4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
    exec := IO_FindExecutable( "graver" );
    filestream := IO_Popen2( exec, [ filename ]);
    while IO_ReadLine( filestream.stdout ) <> "" do od;
    matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".gra" ) );

    trunc:=function(ls)
		return List(ls, y->Maximum(y,0));
	end;

	matrix:=Set(matrix,trunc);
    return Union(Set(matrix, x->x*ls),ls);
end);


InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousEquations,
        "Computes a Hilbert basiss of a systemd of linear Diophantine equations, some eventually in congruences.",
        [IsMatrix,IsHomogeneousList],4,
        function(ls,md)
    local  homogeneous, withCongruences;
    
    homogeneous:= function(l)
        local  dir, filename, exec, filestream, matrix,mat,sign;

        Info(InfoNumSgps,2,"Using 4ti2 for Hilbert.");

        if not(IsRectangularTable(l)) then 
            Error("The argument must be a matrix.");
        fi;
        if not(IsInt(l[1][1])) then
            Error("The matrix must be of integers.");
        fi;

        dir := DirectoryTemporary();
        filename := Filename( dir, "gap_4ti2_temp_matrix" );

	mat:=l;
        sign:=[List(l[1],_->1)];
        #Print(mat,"\n");
        4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
        4ti2Interface_Write_Matrix_To_File( sign, Concatenation( filename, ".sign" ) );
        exec := IO_FindExecutable( "zsolve" );
        filestream := IO_Popen2( exec, [ filename ]);
        while IO_ReadLine( filestream.stdout ) <> "" do od;
        matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".zhom" ) );
        return matrix;
        
    end; 

  withCongruences:=function(ls,md)
      local l,n,m,diag,dim,d, hil, zero, leq;
      
      leq:= function(v1,v2)
          local v;
          v:=v2-v1;
          return (First(v,n->n<0)=fail);
      end;

      if not(IsRectangularTable(ls)) then
          Error("The first argument must be a matrix.");
      fi;

      if not(IsListOfIntegersNS(md)) or ForAny(md, x->not(IsPosInt(x))) then
          Error("The second argument must be a list of positive integers.");
      fi;

      n:=Length(ls);
      dim:=Length(ls[1]);
      m:=Length(md);
      if m>n then 
          Error("There are more modulus than equations.");
      fi;

      diag:=Concatenation(md,List([1..n-m],_->0));
      d:=DiagonalMat(diag);
      l:=TransposedMat(Concatenation(TransposedMat(ls),d,-d));
      zero:=List([1..dim],_->0);

      hil:=Difference(List(homogeneous(l), x->x{[1..dim]}),[zero]);
      return hil;

      return Filtered(hil, y->Filtered(hil,x->leq(x,y))=[y]);
  end;
  ## end of local functions ...

  #ls := arg[1][1];
  #md := arg[1][2];
  if md = [] then
      return homogeneous(ls);
  else
      return withCongruences(ls,md);

  fi;
    
end);

InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousInequalities,
        "Computes a Hilbert basis of l*x>=0, x>=0",
        [IsMatrix],4,
        function(l)
    local  dir, filename, exec, filestream, matrix,mat,sign,rel;

    Info(InfoNumSgps,2,"Using 4ti2 for Hilbert.");

    if not(IsRectangularTable(l)) then 
        Error("The argument must be a matrix.");
    fi;
    if not(IsInt(l[1][1])) then
        Error("The matrix must be of integers.");
    fi;

    dir := DirectoryTemporary();
    filename := Filename( dir, "gap_4ti2_temp_matrix" );

    mat:=l;
    sign:=[List(l[1],_->1)];
    rel:=[List(l[1],_->">")];
    #Print(mat,"\n");
    4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
    4ti2Interface_Write_Matrix_To_File( sign, Concatenation( filename, ".sign" ) );
    4ti2Interface_Write_Matrix_To_File( rel, Concatenation( filename, ".rel" ) );
    exec := IO_FindExecutable( "zsolve" );
    filestream := IO_Popen2( exec, [ filename ]);
    while IO_ReadLine( filestream.stdout ) <> "" do od;
    matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".zhom" ) );
    return matrix;
    
end); 


InstallOtherMethod(FactorizationsVectorWRTList,
        "Computes the factorizations of v in terms of the elments in ls",
        [IsHomogeneousList,IsMatrix],4,
        function(v,l)
    local  dir, filename, exec, filestream, matrix,mat,rhs,sign;

    Info(InfoNumSgps,2,"Using 4ti2 for factorizations.");
    
    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;
    
    if not(IsInt(l[1][1])) then
        Error("The matrix must be of integers.");
    fi;

    dir := DirectoryTemporary();
    filename := Filename( dir, "gap_4ti2_temp_matrix" );

    mat:=TransposedMat(l);
    sign:=[List(mat[1],_->1)];
    rhs:=[v];
    #Print(mat,"\n");
    4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
    4ti2Interface_Write_Matrix_To_File( sign, Concatenation( filename, ".sign" ) );
    4ti2Interface_Write_Matrix_To_File( rhs, Concatenation( filename, ".rhs" ) );
    exec := IO_FindExecutable( "zsolve" );
    filestream := IO_Popen2( exec, [ filename ]);
    while IO_ReadLine( filestream.stdout ) <> "" do od;
    matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".zinhom" ) );
    return matrix;
    
end); 


InstallOtherMethod(MinimalPresentationOfAffineSemigroup,
        "Computes a minimimal presentation of the affine semigroup",
        [IsAffineSemigroup],3,
        function(a)
    local gens, positive, gr, candidates, pres, rclass,exps, c;
    
    positive:=function(x)
        local p,i;
        
        p:=[];
        for i in [1..Length(x)] do
            p[i]:=Maximum(x[i],0);
        od;

        return p;
    end;
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    
    gens:=GeneratorsOfAffineSemigroup(a);
    
    gr:=4ti2Interface_groebner_matrix(gens);
    Info(InfoNumSgps,2,"4ti output:",gr);
    
    candidates:=Set(gr,q->positive(q));
    candidates:=Set(candidates,c->c*gens);
    Info(InfoNumSgps,2, "Candidates to Betti elements",candidates);
    pres:=[];
    for c in candidates do
        exps:=FactorizationsVectorWRTList(c,gens);
        rclass:=RClassesOfSetOfFactorizations(exps);
        if Length(rclass)>1 then
            pres:=Concatenation(pres,List([2..Length(rclass)], 
                          i->[rclass[1][1],rclass[i][1]]));
        fi;
    od;
    return pres;
    
    
end);



#####################################################################
# Computes the omega-primality of v in the affine semigroup a
#####################################################################
InstallOtherMethod(OmegaPrimalityOfElementInAffineSemigroup,
        "Computes the omega-primality of v in the affine semigroup a",
        [IsHomogeneousList,IsAffineSemigroup],4,
        function(v,a)
    local  ls, n, mat,extfact,par,tot,le;
    
    le:=function(a,b)  #ordinary partial order
    	return ForAll(b-a,x-> x>=0);
    end;
    
    if not(IsAffineSemigroup(a)) then
        Error("The second argument must be an affine semigroup");
    fi;
        
    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;
    
    if not(ForAll(v, x-> x>=0)) then
        Error("The first argument must be a list of on nonnegative integers.");		
    fi;
	
    ls:=GeneratorsOfAffineSemigroup(a);
    n:=Length(ls);
    mat:=TransposedMat(Concatenation(ls,-ls,[-v]));

    if not(IsHomogeneousList(mat)) then
        Error("The first argument has not the dimension of the second.");
    fi;
    
    extfact:=FactorizationsVectorWRTList(v,Concatenation(ls,-ls));
    
    par:=Set(extfact, f->f{[1..n]});
    tot:=Filtered(par, f-> Filtered(par, g-> le(g,f))=[f]);
    Info(InfoNumSgps,2,"Minimals of v+ls =",tot);
    if tot=[] then 
        return 0;      
    fi;
    
    return Maximum(Set(tot, Sum));

    
end);
