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
    return Set(matrix, x->x*ls);
end);


InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousEquations,
        "Computes a Hilbert basiss of a systemd of linear Diophantine equations, some eventually in congruences.",
        [IsMatrix,IsHomogeneousList],4,
        function(ls,md)
    local  homogeneous, withCongruences;
    
    homogeneous:= function(l)
        local  dir, filename, exec, filestream, matrix,mat,sign;

        Info(InfoAffSgps,2,"Using 4ti2 for Hilbert.");

        if not(IsMatrix(l)) then 
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

      if not(IsMatrix(ls)) then
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
