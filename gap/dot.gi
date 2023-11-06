#############################################################################
##
#W  dot.gi                  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Andrés Herrera-Poyatos <andreshp9@gmail.com>
##
##
#Y  Copyright 2017 by Manuel Delgado and Pedro Garcia-Sanchez
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
InstallGlobalFunction(DotSplash, function(dots...)
  local str, temp_file, digraph, html, i, line, out;

  str:=function(s)
    return Concatenation("\"",String(s),"\"");
  end;
  
  # Open a temporal file
  temp_file := Filename(DirectoryTemporary(), "graph-viz.html");
  out := OutputTextFile(temp_file, false);
  SetPrintFormattingStatus(out, false);  
  
  # HTML header
  html := "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n <title>Graph Viz</title>\n";
  html := Concatenation(html, "<style>\n .content {\n display: inline-block;\n text-align: center;\n vertical-align: top;\n}\n</style></head>\n<body>\n<script  src=\"https://github.com/mdaines/viz-js/releases/download/release-viz-3.2.3/viz-standalone.js\">\n</script>\n");
  #html:=Concatenation(html, "<style>\n .content {\n display: inline-block;\n text-align: center;\n vertical-align: top;\n}\n</style></head>\n<body>\n<script  src=\"viz.js\">\n</script>\n");
  
  # # Assign an ID to each graph
  # for i in [1..Length(dots)] do
  #   line := Concatenation("<span id=", str(i), " class='content'>Graph to be displayed here (internet connection required) </span>\n");
  #   html := Concatenation(html, line);
  # od;
  
  # Draw each graph
  line := "<script>\n Viz.instance().then(function(viz) {\n";
  html := Concatenation(html, line);
  i := 1;
  for digraph in dots do
    line := Concatenation(" document.body.appendChild(viz.renderSVGElement('", NormalizedWhitespace(digraph), "', {engine: \"", DotNSEngine, "\"}));\n");
    html := Concatenation(html, line);
    line := " document.body.appendChild(document.createElement('hr'));\n";
    html := Concatenation(html, line);
    i := i+1;
  od;
  
  # End the HTML code
  line := "});\n</script>\n</body>\n</html>";
  html := Concatenation(html, line);
  
  # Draw the graph
  PrintTo(out, html);  
  CloseStream(out);
  Print("Saved to ", temp_file, "\n");
  if ARCH_IS_MAC_OS_X() then
    Exec("open ", temp_file);
  elif ARCH_IS_WINDOWS() then
    Exec("start ", temp_file);
  elif ARCH_IS_UNIX() then
    Exec("xdg-open ", temp_file);
  fi;

  return html;
end);

############################################################################
##
#F  DotBinaryRelation(br)
##  Returns a GraphViz dot which represents the binary relation br.
##  The set of vertices of the resulting graph is the source of br.
##  Edges join those elements which are related in br.
##
############################################################################
InstallGlobalFunction(DotBinaryRelation, function(br)
  local pre, element, im, output, out, str, i, d;

  str := function(i)
    return Concatenation("\"",String(i),"\"");
  end;

  if not IsBinaryRelation(br) then
    Error("The argument must be a binary relation");
  fi;
  
  # Add the header of the GraphViz code
  out := "";
  output := OutputTextString(out, true);
  AppendTo(output,"digraph  NSGraph{rankdir = TB; edge[dir=back];\n");
  
  # Add the vertices
  pre := Source(br);  
  d := NewDictionary(false, true, pre);
  i := 1;  
  for element in pre do
    AddDictionary(d, element, i);      
    AppendTo(output, i," [label=", str(element), "];\n");
    i := i+1;    
  od;
  
  # Add the edges
  i := 1;  
  for element in pre do
    for im in Image(br, [element]) do
      AppendTo(output, LookupDictionary(d, im), " -> ", i, ";\n");
    od;
    i := i+1;    
  od;
  
  AppendTo(output, "}");
  CloseStream(output);
  return out;
end);

############################################################################
##
#F HasseDiagramOfNumericalSemigroup(s, A)
##  Returns a binary relation which is the Hasse diagram of A with 
##  respect to the ordering a <= b if b - a in S.
##
############################################################################
InstallGlobalFunction(HasseDiagramOfNumericalSemigroup, function(s, A)
  local rel, p, D;
  
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
  
  # Build the binary relation and returns its Hasse diagram
  D := Domain(Set(A));
  rel := Tuples(D, 2);
  rel := Filtered(rel, p -> p[2] - p[1] in s);
  rel := List(rel, p -> DirectProductElement([p[1], p[2]]));  
  rel := BinaryRelationByElements(D, rel);  
  return HasseDiagramBinaryRelation(rel);  
end);

############################################################################
##
#F HasseDiagramOfBettiElementsOfNumericalSemigroup(s)
##  Returns a binary relation which is the Hasse diagram of the Betti
##  elements of s with respect to the ordering a <= b if b - a in S.
##
############################################################################
InstallGlobalFunction(HasseDiagramOfBettiElementsOfNumericalSemigroup, function(s)
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
    
  return HasseDiagramOfNumericalSemigroup(s, BettiElementsOfNumericalSemigroup(s));    
end);

############################################################################
##
#F HasseDiagramOfAperyListOfNumericalSemigroup(s, n)
##
############################################################################
InstallGlobalFunction(HasseDiagramOfAperyListOfNumericalSemigroup, function(s, n...)
  local a;
    
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
  
  if Length(n) = 0 then
    a := MultiplicityOfNumericalSemigroup(s);
  elif Length(n) = 1 then
    a := n[1];
  else
    Error("The number of arguments must be one or two");
  fi;
    
  return HasseDiagramOfNumericalSemigroup(s, AperyListOfNumericalSemigroup(s, a));    
end);

############################################################################
##
#F DotTreeOfGluingsOfNumericalSemigroup(s, depth...)
##  Returns a GraphViz dot that represents the tree of gluings of the
##  numerical semigroup s.
##  The tree is truncated at the given depth. If the depth is not provided,
##  then the tree is fully built.
##
############################################################################
InstallGlobalFunction(DotTreeOfGluingsOfNumericalSemigroup, function(s, depth...)
  local SystemOfGeneratorsToString, rgluings, out, output, labels, edges, index, d;

  SystemOfGeneratorsToString := function(sg)
    return Concatenation("〈 ", JoinStringsWithSeparator(sg, ", "), " 〉");
  end;
    
  if not IsNumericalSemigroup(s) then
    Error("The argument must be a numerical semigroup.\n");
  fi;
  
  if Length(depth) = 0 then
    d := -1;
  else
    d:= depth[1];      
  fi;

  # Recursively plot the gluings tree 
  rgluings := function(s, level, parent)
    local lg, label, gen1, gen2, p, son1, son2;
      
    lg := AsGluingOfNumericalSemigroups(s);    
    
    labels := Concatenation(labels, String(parent), " [label=\"", SystemOfGeneratorsToString(MinimalGenerators(s)), "\", style=filled]; \n");
    #labels := Concatenation(labels, String(parent), " [label=\"", SystemOfGeneratorsToString(MinimalGenerators(s)), "\"]; \n");
        
    if level = 0 then
      return ;
    fi;
    
    # For each possible gluing plot the gluing and the numerical semigroups associated.
    for p in lg do
      # Add the gluing 
      label := Concatenation(SystemOfGeneratorsToString(p[1])," + ", SystemOfGeneratorsToString(p[2]));
      labels := Concatenation(labels, String(index), " [label=\"", label, "\" , shape=box]; \n");
      edges := Concatenation(edges, String(parent), " -> ", String(index), "; \n");
      
      # Add the two numerical semigroups involved
      son1 := index+1;
      son2 := index+2;
      
      gen1 := p[1] / Gcd(p[1]);
      gen2 := p[2] / Gcd(p[2]);      
      
      edges := Concatenation(edges, String(index), " -> ", String(son1), "; \n");
      edges := Concatenation(edges, String(index), " -> ", String(son2), "; \n");
      
      # Call the function recursively
      index := index + 3;      
      rgluings(NumericalSemigroup(gen1), level-1, son1);      
      rgluings(NumericalSemigroup(gen2), level-1, son2);      
    od;
    
    return ;
  end;
  
  # Create the root of the tree
  labels := "";
  edges := "";  
  index := 1;
  labels := Concatenation(labels, "0", " [label=\"", SystemOfGeneratorsToString(MinimalGenerators(s)), "\"]; \n");  
  # Compute the tree
  rgluings(s, d, 0);
  
  # Prepare the output
  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"digraph  NSGraph{rankdir = TB; \n");
  AppendTo(output, labels);
  AppendTo(output, edges);
  AppendTo(output, "}");
  CloseStream(output);
  
  return out;
end);


############################################################################
##
#F DotOverSemigroupsNumericalSemigroup(s)
##  Returns a GraphViz dot that represents the Hasse diagram of 
##  oversemigroupstree of the numerical semigroup s.
##  Irreducible numerical semigroups are highlighted.
##
############################################################################
InstallGlobalFunction(DotOverSemigroupsNumericalSemigroup, function(s)
  local ov, c,i,r,n,hasse, str, output, out, SystemOfGeneratorsToString;
  
  hasse:=function(rel)
    local dom, out;
    dom:=Flat(rel);
    out:=Filtered(rel, p-> ForAny(dom, x->([p[1],x] in rel) and ([x,p[2]] in rel)));
    return Difference(rel,out);
  end;

  str := function(i)
    return Concatenation("\"",String(i),"\"");
  end;

  SystemOfGeneratorsToString := function(sg)
    return Concatenation("〈 ", JoinStringsWithSeparator(sg, ", "), " 〉");
  end;

  ov:=OverSemigroupsNumericalSemigroup(s);
  n:=Length(ov);

  # Add the header of the GraphViz code
  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"digraph  NSGraph{rankdir = TB; edge[dir=back];\n");

  # Add vertices
  for i in [1..n] do
   if IsIrreducible(ov[i]) then 
    AppendTo(output,i," [label=\"",SystemOfGeneratorsToString(MinimalGenerators(ov[i])) ,"\", style=filled];\n");
   else 
    AppendTo(output,i," [label=\"",SystemOfGeneratorsToString(MinimalGenerators(ov[i])) ,"\"];\n");
   fi;
  od;

  # Add edges
  c:=Cartesian([1..n],[1..n]);
  c:=Filtered(c, p-> p[2]<>p[1]);
  c:=Filtered(c, p-> IsSubset(ov[p[1]],ov[p[2]]));
  c:=hasse(c);

  for r in c do
    AppendTo(output,r[1]," -> ",r[2],";\n");
  od;

  AppendTo(output, "}");
  CloseStream(output);
  return out;  
end);

InstallMethod(DotOverSemigroups,
  "for numerical semigrousp",
  [IsNumericalSemigroup],
  DotOverSemigroupsNumericalSemigroup);

############################################################################
##
#O DotRosalesGraph(n,s)
## s is either a numerical semigroup or an affine semigroup, and n is an
## element of s
## returns the graph associated to n in s in dot.
##
#############################################################################
InstallMethod(DotRosalesGraph, "for numerical semigroups",  [IsInt,IsNumericalSemigroup], 
function(n,s)
  local msg, out, output, c, i, r, e;
  msg:=Filtered(MinimalGenerators(s), g->n-g in s);
  e:=Length(msg);
  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"graph  NSGraph{\n");

  # Add vertices
  for i in [1..e] do
    AppendTo(output,i," [label=\"",String(msg[i]) ,"\"];\n");
  od;

  # Add edges
  c:=Cartesian([1..e],[1..e]);
  c:=Filtered(c, p-> p[2]< p[1]);
  c:=Filtered(c, p-> n-msg[p[1]]-msg[p[2]] in s);

  for r in c do
    AppendTo(output, r[1]," -- ",r[2],";\n");
  od;

  AppendTo(output, "}");
  CloseStream(output);
  return out;  
end);

InstallMethod(DotRosalesGraph, "for affine semigroups", [IsHomogeneousList,IsAffineSemigroup], 
function(n,s)
  local msg, out, output, c, i, r, e;
  msg:=Filtered(MinimalGenerators(s), g->n-g in s);
  e:=Length(msg);
  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"graph  NSGraph{\n");

  # Add vertices
  for i in [1..e] do
    AppendTo(output,i," [label=\"",String(msg[i]) ,"\"];\n");
  od;

  # Add edges
  c:=Cartesian([1..e],[1..e]);
  c:=Filtered(c, p-> p[2]< p[1]);
  c:=Filtered(c, p-> n-msg[p[1]]-msg[p[2]] in s);

  for r in c do
    AppendTo(output, r[1]," -- ",r[2],";\n");
  od;

  AppendTo(output, "}");
  CloseStream(output);
  return out;  
end);

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
InstallMethod(DotFactorizationGraph, [IsHomogeneousList],
  function(f)
  local fs, c, nf, i, p, ln, distance, Kruskal, tv, out, output, d;

  Kruskal := function(V, E)
      local trees, needed, v, e, i,j, nv;

      trees := List(V, v-> [v]);
      needed := [];
      nv:=Length(V);
      for e in E do
        i:=First([1..Length(trees)], k-> e[1] in trees[k]);
        j:=First([1..Length(trees)], k-> e[2] in trees[k]);
        if i<>j then
          trees[i]:=Union(trees[i], trees[j]);
          trees[j]:=[];
          Add(needed,e);
        fi;
        if Length(needed)=nv-1 then
          break;
        fi;
      od;
      return needed;
  end;

  distance := function(a,b)
      local   k,  gcd,  i;

      k := Length(a);
      if k <> Length(b) then
          Error("The lengths of a and b are different.\n");
      fi;


      gcd := [];
      for i in [1..k] do
          Add(gcd, Minimum(a[i],b[i]));
      od;
      return(Maximum(Sum(a-gcd),Sum(b-gcd)));

  end;

  if not(IsRectangularTable(f) and ForAll(f,IsListOfIntegersNS)) then
    Error("The argument must be an array of integers.");
  fi;
  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"graph  NSGraph{\n");

  nf:=Length(f);
  fs:=[];
  for i in [1..nf] do 
    AppendTo(output,i," [label=\" (", JoinStringsWithSeparator(f[i], ", ") ,")\"];\n");
  od;
  c:=Cartesian([1..nf],[1..nf]);
  c:=Filtered(c,p->p[1]<p[2]);
  Sort(c,function(e,ee) return distance(f[e[1]],f[e[2]])<distance(f[ee[1]],f[ee[2]]); end);
  tv:=Kruskal(f,List(c,p->[f[p[1]],f[p[2]]]));
  for p in c do 
    d:= distance(f[p[1]],f[p[2]]);
    if [f[p[1]],f[p[2]]] in tv then 
      AppendTo(output, p[1], " -- ", p[2], "[label=\"", d ,"\", color=\"red\"];\n" );
    else
      AppendTo(output, p[1], " -- ", p[2], "[label=\"", d,"\" ];\n" );
    fi;    
  od;
  AppendTo(output, "}");
  CloseStream(output);
  return out;  
end);


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
InstallMethod(DotEliahouGraph, [IsHomogeneousList],
  function(f)
  local fs, c, nf, i, p, ln, distance, tv, out, output, d;

  distance := function(a,b)
      local   k,  gcd,  i;

      k := Length(a);
      if k <> Length(b) then
          Error("The lengths of a and b are different.\n");
      fi;


      gcd := [];
      for i in [1..k] do
          Add(gcd, Minimum(a[i],b[i]));
      od;
      return(Maximum(Sum(a-gcd),Sum(b-gcd)));

  end;

  if not(IsRectangularTable(f) and ForAll(f,IsListOfIntegersNS)) then
    Error("The argument must be an array of integers.");
  fi;

  out := "";
  output := OutputTextString(out, true);
  SetPrintFormattingStatus(output, false);
  AppendTo(output,"graph  NSGraph{\n");

  nf:=Length(f);
  fs:=[];
  for i in [1..nf] do 
    AppendTo(output,i," [label=\" (", JoinStringsWithSeparator(f[i], ", ") ,")\"];\n");
  od;
  c:=Cartesian([1..nf],[1..nf]);
  c:=Filtered(c,p->p[1]<p[2] and f[p[1]]*f[p[2]]<>0);
  Sort(c,function(e,ee) return distance(f[e[1]],f[e[2]])<distance(f[ee[1]],f[ee[2]]); end);
  for p in c do 
    d:= distance(f[p[1]],f[p[2]]);
    AppendTo(output, p[1], " -- ", p[2], "[label=\"", d,"\" ];\n" );
  od;
  AppendTo(output, "}");
  CloseStream(output);
  return out;  
end);

############################################################################
##
#F SetDotNSEngine(engine)
##
## This sets de value of DotNSEngine to engine, which must be any of 
## the following "circo","dot","fdp","neato","osage","twopi".
##
############################################################################
InstallGlobalFunction(SetDotNSEngine,
function(s)
  if not(IsString(s)) then
    Error("The argument must be a string");
  fi;
  if not(s in ["circo","dot","fdp","neato","osage","twopi"]) then
    Error("Engine not recognized");
  fi;
  MakeReadWriteGlobal("DotNSEngine");
  DotNSEngine:=s;
  MakeReadOnlyGlobal("DotNSEngine");
  return true;
end);