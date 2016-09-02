The GAP 4 package `NumericalSgps' to compute with Numerical Semigroups
======================================================================

Introduction
------------

This is release 1.0 of  the package `NumericalSgps`.

The features of this package include

	- defining numerical semigroups;
	- computing several properties of numerical semigroups,
	   namely: multiplicity, Frobenius number,
           (minimal) system of generators,
           Apéry set, gaps, fundamental gaps, etc.;
	- perform several operations on numerical semigroups and ideals,
	   namely: intersection, quotient by an integer,
	   decompose into irreducible semigroups, add a
	   special gap, ...;
	-computing and testing membership to relevant families 
	  of numerical semigroups.

There is a manual in the sub-directory 'doc' written using the GAP package
gapdoc which describes the available functions in detail. The dvi, pdf, html
versions of the manual are also available there.

If you have used this package, please let us know by sending
us an email.  If you  have found important features missing or if there is a
bug, we would appreciate it very much if you send us an email.

The current maintainers of the package are:

Manuel Delgado			<mdelgado@fc.up.pt>

Pedro A. García-Sánchez		<pedro@ugr.es>

Contents
--------
With this version you should have obtained the following files and
directories:

| File/directory | Description |
|:-----|:------|
|README |   this file|
|EXAMPLES|	some examples|
|CHANGES|	changelog|
|doc  |	the manual|
|gap  |the GAP code|
|init.g| the file that initializes this package|
|read.g |         		the file that reads in the package|
|PackageInfo.g	| information file for automatic processing|
|version	|the version number|

Usage
-----
The package is distributed with the main GAP archive. In order to use it you
just have to start GAP and type

      LoadPackage( "numericalsgps" );

-----

For updates between releases of GAP itself check the package web page

	https://bitbucket.org/gap-system/numericalsgps

Unpacking
---------

You may get `NumericalSgps` as a compressed tar archive (file name ends with
.tar.gz). Use the  appropriate  command  on  your system   to unpack the
archive.

On UNIX systems the compressed tar archive may be unpacked by

    tar xzf numericalsgps-<version>.tar.gz

or, if tar on your system does not understand the option z, by

    gunzip numericalsgps-<version>.tar.gz
    tar xf numericalsgps-<version>.tar

which will in each case unpack the code into a directory 'numericalsgps'
in the current directory. We assume that the current directory is the
directory /usr/local/lib/gap4r7/pkg/.

Installation
------------

Copy the folder 'numericalsgp' to your pkg gap folder (for instance /usr/local/lib/gap4r7/pkg). 
You can also place it somewhere else and then use the command `SetPackagePath`:

	SetPackagePath("numericalsgps","whereveryouwant/numericalsgps"); 

or add this line to your `gaprc`.

In order to check the installation, launch `gap`:

	gap 

Then try the following
```gap
gap> LoadPackage( "numericalsgps" );
true
gap>
```
Good luck!

You can also add the line 

	LoadPackage( "numericalsgps" );

to your `gaprc` file.
----------

In other systems, there are equivalent ways to do the same.
