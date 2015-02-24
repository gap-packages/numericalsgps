The GAP 4 package `NumericalSgps' to compute with Numerical Semigroups
======================================================================

Introduction
------------

This is release 0.98 of  the package `NumericalSgps`.

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

	README          	this file

	EXAMPLES		some examples
	
	CHANGES 		changelog

	doc             		the manual

	gap             		the GAP code

	init.g          		the file that initializes this package

	read.g          		the file that reads in the package

	PackageInfo.g	information file for automatic processing

	version				the version number

Usage
-----
The package is distributed with the main GAP archive. In order to use it you
just have to start GAP and type

      LoadPackage( "numericalsgps" );

-----

For updates between releases of GAP itself check the package Web pages

    http://www.fc.up.pt/cmup/mdelgado/numericalsgps/

Unpacking
---------

You may get `NumericalSgps' as a compressed tar archive (file name ends with
.tar.gz). Use the  appropriate  command  on  your system   to unpack the
archive.

On UNIX systems the compressed tar archive may be unpacked by

    tar xzf numericalsgps-<version>.tar.gz

or, if tar on your system does not understand the option z, by

    gunzip numericalsgps-<version>.tar.gz
    tar xf numericalsgps-<version>.tar

which will in each case unpack the code into a directory 'numericalsgps'
in the current directory. We assume that the current directory is the
directory /usr/local/lib/gap4r5/pkg/.

Installation
------------


You may have to start GAP with the -l option, for instance,

	gap -l "/usr/local/lib/gap4r5"

Then try the following
```gap
gap> LoadPackage( "numericalsgps" );
true
gap>
```
Good luck!

If you use a LINUX system, you may want to, in order to save typing, write
aliases:

in the file `.bashrc' (or something equivalent, maybe with another syntax):

	alias gap='gap -l "/usr/local/lib/gap4r5;"'

and in the file `.gap/gaprc`

	LoadPackage( "numericalsgps" );

----------

In other systems, there are equivalent ways to do the same.