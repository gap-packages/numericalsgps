---
layout: default
---

# GAP Package {{site.data.package.name}}

{{site.data.package.abstract}}

The current version of this package is version {{site.data.package.version}}.
For more information, please refer to [the package manual]({{site.data.package.doc-html}}).

## Dependencies

This package requires GAP version {{site.data.package.GAP}}
{% if site.data.package.needed-pkgs %}
The following other GAP packages are needed:
{% for pkg in site.data.package.needed-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}
{% if site.data.package.suggested-pkgs %}
The following additional GAP packages are not required, but suggested:
{% for pkg in site.data.package.suggested-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}


## Author{% if site.data.package.authors.size != 1 %}s{% endif %}
{% for person in site.data.package.authors %}
 {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
 {%- if forloop.last -%}.{% else %}, {%- endif -%}
{% endfor %}

{% if site.data.package.contributors.size != 0 %}
## Contributor{% if site.data.package.contributors.size != 1 %}s{% endif %}
 {% for person in site.data.package.contributors %}
  {% if person.url %}{{ person.name }}{% else %}{{ person.name }}{% endif %}
  {%- if forloop.last -%}.{% else %}, {%- endif -%}
 {% endfor %}
{% endif %}

{% if site.github.issues_url %}
## Feedback

For bug reports, feature requests and suggestions, please use the
[issue tracker]({{site.github.issues_url}}).
{% endif %}


Introduction
------------

The features of this package include

- defining numerical semigroups;
- computing several properties of numerical semigroups, namely: multiplicity, Frobenius number, (minimal) system of generators, Apéry set, gaps, fundamental gaps, etc.;
- perform several operations on numerical semigroups and ideals, namely: intersection, quotient by an integer, decompose into irreducible semigroups, add a special gap, ...;
-computing and testing membership to relevant families of numerical semigroups.


Usage
-----
The package is distributed with the main GAP archive. In order to use it you
just have to start GAP and type

      LoadPackage( "numericalsgps" );


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
```gap
LoadPackage( "numericalsgps" );
```
to your `gaprc` file.

In other systems, there are equivalent ways to do the same.
