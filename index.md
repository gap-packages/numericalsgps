---
layout: default
---

# GAP Package {{site.data.package.name}}

{{site.data.package.abstract}}

The current version of this package is version {{site.data.package.version}}, released on {{site.data.package.date}}.
For more information, please refer to [the package manual]({{site.data.package.doc-html}}).

There is also a [README](README.html) file.

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

{% if site.data.package.contributors and site.data.package.contributors.size > 0 %}
## Contributor{% if site.data.package.contributors.size != 1 %}s{% endif %}
 {% for person in site.data.package.contributors %}
  {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
  {%- if forloop.last -%}.{% else %}, {%- endif -%}
 {% endfor %}
{% endif %}

{% if site.data.package.citeas %}
## Citing

Please, cite this package as

{{site.data.package.citeas}}

You can get more info by typing `Cite("{{ site.data.package.name }}");` in the gap prompt.

{% include button-bibtex.html %}

{% endif %}

{% if site.github.issues_url %}
## Feedback

For bug reports, feature requests and suggestions, please use the
[issue tracker]({{site.github.issues_url}}).
{% endif %}


## Features

The features of this package include

- defining numerical semigroups;
- computing several properties of numerical semigroups, namely: multiplicity, Frobenius number, (minimal) system of generators, Apéry set, gaps, fundamental gaps, etc.;
- perform several operations on numerical semigroups and ideals, namely: intersection, quotient by an integer, decompose into irreducible semigroups, add a special gap, ...;
- computing and testing membership to relevant families of numerical semigroups.

There is a manual in the sub-directory `doc` written using the GAP package
gapdoc which describes the available functions in detail. The pdf, html
versions of the manual are also available there.


## Installation

This package consists only of GAP code, so for its installation please refer to the [GAP manual](https://www.gap-system.org/Manuals/doc/ref/chap76.html#X82473E4B8756C6CD).

