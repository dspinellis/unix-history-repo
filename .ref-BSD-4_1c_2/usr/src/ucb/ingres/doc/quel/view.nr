.th VIEW QUEL 2/7/79
.sh NAME
view \- define a virtual relation
.sh SYNOPSIS
.bd define
.bd view
name (target-list) [
.bd where
qual ]
.sh DESCRIPTION
The syntax of the
.it view
statement is almost identical
to the
.it retrieve
.it into
statment;
however,
the data is not retrieved.
Instead,
the definition is stored.
When the relation
.it name
is later used,
the query is converted
to operate on the relations specified
in the
.it target-list.
.s1
All forms of retrieval on the view
are fully supported,
but only a limited set of updates
are supported
because of anomolies which can appear.
Almost no updates are supported
on views
which span more than one relation.
No updates are supported
that affect a domain in the qualification of the view or
that affect a domain
which does not translate into a simple attribute.
.s3
In general,
updates are supported
if and only if it can be guaranteed
(without looking at the actual data)
that the result of updating the view
is identical to that of updating the corresponding real relation.
.s1
The person who defines a view
must own all relations
upon which the view is based.
.sh EXAMPLE
range of e is employee
.br
range of d is dept
.br
define view empdpt (ename = e.name, e.sal, dname = d.name)
.br
	where e.mgr = d.mgr
.sh "SEE ALSO"
retrieve(quel), destroy(quel)
