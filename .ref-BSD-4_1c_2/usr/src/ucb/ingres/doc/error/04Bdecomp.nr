.th DECOMP ERROR 3/30/79
.sh NAME
Decomposition error message summary
.sh SYNOPSIS
Error numbers 4500 \- 4999.
.sh DESCRIPTION
These error messages are associated with the process
of decomposing a multi-variable query
into a sequence of one variable queries
which can be executed by OVQP.
.sh ERRORS
.lp +8 8
4602	query involves too many relations to create
aggregate function intermediate result.
.s3
In the processing of aggregate functions it is usually
necessary to create an intermediate relation for
.it each
aggregate function.
However, no query may have more than ten variables.
Since aggregate functions implicitly increase the number of variables
in the query, you can exceed this limit.
You must either break the
interaction apart and process the aggregate functions separately or
you must recompile \*(II to support more variables per query.
.s3
.lp +8 8
4610	Query too long for available buffer space (qbufsize).
.lp +8 8
4611	Query too long for available buffer space (varbufsiz)
.lp +8 8
4612	Query too long for available buffer space (sqsiz)
.lp +8 8
4613	Query too long for available buffer space (stacksiz)
.lp +8 8
4614	Query too long for available buffer space (agbufsiz).
.s3
These will happen if the internal form of the interaction processed by decomp
is too long for the available buffer space.
You must either shorten your
interaction or recompile decomp.
The name in parenthesis gives the internal
name of which buffer was too small.
.s3
.lp +8 8
4615	Aggregate function is too wide or has too many domains.
.s3
The internal form
of an aggregate function
must not contain
more than 49 domains
or be more than 1010 bytes wide.
Try breaking the aggregate function
into two or more parts.
.s3
.lp +8 8
4620	Target list for "retrieve unique" has more than 49
domains or is wider than 1010 bytes.
