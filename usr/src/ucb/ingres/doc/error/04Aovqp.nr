.th OVQP ERROR 3/30/79
.sh NAME
One Variable Query Processor error message summary
.sh SYNOPSIS
Error numbers 4000 \- 4499.
.sh DESCRIPTION
These error messages
can be generated at run time.
The One Variable Query Processor
actually references the data,
processing the tree produced by the parser.
Thus,
these error messages are associated
with type conflicts detected at run time.
.sh ERRORS
.lp +8 8
4100	ovqp query list overflowed
.s3
This error is produced in the unlikely event that the internal form of your
interaction requires more space in the one variable query processor than has
been allocated for a query buffer.
There is not much you can do except
shorten your interaction or recompile OVQP with a larger query buffer.
.s3
.lp +8 8
4106	the interpreters stack overflowed -- query too long
.lp +8 8
4107	the buffer for ASCII and CONCAT commands overflowed
.s3
More buffer overflows.
.s3
.lp +8 8
4108	cannot use arithmetic operators on two character fields
.lp +8 8
4109	cannot use numeric values with CONCAT operator
.s3
You have tried to perform
a numeric operation
on character fields.
.s3
.lp +8 8
4110	floating point exception occurred.
.s3
If you have floating point hardware instead of the floating point software
interpreter, you will get this error upon a floating point exception
(underflow or overflow).
Since the software interpreter ignores such
exceptions, this error is only possible with floating point hardware.
.s3
.lp +8 8
4111	character value cannot be converted to numeric
due to incorrect syntax.
.s3
When using int1, int2, int4, float4, or float8 to convert
a character to value to a numeric value,
the character value must have the proper syntax.
This error will occur if the character value
contained non-numeric characters.
.s3
.lp +8 8
4112	ovqp query vector overflowed
.s3
Similar to error 4100.
.s3
.lp +8 8
4113	compiler text space ran out
.lp +8 8
4114	compiler ran out of registers
.s3
These errors refer to an experimental version of the system
that is not currently released.
.s3
.lp +8 8
4199	you must convert your 6.0 secondary index before
running this query!
.s3
The internal format of
secondary indices was changed between
versions 6.0 and 6.1 of \*(II.
Before deciding to use a secondary
index OVQP checks that it is
not a 6.0 index.
The solution is to destroy the
secondary index and recreate it.
