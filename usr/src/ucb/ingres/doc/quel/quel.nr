.th QUEL QUEL 2/23/79
.ds QU \s-2QUEL\s0
.sh NAME
quel \- \fBQUE\fPry \fBL\fPanguage for \*(II
.sh DESCRIPTION
The following is a description of the general syntax
of
.nh
\*(QU.
.hy
Individual 
\*(QU
statements and commands
are treated separately in the document;
this section describes the syntactic classes from which the constituent
parts of 
\*(QU
statements are drawn.
.s1
1. Comments
.s2
A comment is an arbitrary sequence of characters
bounded on the left by
``/\*(**''
and on the right by
``\*(**/'':
.s3
/\*(** This is a comment \*(**/
.s1
2. Names
.s2
Names in 
\*(QU
are sequences of no more than 12 alphanumeric
characters, starting with an alphabetic. Underscore (_) is considered
an alphabetic.
All upper-case alphabetics
appearing anywhere except in strings are automatically
and silently mapped into their
lower-case counterparts.
.s1
3. Keywords
.s2
The following identifiers are reserved for use as keywords and
may not be used otherwise:
.s3
.ft B
.if n .ta 5 25 45
.if t .ta 0.5i 2.5i 4.5i
.de xx
\t\\$1\t\\$2\t\\$3
.br
..
.xx abs all and 
.xx any append ascii 
.xx at atan avg 
.xx avgu by concat 
.xx copy cos count 
.xx countu create define 
.xx delete destroy exp 
.xx float4 float8 from 
.xx gamma help in 
.xx index int1 int2 
.xx int4 integrity into 
.xx is log max 
.xx min mod modify 
.xx not of on 
.xx onto or permit 
.xx print range replace 
.xx retrieve save sin 
.xx sqrt sum sumu 
.xx to unique until 
.xx view where 
.ft
.dt
.s1
4. Constants
.s2
There are three types of constants,
corresponding to the three data types available in 
\*(QU
for data storage.
.s1
4.1. String constants
.s2
Strings in 
\*(QU
are sequences of no more than 255 arbitrary
ASCII characters bounded by
double quotes ( " " ). Upper case alphabetics within strings
are accepted literally.
Also, in order to imbed quotes
within strings, it is necessary to prefix them with `\e' .
The same convention applies to `\e' itself.
.s3
Only printing characters are allowed within strings.
Non-printing characters (i.e. control characters)
are converted to blanks.
.s1
4.2. Integer constants
.s2
.br
Integer constants in 
\*(QU
range from \*-2,147,483,647
to +2,147,483,647.
Integer constants beyond that range will be converted to floating point.
If the integer is greater than 32,767 or less than \*-32,767
then it will be left as a two byte integer.
Otherwise it is converted to a four byte integer.
.s1
4.3. Floating point constants
.s2
Floating constants consist of an integer part, a decimal point, and
a fraction part or scientific notation
of the following format:
.s3
	{<dig>} [.<dig>] [e\*vE [+\*v\*-] {<dig>}]
.s3
Where <dig> is a digit, [] represents zero or one,
{} represents zero or more, and | represents alternation.
An exponent with a missing mantissa has a mantissa
of 1 inserted.
There may be no extra characters embedded in the string.
Floating constants
are taken to be double-precision quantities with a range of
approximately
.if n -10**38 to +10**38
.if t \*-10\x'-0.2v'\u\s-3\&38\s0\d to 10\u\x'-0.2v'\s-3\&38\s0\d
and a precision of 17 decimal digits.
.s1
5. Attributes
.s2
An attribute is a construction of the form:
.s3
	variable.domain
.s3
.it Variable 
identifies a particular relation and can be thought of
as standing for the rows or tuples of that relation.
A variable is associated with a relation by means of a
.it range
statement.
.it Domain 
is the name of one of the columns of the relation
over which the variable ranges.
Together they make up an attribute, which represents
values of the named domain.
.s1
6. Arithmetic operators
.s2
Arithmetic operators take numeric type expressions as operands.
Unary operators group right to left; binary operators group
left to right. The operators (in order of descending
precedence) are:
.s3
.nf
	+,\*-	(unary) plus, minus
	\*(**\*(**	exponentiation
	\*(**,/	multiplication, division
	+,\*-	(binary) addition, subtraction
.dt
.fi
.i0
.s3
Parentheses may be used for arbitrary grouping.
Arithmetic overflow and divide by zero are
not checked on integer operations.
Floating point operations are checked for
overflow, underflow, and divide by zero only
if the appropriate machine hardware exists
and has been enabled.
.s1
7. Expressions (a_expr)
.s2
An expression is one of the following:
.s3
.nf
.if t .in +0.5i
.if n .in +5
constant
attribute
functional expression
aggregate or aggregate function
a combination of numeric expressions and arithmetic operators
.i0
.fi
.s3
For the purposes of this document,
an arbitrary expression will be
refered to by the name
.it a_expr.
.s1
8. Formats
.s2
Every 
.it a_expr 
has a format
denoted by
a letter (\c
.bd c,
.bd i,
or 
.bd f,
for character, integer, or floating data
types respectively) and a number indicating the number of bytes
of storage occupied.
Formats currently supported are listed below.
The ranges of numeric types are indicated in parentheses.
.s3
.lp +20 15
c1 \- c255	character data of length 1\-255 characters
.lp +20 15
i1	1-byte integer (\*-128 to +127)
.lp +20 15
i2	2-byte integer (\*-32768 to +32767)
.lp +20 15
i4	4-byte integer  (\*-2,147,483,648 to +2,147,483,647)
.lp +20 15
.if n f4	4-byte floating (\*-10**38 to +10**38,
.if t f4	4-byte floating (\*-10\x'-0.2v'\u\s-3\&38\s0\d to +10\x'-0.2v'\u\s-3\&38\s0\d,
7 decimal digit precision)
.lp +20 15
.if n f4	8-byte floating (\*-10**38 to +10**38,
.if t f4	8-byte floating (\*-10\u\x'-0.2v'\s-3\&38\s0\d to +10\u\x'-0.2v'\s-3\&38\s0\d,
17 decimal digit precision)
.i0
.s3
One numeric format can be converted to
or substituted for any other numeric format. 
.s1
9. Type Conversion.
.s2
When operating on two numeric domains of
different types,
\*(II converts as necessary to make the
types identical.
.s3
When operating on an integer and a floating
point number,
the integer is converted to a floating point
number before the operation.
When operating on two integers of different
sizes, the smaller is converted to
the size of the larger.
When operating on two floating point number
of different size,
the larger is converted to the smaller.
.s3
The following table summarizes the possible combinations:
.s3
.dt
.if n .in +4
.if t .in +1i
.nf
	i1	i2	i4	f4	f8
.s3
i1 \-	i1	i2	i4	f4	f8
i2 \-	i2	i2	i4	f4	f8
i4 \-	i4	i4	i4	f4	f8
f4 \-	f4	f4	f4	f4	f4
f8 \-	f8	f8	f8	f4	f8
.dt
.i0
.fi
.s3
\*(II provides five type conversion
operators specifically for
overriding the default actions.
The operators are:
.s3
.nf
.in +4
.if n .ta 18
.if t .ta 1.4i
int1(a_expr)	result type i1
int2(a_expr)	result type i2
int4(a_expr)	result type i4
float4(a_expr)	result type f4
float8(a_expr)	result type f8
.dt
.fi
.i0
.s3
The type conversion operators convert their argument
a_expr to the requested type.
.it A_expr 
can be anything including character.
If a character value cannot be converted,
an error occures and processing is halted.
This can happen only if the syntax of the
character value is incorrect.
.s3
Overflow is not checked on conversion.
.s1
10. Target_list
.s2
.br
A target list is a parenthesized, comma separated list of one
or more elements , each of which must be of one of the following
forms:
.s3
a)
.it result_attname 
.bd is 
.it a_expr
.s3
.it Result_attname 
is the name of the attribute to be created (or an
already existing attribute name in the case of update statements.)
The equal sign (``='') may be used
interchangeably with 
.bd is.
In the case where 
.it a_expr
is anything other than
a single attribute, this form
must be used to assign a result
name to the expression.
.s3
b)  
.it attribute
.s3
In the case of a 
.it retrieve,
the resultant domain
will acquire the same name as that of the attribute being retrieved.
In the case of update statements
(\c
.it "append, replace\c"
),
the relation being updated must have
a domain with exactly that name.
.s3
Inside the target list the keyword 
.bd all
can be used to represent all domains.
For example:
.if n .in +5
.if t .in +0.5i
.s3
range of e is employee
.br
retrieve (e.all) where e.salary > 10000
.i0
.s3
will retrieve all domains of employee for
those tuples which satisfy the qualification.
.bd All
can be used in the target list of
a 
.it retrieve 
or an 
.bd append.
The domains will be inserted in their
``create'' order, that is,
the same order they were listed in the
.it create 
statement.
.s1
11. Comparison operators
.s2
Comparison operators take arbitrary expressions as operands.
.s3
.dt
.nf
	<	(less than)
	<=	(less than or equal)
	>	(greater than)
	>=	(greater than or equal)
	=	(equal to)
	!=	(not equal to)
.fi
.s3
They are all of equal precedence.
When comparisons are made on
character attributes, all blanks are ignored.
.s1
12. Logical operators
.s2
Logical operators take clauses as operands and
group left-to-right:
.s3
.nf
	not	(logical not; negation)
	and	(logical and; conjunction)
	or	(logical or; disjunction)
.fi
.i0
.dt
.s3
.bd Not
has the highest precedence of the three.
.bd And
and 
.bd or 
have equal precedence.
Parentheses may be used for arbitrary grouping.
.s1
13. Qualification (qual)
.s2
A 
.it qualification 
consists of any number of clauses connected
by logical operators.
A clause  is a pair of expressions connected by a comparison operator:
.s3
.dt
	a_expr comparison_operator a_expr
.s3
Parentheses may be used for arbitrary
grouping.
A qualification may thus be:
.s3
.in +4
.it clause
.br
.bd not 
.it qual
.br
.it qual 
.bd or
.it qual
.br
.it qual 
.bd and
.it qual
.br
(
.it qual
)
.i0
.s1
14. Functional expressions
.s2
A 
.it "functional expression"
consists of a function name followed
by a parenthesized (list of) operand(s).
Functional expressions can be nested to any level.
In the following list of functions supported (\c
.it n\c
)
represents an arbitrary numeric type expression.
The format of the result is indicated on the right.
.s3
.if n .ta 10 25
.if n .in 24
.if t .ta 1.0i 2.5i
.if t .in 2.5i
.de xx
.lp +20 15
\fB\\$1(\fI\\$2\fB)\fR \-\t\c
..
.xx abs n
same as 
.it n
(absolute value)
.xx ascii n
character string (converts numeric to character)
.xx atan n
f8 (arctangent)
.xx concat a,b
character (character concatenation. See 16.2)
.xx cos n
f8 (cosine)
.xx exp n 
f8 (exponential of 
.it n\c
)
.xx gamma n 
f8 (log gamma)
.xx log n
f8 (natural logarithm)
.xx mod n,b
same as 
.it b
(\c
.it n
modulo
.it "b. n" 
and 
.it b 
must be i1, i2, or i4)
.xx sin n 
f8 (sine)
.xx sqrt n
f8 (square root)
.dt
.i0
.s1
15. Aggregate expressions
.s2
Aggregate expressions provide a way to aggregate a computed expression
over a set of tuples.
.s1
15.1. Aggregation operators
.s2
The definitions
of the aggregates are listed below.
.s3
.de xx
.lp +20 15
\fB\\$1\fP \-\t\c
..
.xx count
(i4) count of occurrences
.xx countu
(i4) count of unique occurrences
.xx sum
summation
.xx sumu
summation of unique values
.xx avg
(f8) average (sum/count)
.xx avgu
(f8) unique average (sumu/countu)
.xx max
maximum
.xx min
minimum
.xx any
(i2) value is 1 if any tuples satisfy
the qualification, else it is 0
.dt
.i0
.s1
15.2. Simple aggregate
.s2
.it "\taggregation_operator" 
(\c
.it a_expr  
[ 
.bd where
.it qual
] )
.dt
.s3
A simple aggregate evaluates to a single scalar value.
.it A_expr 
is aggregated over the set of tuples satisfying
the qualification (or all tuples in the range of the expression if
no qualification is present).
Operators 
.it sum 
and 
.it avg 
require numeric type 
.it a_expr;
.it "count, any, max"
and 
.it min 
permit a character type attribute as well as
numeric type 
.it a_expr.
.s3
.ul 1
Simple aggregates are completely local.
That is, they are logically removed from the
query, processed separately, and replaced
by their scalar value.
.s1
15.3. ``\c
.bd any\c
\&''
aggregate
.s2
It is sometimes useful to know if any tuples satisfy a
particular qualification.
One way of doing this is by using the aggregate 
.it count
and checking whether the return is zero or non-zero.
Using
.it any
instead of 
.it count
is more efficient since
processing is stopped, if possible, the first time a tuple
satisfies a qualification.
.s3
.it Any
returns 1 if the qualification is true
and 0 otherwise.
.s1
15.4. Aggregate functions
.s2
.it "\taggregation_operator" 
(\c 
.it a_expr 
.bd by
.it by_domain
.br
.it "\t\t{, by_domain}" 
[
.bd where 
.it qual
] )
.s3
Aggregate functions are extensions of simple aggregates.
The 
.it by
operator groups (i.e. partitions) the set of qualifying tuples by 
.it by_domain 
values.
For more than one 
.it by_domain, 
the values which are grouped by are the concatenation
of individual 
.it by_domain 
values.
.it A_expr 
is as in simple aggregates.
The aggregate function evaluates to a set of aggregate results,
one for each partition into which the set of qualifying
tuples has been grouped.
The aggregate value used during evaluation of the query is the
value associated with the partition into which
the tuple currently being processed would fall.
.s3
Unlike simple aggregates, aggregate functions
are not completely local.
The 
.it by_list,
which differentiates
aggregate functions from simple aggregates,
is global
to the query.
Domains in the 
.it by_list
are automatically linked to the
other domains in the query which are
in the same relation.
.s3
Example:
.ft B
.nf 
	/\*(** retrieve the average salary for the employees
	working for each manager \*(**/
	range of e is employee
	retrieve (e.manager, avesal=avg(e.salary by e.manager))
.fi
.i0
.ft
.s1
15.5 Aggregates on Unique Values.
.s2
It is occasionally necessary to aggregate on
unique values of an expression.
The 
.it avgu\c
, 
.it sumu\c
, and 
.it countu
aggregates
all remove duplicate values before
performing the aggregation.
For example:
.s3
	count(e.manager)
.s3
would tell you how many occurrences of 
.it "e.manager"
exist.
But
.s3
	countu(e.manager)
.s3
would tell you how many unique values of
.it "e.manager"
exist.
.s1
16. Special character operators
.s2
There are three special features which are particular
to character domains.
.s1
16.1 Pattern matching characters
.s2
There are four characters which take
on special meaning when used in character constants
(strings):
.s3
.de xx
.lp +10 6
\\$1\t\c
..
.xx \*(**     
matches any string of zero or more characters.
.xx ?     
matches any single character.
.xx [..]  
matches any of characters in the brackets.
.i0
.s3
These characters can be used in any combination to
form a variety of tests.
For example:
.s3
.lp +25 20
where e.name = "\*(**" \- matches any name.
.lp +25 20
where e.name = "E\*(**" \- matches any name starting with "E".
.lp +25 20
where e.name = "\*(**ein" \- matches all names ending with "ein"
.lp +25 20
where e.name = "\*(**[aeiou]\*(**" \- matches any name with at least one vowel.
.lp +25 20
where e.name = "Allman?" \- matches any seven character name starting with "Allman".
.lp +25 20
where e.name = "[A\-J]\*(**" \- matches any name starting with A,B,..,J.
.i0
.s3
The special meaning of the pattern matching characters
can be disabled by preceding them with a `\e'.
Thus ``\e\*(**'' refers to the character ``\*(**''.
When the special characters appear in the target
list they must be escaped.
For example:
.s3
.dt
	title = "\e\*(**\e\*(**\e\*(** ingres \e\*(**\e\*(**\e\*(**"
.s3
is the correct way to assign the string
``\*(**\*(**\*(** ingres \*(**\*(**\*(**'' to the domain ``title''.
.s1
16.2 Concatenation
.s2
There is a concatenation operator which can
form one character string from two.
Its syntax is ``concat(field1, field2)''.
The size of the new character string is the
sum of the sizes of the original two.
Trailing blanks are trimmed from the first
field, the second field is concatenated
and the remainder is blank padded. The result is never
trimmed to 0 length, however.
Concat can be arbitrarily nested inside other
concats. For example:
.s3
.dt
	name = concat(concat(x.lastname, ","), x.firstname)
.s3
will concatenate 
x.lastname with a comma and
then concatenate x.firstname to that.
.s1
16.3 Ascii (numeric to character translation)
.s2
The 
.it ascii 
function can be used to convert a
numeric field to its character representation.
This can be useful when it is desired to compare
a numeric value with a character value.
For example:
.nf
.s3
.dt
	retrieve ( ... ) 
		where x.chardomain = ascii(x.numdomain)
.fi
.s3
.it Ascii 
can be applied to a character value.
The result is simply the character value unchanged.
The numeric conversion formats are determined by
the printing formats (see ingres(unix)).
.sh "SEE ALSO"
append(quel),
delete(quel), 
range(quel), 
replace(quel), 
retrieve(quel), 
ingres(unix)
.sh BUGS
The maximum number of variables which can
appear in one query is 10.
.s3
Numeric overflow, underflow, and divide by zero
are not detected.
.s3
When converting between numeric types, overflow is
not checked.
