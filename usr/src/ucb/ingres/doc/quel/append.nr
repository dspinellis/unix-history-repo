.th APPEND QUEL 1/26/79
.sh NAME
append \- append tuples to a relation
.sh SYNOPSIS
.bd append 
[\c
.bd to\c
] relname (target_list) [\c
.bd where 
qual\c
]
.sh DESCRIPTION
.it Append 
adds tuples which satisfy the qualification to 
.it relname.
.it Relname 
must be the name of an existing relation.
The 
.it target_list 
specifies
the values of the attributes to be appended to 
.it relname.
The domains may be listed in any order.
Attributes of the result relation which do not appear in the 
.it target_list
as result_attnames (either explicitly or by default)
are assigned default values of 0, for numeric attributes, or blank,
for character attributes.

Values or expressions of any numeric type may be used to
set the value of a numeric type domain. Conversion to the result domain
type takes place.
Numeric values cannot be directly assigned to
character domains.
Conversion from numeric to character can be done using
the
.bd ascii
operator (see quel(quel)).
Character values cannot be directly assigned to numeric domains.
Use the 
.bd int1\c
,
.bd int2\c
, etc. functions to convert
character values to numeric (see quel(quel)).
.s3
The keyword 
.bd all 
can be used when it is desired
to append all domains of a relation.
.s3
An
.it append
may only be issued by the owner of the relation
or a user with 
.it append
permission on the given relation.
.sh EXAMPLE
.nf
/\*(** Make new employee Jones work for Smith \*(**/
   range of n is newemp
   append to emp(n.name, n.sal, mgr = "Smith", bdate = 1975\-n.age)
		where n.name = "Jones"
/\*(** Append the newemp1 relation to newemp \*(**/
   range of n1 is newemp1
   append to newemp(n1.all)
.fi
.sh "SEE ALSO"
copy(quel), permit(quel), quel(quel), retrieve(quel)
.sh DIAGNOSTICS
Use of a numeric type expression to set a character type domain or vice versa will
produce diagnostics.
.sh BUGS
Duplicate tuples appended to a relation stored as a ``paged heap''
(unkeyed, unstructured) are not removed.
