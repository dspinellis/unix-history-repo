.th COPY QUEL 1/19/79
.sh NAME
copy \- copy data into/from a relation from/into a \*(UU file.
.sh SYNOPSIS
.bd copy
relname 
(domname = format {, domname = format })
.br
.ti +10
direction "filename"
.sh DESCRIPTION
.it Copy 
moves data between \*(II relations and standard \*(UU files.
.it Relname 
is the name of an existing relation.
In general 
.it domname 
identifies a domain in 
.it relname.
.it Format 
indicates the format the \*(UU file
should have for the corresponding domain.
.it Direction 
is either 
.bd into
or 
.bd from.
.it Filename 
is the full \*(UU pathname of the file.
.s3
On a copy 
.bd from 
a file to a relation,
the relation cannot have a secondary index,
it must be owned by you,
and it must be updatable (not a secondary index
or system relation).
.s3
.it Copy 
cannot be used on a relation which is a view.
For a copy 
.bd into
a \*(UU file,
you must either be the owner of the relation or
the relation must have retrieve permission for
all users,
or all permissions for all users.
.s3
The formats allowed by copy are:
.in +5
.s1
.ti-5
.bd "i1,i2,i4"
\- The data is
stored as an integer of length 1, 2, or 4 bytes in the \*(UU file.
.s1
.ti-5
.bd "f4,f8"
\- The data is stored as a floating point
number (either single or double precision) in the \*(UU file.
.s1
.ti-5
.bd "c1,c2,...,c255"
\- The data is stored as a fixed length string of
characters.
.s1
.ti -5
.bd "c0"
\- Variable length character string.
.s1
.ti -5
.bd "d0,d1,...,d255"
\- Dummy domain.
.in -5
.s3
Corresponding domains in the relation
and the \*(UU file do not have to be the
same type or length.
.it Copy 
will convert as necessary.
When converting anything except character to
character, 
.it copy 
checks for overflow.
When converting from character to character,
.it copy 
will blank pad or truncate on the right as necessary.
.s3
The domains should be ordered according to
the way they should appear in the \*(UU file.
Domains are matched according to name,
thus the order of the domains in the relation
and in the \*(UU file does not have to be the same.
.s3
.it Copy 
also provides for variable length strings
and dummy domains.
The action taken depends on whether
it is a copy 
.bd into
or a copy 
.bd from.
Delimitors for variable length strings and
for dummy domains can be selected from the list of:
.in+4
.s3
.bd "nl"
\- new line character
.br
.bd "tab"
\- tab character
.br
.bd "sp"
\- space
.br
.bd nul 
or 
.bd null
\- null character
.br
.bd "comma"
\- comma
.br
.bd "colon"
\- colon
.br
.bd "dash"
\- dash
.br
.bd "lparen"
\- left parenthesis
.br
.bd "rparen"
\- right parenthesis
.br
.it "x"
\- any single character 'x'
.in -4
.s3
The special meaning of any delimitor can be
turned off by preceeding the delimitor with
a `\\'.
The type specifier can optionally be in quotes ("c0delim").
This is usefully if you wish to use a single character
delimitor which has special meaning to the \s-2QUEL\s0 parser.
.s3
When the 
.it direction 
is 
from, 
.it copy 
appends data into the relation
.bd from
the \*(UU file.
Domains in the \*(II relation which are not
assigned values from the \*(UU file are assigned
the default value of zero for numeric
domains, and blank for character
domains.
When copying in this direction the following
special meanings apply:
.in +5
.s3
.ti -5
.bd c0\c
.it delim 
\- The data in the \*(UU file is a variable
length character string terminated by the
delimitor 
.it delim.
If 
.it delim
is missing then the first
comma, tab, or newline encountered will
terminate the string.
The delimitor is not copied.
.s3
For example:
.in +5
pnum=c0
\- string ending in comma, tab, or nl.
.br
pnum=c0nl 
\- string ending in nl.
.br
pnum=c0sp 
\- string ending in space.
.br
pnum=c0z  
\- string ending in the character `z'.
.br
pnum="c0%"  
\- string ending in the character `%'.
.in -5
.s3
A delimitor can be escaped by preceeding it with a
`\\'.
For example,
using 
.bd "name = c0\c"
\&, the string 
``Blow\\, Joe,''
will be accepted into the domain as
``Blow, Joe''.
.s1
.ti -5
.bd d0\c
.it delim 
\- The data in the \*(UU file
is a variable length character string
delimited by 
.it delim\c
\&.
The string is read and discarded.
The delimitor rules are identical for
.bd c0
and
.bd d0.
The domain name is ignored.
.s1
.ti -5
.bd d1,d2,...,d255 
\- The data in the \*(UU file is a
fixed length character string.
The string is read and discarded.
The domain name is ignored.
.s3
.in -5
When the direction is 
.bd into,
.it copy 
transfers data
.bd into
the \*(UU file
from the relation.
If the file already existed, it is truncated to zero
length before copying begins.
When copying in this direction,
the following special meanings apply:
.s3
.in +5
.ti -5
.bd c0 
\- The domain value is converted to a
fixed length character string and writted
into the \*(UU file.
For character domains, the length will be the
same as the domain length.
For numeric domains, the standard \*(II
conversions will take place as specified
by the `\c
.bd \-i\c
\&', `\c
.bd \-f\c
\&',
and `\c
.bd \-c\c
\&'
flags (see ingres(unix)).
.s1
.ti -5
.bd c0\c
.it delim 
\- The domain will be converted according to
the rules for 
.bd c0
above.
The one character delimitor will be inserted
immediately after the domain.
.s1
.ti -5
.bd d1,d2,...,d255 
\- The domain name is taken to be
the name of the delimitor.
It is written into the \*(UU file 1 time for 
.bd d1,
2 times for 
.bd d2,
etc.
.s1
.ti -5
.bd d0 
\- This format is ignored on a copy 
.bd into.
.s1
.ti -5
.bd d0\c
.it delim 
\- The 
.it delim 
is written into the file.
The domain name is ignored.
.s1
.in -5
.s3
If no domains appear in the copy command
(i.e. copy relname () into/from "filename")
then 
.it copy
automatically does a ``bulk'' copy of
all domains, using the order and format
of the domains in the relation.
This is provided as a convenient shorthand
notation for copying and restoring entire relations.
.s3
To 
.it copy
into a relation, you must be the owner
or all users must have all permissions set.
Correspondingly, to 
.it copy
from a relation you must own the relation
or all users must have at least retrieve permission on the
relation.
Also, you may not 
.it copy 
a view.
.sh EXAMPLE
.nf
/\*(** Copy data into the emp relation \*(**/
   copy emp (name=c10,sal=f4,bdate=i2,mgr=c10,xxx=d1) 
              from "/mnt/me/myfile"
.s1
/\*(** Copy employee names and their salaries into a file \*(**/
   copy emp (name=c0,comma=d1,sal=c0,nl=d1)
                into "/mnt/you/yourfile"
.s1
/\*(** Bulk copy employee relation into file \*(**/
   copy emp ()
                into "/mnt/ours/ourfile"
.s1
/\*(** Bulk copy employee relation from file \*(**/
   copy emp ()
                from "/mnt/thy/thyfile"
.fi
.sh "SEE ALSO"
append(quel), create(quel), quel(quel),
permit(quel), view(quel),
ingres(unix)
.sh BUGS
.it Copy 
stops operation at the first error.
.s3
When specifying 
.it filename\c
, the entire \*(UU directory
pathname must be provided, since \*(II operates out of
a different directory than the user's working directory at the
time \*(II is invoked.
