.th PERMIT QUEL 2/7/79
.sh NAME
permit \- add permissions to a relation
.sh SYNOPSIS
.bd define
.bd permit
oplist {
.bd on
|
.bd of
|
.bd to
} var [ (attlist) ]
.br
.if n .ti +5
.if t .ti +0.5i
.bd to
name [
.bd at
term ] 
[
.bd from
time
.bd to
time ] 
.br
.if n .ti +5
.if t .ti +0.5i
[
.bd on
day
.bd to
day ] [
.bd where
qual ]
.sh DESCRIPTION
The
.it permit
statement extends the current permissions
on the relation specified by
.it var.
.it Oplist
is a comma separated list
of possible operations,
which can be
.bd retrieve\c
,
.bd replace\c
,
.bd delete\c
,
.bd append\c
, or
.bd all\c
;
.it all
is a special case
meaning all permissions.
.it Name
is the login name
of a user or the word
.bd all\c
\&.
.it Term
is a terminal name
of the form `tty\fIx\fP'
or the keyword
.bd all\c
; omitting this phrase
is equivalent to specifying
.it all\c
\&.
.it Time\c
s are of the form
`hh:mm'
on a twenty-four hour clock
which limit the times of the day
during which this permission applies.
.it "Day\c"
s are three-character abbreviations
for days of the week.
The
.it qual
is appended to the qualification
of the query
when it is run.
.s1
Separate parts
of a single
.it permit
statement
are conjoined (ANDed).
Different
.it permit
statements are disjoined (ORed).
For example,
if you include
.s2
	. . . to eric at tty4 . . .
.s2
the
.it permit
applies only to eric
when logged in on tty4,
but if you include two
.it permit
statements
.s2
	. . . to eric at all . . .
.br
	. . . to all at tty4 . . .
.s2
then when eric logs in on tty4
he will get the union of the permissions
specified by the two statements.
If eric logs in on ttyd
he will get only the permissions specified
in the first
.it permit
statement,
and if bob logs in on tty4
he will get only the permissions specified
in the second
.it permit
statment.
.s1
The
.it permit
statement may only be issued
by the owner of the relation.
Although a user
other than the DBA
may issue a
.it permit
statement,
it is useless because noone else
can access her relations anyway.
.s3
.it Permit
statements
do not apply to the owner
of a relation
or to views.
.s1
The statements
.s2
	define permit all on x to all
.br
	define permit retrieve of x to all
.s2
with no further qualification
are handled as special cases
and are thus particularly efficient.
.sh EXAMPLES
  range of e is employee
.br
  define permit retrieve of e (name, sal) to marc
.br
.if t .ti +0.5i
.if n .ti +5
  at ttyd from 8:00 to 17:00
.br
.if t .ti +0.5i
.if n .ti +5
  on Mon to Fri
.br
.if t .ti +0.5i
.if n .ti +5
  where e.mgr = "marc"
.br

  range of p is parts
.br
  define permit retrieve of e to all
.sh "SEE ALSO"
destroy(quel)
