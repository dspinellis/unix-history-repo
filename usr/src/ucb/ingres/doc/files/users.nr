.th USERS FILES
.sh NAME
\&.../files/users \- \*(II user codes and parameters
.sh DESCRIPTION
This file contains the user information in fields seperated by colons.
The fields are as follows:
.in +4
.ti -4
*\ User name, taken directly from
.bd /etc/passwd
file.
.ti -4
*\ User code, assigned by the \*(II super-user.
It must be a unique two character code.
.ti -4
*\ \*(UU user id.
This MUST match the entry in the
.bd /etc/passwd
file.
.ti -4
*\ \*(UU group id.
Same comment applies.
.ti -4
*\ Status word in octal.
Bit values are:
.nf
0000001	creatdb permission
0000002	permits batch update override
0000004	permits update of system catalogs
0000020	can use trace flags
0000040	can turn off qrymod
0000100	can use arbitrary proctabs
0000200	can use the =proctab form
0100000	ingres superuser
.fi
.ti -4
*\ A list of flags
automatically set
for this user.
.ti -4
*\ The process table
to use for this user.
.ti -4
*\ An initialization file
to read be read by the monitor
at login time.
.ti -4
*\ Unassigned.
.ti -4
*\ Comma seperated list of databases.
If this list is null,
the user may enter any database.
If it begins with a `\-',
the user may enter any database
except the named databases.
Otherwise,
the user may only enter the named databases.
.in -4
.s1
Giving permission to a user
to use arbitrary process tables
is tantamount to turning off the protection system
for that user.
.sh EXAMPLE
.nf
ingres:aa:5:2:177777:\-d:=special:/mnt/ingres/.ingres::
guest:ah:35:1:000000:::::demo,guest
.fi
.sh "SEE ALSO"
.nf
initucode(util)
.fi
