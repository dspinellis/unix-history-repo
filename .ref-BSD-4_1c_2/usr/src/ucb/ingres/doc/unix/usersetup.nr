.th USERSETUP UNIX 3/14/79
.sh NAME
usersetup \- setup users file
.sh SYNOPSIS
.bd \&.../bin/usersetup
[ flags [ pathname ] ]
.sh DESCRIPTION
The
.bd /etc/passwd
file
is read
and reformatted to become
the \*(II users file,
stored into
.bd \&.../files/users.
If
.it pathname
is specified,
it replaces ``...''.
If pathname is ``\c
.bd \-\c
\&'',
the result is written to the standard output.
.s3
The user name, user, and group id's
are initialized to be identical to the corresponding entry
in the
.bd /etc/passwd
file.
The status field is initialized to be 000001,
except for user
.bd ingres,
which is initialized to all permission bits set.
If the
.it status
parameter is provided,
the field is set to this instead.
The ``initialization file'' parameter
is set to the file
.bd \&.ingres
in the user's login directory.
The user code field is initialized
with sequential two-character codes.
All other fields are initialized
to be null.
.s3
After running
.it usersetup,
the
.bd users
file must be edited.
Any users who are to have any special authorizations
should have the status field changed,
according to the specifications
in users(files).
To disable a user from executing \*(II entirely,
completely remove her line
from the users file.
.s3
As \*(UU users
are added or deleted
from the
.bd /etc/passwd
file,
the
.bd users
file will need to be editted
to reflect the changes.
For deleted users,
it is only necessary
to delete the line for that user
from the
.bd users
file.
To add a user,
you must assign that user a code
in the form "aa"
and enter a line in the users file
in the form:
.br
	name:cc:uid:gid:status:flags:proctab:initfile::databases
.br
where
.it name
is the user name
(taken from the first field of the
.bd /etc/passwd
file
entry for this user),
.it cc
is the user code assigned,
which must be exactly two characters long
and must not be the same as any other existing user codes,
.it uid
and
.it gid
are the user and group ids
(taken from the third and fourth fields
in the
.bd /etc/passwd
entry),
.it status
is the status bits for this user,
normally 000000,
.it flags
are the default flags
for \*(II
(on a per-user basis),
.it proctab
is the default process table
for this user
(which defaults to
.bd =proctab7\c
),
and
.it databases
is a list of the databases
this user may enter.
If null,
she may use all databases.
If the first character is a dash
(``\-''),
the field is a comma separated list
of databases
which she may not enter.
Otherwise,
it is a list
of databases
which she may enter.
.s3
The
.it databases
field
includes the names of databases
which may be created.
.s3
.it Usersetup
may be executed only once,
to initially create the
.bd users
file.
.sh FILES
\&.../files/users
.br
\&/etc/passwd
.sh "SEE ALSO"
ingres(unix),
passwd(V),
users(files)
.sh BUGS
It should be able to bring the
.bd users
file
up to date.
