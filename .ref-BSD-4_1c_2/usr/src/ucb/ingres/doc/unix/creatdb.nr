.th CREATDB UNIX 11/6/79
.sh NAME
creatdb \- create a data base
.sh SYNOPSIS
.bd creatdb
[
.bd \-u\c
.it name
] [
.bd \-e
] [
.bd \-m
] [
.bd \*(+-c
] [
.bd \*(+-q
] dbname
.sh DESCRIPTION
Creatdb creates a new \*(II database,
or modifies the status of an existing database.
The person who executes this command
becomes the Database Administrator
(DBA) for the database.
The DBA has special powers
not granted to ordinary users.
.s3
.it Dbname
is the name of the database
to be created.
The name must be unique
among all \*(II users.
.s1
The flags
.bd \*(+-c
and
.bd \*(+-q
specify options
on the database.
The form
.bd +\c
.it x
turns an option on,
while
.bd \-\c
.it x
turns an option off.
The
.bd \-c
flag
turns off the concurrency control scheme
(default on).
The
.bd +q
flag
turns on query modification
(default on).
.s3
Concurrency control should not be turned off
except on databases
which are never accessed
by more than one user.
This applies
even if users do not share
data relations,
since system relations
are still shared.
If the concurrency control scheme is not installed
in \*(UU,
or if the special file
/dev/lock
does not exist or is not accessible
for read-write by \*(II,
concurrency control acts as though it is off
(although it will suddenly come on
when the lock driver is installed in \*(UU).
.s3
Query modification must be turned on
for the protection,
integrity,
and view subsystems
to work,
however,
the system will run
slightly slower
in some cases
if it is turned on.
It is possible to turn query modification on
if it is already off
in an existing database,
but it is not possible
to turn it off
if it is already on.
.s3
Databases with query modification turned off
create new relations with all access permitted
for all users,
instead of no access except to the owner,
the default for databases with query modification enabled.
.s1
Database options
for an existing database
may be modified
by stating the
.bd \-e
flag.
The database is adjusted
to conform to the option flags.
For example:
.s3
	creatdb \-e +q mydb
.s3
turns query modification on
for database
``mydb''
(but leaves concurrency control alone).
Only the database administrator
(DBA)
may use the
.bd \-e
flag.
.s3
When query modification is turned on,
new relations will be created with no access,
but previously created relations will still have all access
to everyone.
The
.it destroy
command may be used to remove this global permission,
after which more selective permissions may be specified
with the
.it permit
command.
.s1
The \*(II user
may use the
.bd \-u
flag
to specify a different DBA:
the flag should be immediately followed
by the login name
of the user who should be the DBA.
.s3
The
.bd \-m
flag
specifies that the \*(UU directory
in which the database is to reside
already exists.
This should only be needed
if the directory
if a mounted file system,
as might occur for a very large database.
The directory must exist
(as
.bd \&.../data/base/\c
.it dbname\c
),
must be mode 777,
and must be empty of all files.
.s1
The user who executes this command
must have the U_CREATDB bit set in
the status field
of her entry in
the users file.
.s3
The \*(II superuser can create a file in
.bd \&.../data/base
containing a single line which is the full pathname
of the location of the database.
The file must be owned by \*(II and
be mode 600.
When the database is created,
it will be created in the file named,
rather than in the directory
.bd \&.../data/base.
For example, if the file
.bd \&.../data/base/ericdb
contained the line
.s3
	/mnt/eric/database
.s3
then the database called ``ericdb''
would be physically stored in the directory
/mnt/eric/database
rather than in the directory
\&.../data/base/ericdb.
.sh EXAMPLE
.nf
creatdb demo
creatdb \-ueric \-q erics_db
creatdb \-e +q \-c \-u:av erics_db
.fi
.sh FILES
\&.../files/dbtmplt7
.br
\&.../files/data/base/*
.br
\&.../files/datadir/* (for compatibility with previous versions)
.sh "SEE ALSO"
demodb(unix),
destroydb(unix),
users(files),
chmod(I),
destroydb(quel),
permit(quel)
.sh DIAGNOSTICS
.in +4
.ti -4
No database name specified.
.br
You have not specified the name of the database
to create
(or modify)
with the command.
.ti -4
You may not access this database
.br
Your entry in the users file says
you are not authorized to access this database.
.ti -4
You are not a valid \*(II user
.br
You do not have a users file entry, and can
not do anything with \*(II at all.
.ti -4
You are not allowed this command
.br
The U_CREATDB bit is not set in your users
file entry.
.ti -4
You may not use the \-u flag
.br
Only the \*(II superuser may become
someone else.
.ti -4
<name> does not exist
.br
With
.bd \-e
or
.bd \-m,
the directory does not exist.
.ti -4
<name> already exists
.br
Without either
.bd \-e
or
.bd \-m,
the database
(actually, the directory) already exists.
.ti -4
<name> is not empty
.br
With the
.bd \-m
flag, the directory you named
must be empty.
.ti -4
You are not the DBA for this database
.br
With the
.bd \-e
flag, you must be the database
administrator.
.in -4
