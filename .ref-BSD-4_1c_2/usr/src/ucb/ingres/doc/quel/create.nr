.th CREATE QUEL 1/26/79
.sh NAME
create \- create a new relation
.sh SYNOPSIS
.bd create
relname (domname\c
.it 1
= format {, domname\c
.it 2
= format })
.sh DESCRIPTION
.it Create 
will enter a new relation into the data base.
The relation will be ``owned'' by the user and will
be set to expire
after seven days.
The name of the relation is 
.it relname 
and the domains are named
.it "domname1, domname2,"
etc.
The domains are created with the type specified by 
.it format.
Formats
are described in the quel(quel) manual section.
.s3
The relation is created as a paged heap with
no data initially in it.
.s3
A relation can have no more than 49 domains.
A relation cannot have the same name as
a system relation.
.sh EXAMPLE
.nf
/\*(** Create relation emp with domains name, sal and bdate \*(**/
   create emp (name = c10, salary = f4, bdate = i2)
.fi
.sh "SEE ALSO"
append(quel),
copy(quel),
destroy(quel),
save(quel)
.sh BUGS
