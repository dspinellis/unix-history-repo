.th DEFINE QUEL 2/7/79
.sh NAME
define \- define subschema
.sh SYNOPSIS
.in +5
.ti -5
.bd define
.bd view
name (target list) [
.bd where
qual ]
.ti -5
.bd "define permit"
oplist
{
.bd on
|
.bd of
|
.bd to
}
var
[ (attlist) ]
.bd to
name
[
.bd at
term ]
[
.bd from
time
.bd to
time ] 
[
.bd on
day
.bd to
day ] [
.bd where
qual ]
.ti -5
.bd define
.bd integrity
.bd on
var
.bd is
qual
.i0
.sh DESCRIPTION
The
.it define
statement
creates entries for the subschema definitions.
See the manual sections listed below
for complete descriptions of these commands.
.sh "SEE ALSO"
integrity(quel),
permit(quel),
view(quel)
