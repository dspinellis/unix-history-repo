.(l C
.sz 12
.b
ADDENDUM TO MAIL REFERENCE MANUAL
.r
.sz 10

Version 1.3
.)l
.sp
.sh 1 "Distribution groups (aliases)"
.pp
It is possible to create short names, or
.i aliases
for groups of people using the
.b alias
command.  For example, one might give the command
.(l
alias staff dmr ken root
.)l
after which one could mail to
.q "staff"
and have mail sent to
.q "dmr,"
.q "ken,"
and
.q "root."
To print out the aliases that you currently have defined, just type
.(l
alias
.)l
.sh 1 "Startup files"
.pp
Whenever Mail is invoked, it reads the files
.i /usr/lib/Mail.rc
and
.i .mailrc
in the user's home directory in that order.
Commonly, system-wide aliases are defined in
.i /usr/lib/Mail.rc
and personal aliases and
.b set
options are defined in
.i .mailrc .
