.ds OK [\|
.ds CK \|]
.ds ' \s+4\v@.3m@\'\v@-.3m@\s-4
.ds ` \s+4\v@.3m@\`\v@-.3m@\s-4
.de P
.br
..
.TH SH 1
.(B
.tl @Copyright (c) 1984, 1985, 1986, 1987@@AT&T All Rights Reserved@
.(E 2
.(B
.tl @Copyright (c) 1984, 1985, 1986, 1987@@AT&T All Rights Reserved@
.(E 2
.SH NAME
sh, rsh \- shell, the standard/restricted command programming language
.SH SYNOPSIS
.B sh
[
.B \-aefhikmnoprstuvx
] [
.B \-o
option ] .\|.\|.
[
.B \-c
string ] 
[ arg  .\|.\|. ]
.br
.B rsh
[
.B \-aefhikmnoprstuvx
] [
.B \-o
option ] .\|.\|.
[
.B \-c
string ] 
[ arg  .\|.\|. ]
.SH DESCRIPTION
.I Sh\^
is a command programming language
that executes commands read from a terminal
or a file.
.I Rsh\^
is a restricted version of the standard command interpreter
.IR sh ;
it is used to set up login names and execution environments whose
capabilities are more controlled than those of the standard shell.
See
.I Invocation\^
below
for the meaning of arguments to the shell.
.SS Definitions.
A
.I metacharacter\^
is one of the following characters:
.RS
.PP
\f3;   &   (   )   \(bv   <   >   new-line   space   tab\fP
.RE
.PP
A
.I blank\^
is a
.B tab
or a
.BR space .
An
.I identifier\^
is a sequence of letters, digits, or underscores
starting with a letter or underscore.
Identifiers are used as names for
.IR aliases ,
.IR functions ,
and
.IR "named parameters" .
A
.I word\^
is a sequence of
.I characters\^
separated by one or more non-quoted
.IR metacharacters .
.SS Commands.
A
.I simple-command\^
is a sequence of
.I blank\^
separated words
which may be preceded by a parameter assignment list.
(See
.I Environment\^
below).
The first word specifies the name of the command to
be executed.
Except as specified below,
the remaining words are passed as arguments
to the invoked command.
The command name is passed as argument 0
(see
.IR exec (2)).
The
.I value\^
of a simple-command is its exit status
if it terminates normally, or (octal) 200+\f2status\^\fP if
it terminates abnormally (see
.IR signal (2)
for a list of
status values).
.PP
A
.I pipeline\^
is a sequence of one or more
.I commands\^
separated by
.BR \(bv .
The standard output of each command but the last
is connected by a
.IR pipe (2)
to the standard input of the next command.
Each command is run as a separate process;
the shell waits for the last command to terminate.
The exit status of a pipeline is the exit
status of the last command.
.PP
A
.I list\^
is a sequence of one or more
pipelines
separated by
.BR ; ,
.BR & ,
.BR && ,
or
.BR \(bv\|\(bv ,
and optionally terminated by
.BR ; ,
.BR & ,
or
.BR \(bv& .
Of these five symbols,
.BR ; ,
.BR & ,
and
.BR \(bv&
have equal precedence,
which is lower than that of
.B &&
and
.BR \(bv\|\(bv .
The symbols
.B &&
and
.B \(bv\|\(bv
also have equal precedence.
A semicolon
.RB ( ; )
causes sequential execution of the preceding pipeline; an ampersand
.RB ( & )
causes asynchronous execution of the preceding pipeline (i.e., the shell does
.I not\^
wait for that pipeline to finish).
The symbol
.B \(bv&
causes asynchronous execution of the preceding command or pipeline
with a two-way pipe established to the parent shell.
The standard input and output of the spawned command
can be written to and read from by the parent Shell
using the
.B \-p
option of
the special commands
.B read
and
.B print\^
described later.
Only one such command can be active
at any given time.
The symbol
.B &&
.RB (\| \(bv\|\(bv \^)
causes the
.I list\^
following it to be executed only if the preceding
pipeline
returns a zero (non-zero) value.
An arbitrary number of new-lines may appear in a
.I list,\^
instead of semicolons,
to delimit commands.
.PP
A
.I command\^
is either a simple-command
or one of the following.
Unless otherwise stated,
the value returned by a command is that of the
last simple-command executed in the command.
.TP
\f3for\fP \f2identifier\^\fP \*(OK \f3in\fP \f2word\^\fP .\|.\|. \*(CK \f3do\fP \f2list\^\fP \f3done\fP
Each time a
.B for
command is executed,
.I identifier\^
is set to the next
.I word\^
taken from the
.B in
.I word\^
list.
If
.BI in " word\^"
\&.\|.\|.
is omitted, then
the
.B for
command executes the \f3do\fP \f2list\^\fP once for each positional parameter
that is set
(see
.I "Parameter Substitution\^"
below).
Execution ends when there are no more words in the list.
.TP
\f3select\fP \f2identifier\^\fP \*(OK \f3in\fP \f2word\^\fP .\|.\|. \*(CK \f3do\fP \f2list\^\fP \f3done\fP
A
.B select
command prints on standard error (file descriptor 2), the set of
.IR word s,
each preceded by a number.
If
.BI in " word\^"
\&.\|.\|.
is omitted, then
the
positional parameters
are used instead
(see
.I "Parameter Substitution\^"
below).
The
.SM
.B PS3
prompt is printed
and a line is read from the standard input.
If this line consists of the number
of one of the listed
.BR word s,
then the value of the parameter
.I identifier\^
is set to the
.I word\^
corresponding to this number.
If this line is empty the selection list is
printed again.
Otherwise the value of the parameter
.I identifier\^
is set to
.BR null .
The contents of the line read from standard input is
saved in
the parameter
.SM
.BR REPLY.
The
.I list\^
is executed for each selection until a
.B break\^
or
.I end-of-file\^
is encountered.
.TP
\f3case\fP \f2word\^\fP \f3in\fP \*(OK \f2pattern\^\fP \*(OK \(bv \
\f2pattern\^\fP \*(CK .\|.\|. \f3)\fP \f2list\^\fP \f3;;\fP \*(CK .\|.\|. \f3esac\fP
A
.B case
command executes the
.I list\^
associated with the first
.I pattern\^
that matches
.IR word .
The form of the patterns is
the same as that used for
file-name generation (see 
.I "File Name Generation\^"
below).
.TP
\f3if\fP \f2list\^\fP \f3then\fP \f2list\^\fP \*(OK \
\f3elif\fP \f2list\^\fP \f3then\fP \f2list\^\fP \*(CK .\|.\|. \
\*(OK \f3else\fP \f2list\^\fP \*(CK \f3f\&i\fP
The
.I list\^
following \f3if\fP is executed and,
if it
returns a zero exit status, the
.I list\^
following
the first
.B then
is executed.
Otherwise, the
.I list\^
following \f3elif\fP
is executed and, if its value is zero,
the
.I list\^
following
the next
.B then
is executed.
Failing that, the
.B else
.I list\^
is executed.
If no
.B else
.I list\^
or
.B then
.I list\^
is executed, then the
.B if
command returns a zero exit status.
.TP
.PD 0
\f3while\fP \f2list\^\fP \f3do\fP \f2list\^\fP \f3done\fP
.TP
\f3until\fP \f2list\^\fP \f3do\fP \f2list\^\fP \f3done\fP
.PD
A
.B while
command repeatedly executes the
.B while
.I list\^
and, if the exit status of the last command in the list is zero, executes
the
.B do
.IR list ;
otherwise the loop terminates.
If no commands in the
.B do
.I list\^
are executed, then the
.B while
command returns a zero exit status;
.B until
may be used in place of
.B while
to negate
the loop termination test.
.TP
\f3(\fP\f2list\^\fP\f3)\fP
.br
Execute
.I list\^
in a separate environment.
Note, that if two adjacent open parentheses are
needed for nesting, a space must be inserted to avoid
arithmetic evaluation as described below.
A parenthesized list used as a command argument denotes
.I "process substitution"
as described below.
.TP
\f3{ \fP\f2list\^\fP\f3;}\fP
.br
.I list\^
is simply executed.
Note that
.B {
is a
.I keyword\^
and requires a blank
in order to be recognized.
.TP
.PD 0
\f3function\fP \f2identifier\^\fP  \f3{\fP \f2list\^\fP \f3;}\fP
.TP
\f2identifier\^\fP  \f3() {\fP \f2list\^\fP \f3;}\fP
.PD
Define a function which is referenced by
.IR identifier .
The body of the function is the
.I list\^
of commands between
.B {
and
.BR } .
(See
.I Functions\^
below).
.TP
\f3time \fP\f2pipeline\^\fP
.br
The
.I pipeline\^
is executed and the elapsed time as well as
the user and system time are printed on standard error.
.PP
The following keywords
are only recognized as the first word of a command
and when not quoted:
.if t .RS
.PP
.B
.if n if then else elif fi case esac for while until do done { } function select time
.if t if   then   else   elif   fi   case   esac   for   while   until   do   done   {   }   function   select   time
.if t .RE
.SS Comments.
A word beginning with
.B #
causes that word and all the following characters up to a new-line
to be ignored.
.SS Aliasing.
The first word of each command is replaced by the text of an
.B alias
if an
.B alias
for this word has been defined.
The
first character of an
.B alias
name can be any non-special printable character,
but the rest of the characters
must be the same as for a valid
.IR identifier .
The replacement string can contain any
valid Shell script
including the metacharacters listed above.
The first word of each command of the
replaced text will not be tested for additional aliases.
If the last character of the alias value is a
.I blank\^
then the word following the alias will also be checked for alias
substitution.
Aliases can be used to redefine special
builtin commands but cannot be used to redefine
the keywords listed above.
Aliases can be created, listed, and exported with the
.B alias
command and can be removed with the
.B unalias
command.
Exported aliases remain in effect for sub-shells
but must be reinitialized for separate invocations
of the Shell (See
.I Invocation\^
below).
.PP
.I Aliasing\^
is performed when
scripts are read,
not while they are executed.
Therefore,
for an alias to take effect
the
.B
alias
command has to be executed before
the command which references the alias is read.
.PP
Aliases are frequently used as a short hand for full path
names.
An option to the aliasing facility allows the value of the alias
to be automatically set to the full pathname of
the corresponding command.
These aliases are called
.I tracked
aliases.
The value of a
.I tracked
alias is defined the first time the corresponding command
is looked up and becomes undefined each time
the
.SM
.B PATH
variable is reset.
These aliases remain
.I tracked
so that the next
subsequent reference will redefine the value.
Several tracked aliases are compiled into the shell.
The
.B \-h
option of the
.B set
command makes each command name which is a
valid alias name
into a tracked alias.
.PP
The following
.I exported aliases
are compiled into the shell
but can be unset or redefined:
.RS 20
.PD 0
.TP
.B "false=\(fmlet 0\(fm"
.TP
.B "functions=\(fmtypeset \-f\(fm"
.TP
.B "history=\(fmfc \-l\(fm"
.TP
.B "integer=\(fmtypeset \-i\(fm"
.TP
.B "nohup=\(fmnohup \(fm"
.TP
.B "r=\(fmfc \-e \-\(fm"
.TP
.B "true=\(fm:\(fm"
.TP
.B "type=\(fmwhence \-v\(fm"
.TP
.B "hash=\(fmalias \-t\(fm"
.PD
.RE 
.SS Tilde Substitution.
After alias substitution is performed, each word
is checked to see if it begins with an unquoted
.BR \(ap .
If it does, then the word up to a
.B /
is checked to see if it matches a user name in the
.B /etc/passwd
file.
If a match is found, the
.B \(ap
and the matched login name is replaced by the
login directory of the matched user.
This is called a
.I tilde
substitution.
If no match is found, the original text is left unchanged.
A
.B \(ap
by itself, or in front of a
.BR / ,
is replaced by the value of the
.B
.SM HOME
parameter.
A
.B \(ap
followed by a
.B +
or
.B \-
is replaced by the value of
the parameter
.B
.SM PWD
and
.B
.SM OLDPWD
respectively.
.PP
In addition, the value of each
.I "keyword parameter"
is checked to see if it begins with a
.B \(ap
or if a
.B \(ap
appears after a
.BR : .
In either of these cases a
.I tilde
substitution is attempted.
.SS Command Substitution.
The standard output from a command enclosed in
parenthesis preceded by a dollar sign (
.B $()
)
or a pair of grave accents (\^\f3\*`\^\*`\fP\^)
may be used as part or all
of a word;
trailing new-lines are removed.
In the second (archaic) form, the string between the quotes is processed
for special quoting characters before the command is executed. (See
.I Quoting\^
below).
The command substitution
\^\f3$(\^cat file\^)\fP\^
can be replaced by the equivalent but faster
\^\f3$(\^<file\^)\fP\^.
Command substitution of most special commands
that do not perform input/output redirection are
carried out without creating a separate process.
.SS Process Substitution.
This feature is only available on
versions of the UNIX operating system that support the
.B /dev/fd
directory for naming open files.
Each command argument of the form
\f3(\fP\f2list\^\fP\f3)\fP,
\f3<(\fP\f2list\^\fP\f3)\fP,
or
\f3>(\fP\f2list\^\fP\f3)\fP
will run process
.I list
asynchronously connected to some file in
.BR /dev/fd .
The name of this file will become the argument to the command.
If the form with
.B >
is selected then writing on this file will provide input for
.IR list .
If
.B <
is used or omitted,
then the file passed as an argument will contain the output of the
.I list
process.
For example,
.RS
.PP
\f3paste  (cut \-f1\fP \f2file1\fP\f3)  (cut \-f3\fP \f2file2\f3) | tee >(\fP\f2process1\fP\f3)  >(\fP\f2process2\fP\f3)\fP
.RE
.PP
.I cuts
fields 1 and 3 from
the files
.I file1
and
.I file2
respectively,
.I pastes
the results together, and
sends it
to the processes
.I process1
and
.IR process2 ,
as well as putting it onto the standard output.
Note that the file, which is passed as an argument to the command,
is a UNIX
.IR pipe (2)
so programs that expect to
.IR lseek (2)
on the file will not work.
.SS Parameter Substitution.
A
.I parameter\^
is an
.IR identifier ,
one or more digits,
or any of the characters
.BR \(** ,
.BR @ ,
.BR # ,
.BR ? ,
.BR \- ,
.BR $ ,
and
.BR !\\^ .
A
.I named parameter\^
(a parameter denoted by an identifier)
has a
.I value\^
and zero or more
.IR attributes .
.I Named parameters \^
can be assigned
.I values\^
and
.I attributes
by using the
.B typeset\^
special command.
The attributes supported by the Shell are described
later with the
.B typeset\^
special command.
Exported parameters pass values and attributes to
sub-shells but only values to the environment.
.PP
The shell supports a limited one-dimensional array facility.
An element of an array parameter is referenced by a
.IR subscript .
A
.I subscript\^
is denoted by a
.BR [ ,
followed by an
.I arithmetic expression\^
(see Arithmetic evaluation below)
followed by a
.BR ] .
The value of all
subscripts must be in the
range of
0 through 511.
Arrays need not be declared.
Any reference to a named parameter
with a valid subscript is
legal and an array will be created if necessary.
Referencing an array without a subscript
is equivalent to referencing the first element.
.PP
The
.I value\^
of a 
.I named parameter\^
may also be assigned by writing:
.RS
.PP
.IB name = value\^\|
\*(OK
.IB name = value\^
\*(CK .\|.\|.
.RE
.PP
.PD 0
If the integer attribute,
.BR \-i ,
is set for
.I name\^
the
.I value\^
is subject to arithmetic evaluation as described below.
.PP
Positional parameters,
parameters denoted by a number,
may be assigned values with the
.B set\^
special command.
Parameter
.B $0
is set from argument zero when the shell
is invoked.
.PP
The character
.B $
is used to introduce substitutable
.IR parameters .
.TP
\f3${\fP\f2parameter\^\fP\f3}\fP
The value, if any, of the parameter is substituted.
The braces are required when
.I parameter\^
is followed by a letter, digit, or underscore
that is not to be interpreted as part of its name
or when a named parameter is subscripted.
If
.I parameter\^
is one or more digits then it is a positional parameter.
A positional parameter of more than one digit must be
enclosed in braces.
If
.I parameter\^
is
.BR \(**
or
.BR @ ,
then all the positional
parameters, starting with
.BR $1 ,
are substituted
(separated by a field separator character).
If an array
.I identifier\^
with subscript
.B \(**
or
.B @
is used,
then the value
for each of the
elements
is substituted
(separated by a field separator character).
.TP
\f3${#\fP\f2parameter\^\fP\f3}\fP
If
.I parameter\^
is
.B \(**
or
.BR @ ,
the number of positional parameters is substituted.
Otherwise, the length of the value of the
.I parameter\^
is substituted.
.TP
\f3${#\fP\f2identifier\fP\f3[*]}\fP
The number of elements in the array
.I identifier\^
is substituted.
.TP
\f3${\fP\f2parameter\^\fP\f3:\-\fP\f2word\^\fP\f3}\fP
If
.I parameter\^
is set and is non-null then substitute its value;
otherwise substitute
.IR word .
.TP
\f3${\fP\f2parameter\^\fP\f3:=\fP\f2word\^\fP\f3}\fP
If
.I parameter\^
is not set or is null then set it to
.IR word ;
the value of the parameter is then substituted.
Positional parameters may not be assigned to
in this way.
.TP
\f3${\fP\f2parameter\^\fP\f3:?\fP\f2word\^\fP\f3}\fP
If
.I parameter\^
is set and is non-null then substitute its value;
otherwise, print
.I word\^
and exit from the shell.
If
.I word\^
is omitted then a standard message is printed.
.TP
\f3${\fP\f2parameter\^\fP\f3:+\fP\f2word\^\fP\f3}\fP
If
.I parameter\^
is set and is non-null then substitute
.IR word ;
otherwise substitute nothing.
.TP
.PD 0
\f3${\fP\f2parameter\^\fP\f3#\fP\f2pattern\^\fP\f3}\fP
.TP
\f3${\fP\f2parameter\^\fP\f3##\fP\f2pattern\^\fP\f3}\fP
.PD
If
the Shell
.I pattern\^
matches the beginning of the value of
.IR parameter ,
then the value of
this substitution is the value of the
.I parameter\^
with the matched portion deleted;
otherwise the value of this
.I parameter\^
is substituted.
In the first form the smallest matching pattern is deleted and in the
latter form the largest matching pattern is deleted.
.TP
.PD 0
\f3${\fP\f2parameter\^\fP\f3%\fP\f2pattern\^\fP\f3}\fP
.TP
\f3${\fP\f2parameter\^\fP\f3%%\fP\f2pattern\^\fP\f3}\fP
.PD
If
the Shell
.I pattern\^
matches the end of the value of
.IR parameter ,
then the value of
.I parameter\^
with the matched part deleted;
otherwise substitute the value of
.IR parameter .
In the first form the smallest matching pattern is deleted and in the
latter form the largest matching pattern is deleted.
.PD
.PP
In the above,
.I word\^
is not evaluated unless it is
to be used as the substituted string,
so that, in the following example,
.B pwd\^
is executed only if
.B d\^
is not set or is null:
.RS
.PP
echo \|${d:\-\^$(\^pwd\^)\^}
.RE
.PP
If the colon (
.B : )
is omitted from the above expressions,
then the shell only checks whether
.I parameter\^
is set or not.
.PP
The following
parameters
are automatically set by the shell:
.RS
.PD 0
.TP
.B #
The number of positional parameters in decimal.
.TP
.B \-
Flags supplied to the shell on invocation or by
the
.B set
command.
.TP
.B ?
The decimal value returned by the last executed command.
.TP
.B $
The process number of this shell.
.TP
.B _
The last argument of the previous command.
This parameter is not set for commands which are asynchronous.
This parameter is also used to hold the name of the matching
.B
.SM MAIL
file when checking for mail.
Finally, the value of this parameter is set to the full path name of
each program the shell invokes and is passed in the
.IR environment .
.TP
.B !
The process number of the last background command invoked.
.TP
.B
.SM PPID
The process number of the parent of the shell.
.TP
.B
.SM PWD
The present working directory set by the
.B cd
command.
.TP
.B
.SM OLDPWD
The previous working directory set by the
.B cd
command.
.TP
.B
.SM RANDOM
Each time this parameter is referenced, a random integer is generated.
The sequence of random numbers can be initialized by assigning
a numeric value to
.SM
.BR RANDOM .
.TP
.B
.SM REPLY
This parameter is set by the
.B select
statement and by
the
.B read
special command when no arguments are supplied.
.TP
.B
.SM SECONDS
Each time this parameter is referenced, the number of
seconds since shell invocation is returned.  If this parameter is
assigned a value, then the value returned upon reference will
be the value that was assigned plus the number of seconds since the assignment.
.PD
.RE
.PP
The following
parameters
are used by the shell:
.RS
.PD 0
.TP
.B
.SM CDPATH
The search path for the
.I cd
command.
.TP
.B
.SM COLUMNS
If this variable is set,
the value is used to define the width of the edit window
for the shell edit modes and for printing
.B select
lists.
.TP
.B
.SM EDITOR
If the value of this variable ends in
.IR emacs ,
.IR gmacs ,
or
.I vi
and the
.B
.SM VISUAL
variable is not set,
then the corresponding option
(see Special Command
.B set
below)
will be turned on.
.TP
.SM
.B ENV
If this parameter is set, then
parameter substitution is performed on
the value to generate
the  pathname of the script that will be
executed when the
.I shell\^
is invoked.
(See
.I Invocation\^
below.)
This file is typically used for
.I alias
and
.I function
definitions.
.TP
.B
.SM FCEDIT
The default editor name for the
.B fc
command.
.TP
.SM
.B IFS
Internal field separators,
normally
.BR space ,
.BR tab ,
and
.B new-line
that is used to separate command words which result from
command or parameter substitution
and for separating words with  the special command
.BR read .
The first character of the
.SM
.B IFS
parameter is used to separate arguments for the
.B
"$\(**"
substitution (See
.I Quoting
below).
.TP
.SM
.B HISTFILE
If this parameter is set when the shell is invoked, then
the value is the  pathname of the file that will be
used to store the command history.
(See
.I "Command re-entry\^"
below.)
.TP
.SM
.B HISTSIZE
If this parameter is set when the shell is invoked, then
the number of previously entered commands that
are accessible by this shell
will be greater than or equal to this number.
The default is 128.
.TP
.B
.SM HOME
The default argument (home directory) for the
.B cd
command.
.TP
.B
.SM LINES
If this variable is set,
the value is used to determine the column length for printing
.B select
lists.
Select lists will print vertically until about two-thirds of
.B
.SM LINES
lines are filled.
.TP
.B
.SM MAIL
If this parameter is set to the name of a mail file
.I and\^
the
.B
.SM MAILPATH
parameter is not set,
then the shell informs the user of arrival of mail
in the specified file.
.TP
.B
.SM MAILCHECK
This variable specifies how often (in seconds) the
shell will check for changes in the modification time
of any of the files specified by the
.B
.SM MAILPATH
or
.B
.SM MAIL
parameters.
The default value is 600 seconds.
When the time has elapsed
the shell will check before issuing the next prompt.
.TP
.B
.SM MAILPATH
A colon (
.B :
)
separated list of file names.
If this parameter is set
then the shell informs the user of
any modifications to the specified files
that have occurred within the last
.B
.SM MAILCHECK
seconds.
Each file name can be followed by a
.B ?
and a message that will be printed.
The message will undergo parameter and command substitution
with the parameter,
.B $_
defined as the name of the file that has changed.
The default message is
.I you have mail in $_\^.
.TP
.B
.SM PATH
The search path for commands (see
.I Execution\^
below).
The user may not change
.B \s-1PATH\s+1
if executing under
.I rsh
(except in
.I .profile\^
).
.TP
.SM
.B PS1
The value of this parameter is expanded for parameter
substitution to define the
primary prompt string which by default is
.RB `` "$ \|" ''.
The character
.B !
in the primary prompt string is replaced by the
.I command\^ 
number (see
.I Command Re-entry
below).
.TP
.SM
.B PS2
Secondary prompt string, by default
.RB `` "> \|" ''.
.TP
.SM
.B PS3
Selection prompt string
used within a
.B select
loop, by default
.RB `` "#? \|" ''.
.TP
.SM
.B SHELL
The pathname of the
.I shell\^
is kept in the environment.
At invocation, if the value of this variable contains an
.B r
in the basename,
then the shell becomes restricted.
.TP
.B
.SM TMOUT
If set to a value greater than zero,
the shell will terminate if a command is not entered within
the prescribed number of seconds after issuing the
.B
.SM PS1
prompt.
(Note that the shell can be compiled with a maximum bound
for this value which cannot be exceeded.)
.TP
.B
.SM VISUAL
If the value of this variable ends in
.IR emacs ,
.IR gmacs ,
or
.I vi
then the corresponding option
(see Special Command
.B set
below)
will be turned on.
.PD
.RE
.PP
The shell gives default values to
\f3\s-1PATH\s+1\fP, \f3\s-1PS1\s+1\fP, \f3\s-1PS2\s+1\fP, \f3\s-1MAILCHECK\s+1\fP,
\f3\s-1TMOUT\s+1\fP  and \f3\s-1IFS\s+1\fP,
while
.SM
.BR HOME ,
.SM
.B SHELL
.SM
.B ENV
and
.SM
.B MAIL
are
not set at all by the shell (although
.SM
.B HOME
.I is\^
set by
.IR login (1)).
On some systems
.SM
.B MAIL
and
.SM
.B SHELL
are also
set by
.IR login (1)).
.SS Blank Interpretation.
After parameter and command substitution,
the results of substitutions are scanned for the field separator
characters (
those found in
.SM
.B IFS
)
and split into distinct arguments where such characters are found.
Explicit null arguments (\^\f3"\^"\fP or \f3\*\(fm\^\*\(fm\fP\^) are retained.
Implicit null arguments
(those resulting from
.I parameters\^
that have no values) are removed.
.SS File Name Generation.
Following substitution, each command
.I word\^
is scanned for
the characters
.BR \(** ,
.BR ? ,
and
.B \*(OK\^ 
unless the
.B \-f
option has been
.BR set .
If one of these characters appears
then the word is regarded as a
.IR pattern .
The word is replaced with alphabetically sorted file names that match the pattern.
If no file name is found that matches the pattern, then
the word is left unchanged.
When a
.I pattern\^
is used for file name generation,
the character
.B .
at the start of a file name
or immediately following a
.BR / ,
as well as the character
.B /
itself,
must be matched explicitly.
In other instances of pattern matching the
.B /
and
.B .
are not treated specially.
.PP
.PD 0
.RS
.TP
.B \(**
Matches any string, including the null string.
.TP
.B ?
Matches any single character.
.TP
.BR \*(OK \^.\|.\|.\^ \*(CK
Matches any one of the enclosed characters.
A pair of characters separated by
.B \-
matches any
character lexically between the pair, inclusive.
If the first character following the opening "[ \|"
is a "! \|" then any character not enclosed is matched.
A
.B \-
can be included in the character set by putting it as the
first or last character.
.PD
.RE
.SS Quoting.
Each of the
.I metacharacters\^
listed above (See
.I Definitions
above)
has a special meaning to the shell
and causes termination of a word unless quoted.
A character may be
.I quoted\^
(i.e., made to stand for itself)
by preceding
it with a
.BR \e .
The pair
.B \enew-line
is ignored.
All characters enclosed between a pair of single quote marks (\^\f3\(fm\^\(fm\fP\^),
are quoted.
A single quote cannot appear within single quotes.
Inside double quote marks
(\f3"\^"\fP),
parameter and command substitution occurs and
.B \e
quotes the characters
.BR \e ,
.BR \*` ,
\f3"\fP,
and
.BR $ .
The meaning of
.B "$\(**"
and
.B "$@"
is identical when not quoted or when used as a parameter assignment value
or as a file name.  However,
when used as a command argument,
.B
"$\(**"
is equivalent to
\f3"$1\fP\f2d\fP\f3\|$2\fP\f2d\fP\|.\|.\|.\f3"\fP,
where
.I d
is the first character of the
.SM
.B IFS
parameter, whereas
.B
"$@"
is equivalent to
.B
"$1"\|
.B
"$2"\|
\&.\|.\|.\^.
Inside grave quote marks
(\f3\*`\^\*`\fP)
.B \e
quotes the characters
.BR \e ,
.BR \*` ,
and
.BR $ .
If the grave quotes occur within double quotes then
.BR \e 
also quotes the character
\f3"\fP.
.PP
The special meaning of keywords or aliases can be removed by quoting any
character of the keyword.
The recognition of function names or special command names listed below
cannot be altered by quoting them.
.SS Arithmetic Evaluation.
An ability to perform integer arithmetic
is provided with the special command
.BR let .
Evaluations are performed using
.I long\^
arithmetic.
Constants are of the form
\*(OK\f2base\f3#\^\f1\*(CK\f2n\^\fP
where
.I base\^
is a decimal number between two and thirty-six
representing the arithmetic base
and
.I n\^
is a number in that base.
If
.I base\^
is omitted 
then base 10 is used.
.PP
An internal integer representation of a
.I named parameter\^
can be specified with the
.B \-i
option of the
.B typeset
special command.
When this attribute is selected
the first assignment to the
parameter determines the arithmetic base
to be used when 
parameter substitution occurs.
.PP
Since many of the arithmetic operators require
quoting, an alternative form of the
.B let
command is provided.
For any command which begins with a
.BR (( ,
all the characters until a matching
.B ))
are treated as a quoted expression.
More precisely,
.BR (( .\|.\|. ))
is equivalent to
.B let\^
\f3"\fP\|.\|.\|.\f3"\fP.
.SS Prompting.
When used interactively,
the shell prompts with the value of
.SM
.B PS1
before reading a command.
If at any time a new-line is typed and further input is needed
to complete a command, then the secondary prompt
(i.e., the value of
.BR \s-1PS2\s+1 )
is issued.
.SS Input/Output.
Before a command is executed, its input and output
may be redirected using a special notation interpreted by the shell.
The following may appear anywhere in a simple-command
or may precede or follow a
.I command\^
and are
.I not\^
passed on to the invoked command.
Command and parameter substitution occurs before
.I word\^
or
.I digit\^
is used except as noted below.
File name generation
occurs only if the pattern matches a single file
and blank interpretation is not performed.
.TP 14
.BI < word
Use file
.I word\^
as standard input (file descriptor 0).
.TP
.BI > word
Use file
.I word\^
as standard output (file descriptor 1).
If the file does not exist then it is created;
otherwise, it is truncated to zero length.
.TP
.BI >\h@-.3m@> word
Use file
.I word\^
as standard output.
If the file exists then output is appended to it (by first seeking to the end-of-file);
otherwise, the file is created.
.TP
\f3<\h@-.3m@<\fP\*(OK\f3\-\fP\*(CK\f2word\fP
The shell input is read up to a line that is the same as
.IR word ,
or to an end-of-file.
No parameter substitution, command substitution or
file name generation is performed on
.IR word .
The resulting document,
called a
.IR here-document ,
becomes
the standard input.
If any character of
.I word\^
is quoted, then no interpretation
is placed upon the characters of the document;
otherwise, parameter and command substitution occurs,
.B \enew-line
is ignored,
and
.B \e
must be used to quote the characters
.BR \e ,
.BR $ ,
.BR \*` ,
and the first character of
.IR word .
If
.B \-
is appended to
.BR <\h@-.3m@< ,
then all leading tabs are stripped from
.I word\^
and from the document.
.TP
.BI <& digit
The standard input is duplicated from file descriptor
.I digit
(see
.IR dup (2)).
Similarly for the standard output using
.BR >& 
.IR digit .
.TP
.B <&\-
The standard input is closed.
Similarly for the standard output using
.BR >&\- .
.PP
If one of the above is preceded by a digit,
then the
file descriptor number referred to is that specified
by the digit
(instead of the default 0 or 1).
For example:
.RS
.PP
\&.\|.\|. \|2>&1
.RE
.PP
means file descriptor 2 is to be opened
for writing as a duplicate
of file descriptor 1.
.PP
The order in which redirections are specified is significant.
The shell evaluates each redirection in terms of the 
.RI ( "file descriptor" ", " file ) 
association at the time of evaluation.
For example:
.RS
.PP
\&.\|.\|. \|1>\f2fname\^\fP 2>&1
.RE
.PP
first associates file descriptor 1 with file 
.IR fname\^ .
It then associates file descriptor 2 with the file associated with file
descriptor 1 (i.e. 
.IR fname\^ ).
If the order of redirections were reversed, file descriptor 2 would be associated 
with the terminal (assuming file descriptor 1 had been) and then file descriptor 
1 would be associated with file 
.IR fname\^ .
.PP
If a command is followed by
.B &
and job control is not active,
then the default standard input
for the command
is the empty file
.BR /dev/null .
Otherwise, the environment for the execution of a command contains the
file descriptors of the invoking shell as modified by
input/output specifications.
.SS Environment.
The
.I environment\^
(see
.IR environ (7))
is a list of name-value pairs that is passed to
an executed program in the same way as a normal argument list.
The names must be
.I identifiers\^
and the values are character strings.
The shell interacts with the environment in several ways.
On invocation, the shell scans the environment
and creates a
parameter
for each name found,
giving it the corresponding value and marking it
.I export .
Executed commands inherit the environment.
If the user modifies the values of these
parameters
or creates new ones,
using the
.B export
or
.B typeset \-x
commands they become part of the
environment.
The environment seen by any executed command is thus composed
of any name-value pairs originally inherited by the shell,
whose values may be modified by the current shell,
plus any additions
which must be noted in
.B export
or
.B typeset \-x
commands.
.PP
The environment for any
.I simple-command\^
or function
may be augmented by prefixing it with one or more parameter assignments.
A parameter assignment argument is a word of the form
.IR identifier=value .
Thus:
.RS
.PP
\s-1TERM\s+1=450 \|cmd \|args				and
.br
(export \|\s-1TERM\s+1; \|\s-1TERM\s+1=450; \|cmd \|args)
.RE
.PP
are equivalent (as far as the above execution of
.I cmd\^
is concerned).
.PP
If the
.B \-k
flag is set,
.I all\^
parameter assignment arguments are placed in the environment,
even if they occur after the command name.
The following
first prints
.B "a=b c"
and then
.BR c:
.PP
.RS
.nf
echo \|a=b \|c
set \|\-k
echo \|a=b \|c
.fi
.RE
.SS Functions.
.PP
The
.B function\^
keyword, described in the
.I Commands
section above,
is used to define shell functions.
Shell functions are read in and stored internally.
Alias names are resolved when the function is read.
Functions are executed like commands with the arguments
passed as positional parameters.
(See
.I Execution
below).
.PP
Functions execute in the same process as the caller and
share all files, traps ( other than
.SM
.B EXIT
and
.SM
.BR ERR )
and present working directory with the
caller.
A trap set on
.SM
.B EXIT
inside a function
is executed after the function completes.
Ordinarily,
variables are shared between the calling program
and the function.
However,
the
.B typeset
special command used within a function
defines local variables whose scope includes
the current function and
all functions it calls.
.PP
The special command
.B return
is used to return
from function calls.
Errors within functions return control to the caller.
.PP
Function identifiers
can be listed with the
.B \-f
option of the
.B typeset
special command.
The text of functions will also
be listed.
Function can be undefined with the
.B \-f
option of the
.B unset
special command.
.PP
Ordinarily,
functions are unset when the shell executes a shell script.
The
.B \-xf
option of the
.B typeset
command allows a function to be exported
to scripts that are executed without a separate
invocation of the shell.
Functions that need to be defined across separate
invocations of the shell should be placed in the
.B
.SM
ENV
file.
.SS Jobs.
.PP
If the
.B monitor
option of the
.B set
command is turned on,
an interactive shell associates a \fIjob\fR with each pipeline.  It keeps
a table of current jobs, printed by the
.B jobs
command, and assigns them small integer numbers.  When
a job is started asynchronously with
.BR & ,
the shell prints a line which looks
like:
.PP
.DT
	[1] 1234
.PP
indicating that the job which was started asynchronously was job number
1 and had one (top-level) process, whose process id was 1234.
.PP
This paragraph and the next require features that are
not in all versions of UNIX and may not apply.
If you are running a job and wish to do something else you may hit the key
\fB^Z\fR (control-Z) which sends a STOP signal to the current job.
The shell will then normally indicate that the job has been `Stopped',
and print another prompt.  You can then manipulate the state of this job,
putting it in the background with the
.B bg
command, or run some other
commands and then eventually bring the job back into the foreground with
the foreground command
.BR fg .
A \fB^Z\fR takes effect immediately and
is like an interrupt in that pending output and unread input are discarded
when it is typed.
.PP
A job being run in the background will stop if it tries to read
from the terminal.  Background jobs are normally allowed to produce output,
but this can be disabled by giving the command ``stty tostop''.
If you set this
tty option, then background jobs will stop when they try to produce
output like they do when they try to read input.
.PP
There are several ways to refer to jobs in the shell.  The character
.B %
introduces a job name.  If you wish to refer to job number 1, you can
name it as
.B %1 . 
Jobs can also be named by prefixes of the string typed in to
.B kill
or restart them.
Thus, on systems that support job control,
.RB ` fg
.BR %ed '
would normally restart
a suspended
.IR ed (1)
job, if there were a suspended job whose name began with
the string `ed'. 
.PP
The shell maintains a notion of the current and previous jobs.
In output pertaining to jobs, the current job is marked with a
.B +
and the previous job with a
.BR \- .
The abbreviation
.B %+
refers
to the current job and
.B %\-
refers to the previous job.
.B %%
is also a synonym for the current job.
.PP
This shell learns immediately whenever a process changes state.
It normally informs you whenever a job becomes blocked so that
no further progress is possible, but only just before it prints
a prompt.  This is done so that it does not otherwise disturb your work.
.PP
When you try to leave the shell while jobs are running or stopped, you will
be warned that `You have stopped(running) jobs.'  You may use the
.B jobs
command to see what they are.  If you do this or immediately try to
exit again, the shell will not warn you a second time, and the stopped
jobs will be terminated.
.SS Signals.
The \s-1INT\s+1 and \s-1QUIT\s+1 signals for an invoked
command are ignored if the command is followed by
.B & 
and job
.B monitor
option is not active.
Otherwise, signals have the values
inherited by the shell from its parent
(but see also
the
.B trap
command below).
.SS Execution.
Each time a command is executed, the above substitutions
are carried out.
If the command name matches one
of the
.I "Special Commands\^"
listed below,
it is executed within the
current shell process.
Next, the command name is checked to see if
it matches one of the user defined functions.
If it does,
the positional parameters are saved
and then reset to the arguments of the
.I function\^
call.
When the
.I function\^
completes or issues a
.BR return ,
the positional parameter list is restored
and any trap set on
.SM
.B EXIT
within the function is executed.
The value of a
.I function\^
is the value of the last command executed.
A function is also executed in the
current shell process.
If a command name is not a
.I "special command\^"
or a user defined
.IR function ,
a process is created and
an attempt is made to execute the command via
.IR exec (2).
.PP
The shell parameter
.B
.SM PATH
defines the search path for
the directory containing the command.
Alternative directory names are separated by
a colon
.RB ( : ).
The default path is
.B /bin:/usr/bin:
(specifying
.BR /bin ,
.BR /usr/bin ,
and the current directory
in that order).
The current directory can be specified by
two or more adjacent colons, or by a colon
at the beginning or end of the path list.
If the command name contains a \f3/\fP then the search path
is not used.
Otherwise, each directory in the path is
searched for an executable file.
If the file has execute permission but is not a
directory or an
.B a.out
file,
it is assumed to be a file containing shell commands.
A sub-shell is spawned to read it.
All non-exported aliases,
functions,
and named parameters are removed in this case.
If the shell command
file doesn't have read permission,
or if the
.I setuid
and/or
.I setgid
bits are set on the file,
then the shell executes an agent whose job it is to
set up the permissions and execute the shell with the
shell command file passed down as an open file.
A parenthesized command is also executed in
a sub-shell without removing non-exported quantities.
.SS Command Re-entry.
The text of the last
.B
.SM
HISTSIZE
(default 128)
commands entered from a terminal device
is saved in a
.I history
file.
The file
.B \s-1$HOME\s+1/.sh_history
is used if the
.B
.SM
HISTFILE
variable is not set
or is not writable.
A shell can access the commands of
all
.I interactive
shells which use the same named
.SM
.BR HISTFILE .
The special command
.B fc\^
is used to list or
edit a portion of this file.
The portion of the file to be edited or listed can be selected by
number or by giving the first character or
characters of the command.
A single command or range of commands can be specified.
If you do not specify an editor program as
an argument to
.B fc\^
then the value of the parameter
.SM
.B FCEDIT
is used.
If
.SM
.B FCEDIT
is not defined then
.B /bin/ed
is used.
The edited command(s) is printed and re-executed upon
leaving the editor.
The editor name
.B \-
is used to skip the editing phase and
to re-execute the command.
In this case a substitution parameter of the form
\f2old\fP\f3=\fP\f2new\fP
can be used to modify the command before execution.
For example, if
.B r
is aliased to
.B \(fmfc \-e \-\(fm
then typing
`\f3r bad=good c\fP'
will re-execute the most recent command which starts with the letter
.BR c ,
replacing the first occurrence of the string
.B bad
with the string
.BR good .
.SS In-line Editing Options
Normally, each command line entered from a terminal device is simply
typed followed by a new-line (`RETURN' or `LINE\ FEED').
If either the
.BR emacs ,
.BR gmacs ,
or
.B vi
option is active, the user can edit the command line.
To be in either of these edit modes
.B set
the corresponding
option.
An editing option is automatically selected each time the
.SM
.B VISUAL
or
.SM
.B EDITOR
variable is assigned a value ending in either of these
option names.
.PP
The editing features require that the user's terminal
accept `RETURN' as carriage return without line feed
and that a space (`\ ') must overwrite the current character on
the screen.
ADM terminal users should set the "space - advance"
switch to `space'.
Hewlett-Packard series 2621 terminal users should set the straps to
`bcGHxZ\ etX'.
.PP
The editing modes implement a concept where the user is looking through a
window at the current line.
The window width is the value of
.SM
.B COLUMNS
if it is defined, otherwise 80.
If the line is longer than the window width minus two, a mark is
displayed at the end of the window to notify the user.
As the cursor moves and reaches the window boundaries the window will be
centered about the cursor.
The mark is a
.BR > " (<" ,
.BR * )
if the line extends on the 
right (left, both) side(s) of the window.
.SS Emacs Editing Mode
This mode is entered by enabling either the
.I emacs
or
.I gmacs
option.
The only difference between these two modes is the way
they handle
.BR ^T .
To edit, the user
moves the cursor to the point needing correction and
then inserts or deletes characters or words as needed.
All the editing commands are control characters or escape
sequences.
The notation for control characters is caret (
.B ^
) followed
by the character.
For example,
.B ^F
is the notation for control
.BR F .
This is entered by depressing `f' while holding down the
`CTRL' (control) key.
The `SHIFT' key is
.I not 
depressed.
(The notation
.B ^?
indicates the DEL (delete) key.)
.PP
The notation for escape sequences is
.B M-
followed by a
character.
For example,
.B M-f
(pronounced Meta f)
is entered by depressing ESC
(ascii
.BR 033 )
followed by `f'.
.RB ( M-F
would be the notation for ESC followed by `SHIFT' (capital) `F'.)
.PP
All edit commands
operate from any place on the line
(not just at the beginning).
Neither the "RETURN" nor the "LINE FEED" key is
entered after edit commands except when noted.
.PP
.PD 0
.TP 10
.BI ^F
Move cursor forward (right) one character. 
.PP
.TP 10
.BI M-f
Move cursor forward one word.
(The editor's idea of a word is a string of characters
consisting of only letters, digits and underscores.)
.PP
.TP 10
.BI ^B 
Move cursor backward (left) one character.
.PP
.TP 10
.BI M-b
Move cursor backward one word.
.PP
.TP 10
.BI ^A 
Move cursor to start of line.
.PP
.TP 10
.BI ^E 
Move cursor to end of line.
.PP
.TP 10
.BI ^] char 
Move cursor to character
.I char
on current line.
.PP
.TP 10
.BI ^X^X
Interchange the cursor and mark.
.PP
.TP 10
.I erase
(User defined erase character as defined
by the stty command, usually
.B ^H
or
.BR # .)
Delete previous character.
.PP
.TP 10
.BI ^D 
Delete current character.
.PP
.TP 10
.BI M-d
Delete current word.
.PP
.TP 10
.BI M-^H
(Meta-backspace) Delete previous word.
.PP
.TP 10
.BI M-h
Delete previous word.
.PP
.TP 10
.BI M-^?
(Meta-DEL) Delete previous word (if your interrupt character is
.B ^?
(DEL, the default) then this command will not work).
.PP
.TP 10
.BI ^T
Transpose current character with next character in
.I emacs
mode.
Transpose two previous characters in
.I gmacs
mode.
.PP
.TP 10
.BI ^C
Capitalize current character.
.PP
.TP 10
.BI M-c
Capitalize current word.
.PP
.TP 10
.BI M-l
Change the current word to lower case.
.PP
.TP 10
.BI ^K 
Kill from the cursor to the end of the line.
If given a parameter of zero then kill from
the start of line to the cursor.
.PP
.TP 10
.BI ^W
Kill from the cursor to the mark.
.PP
.TP 10
.BI M-p
Push the region from the cursor to the mark on the stack.
.PP
.TP 10
.I kill
(User defined kill character as defined
by the stty command, usually
.B ^G
or
.BR @ .)
Kill the entire current line.
If two
.I kill
characters are entered in succession, all
kill characters from then on cause a line feed
(useful when using paper terminals).
.PP
.TP 10
.BI ^Y
Restore last item removed from line. (Yank item back to the line.)
.PP
.TP 10
.BI ^L 
Line feed and print current line.
.PP
.TP 10
.BI ^@
(Null character) Set mark.
.PP
.TP 10
.BI M-
(Meta space) Set mark.
.PP
.TP 10
.BI ^J	
(New\ line)  Execute the current line.
.PP
.TP 10
.BI ^M	
(Return)  Execute the current line.
.PP
.TP 10
.I eof
End-of-file character,
normally
.BR ^D ,
will terminate the shell
if the current line is null.
.PP
.TP 10
.BI ^P
Fetch previous command. 
Each time 
.B ^P 
is entered
the previous command back in time is accessed.
.PP
.TP 10
.BI M-<
Fetch the least recent (oldest) history line.
.PP
.TP 10
.BI M->
Fetch the most recent (youngest) history line.
.PP
.TP 10
.BI ^N
Fetch next command. 
Each time 
.B ^N 
is entered
the next command forward in time is accessed.
.PP
.TP 10
.BI ^R string
Reverse search history for a previous command line containing
.IR string .
If a parameter of zero is given, the search is forward.
.I String
is terminated by a "RETURN" or "NEW\ LINE".
If
.I string
is omitted,
then the next command line containing the most recent
.I string
is accessed.
In this case a parameter of zero
reverses the direction of the search.
.PP
.TP 10
.B  ^O
Operate \- Execute the current line and fetch
the next line relative to current line from the
history file.
.PP
.TP 10
.BI M- digits
(Escape) Define numeric parameter, the digits
are taken as a parameter to the next command.
The commands that accept a parameter are 
.BR . ,
.BR ^F ,
.BR ^B ,
.IR erase ,
.BR ^D ,
.BR ^K ,
.BR ^R ,
.BR ^P ,
.BR ^N ,
.BR M-. ,
.BR M-_ ,
.BR M-b ,
.BR M-c ,
.BR M-d ,
.BR M-f ,
.B M-h
and
.BR M-^H .
.PP
.TP 10
.BI M- letter
Soft-key \- Your alias list is searched for an
alias by the name
.BI _ letter
and if an alias of this name is defined, its
value will be inserted on the input queue.
The
.I letter
must not be one of the above meta-functions.
.PP
.TP 10
.B  M-.
The last word of the previous command is inserted
on the line.
If preceded by a numeric parameter, the value
of this parameter determines which word to insert rather than
the last word.
.PP
.TP 10
.B  M-_
Same as
.BR  M-. .
.PP
.TP 10
.B  M-*
Attempt file name generation on the current word.
An asterisk is appended if the word doesn't contain any special
pattern characters.
.PP
.TP 10
.B  M-ESC
Same as
.BR M-* .
.PP
.TP 10
.B  M-=
List files matching current word pattern
if an asterisk were appended.
.PP
.TP 10
.BI ^U
Multiply parameter of next command by 4.
.PP
.TP 10
.BI \e
Escape next character.  
Editing characters, the user's erase, kill and 
interrupt (normally
.BR ^? )
characters 
may be entered
in a command line or in a search string if preceded by a
.BR \e .
The
.B \e
removes the next character's
editing features (if any).
.PP
.TP 10
.BI ^V
Display version of the shell.
.PD
.SS Vi Editing Mode
There are two typing modes.
Initially, when you enter a command you are in the
.I input\^
mode.
To edit, the user enters
.I control\^
mode by typing ESC (
.B 033
) and
moves the cursor to the point needing correction and
then inserts or deletes characters or words as needed.
Most control commands accept an optional repeat
.I count
prior to the command.
.P
When in vi mode on most systems,
canonical processing is initially enabled and the
command will be echoed again if the speed is 1200 baud or greater and it
contains any control characters or less than one second has elapsed
since the prompt was printed.
The ESC character terminates canonical processing for the remainder of the command
and the user can than modify the command line.
This scheme has the advantages of canonical processing with the type-ahead
echoing of raw mode.
.P
If the option
.B viraw\^
is also set, the terminal will always have canonical processing
disabled.  This mode is implicit for systems that do not support two
alternate end of line delimiters,
and may be helpful for certain terminals.
.SS "\ \ \ \ \ Input Edit Commands"
.PP
.RS
By default the editor is in input mode.
.PD 0
.TP 10
.I erase
(User defined erase character as defined
by the stty command, usually
.B ^H
or
.BR # .)
Delete previous character.
.TP 10
.BI ^W
Delete the previous blank separated word.
.TP 10
.BI ^D
Terminate the shell.
.TP 10
.BI ^V
Escape next character.  
Editing characters, the user's erase or kill
characters may be entered
in a command line or in a search string if preceded by a
.BR ^V .
The
.B ^V 
removes the next character's
editing features (if any).
.TP 10
.BI \e
Escape the next
.I erase
or
.I kill
character.
.P
.RE
.SS "\ \ \ \ \ Motion Edit Commands"
.RS
These commands will move the cursor.
.TP 10
[\f2count\fP]\f3l\fP
Cursor forward (right) one character. 
.TP 10
[\f2count\fP]\f3w\fP
Cursor forward one alpha-numeric word.
.TP 10
[\f2count\fP]\f3W\fP
Cursor to the beginning of the next word that follows a blank.
.TP 10
[\f2count\fP]\f3e\fP
Cursor to end of word.
.TP 10
[\f2count\fP]\f3E\fP
Cursor to end of the current blank delimited word.
.TP 10
[\f2count\fP]\f3h\fP
Cursor backward (left) one character.
.TP 10
[\f2count\fP]\f3b\fP
Cursor backward one word.
.TP 10
[\f2count\fP]\f3B\fP
Cursor to preceding blank separated word.
.TP 10
[\f2count\fP]\f3f\fP\f2c\fP
Find the next character \fIc\fP in the current line.
.TP 10
[\f2count\fP]\f3F\fP\f2c\fP
Find the previous character \fIc\fP in the current line.
.TP 10
[\f2count\fP]\f3t\fP\f2c\fP
Equivalent to
.B f
followed by
.BR h .
.TP 10
[\f2count\fP]\f3T\fP\f2c\fP
Equivalent to
.B F
followed by
.BR l .
.TP 10
.B ;
Repeats the last single character find command,
.BR f ,
.BR F ,
.BR t ,
or
.BR T .
.TP 10
.B ,
Reverses the last single character find command.
.TP 10
.B 0 
Cursor to start of line.
.TP 10
.B ^ 
Cursor to first non-blank character in line.
.TP 10
.B $ 
Cursor to end of line.
.RE
.SS "\ \ \ \ \ Search Edit Commands"
.RS
These commands access your command history.
.TP 10
[\f2count\fP]\f3k\fP
Fetch previous command. 
Each time 
.B k 
is entered
the previous command back in time is accessed.
.TP 10
[\f2count\fP]\f3\-\fP
Equivalent to
.BR k .
.TP 10
[\f2count\fP]\f3j\fP
Fetch next command. 
Each time 
.B j 
is entered
the next command forward in time is accessed.
.TP 10
[\f2count\fP]\f3+\fP
Equivalent to
.BR j .
.TP 10
[\f2count\fP]\f3G\fP
The command number
.I count
is fetched.
The default is the least recent history command.
.TP 10
.BI / string
Search backward through history for a previous command containing
.IR string .
.I String
is terminated by a "RETURN" or "NEW\ LINE".
If \fIstring\fP is null the previous string will be used.
.TP 10
.BI ? string
Same as
.B /
except that search will be in the forward direction.
.TP 10
.B n
Search for next match of the last pattern to
.B /
or
.B ?
commands.
.TP 10
.B N
Search for next match of the last pattern to
.B /
or
.BR ? ,
but in reverse direction.
Search history for the \fIstring\fP entered by the previous \fB/\fP command.
.RE
.SS "\ \ \ \ \ Text Modification Edit Commands"
.RS
These commands will modify the line.
.TP 10
.B a
Enter input mode and enter text after the current character.
.TP 10
.B A
Append text to the end of the line.  Equivalent to
.BR $a .
.TP 10
[\f2count\fP]\f3c\fP\f2motion\fP
.TP 10
\f3c\fP[\f2count\fP]\f2motion\fP
Delete current character through the character that
.I motion
would move the cursor to and enter input mode.
If \fImotion\fP is
.BR c ,
the entire line will be deleted and
input mode entered.
.TP 10
.B C
Delete the current character through the end of line and enter input mode.
Equivalent to
.BR c$ .
.TP 10
.B S
Equivalent to
.BR cc .
.TP 10
.B D
Delete the current character through the end of line.
Equivalent to
.BR d$ .
.TP 10
[\f2count\fP]\f3d\fP\f2motion\fP
.TP 10
\f3d\fP[\f2count\fP]\f2motion\fP
Delete current character through the character that
.I motion
would move to.
If \fImotion\fP is
.B d ,
the entire line will be deleted.
.TP 10
.B i
Enter input mode and insert text before the current character.
.TP 10
.B I
Insert text before the beginning of the line.  Equivalent to
the two character sequence
.BR ^i .
.TP 10
[\f2count\fP]\f3P\fP
Place the previous text modification before the cursor.
.TP 10
[\f2count\fP]\f3p\fP
Place the previous text modification after the cursor.
.TP 10
.B R
Enter input mode and
replace characters on the screen with characters you type overlay fashion.
.TP 10
.BI r c
Replace the current character with
.IR c .
.TP 10
[\f2count\fP]\f3x\fP
Delete current character.
.TP 10
[\f2count\fP]\f3X\fP
Delete preceding character.
.TP 10
[\f2count\fP]\f3.\fP
Repeat the previous text modification command.
.TP 10
.B \(ap
Invert the case of the current character and advance the cursor.
.TP 10
[\f2count\fP]\f3_\fP
Causes the
.I count\^
word of the previous command to be appended and
input mode entered.
The last word is used
if
.I count\^
is omitted.
.TP 10
.B *
Causes an
.B *
to be appended to the current word and file name generation attempted.
If no match is found,
it rings the bell.  Otherwise, the word is replaced
by the matching pattern and input mode is entered.
.RE
.SS "\ \ \ \ \ Other Edit Commands"
.RS
Miscellaneous commands.
.TP 10
[\f2count\fP]\f3y\fP\f2motion\fP
.TP 10
\f3y\fP[\f2count\fP]\f2motion\fP
Yank current character through character that
.I motion
would move the cursor to and puts them into the
delete buffer.  The text and cursor are unchanged.
.TP 10
.B Y
Yanks from current position to end of line.
Equivalent to
.BR y$ .
.TP 10
.B u
Undo the last text modifying command.
.TP 10
.B U
Undo all the text modifying commands performed on the line.
.TP 10
[\f2count\fP]\f3v\fP
Returns the command
.BI "fc \-e ${\s-1VISUAL\s+1:\-${\s-1EDITOR\s+1:\-vi}}" " count"
in the input buffer.
If
.I count\^
is omitted, then the current line is used.
.TP 10
.BI ^L 
Line feed and print current line.
Has effect only in control mode.
.TP 10
.BI ^J	
(New\ line)  Execute the current line, regardless of mode.
.TP 10
.BI ^M	
(Return)  Execute the current line, regardless of mode.
.TP 10
.B \#
Sends the line after
inserting a
.B \#
in front of the line and after each new-line.
Useful for causing the current line to be
inserted in the history without being executed.
.TP 10
.B =	
List the filenames that match the current word if an asterisk were
appended it.
.TP 10
.BI @ letter
Your alias list is searched for an
alias by the name
.BI _ letter
and if an alias of this name is defined, its
value will be inserted on the input queue for processing.
.RE
.PD
.SS Special Commands.
The following simple-commands are executed in the shell process.
Input/Output redirection is permitted.
Unless otherwise indicated, the output is written on file descriptor 1.
Commands that are preceded by one or two \(dg
are treated specially in the following ways:
.PD 0
.TP
1.
Parameter assignment lists preceding the command 
remain in effect when the command completes.
.TP
2.
They are executed in a separate
process when used within command substitution.
.TP
3.
Errors in
commands preceded by \(dg\(dg
cause the script
that contains them to abort.
.PD
.TP
\(dg \f3:\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
The command only expands parameters.
A zero exit code is returned.
.br
.ne 2
.TP
\(dg\(dg \f3\|. \f2file\^\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
Read and execute commands from
.I file\^
and return.
The commands are executed in the current Shell environment.
The search path
specified by
.B
.SM PATH
is used to find the directory containing
.IR file .
If any arguments
.I arg\^
are given,
they become the positional parameters.
Otherwise the positional parameters are unchanged.
.TP
\f3alias\fP \*(OK \f3\-tx\fP \*(CK \*(OK \f2name\fP\*(OK \f2=value\^\fP \*(CK  .\|.\|. \*(CK
.I Alias\^
with no arguments prints the list of aliases
in the form
.I name=value\^
on standard output.
An
.I alias\^
is defined
for each name whose
.I value\^
is given.
A trailing space in
.I value\^
causes the next word to be checked for
alias substitution.
The
.B \-t
flag is used to set and list tracked aliases.
The value of a tracked alias is the full pathname
corresponding to the given
.IR name .
The value becomes undefined when the value of
.SM
.B PATH
is reset but the aliases remained tracked.
Without the
.B \-t
flag,
for each
.I name\^
in the argument list
for which no
.I value\^
is given, the name
and value of the alias is printed.
The
.B \-x
flag is used to set or print exported aliases.
An exported alias is defined across sub-shell environments.
Alias returns true unless a
.I name\^
is given for which no alias has been defined.
.TP
\f3bg\fP \*(OK \f3%\fP\f2job\^\fP \*(CK
This command is only built-in on systems that support job control.
Puts the specified
.I job\^
into the background.
The current job is put in the background
if
.I job\^
is not specified.
.TP
\f3break\fP \*(OK \f2n\^\fP \*(CK
Exit from the enclosing
.BR for
.BR while
.BR until
or
.B select\^
loop, if any.
If
.I n\^
is specified then break
.I n\^
levels.
.TP
\f3continue\fP \*(OK \f2n\^\fP \*(CK
Resume the next iteration of the enclosing
.BR for
.BR while
.BR until
or
.B select\^
loop.
If
.I n\^
is specified then resume at the
.IR n -th
enclosing loop.
.TP
.PD 0
\(dg \f3cd\fP \*(OK \f2arg\^\fP \*(CK
.TP
\(dg \f3cd\fP  \f2old\^\fP \f2new\^\fP
.PD
This command can be in either of two forms.
In the first form it
changes the current directory to
.IR arg .
If
.I arg\^
is
.B \-
the directory is changed to the previous
directory.
The shell
parameter
.B
.SM HOME
is the default
.IR arg .
The parameter
.SM
.B PWD
is set to the current directory.
The shell parameter
.B
.SM CDPATH
defines the search path for
the directory containing 
.IR arg .
Alternative directory names are separated by
a colon
.RB ( : ).
The default path is
.B <null>
(specifying the current directory).
Note that the current directory is specified by a null path name,
which can appear immediately after the equal sign
or between the colon delimiters anywhere else in the path list.
If 
.I arg
begins with a \f3/\fP then the search path
is not used.
Otherwise, each directory in the path is
searched for
.IR arg .
.P
The second form of
.B cd
substitutes the string
.I new
for the string
.I old
in the current directory name,
.SM
.B PWD
and tries to change to this new directory.
.P
The
.B cd\^
command may not be executed by
.I rsh\^.
.TP
\f3echo\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
See
.IR echo (1)
for usage and description.
.TP
\(dg\(dg \f3eval\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
The arguments are read as input
to the shell
and the resulting command(s) executed.
.TP
\(dg\(dg \f3exec\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
If
.I arg\^
is given,
the command specified by
the arguments is executed in place of this shell
without creating a new process.
Input/output arguments may appear and
affect the current process.
If no 
arguments are given
the effect of this command is to
modify file descriptors
as prescribed by the input/output redirection list.
In this case,
any file descriptor numbers greater than 2 that are
opened with this mechanism are closed when invoking
another program.
.TP
\f3exit\fP \*(OK \f2n\^\fP \*(CK
Causes the shell to exit
with the exit status specified by
.IR n .
If
.I n\^
is omitted then the exit status is that of the last command executed.
An end-of-file will also cause the shell to exit
except for a
shell which has the
.I ignoreeof
option (See
.B set
below) turned on.
.TP
\(dg\(dg \f3export\fP \*(OK \f2name\^\fP .\|.\|. \*(CK
The given
.IR name s
are marked for automatic
export to the
.I environment\^
of subsequently-executed commands.
.TP
.PD 0
\(dg\(dg \f3fc\fP \*(OK \f3\-e\fP \f2ename\^\fP \ \*(CK \*(OK \f3\-nlr\^\fP \*(CK \*(OK \f2first\^\fP \*(CK \*(OK \f2last\^\fP \*(CK
.TP
\(dg\(dg \f3fc \-e \-\fP  \*(OK \f2old\fP\f3\=\fP\f2new\^\fP \*(CK \*(OK \f2command\^\fP \*(CK
.PD
In the first form,
a range of commands from
.I first\^
to
.I last\^
is selected from the last
.SM
.B HISTSIZE
commands that were typed at the terminal.
The arguments
.I first\^
and
.I last\^
may be specified as a number or as a string.
A string is used to locate the most recent command starting with
the given string.
A negative number is used as an offset to the current command number.
If the flag
.BR \-l ,
is selected,
the commands are listed on standard output.
Otherwise, the editor program
.I ename\^
is invoked on a file containing these
keyboard commands.
If
.I ename\^
is not supplied, then the value of the parameter
.SM
.B FCEDIT
(default /bin/ed)
is used as the editor.
When editing is complete, the edited command(s)
is executed.  If
.I last\^
is not specified
then it will be set to
.IR first .
If
.I first\^
is not specified
the default is the previous command
for editing and \-16 for listing.
The flag
.B \-r
reverses the order of the commands and
the flag
.B \-n
suppresses command numbers when listing.
In the second form the
.I command\^
is re-executed after the substitution
\f2old\^\fP\f3=\fP\f2new\^\fP
is performed.
.TP
\f3fg\fP \*(OK \f3%\fP\f2job\^\fP \*(CK
This command is only built-in on systems that support job control.
If
.I job\^
is specified it brings it to the foreground.
Otherwise, the current job is
brought into the foreground.
.TP
\f3jobs\fP \*(OK \f3\-l\^\fP \*(CK
Lists the active jobs; given the
.B \-l
options lists process id's in addition to the normal information.
.TP
\f3kill\fP \*(OK \f3\-\fP\f2sig\^\fP \*(CK \f2process\^\fP .\|.\|.
Sends either the TERM (terminate) signal or the
specified signal to the specified jobs or processes.
Signals are either given by number or by names (as given in
.BR /usr/include/signal.h ,
stripped of the prefix ``SIG'').
The signal numbers and names are listed by
.RB "'" "kill \-l" "'."
If the signal being sent is TERM (terminate) or HUP (hangup),
then the job or process will be sent a CONT (continue) signal
if it is stopped.
The argument
.I process\^
can be either a process id or a job.
.TP
\f3let\fP  \f2arg\^\fP .\|.\|.
Each
.I arg
is an
.IR "arithmetic expression"
to be evaluated.
All calculations are done as long
integers and no check for overflow
is performed.
Expressions consist of constants,
named parameters, and operators.
The following set of operators,
listed in order of decreasing precedence,
have been implemented:
.RS
.PD 0
.TP
.B \-
unary minus
.TP
.B !
logical negation
.TP
.B "*  /  %"
.br
multiplication, division, remainder
.TP
.B "+  \-"
addition, subtraction
.TP
.B "<=  >=  <  >"
.br
comparison
.TP
.B "==  !="
.br
equality  inequality
.TP
.B =
arithmetic replacement
.PD
.PP
Sub-expressions in parentheses
.B (\|)
are evaluated first and can be used
to override the above precedence rules.
The evaluation within a precedence group
is from right to left for the
.B =
operator
and from left to right for the others.
.PP
A parameter name must be a valid
.IR identifier .
When a parameter is encountered,
the value associated with the
parameter name is substituted and expression evaluation resumes.
Up to nine levels of recursion are
permitted.
.PP
The return code is
0 if the value of the last expression
is non-zero, and 1 otherwise.
.RE
.TP
\(dg\(dg \f3newgrp\fP \*(OK \f2arg\^\fP .\|.\|. \*(CK
Equivalent to
.BI "exec newgrp" " arg\^"
\&.\|.\|.\^.
.TP
\f3print\fP \*(OK \f3\-Rnprsu\^\fP\*(OK\f2n\fP \*(CK  \*(CK \*(OK \f2arg\^\fP .\|.\|. \*(CK
The shell output mechanism.
With no flags or with flag
.BR \- ,
the arguments are printed
on standard output as described by
.IR echo (1).
In raw mode,
.B \-R
or
.BR \-r ,
the escape conventions of
.I echo
are ignored.
The
.B \-R
option will print all subsequent arguments and options
other than
.BR \-n .
The
.B \-p
option causes the 
arguments to be written onto the pipe
of the process spawned with
.B \(bv& 
instead of standard output.
The
.B \-s
option causes the 
arguments to be written onto the history file
instead of standard output.
The
.B \-u
flag can be used to specify a one digit
file descriptor unit number
.B n\^
on which the
output will be placed.
The default is 1.
If the flag
.B \-n
is used, no 
.B new-line\^
is added to the output.
.TP
\f3pwd\fP
Equivalent to
\f3print \-r \- $\s-1PWD\s+1\fP
.TP
\f3read\fP \*(OK \f3\-prsu\^\fP\*(OK \f2n\^\fP \*(CK \*(CK \*(OK \f2name\f3?\f2prompt\^\f1 \*(CK \*(OK \f2name\^\fP .\|.\|. \*(CK
The shell input mechanism.
One line is read and
is broken up into words using the characters in
.B
.SM IFS
as separators.
In raw mode,
.B \-r,
a 
.B \e
at the end of a line does not signify
line continuation.
The first
word is assigned to the first
.IR name ,
the second word
to the second
.IR name ,
etc., with leftover words assigned to the last
.IR name .
The
.B \-p
option causes the input line
to be taken from the input pipe
of a process spawned by the shell
using
.BR \(bv& .
If the
.B \-s
flag is present,
the input will be saved as a command in the history file.
The flag
.B \-u
can be used to specify a one digit file
descriptor unit to read from.
The file descriptor can be opened with the
.B exec\^
special command.
The default value of
.I n\^
is 0.
If
.IR name
is omitted then
.SM
.B REPLY
is used as the default
.IR name.
The return code is 0 unless an end-of-file is encountered.
An end-of-file with the
.B \-p
option causes cleanup for this process
so that another can be spawned.
If the first argument contains a
.BR ? ,
the remainder of this word is used as a
.I prompt\^
when the shell is interactive.
If the given file descriptor is open for writing
and is a terminal device then the prompt is placed
on this unit.
Otherwise the prompt is issued on file descriptor 2.
The return code is 0 unless an end-of-file is encountered.
.TP
\(dg\(dg \f3readonly\fP \*(OK \f2name\fP .\|.\|. \*(CK
The given
.IR names
are marked
readonly and these
names cannot be changed
by subsequent assignment.
.TP
\(dg\(dg \f3return\fP \*(OK \f2n\^\fP \*(CK
Causes a shell
.I function
to return
to the invoking script
with the return status specified by
.IR n .
If
.I n\^
is omitted then the return status is that of the last command executed.
If
.B return
is invoked while not in a
.I function
or a
\f3\|.\fP
script,
then it is the same as an
.BR exit .
.TP
\f3set\fP \*(OK \f3\-aefhkmnostuvx\fP \*(CK \*(OK \f3\-o\fP \f2option\^\fP .\|.\|. \*(CK \*(OK \f2arg\^\fP .\|.\|. \*(CK
The flags for this command have meaning as follows:
.RS
.PD 0
.TP 8
.B \-a
All subsequent parameters that are defined are automatically exported.
.TP 8
.B \-e
If the shell is non-interactive and if a command fails,
execute the
.SM
.B ERR
trap, if set,
and exit immediately.
This mode is disabled while reading profiles.
.TP 8
.B \-f
Disables file name generation.
.TP 8
.B \-h
Each command whose name is an
.I identifier\^
becomes a tracked alias when first encountered.
.TP 8
.B \-k
All parameter assignment arguments are placed in the environment for a command,
not just those that precede the command name.
.TP 8
.B \-m
Background jobs will run in a separate process group
and a line will print upon completion.
The exit status of background jobs is reported in a completion message.
On systems with job control,
this flag is turned on automatically for
interactive shells.
.TP 8
.B \-n
Read commands but do not execute them.
Ignored for interactive shells.
.TP 8
.B \-o
The following argument can be one of the following option names:
.RS
.TP 8
.B allexport
Same as
.BR \-a .
.TP 8
.B errexit
Same as
.BR \-e .
.TP 8
.B bgnice
All background jobs are run at a lower priority.
.TP 8
.B emacs
Puts you in an
.I emacs
style in-line editor for command entry.
.TP 8
.B gmacs
Puts you in a
.I gmacs
style in-line editor for command entry.
.TP 8
.B ignoreeof
The shell will not exit on end-of-file.
The command
.B exit
must be used.
.TP 8
.B keyword
Same as
.BR \-k .
.TP 8
.B markdirs
All directory names resulting from file name generation have a trailing
.B /
appended.
.TP 8
.B monitor
Same as
.BR \-m .
.TP 8
.B noexec
Same as
.BR \-n .
.TP 8
.B noglob
Same as
.BR \-f .
.TP 8
.B nounset
Same as
.BR \-u .
.TP 8
.B protected
Same as
.BR \-p .
.TP 8
.B verbose
Same as
.BR \-v .
.TP 8
.B trackall
Same as
.BR \-h .
.TP 8
.B vi
Puts you in insert mode of a
.I vi\^
style in-line editor
until you hit escape character
.BR 033 .
This puts you in move mode.
A return sends the line.
.TP 8
.B viraw
Each character is processed as it is typed
in
.I vi\^
mode.
.TP 8
.B xtrace
Same as
.BR \-x .
.TP 8
   
If no option name is supplied then the current option settings are printed.
.RE
.TP 8
.B \-p
Resets the
.SM
.B PATH
variable to the default value, disables processing of the
.B \s-1$HOME\s+1/.profile
file and uses the file
.B /etc/suid_profile
instead of the
.SM
.B ENV
file.
This mode is automatically enabled whenever the effective uid (gid)
is not equal to the real uid (gid).
.TP 8
.B \-s
Sort the positional parameters.
.TP 8
.B \-t
Exit after reading and executing one command.
.TP 8
.B \-u
Treat unset parameters as an error when substituting.
.TP 8
.B \-v
Print shell input lines as they are read.
.TP 8
.B \-x
Print commands and their arguments as they are executed.
.TP 8
.B \-
Turns off
.B \-x
and
.B \-v
flags and stops examining arguments for flags.
.TP 8
.B \-\|\-
Do not change any of the flags; useful in setting
.B $1
to a value beginning with
.BR \- .
If no arguments follow this flag then the positional parameters are unset.
.PD
.PP
Using
.B \+
rather than
.B \-
causes these flags to be turned off.
These flags can also be used upon invocation of the shell.
The current set of flags may be found in
.BR $\- .
The remaining arguments are positional
parameters and are assigned, in order,
.if t to\p
.if n to
.BR $1
.BR $2
\&.\|.\|.\^.
If no arguments are given then the values
of all names are printed on the standard output.
.RE
.TP
\(dg \f3shift\fP \*(OK \f2n\^\fP \*(CK
.br
The positional parameters from
\f3$\fP\f2n\fP\f3+1\fP
\&.\|.\|.
are renamed
.B $1
\&.\|.\|.\^
, default
.I n\^
is 1.
The parameter
.I n\^
can be any arithmetic expression that evaluates to a non-negative
number less than or equal to
.BR $# .
.TP
\f3test\fP \*(OK \f2expr\^\fP \*(CK
.br
Evaluate conditional expression
.IR expr .
See
.IR test (1)
for usage and description.
The arithmetic comparison operators
are not restricted to integers.
They allow any arithmetic expression.
Four additional primitive expressions are allowed:
.RS
.PD 0
.TP
\f3\-L\fP \f2file\^\fP
True if
.I file\^
is a symbolic link.
.TP
\f2file1\^\fP \f3\-nt\fP \f2file2\^\fP
True if
.I file1\^
is newer than
.IR file2 .
.TP
\f2file1\^\fP \f3\-ot\fP \f2file2\^\fP
True if
.I file1\^
is older than
.IR file2 .
.TP
\f2file1\^\fP \f3\-ef\fP \f2file2\^\fP
True if
.I file1\^
has the same device and i-node number as
.IR file2 .
.PD
.RE
.TP
\f3times\fP
.br
Print the accumulated user and system times for
the shell and for processes
run from the shell.
.TP
\f3trap\fP \*(OK \f2arg\^\fP \*(CK \*(OK \f2sig\^\fP \*(CK .\|.\|.
.I arg\^
is a command to be read and executed when the shell
receives signal(s)
.IR sig .
(Note that
.I arg\^
is scanned once when
the trap is set and once when the trap
is taken.)
Each
.I sig\^
can be given as a number or as the name of the signal.
Trap commands are executed in order of signal number.
Any attempt to set a trap on a signal that
was ignored on entry to the current shell
is ineffective.
If
.I arg\^
is omitted or is
.BR \- ,
then all trap(s)
.I sig\^
are reset
to their original values.
If
.I arg\^
is the null
string then this signal is ignored by the shell and by the commands
it invokes.
If
.I sig\^
is
.SM
.B ERR
then
.I arg\^
will be executed whenever a command has a non-zero exit code.
This trap is not inherited by functions.
If
.I sig\^
is
.B 0
or
.SM
.B EXIT
and the
.B trap
statement is executed inside the body of a function,
then the command
.I arg\^
is executed
after the function completes.
If
.I sig\^
is
.B 0
or
.SM
.B EXIT
for a
.B trap
set outside any function
then the command
.I arg\^
is executed
on exit from the shell.
The
.B trap
command
with no arguments prints a list
of commands associated with each signal number.
.TP
\(dg\(dg \f3typeset\fP \*(OK \f3\-HLRZfilprtux\^\fP\*(OK\f2n\fP \*(CK \*(OK \f2name\fP\*(OK \f2=value\^\fP \*(CK \^ \*(CK  .\|.\|. \*(CK
When invoked inside a function,
a new instance of the parameter
.I name\^
is created.
The parameter value and type are restored
when the function completes.
The following list of attributes may be specified:
.RS
.PD 0
.TP
.B \-H
This flag provides UNIX to host-name file mapping on non-UNIX
machines.
.TP
.B \-L
Left justify and remove leading blanks from
.IR value .
If
.I n
is non-zero it defines the width 
of the field,
otherwise it is determined by the width of the value of
first assignment.
When the parameter is assigned to, it is
filled on the right with blanks or truncated, if necessary,  to
fit into the field.
Leading zeros are removed if the
.B \-Z
flag is also set.
The
.B \-R
flag is turned off.
.TP
.B \-R
Right justify and fill with leading blanks.
If
.I n
is non-zero it defines the width 
of the field,
otherwise it is determined by the width of the value of
first assignment.
The field is left filled with blanks or
truncated from the end if the
parameter is reassigned.
The
.B L
flag is turned off.
.TP
.B \-Z
Right justify and fill with leading zeros if
the first non-blank character is a digit and the
.B \-L
flag has not been set.
If
.I n
is non-zero it defines the width 
of the field,
otherwise it is determined by the width of the value of
first assignment.
.TP
.B \-f
The names refer to function names rather than
parameter names.
No assignments can be made and the only other
valid flags are
.BR \-t ,
which turns on execution tracing for this function and
.BR \-x ,
to allow the function to remain in effect across shell
procedures executed in the same process environment.
.TP
.B \-i
Parameter is an integer.
This makes arithmetic faster.
If
.I n
is non-zero it defines the output arithmetic base, 
otherwise the first assignment determines the output base.
.TP
.B \-l
All upper-case characters
converted to lower-case.
The upper-case flag,
.B \-u
is turned off.
.TP
.B \-p
The output of this command, if any,  is written onto the two-way pipe
.TP
.B \-r
The given
.IR names
are marked
readonly and these
names cannot be changed
by subsequent assignment.
.TP
.B \-t
Tags the named parameters.
Tags are user definable and have no special
meaning to the shell.
.TP
.B \-u
All lower-case characters are converted
to upper-case characters.
The lower-case flag,
.B \-l
is turned off.
.TP
.B \-x
The given
.IR name s
are marked for automatic
export to the
.I environment\^
of subsequently-executed commands.
.PD
.PP
Using
.B \+
rather than
.B \-
causes these flags to be turned off.
If no
.I name\^
arguments are given but flags are specified,
a list of
.I names\^
(and optionally the
.IR values\^ )
of the
.I parameters\^
which have these
flags set
is printed.
(Using
.B \+
rather than
.B \-
keeps the
values to be printed.)
If no
.IR name s
and flags
are given,
the
.I names\^
and
.I attributes\^
of all
.I parameters\^
are printed.
.RE
.TP
\f3ulimit\fP \*(OK \f3\-acdfmpst\fP \*(CK \*(OK \f2n\^\fP \*(CK
.RS
.PD 0
.TP
.B \-a
Lists all of the current resource limits
(\s-1BSD\s+1 only).
.TP
.B \-c
imposes a size limit of 
.I n\^
512 byte blocks on the size of core dumps
(\s-1BSD\s+1 only).
.TP
.B \-d
imposes a size limit of 
.I n\^
kbytes on the size of the data area
(\s-1BSD\s+1 only).
.TP
.B \-f
imposes a size limit of 
.I n\^
512 byte blocks on files written by child processes (files of any size may be read).
.TP
.B \-m
imposes a soft limit of 
.I n\^
kbytes on the size of physical memory
(\s-1BSD\s+1 only).
.TP
.B \-p
changes the pipe size to
.I n\^
(\s-1UNIX\s+1/\s-1RT\s+1 only).
.TP
.B \-s
imposes a size limit of 
.I n\^
kbytes on the size of the stack area
(\s-1BSD\s+1 only).
.TP
.B \-t
imposes a time limit of 
.I n\^
seconds to be used by each process
(\s-1BSD\s+1 only).
.PD
.PP
If no option is given,
.B \-f
is assumed.
If
.I n\^
is not given the current limit is printed.
.RE
.TP
\f3umask\fP \*(OK \f2nnn\^\fP \*(CK
The user file-creation mask is set to
.I nnn\^
(see
.IR umask (2)).
If
.I nnn\^
is omitted, the current value of the mask is printed.
.TP
\f3unalias\fP \f2name\^\fP .\|.\|.
The
.IR
parameters
given by the list of
.IR name s
are removed from the
.I alias\^
list.
.TP
\f3unset\fP \*(OK \f3\-f\fP \*(CK \f2name\^\fP .\|.\|.
The parameters given by the list of
.IR name s
are unassigned,
i. e.,
their values and attributes are erased.
Readonly variables cannot be unset.
If the flag,
.BR \-f ,
is set, then the names refer to
.I function\^
names.
.TP
\f3wait\fP \*(OK \f2n\^\fP \*(CK
Wait for the specified child process and
report its termination status.
If
.I n\^
is not given then all currently active child processes are waited for.
The return code from this command is that of
the process waited for.
.TP
\f3whence\fP \*(OK \f3\-v\fP \*(CK \f2name\^\fP .\|.\|.
For each
.IR name ,
indicate how it
would be interpreted if used as a command name.
.P
The flag,
.BR \-v ,
produces a more verbose report.
.SS Invocation.
If the shell is invoked by
.IR exec (2),
and the first character of argument zero
.RB ( $0 )
is
.BR \- ,
then the shell is assumed to be a
.I login
shell and
commands are read from
.B /etc/profile
and then from either
.B .profile
in the current directory or
.BR \s-1$HOME\s+1/.profile ,
if either file exists.
Next, commands are read from
the file named by
performing parameter substitution on
the value of the environment parameter
.SM
.B ENV
if the file exists.
If the
.B \-s
flag is not present and
.I arg\^
is, then a path search is performed on the first
.I arg\^
to determine the name of the script to execute.
The script
.I arg\^
must have read permission and any
.I setuid
and
.I getgid
settings will be ignored.
Commands are then read as described below;
the following flags are interpreted by the shell
when it is invoked:
.PP
.PD 0
.TP 10
.BI \-c "\| string\^"
If the
.B \-c
flag is present then
commands are read from
.IR string .
.TP
.B \-s
If the
.B \-s
flag is present or if no
arguments remain
then commands are read from the standard input.
Shell output,
except for the output of the 
.I Special commands\^
listed above,
is written to
file descriptor 2.
.TP
.B \-i
If the
.B \-i
flag is present or
if the shell input and output are attached to a terminal (as told by
.IR ioctl (2))
then this shell is
.IR interactive .
In this case \s-1TERM\s+1 is ignored (so that \f3kill 0\fP
does not kill an interactive shell) and \s-1INTR\s+1 is caught and ignored
(so that
.B wait
is interruptible).
In all cases, \s-1QUIT\s+1 is ignored by the shell.
.TP
.B \-r
If the
.B \-r
flag is present the shell is a restricted shell.
.PD
.PP
The remaining flags and arguments are described under the
.B set
command above.
.SS Rsh Only.
.I Rsh
is used to set up login names and execution environments whose
capabilities are more controlled than those of the standard shell.
The actions of
.I rsh\^
are identical to those of
.IR sh ,
except that the following are disallowed:
.RS
.PD 0
.PP
changing directory (see
.IR cd (1)),
.br
setting the value of
.SM
.BR SHELL ,
.SM
.BR ENV ,
or
.SM
.BR PATH\*S,
.br
specifying path or
command names containing
.BR / ,
.br
redirecting output
.RB ( >
and
.BR >> ).
.PD
.RE
.PP
The restrictions above are enforced
after \f3.profile\fP and the
.SM
.B ENV
files are interpreted.
.PP
When a command to be executed is found to be a shell procedure,
.I rsh\^
invokes
.I sh\^
to execute it.
Thus, it is possible to provide to the end-user shell procedures 
that have access to the full power of
the standard shell,
while imposing a limited menu of commands;
this scheme assumes that the end-user does not have write and
execute permissions in the same directory.
.PP
The net effect of these rules is that the writer of the
.B .profile
has complete control over user actions,
by performing guaranteed setup actions
and leaving the user in an appropriate directory
(probably
.I not\^
the login directory).
.PP
The system administrator often sets up a directory
of commands
(i.e.,
.BR /usr/rbin )
that can be safely invoked by
.IR rsh .
Some systems also provide a restricted editor
.IR red .
.SH EXIT STATUS
Errors detected by the shell, such as syntax errors,
cause the shell
to return a non-zero exit status.
Otherwise, the shell returns the exit status of
the last command executed (see also the
.B exit
command above).
If the shell is being used non-interactively
then execution of the shell file is abandoned.
Runtime errors detected by the shell are reported by
printing the command or function name and the error condition.
If the line number that the error occurred on is greater than one,
then the line number is also printed in square brackets
.RB ( "[]" )
after the command or function name.
.SH FILES
/etc/passwd
.br
/etc/profile
.br
/etc/suid_profile
.br
\s-1$HOME\s+1/\f3.\fPprofile
.br
/tmp/sh\(**
.br
/dev/null
.SH SEE ALSO
cat(1),
cd(1),
echo(1),
emacs(1),
env(1),
gmacs(1),
newgrp(1),
test(1),
umask(1),
vi(1),
dup(2),
exec(2),
fork(2),
ioctl(2),
lseek(2),
pipe(2),
signal(2),
umask(2),
ulimit(2),
wait(2),
rand(3),
a.out(5),
profile(5),
environ(7).
.SH CAVEATS
.PP
If a command which is a
.I "tracked alias"
is executed, and then a command with the same name is 
installed in a directory in the search path before the directory where the
original command was found, the shell will continue to 
.I exec\^
the original command.
Use the 
.B \-t
option of the
.B alias\^
command to correct this situation.
.PP
Some very old shell scripts contain a
.B ^
as a synonym for the pipe character.
.BR \(bv .
.PP
If a command is piped into a shell command, then all variables set in
the shell command are lost when the command completes.
.PP
Using the
.B fc\^
built-in command within a compound command will cause the whole
command to disappear from the history file.
.PP
The built-in command \f3\|.\fP \f2file\^\fP
reads the whole file before any commands are executed.
Therefore,
.B alias
and
.B unalias
commands in the file
will not apply to any functions defined in the file.
