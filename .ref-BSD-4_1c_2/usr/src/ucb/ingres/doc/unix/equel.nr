.th EQUEL UNIX 3/14/79
.if n .ds EE EQUEL
.if t .ds EE \s-2EQUEL\s0
.sh NAME
equel \- Embedded QUEL interface to C
.sh SYNOPSIS
.bd equel 
[
.bd \-d
] [
.bd \-f
] [ 
.bd \-r
] file.q ...
.sh DESCRIPTION
.it Equel 
provides the user with a method of interfacing
the general purpose programming language ``C'' with 
\*(II.
It consists of the 
\*(EE 
pre-compiler and the 
\*(EE 
runtime library.
.s1
.bd Compilation
.s2
The precompiler is invoked with the statement:
.s3
.ti +5
.bd equel 
[<flags>] file1.q [<flags>] file2.q ...
.s3
where file\c
.it n\c
\&.q
are the source input file names,
which must end with
.bd \&.q.
The output is written to the file
``file\c
.it n\c
\&.c''.
As many files as wished may be specified.
.br 
The flags that may be used are:
.de xp
.s3
.lp +5 5
.if n \\$1\t\c
.if t \fB\\$1\fP\t\c
..
.xp \-d
Generate code to print source listing file name
and line number when a run-time error occurs. This
can be useful for debugging, but takes up process space.
Defaults to off.
.xp \-f
Forces code to be on the same line in the  output file 
as it is in the input file to ease interpreting C diagnostic
messages.
\*(EE
will usually try to get all C code lines in the 
output file on the same lines as they were in the
input file.
Sometimes it 
must break up queries into several lines to avoid C-preprocessor line
overflows, possibly moving
some C code ahead some lines.
With the 
.bd \-f
flag specified this will never happen and, though the line buffer 
may overflow, C lines will be on the right line.
This is useful
for finding the line in the
source 
file that C error diagnostics on the
output
file refer to.
.xp \-r
Resets flags to default values.
Used to supress other flags for 
some of the files in the argument list.
.i0
.dt
.s3
The output files may than be compiled using the C compiler:
.s3
.ti +5
.bd cc
file1.c file2.c ... 
.bd \-lq
.s3
The
.bd \-lq
requests the use of the 
\*(EE 
object library.
.s3
All 
\*(EE 
routines and globals begin with the characters "II",
and so all globals variables and procedure names of the form
II\c
.it xxx
are reserved for use by 
\*(EE 
and should be avoided by 
\*(EE 
users.
.s1
.bd "Basic Syntax"
.s2
\*(EE
commands are indicated by lines which begin with a double
pound sign (``##'').
Other lines are simply copied as is.
All normal 
\*(II 
commands may be used in 
\*(EE
and have the same effect as if invoked through the interactive
terminal monitor.
Only retrieve commands with no result relation
specified have a different syntax and meaning.
.s3
The format of retrieve without a result relation is modified to:
.s3
.in +5
## retrieve (C-variable = a_fcn { , C-variable = a_fcn } )
.s3
.ti -5
optionally followed (immediately) by:
.s3
##	[
.bd where
.it qual
]
.br
## {
.br
	/\*(** C-code \*(**/
.br
## } 
.i0
.s3
This statement causes the ``C-code'' to be executed once for
each tuple retrieved,
with the ``C-variable''s set appropriately.
Numeric values of any type are converted as necessary.
No conversion is done between numeric and character values.
(The normal \*(II
.it ascii
function may be used for this purpose.)
.s3
Also, the following 
\*(EE 
commands are permitted.
.in +5
.sp
.ti -5
.bd "## ingres"
[ingres flags] data_base_name
.br
This command starts 
\*(II
running, and directs all dynamically following queries to 
the database
.it data_base_name.
It is a run-time error to execute this command twice
without an intervening ``## exit'',
as well as to issue queries while an ``## ingres'' statement is
not in effect.
Each flag should be enclosed in quotes to avoid confusion in the 
\*(EE 
parser:
.s3
	## ingres "\-f4f10.2" "\-i212" demo
.s3
.ti -5
.bd "## exit"
.br
Exit simply exits from 
\*(II.
It is equivalent to the
.bd \eq
command to the teletype monitor.
.i0
.s1
.bd "Parametrized Quel Statements"
.s2
Quel statements with target lists may be ``parametrized''.
This is indicated by 
preceding the statement with the keyword ``param''.
The target list of a parametrized statement has the form:
.s3
	(
.it "tl_var, argv"
)
.s3
where
.it tl_var
is taken to be a string pointer at execution time (it may
be a string constant) and interpreted as follows.
For any parametrized 
\*(EE 
statement except a retrieve without a
result relation (no ``into rel'') (i.e. append, copy, create, replace,
retrieve into) the string
.it tl_var
is taken to be a regular target list
except that wherever a `%' appears a valid 
\*(II
type (f4, f8, i2, i4, c)
is expected to follow.
Each of these is replaced by the value of the 
corresponding entry into
.it argv
(starting at 0)
which is interpreted to be a pointer to a variable of the type indicated
by the `%' sequence.
Neither
.it argv
nor the variables which it points to 
need be declared to 
\*(EE.
For example:
.ne 12
.nf
.s3
.in +5
char	\*(**argv[10];

	argv[0] = &double_var;
	argv[1] = &int_var;
##	param append to rel 
##		("dom1 = %f8, dom2 = %i2", argv)
##	/\*(** to escape the "%<ingres_type>" mechanism use "%%" \*(**/
##	/\*(** This places a single `%' in the string. \*(**/
.i0
.fi
.s3
On a retrieve to C-variables, within
.it tl_var,
instead of 
the C-variable to retrieve
into, the same `%' escape sequences are used to denote the type of the
corresponding argv entry
into which the value will be retrieved.
.s3
The qualification of any query may be replaced by a string valued variable,
whose contents is interpreted at run time 
as the text of the qualification.
.s3
The 
.it copy 
statement may also be parametrized.
The form of the parametrized 
.it copy 
is analogous to the other parametrized statements: the target list 
may be parametrized
in the same manner as the 
.it append
statements, and furthermore, the 
.bd from/into
keyword may be replaced by a string valued variable
whose content at run time should be
.bd into
or
.bd from.
.s1
.bd "Declarations"
.s2
Any valid C variable declaration on a line
beginning with a ``\c
.bd ##\c
\&''
declares a C-variable that may be used 
in an 
\*(EE 
statement and as a normal
variable.
All variables must be declared before being used.
Anywhere a constant may appear in an 
\*(II 
command,
a C-variable may appear.
The value of the C-variable is substituted at execution time.
.s3
Neither nested structures nor variables of type
.it char
(as opposed to pointer to char or array of char)
are allowed.
Furthermore, there are two restrictions in the way variables are
referenced within 
\*(EE
statements.
All variable usages must be dereferenced 
and/or subscripted (for arrays and pointers),
or selected (for structure variables) to yield lvalues (scalar values).
.it Char
variables are used by 
\*(EE
as a means to use 
.it strings.
Therefore when using a 
.it char 
array or pointer it must be dereferenced only to a ``\c
.it "char \*(**\*|\c"
\&''.
Also, variables may not have parentheses in their references.
For example:
.ne 12
.nf 
.in +5
.s3
## struct xxx
## {
	int	i;
##	int	\*(**ip;
## }	\*(**\*(**struct_var;

/\*(** not allowed \*(**/
##	delete p where p.ifield = \*(**(\*(**struct_var)->ip

/\*(** allowed \*(**/
##	delete p where p.ifield = \*(**struct_var[0]->ip
.i0
.fi
.s3
C variables declared to 
\*(EE
have either global or local scope.
Their scope is local if their declaration is within a free
(not bound to a retrieve)
block declared to \*(EE.
For example:
.s3
.ne 10
.in +5
.nf
/\*(** globals scope variable \*(**/
## int	Gint;

func(i)
int	i;
## {
	/\*(** local scope variable \*(**/
##	int	\*(**gintp;
	...
## }
.fi
.in -5
.s3
If a variable of one of the char types is used almost anywhere
in an 
\*(EE 
statement
the content of that variable is used at run time.
For example:
.nf
.in +5
.s3
##	char	\*(**dbname[MAXDATABASES + 1];
	int	current_db;

	dbname[current_db] = "demo";
##	ingres dbname[current_db]
.in -5
.fi
.s3
will cause 
\*(II 
to be invoked with data base ``demo''.
However, if a variable's name is to be used as a 
constant, then the non-referencing operator `#' should be used.
For example:
.nf
.ne 13
.in +5
.s3
## char	\*(**demo;

	demo = "my_database";

	/\*(** ingres \-d my_database \*(**/
##	ingres "\-d" demo

	/\*(** ingres \-d demo \*(**/
##	ingres "\-d" #demo
.in -5
.fi
.s3
The C-preprocessor's #include feature may be used
on files containing
equel statements and declarations if these files
are named 
.it anything\c
.bd \&.q.h.
An 
\*(EE 
processed version of the file, which will
be #included by the C-preprocessor, is left in 
.it anything\c
.bd \&.c.h.
.s1
.bd "Errors and Interrupts"
.s2
\*(II 
and run-time 
\*(EE 
errors cause the routine
.bd IIerror
to be called,
with the error number and the parameters to the error in an
array of string pointers as in a C language main routine.
The error message will be looked up
and printed. before printing the error message, the routine 
(\*(**IIprint_err)() is called with the error number that ocurred as 
its single argument. 
The error message corresponding to the error number returned
by (\*(**IIprint_err)() will be printed.
Printing will be supressed if (\*(**IIprint_err)() returns 0.
IIprint_err may be reassigned to, and
is useful for programs which map 
\*(II 
errors into their own
error messages.
In addition, if the ``\c
.bd \-d\c
\&''
flag was set the file name and line number of the error will be printed.
The user may write an IIerror routine to do other tasks
as long as the setting of IIerrflag is not modified
as this is used to exit retrieves correctly.
.s3
Interrupts are caught by equel if they are not being ignored.
This insures that the rest of \*(II is in sync with the 
\*(EE 
process.
There is a function pointer, IIinterrupt, which points to a function to
call after the interrupt is caught. The user may use this to service the
interrupt.
It is initialized to "exit()" and is called with \*-1 as its argument.
For example:
.nf
.s3
	extern int (\*(**IIinterrupt)();
	extern reset();

	setexit();
	IIinterrupt = reset;
	mainloop();
.s3
.fi
To ignore interrupts, signal() should be called before the
## ingres satatement is executed.
.sh FILES
.in +5
.ti -5
\&.../files/error7_\*(**
.br
Can be used by 
the user to decipher \*(II error numbers.
.ti -5
/lib/libq.a
.br
Run time library.
.sh "SEE ALSO"
\&.../doc/other/equeltut.q,
C reference manual,
ingres(UNIX),
quel(QUEL)
.sh BUGS
The C-code embedded in the tuple-by-tuple retrieve operation
may not contain additional 
QUEL statements or recursive invocations of 
\*(II.
.s3
There is no way to specify an
.bd i1
format C-variable.
.s3
Includes of an equel file within a parameterized target list,
or within a C variable's array subscription brackets, isn't
done correctly.
