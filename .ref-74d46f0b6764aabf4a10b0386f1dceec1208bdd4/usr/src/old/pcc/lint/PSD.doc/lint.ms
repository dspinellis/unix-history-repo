.\" %sccs.include.proprietary.roff%
.\"
.\"	@(#)lint.ms	6.2 (Berkeley) %G%
.\"
.EH 'PS1:9-%''Lint, a C Program Checker'
.OH 'Lint, a C Program Checker''PS1:9-%'
.\".RP
.ND "July 26, 1978"
.OK
.\"Program Portability
.\"Strong Type Checking
.TL
Lint, a C Program Checker
.AU "MH 2C-559" 3968
S. C. Johnson
.AI
.MH
.AB
.PP
.I Lint
is a command which examines C source programs,
detecting
a number of bugs and obscurities.
It enforces the type rules of C more strictly than
the C compilers.
It may also be used to enforce a number of portability
restrictions involved in moving
programs between different machines and/or operating systems.
Another option detects a number of wasteful, or error prone, constructions
which nevertheless are, strictly speaking, legal.
.PP
.I Lint
accepts multiple input files and library specifications, and checks them for consistency.
.PP
The separation of function between
.I lint
and the C compilers has both historical and practical
rationale.
The compilers turn C programs into executable files rapidly
and efficiently.
This is possible in part because the
compilers do not do sophisticated
type checking, especially between
separately compiled programs.
.I Lint
takes a more global, leisurely view of the program,
looking much more carefully at the compatibilities.
.PP
This document discusses the use of
.I lint ,
gives an overview of the implementation, and gives some hints on the
writing of machine independent C code.
.AE
.CS 10 2 12 0 0 5
.SH
Introduction and Usage
.PP
Suppose there are two C
.[
Kernighan Ritchie Programming Prentice 1978
.]
source files,
.I file1. c
and
.I file2.c  ,
which are ordinarily compiled and loaded together.
Then the command
.DS
lint  file1.c  file2.c
.DE
produces messages describing inconsistencies and inefficiencies
in the programs.
The program enforces the typing rules of C
more strictly than the C compilers
(for both historical and practical reasons)
enforce them.
The command
.DS
lint  \-p  file1.c  file2.c
.DE
will produce, in addition to the above messages, additional messages
which relate to the portability of the programs to other operating
systems and machines.
Replacing the
.B \-p
by
.B \-h
will produce messages about various error-prone or wasteful constructions
which, strictly speaking, are not bugs.
Saying
.B \-hp
gets the whole works.
.PP
The next several sections describe the major messages;
the document closes with sections
discussing the implementation and giving suggestions
for writing portable C.
An appendix gives a summary of the
.I lint
options.
.SH
A Word About Philosophy
.PP
Many of the facts which
.I lint
needs may be impossible to
discover.
For example, whether a given function in a program ever gets called
may depend on the input data.
Deciding whether
.I exit
is ever called is equivalent to solving the famous ``halting problem,'' known to be
recursively undecidable.
.PP
Thus, most of the
.I lint
algorithms are a compromise.
If a function is never mentioned, it can never be called.
If a function is mentioned,
.I lint
assumes it can be called; this is not necessarily so, but in practice is quite reasonable.
.PP
.I Lint
tries to give information with a high degree of relevance.
Messages of the form ``\fIxxx\fR might be a bug''
are easy to generate, but are acceptable only in proportion
to the fraction of real bugs they uncover.
If this fraction of real bugs is too small, the messages lose their credibility
and serve merely to clutter up the output,
obscuring the more important messages.
.PP
Keeping these issues in mind, we now consider in more detail
the classes of messages which
.I lint
produces.
.SH
Unused Variables and Functions
.PP
As sets of programs evolve and develop,
previously used variables and arguments to
functions may become unused;
it is not uncommon for external variables, or even entire
functions, to become unnecessary, and yet
not be removed from the source.
These ``errors of commission'' rarely cause working programs to fail, but they are a source
of inefficiency, and make programs harder to understand
and change.
Moreover, information about such unused variables and functions can occasionally
serve to discover bugs; if a function does a necessary job, and
is never called, something is wrong!
.PP
.I Lint
complains about variables and functions which are defined but not otherwise
mentioned.
An exception is variables which are declared through explicit
.B extern
statements but are never referenced; thus the statement
.DS
extern  float  sin(\|);
.DE
will evoke no comment if
.I sin
is never used.
Note that this agrees with the semantics of the C compiler.
In some cases, these unused external declarations might be of some interest; they
can be discovered by adding the
.B \-x
flag to the
.I lint
invocation.
.PP
Certain styles of programming
require many functions to be written with similar interfaces;
frequently, some of the arguments may be unused
in many of the calls.
The
.B \-v
option is available to suppress the printing of
complaints about unused arguments.
When
.B \-v
is in effect, no messages are produced about unused
arguments except for those
arguments which are unused and also declared as
register arguments; this can be considered
an active (and preventable) waste of the register
resources of the machine.
.PP
There is one case where information about unused, or
undefined, variables is more distracting
than helpful.
This is when
.I lint
is applied to some, but not all, files out of a collection
which are to be loaded together.
In this case, many of the functions and variables defined
may not be used, and, conversely,
many functions and variables defined elsewhere may be used.
The
.B \-u
flag may be used to suppress the spurious messages which might otherwise appear.
.SH
Set/Used Information
.PP
.I Lint
attempts to detect cases where a variable is used before it is set.
This is very difficult to do well;
many algorithms take a good deal of time and space,
and still produce messages about perfectly valid programs.
.I Lint
detects local variables (automatic and register storage classes)
whose first use appears physically earlier in the input file than the first assignment to the variable.
It assumes that taking the address of a variable constitutes a ``use,'' since the actual use
may occur at any later time, in a data dependent fashion.
.PP
The restriction to the physical appearance of variables in the file makes the
algorithm very simple and quick to implement,
since the true flow of control need not be discovered.
It does mean that
.I lint
can complain about some programs which are legal,
but these programs would probably be considered bad on stylistic grounds (e.g. might
contain at least two \fBgoto\fR's).
Because static and external variables are initialized to 0,
no meaningful information can be discovered about their uses.
The algorithm deals correctly, however, with initialized automatic variables, and variables
which are used in the expression which first sets them.
.PP
The set/used information also permits recognition of those local variables which are set
and never used; these form a frequent source of inefficiencies, and may also be symptomatic of bugs.
.SH
Flow of Control
.PP
.I Lint
attempts to detect unreachable portions of the programs which it processes.
It will complain about unlabeled statements immediately following
\fBgoto\fR, \fBbreak\fR, \fBcontinue\fR, or \fBreturn\fR statements.
An attempt is made to detect loops which can never be left at the bottom, detecting the
special cases
\fBwhile\fR( 1 ) and \fBfor\fR(;;) as infinite loops.
.I Lint
also complains about loops which cannot be entered at the top;
some valid programs may have such loops, but at best they are bad style,
at worst bugs.
.PP
.I Lint
has an important area of blindness in the flow of control algorithm:
it has no way of detecting functions which are called and never return.
Thus, a call to
.I exit
may cause unreachable code which
.I lint
does not detect; the most serious effects of this are in the
determination of returned function values (see the next section).
.PP
One form of unreachable statement is not usually complained about by
.I lint;
a
.B break
statement that cannot be reached causes no message.
Programs generated by
.I yacc ,
.[
Johnson Yacc 1975
.]
and especially
.I lex ,
.[
Lesk Lex
.]
may have literally hundreds of unreachable
.B break
statements.
The
.B \-O
flag in the C compiler will often eliminate the resulting object code inefficiency.
Thus, these unreached statements are of little importance,
there is typically nothing the user can do about them, and the
resulting messages would clutter up the
.I lint
output.
If these messages are desired,
.I lint
can be invoked with the
.B \-b
option.
.SH
Function Values
.PP
Sometimes functions return values which are never used;
sometimes programs incorrectly use function ``values''
which have never been returned.
.I Lint
addresses this problem in a number of ways.
.PP
Locally, within a function definition,
the appearance of both
.DS
return(  \fIexpr\fR  );
.DE
and
.DS
return ;
.DE
statements is cause for alarm;
.I lint
will give the message
.DS
function \fIname\fR contains return(e) and return
.DE
The most serious difficulty with this is detecting when a function return is implied
by flow of control reaching the end of the function.
This can be seen with a simple example:
.DS
.ta .5i 1i 1.5i
\fRf ( a ) {
	if ( a ) return ( 3 );
	g (\|);
	}
.DE
Notice that, if \fIa\fR tests false, \fIf\fR will call \fIg\fR and then return
with no defined return value; this will trigger a complaint from
.I lint .
If \fIg\fR, like \fIexit\fR, never returns,
the message will still be produced when in fact nothing is wrong.
.PP
In practice, some potentially serious bugs have been discovered by this feature;
it also accounts for a substantial fraction of the ``noise'' messages produced
by
.I lint .
.PP
On a global scale,
.I lint
detects cases where a function returns a value, but this value is sometimes,
or always, unused.
When the value is always unused, it may constitute an inefficiency in the function definition.
When the value is sometimes unused, it may represent bad style (e.g., not testing for
error conditions).
.PP
The dual problem, using a function value when the function does not return one,
is also detected.
This is a serious problem.
Amazingly, this bug has been observed on a couple of occasions
in ``working'' programs; the desired function value just happened to have been computed
in the function return register!
.SH
Type Checking
.PP
.I Lint
enforces the type checking rules of C more strictly than the compilers do.
The additional checking is in four major areas:
across certain binary operators and implied assignments,
at the structure selection operators,
between the definition and uses of functions,
and in the use of enumerations.
.PP
There are a number of operators which have an implied balancing between types of the operands.
The assignment, conditional ( ?\|: ), and relational operators
have this property; the argument
of a \fBreturn\fR statement,
and expressions used in initialization also suffer similar conversions.
In these operations,
\fBchar\fR, \fBshort\fR, \fBint\fR, \fBlong\fR, \fBunsigned\fR, \fBfloat\fR, and \fBdouble\fR types may be freely intermixed.
The types of pointers must agree exactly,
except that arrays of \fIx\fR's can, of course, be intermixed with pointers to \fIx\fR's.
.PP
The type checking rules also require that, in structure references, the
left operand of the \(em> be a pointer to structure, the left operand of the \fB.\fR
be a structure, and the right operand of these operators be a member
of the structure implied by the left operand.
Similar checking is done for references to unions.
.PP
Strict rules apply to function argument and return value
matching.
The types \fBfloat\fR and \fBdouble\fR may be freely matched,
as may the types \fBchar\fR, \fBshort\fR, \fBint\fR, and \fBunsigned\fR.
Also, pointers can be matched with the associated arrays.
Aside from this, all actual arguments must agree in type with their declared counterparts.
.PP
With enumerations, checks are made that enumeration variables or members are not mixed
with other types, or other enumerations,
and that the only operations applied are =, initialization, ==, !=, and function arguments and return values.
.SH
Type Casts
.PP
The type cast feature in C was introduced largely as an aid
to producing more portable programs.
Consider the assignment
.DS
p = 1 ;
.DE
where
.I p
is a character pointer.
.I Lint
will quite rightly complain.
Now, consider the assignment
.DS
p = (char \(**)1 ;
.DE
in which a cast has been used to
convert the integer to a character pointer.
The programmer obviously had a strong motivation
for doing this, and has clearly signaled his intentions.
It seems harsh for
.I lint
to continue to complain about this.
On the other hand, if this code is moved to another
machine, such code should be looked at carefully.
The
.B \-c
flag controls the printing of comments about casts.
When
.B \-c
is in effect, casts are treated as though they were assignments
subject to complaint; otherwise, all legal casts are passed without comment,
no matter how strange the type mixing seems to be.
.SH
Nonportable Character Use
.PP
On the PDP-11, characters are signed quantities, with a range
from \-128 to 127.
On most of the other C implementations, characters take on only positive
values.
Thus,
.I lint
will flag certain comparisons and assignments as being
illegal or nonportable.
For example, the fragment
.DS
char c;
	...
if( (c = getchar(\|)) < 0 ) ....
.DE
works on the PDP-11, but
will fail on machines where characters always take
on positive values.
The real solution is to declare
.I c
an integer, since
.I getchar
is actually returning
integer values.
In any case,
.I lint
will say
``nonportable character comparison''.
.PP
A similar issue arises with bitfields; when assignments
of constant values are made to bitfields, the field may
be too small to hold the value.
This is especially true because
on some machines bitfields are considered as signed
quantities.
While it may seem unintuitive to consider
that a two bit field declared of type
.B int
cannot hold the value 3, the problem disappears
if the bitfield is declared to have type
.B unsigned .
.SH
Assignments of longs to ints
.PP
Bugs may arise from the assignment of
.B long
to
an
.B int ,
which loses accuracy.
This may happen in programs
which have been incompletely converted to use
.B typedefs .
When a
.B typedef
variable
is changed from \fBint\fR to \fBlong\fR,
the program can stop working because
some intermediate results may be assigned
to \fBints\fR, losing accuracy.
Since there are a number of legitimate reasons for
assigning \fBlongs\fR to \fBints\fR, the detection
of these assignments is enabled
by the
.B \-a
flag.
.SH
Strange Constructions
.PP
Several perfectly legal, but somewhat strange, constructions
are flagged by
.I lint;
the messages hopefully encourage better code quality, clearer style, and
may even point out bugs.
The
.B \-h
flag is used to enable these checks.
For example, in the statement
.DS
\(**p++ ;
.DE
the \(** does nothing; this provokes the message ``null effect'' from
.I lint .
The program fragment
.DS
unsigned x ;
if( x < 0 ) ...
.DE
is clearly somewhat strange; the
test will never succeed.
Similarly, the test
.DS
if( x > 0 ) ...
.DE
is equivalent to
.DS
if( x != 0 )
.DE
which may not be the intended action.
.I Lint
will say ``degenerate unsigned comparison'' in these cases.
If one says
.DS
if( 1 != 0 ) ....
.DE
.I lint
will report
``constant in conditional context'', since the comparison
of 1 with 0 gives a constant result.
.PP
Another construction
detected by
.I lint
involves
operator precedence.
Bugs which arise from misunderstandings about the precedence
of operators can be accentuated by spacing and formatting,
making such bugs extremely hard to find.
For example, the statements
.DS
if( x&077 == 0 ) ...
.DE
or
.DS
x<\h'-.3m'<2 + 40
.DE
probably do not do what was intended.
The best solution is to parenthesize such expressions,
and
.I lint
encourages this by an appropriate message.
.PP
Finally, when the
.B \-h
flag is in force
.I lint
complains about variables which are redeclared in inner blocks
in a way that conflicts with their use in outer blocks.
This is legal, but is considered by many (including the author) to
be bad style, usually unnecessary, and frequently a bug.
.SH
Ancient History
.PP
There are several forms of older syntax which are being officially
discouraged.
These fall into two classes, assignment operators and initialization.
.PP
The older forms of assignment operators (e.g., =+, =\-, . . . )
could cause ambiguous expressions, such as
.DS
a  =\-1 ;
.DE
which could be taken as either
.DS
a =\-  1 ;
.DE
or
.DS
a  =  \-1 ;
.DE
The situation is especially perplexing if this
kind of ambiguity arises as the result of a macro substitution.
The newer, and preferred operators (+=, \-=, etc. )
have no such ambiguities.
To spur the abandonment of the older forms,
.I lint
complains about these old fashioned operators.
.PP
A similar issue arises with initialization.
The older language allowed
.DS
int  x  \fR1 ;
.DE
to initialize
.I x
to 1.
This also caused syntactic difficulties: for example,
.DS
int  x  ( \-1 ) ;
.DE
looks somewhat like the beginning of a function declaration:
.DS
int  x  ( y ) {  . . .
.DE
and the compiler must read a fair ways past
.I x
in order to sure what the declaration really is..
Again, the problem is even more perplexing when the
initializer involves a macro.
The current syntax places an equals sign between the
variable and the initializer:
.DS
int  x  =  \-1 ;
.DE
This is free of any possible syntactic ambiguity.
.SH
Pointer Alignment
.PP
Certain pointer assignments may be reasonable on some machines,
and illegal on others, due entirely to
alignment restrictions.
For example, on the PDP-11, it is reasonable
to assign integer pointers to double pointers, since
double precision values may begin on any integer boundary.
On the Honeywell 6000, double precision values must begin
on even word boundaries;
thus, not all such assignments make sense.
.I Lint
tries to detect cases where pointers are assigned to other
pointers, and such alignment problems might arise.
The message ``possible pointer alignment problem''
results from this situation whenever either the
.B \-p
or
.B \-h
flags are in effect.
.SH
Multiple Uses and Side Effects
.PP
In complicated expressions, the best order in which to evaluate
subexpressions may be highly machine dependent.
For example, on machines (like the PDP-11) in which the stack
runs backwards, function arguments will probably be best evaluated
from right-to-left; on machines with a stack running forward,
left-to-right seems most attractive.
Function calls embedded as arguments of other functions
may or may not be treated similarly to ordinary arguments.
Similar issues arise with other operators which have side effects,
such as the assignment operators and the increment and decrement operators.
.PP
In order that the efficiency of C on a particular machine not be
unduly compromised, the C language leaves the order
of evaluation of complicated expressions up to the
local compiler, and, in fact, the various C compilers have considerable
differences in the order in which they will evaluate complicated
expressions.
In particular, if any variable is changed by a side effect, and
also used elsewhere in the same expression, the result is explicitly undefined.
.PP
.I Lint
checks for the important special case where
a simple scalar variable is affected.
For example, the statement
.DS
\fIa\fR[\fIi\|\fR] = \fIb\fR[\fIi\fR++] ;
.DE
will draw the complaint:
.DS
warning: \fIi\fR evaluation order undefined
.DE
.SH
Implementation
.PP
.I Lint
consists of two programs and a driver.
The first program is a version of the
Portable C Compiler
.[
Johnson Ritchie BSTJ Portability Programs System
.]
.[
Johnson portable compiler  1978
.]
which is the basis of the
IBM 370, Honeywell 6000, and Interdata 8/32 C compilers.
This compiler does lexical and syntax analysis on the input text,
constructs and maintains symbol tables, and builds trees for expressions.
Instead of writing an intermediate file which is passed to
a code generator, as the other compilers
do,
.I lint
produces an intermediate file which consists of lines of ascii text.
Each line contains an external variable name,
an encoding of the context in which it was seen (use, definition, declaration, etc.),
a type specifier, and a source file name and line number.
The information about variables local to a function or file
is collected
by accessing the symbol table, and examining the expression trees.
.PP
Comments about local problems are produced as detected.
The information about external names is collected
onto an intermediate file.
After all the source files and library descriptions have
been collected, the intermediate file is sorted
to bring all information collected about a given external
name together.
The second, rather small, program then reads the lines
from the intermediate file and compares all of the
definitions, declarations, and uses for consistency.
.PP
The driver controls this
process, and is also responsible for making the options available
to both passes of
.I lint .
.SH
Portability
.PP
C on the Honeywell and IBM systems is used, in part, to write system code for the host operating system.
This means that the implementation of C tends to follow local conventions rather than
adhere strictly to
.UX
system conventions.
Despite these differences, many C programs have been successfully moved to GCOS and the various IBM
installations with little effort.
This section describes some of the differences between the implementations, and
discusses the
.I lint
features which encourage portability.
.PP
Uninitialized external variables are treated differently in different
implementations of C.
Suppose two files both contain a declaration without initialization, such as
.DS
int a ;
.DE
outside of any function.
The
.UX
loader will resolve these declarations, and cause only a single word of storage
to be set aside for \fIa\fR.
Under the GCOS and IBM implementations, this is not feasible (for various stupid reasons!)
so each such declaration causes a word of storage to be set aside and called \fIa\fR.
When loading or library editing takes place, this causes fatal conflicts which prevent
the proper operation of the program.
If
.I lint
is invoked with the \fB\-p\fR flag,
it will detect such multiple definitions.
.PP
A related difficulty comes from the amount of information retained about external names during the
loading process.
On the
.UX
system, externally known names have seven significant characters, with the upper/lower
case distinction kept.
On the IBM systems, there are eight significant characters, but the case distinction
is lost.
On GCOS, there are only six characters, of a single case.
This leads to situations where programs run on the
.UX
system, but encounter loader
problems on the IBM or GCOS systems.
.I Lint
.B \-p
causes all external symbols to be mapped to one case and truncated to six characters,
providing a worst-case analysis.
.PP
A number of differences arise in the area of character handling: characters in the
.UX
system are eight bit ascii, while they are eight bit ebcdic on the IBM, and
nine bit ascii on GCOS.
Moreover, character strings go from high to low bit positions (``left to right'')
on GCOS and IBM, and low to high (``right to left'') on the PDP-11.
This means that code attempting to construct strings
out of character constants, or attempting to use characters as indices
into arrays, must be looked at with great suspicion.
.I Lint
is of little help here, except to flag multi-character character constants.
.PP
Of course, the word sizes are different!
This causes less trouble than might be expected, at least when
moving from the
.UX
system (16 bit words) to the IBM (32 bits) or GCOS (36 bits).
The main problems are likely to arise in shifting or masking.
C now supports a bit-field facility, which can be used to write much of
this code in a reasonably portable way.
Frequently, portability of such code can be enhanced by
slight rearrangements in coding style.
Many of the incompatibilities seem to have the flavor of writing
.DS
x &= 0177700 ;
.DE
to clear the low order six bits of \fIx\fR.
This suffices on the PDP-11, but fails badly on GCOS and IBM.
If the bit field feature cannot be used, the same effect can be obtained by
writing
.DS
x &= \(ap 077 ;
.DE
which will work on all these machines.
.PP
The right shift operator is arithmetic shift on the PDP-11, and logical shift on most
other machines.
To obtain a logical shift on all machines, the left operand can be
typed \fBunsigned\fR.
Characters are considered signed integers on the PDP-11, and unsigned on the other machines.
This persistence of the sign bit may be reasonably considered a bug in the PDP-11 hardware
which has infiltrated itself into the C language.
If there were a good way to discover the programs which would be affected, C could be changed;
in any case,
.I lint
is no help here.
.PP
The above discussion may have made the problem of portability seem
bigger than it in fact is.
The issues involved here are rarely subtle or mysterious, at least to the
implementor of the program, although they can involve some work to straighten out.
The most serious bar to the portability of
.UX
system utilities has been the inability to mimic
essential
.UX
system functions on the other systems.
The inability to seek to a random character position in a text file, or to establish a pipe
between processes, has involved far more rewriting
and debugging than any of the differences in C compilers.
On the other hand,
.I lint
has been very helpful
in moving the
.UX
operating system and associated
utility programs to other machines.
.SH
Shutting Lint Up
.PP
There are occasions when
the programmer is smarter than
.I lint .
There may be valid reasons for ``illegal'' type casts,
functions with a variable number of arguments, etc.
Moreover, as specified above, the flow of control information
produced by
.I lint
often has blind spots, causing occasional spurious
messages about perfectly reasonable programs.
Thus, some way of communicating with
.I lint ,
typically to shut it up, is desirable.
.PP
The form which this mechanism should take is not at all clear.
New keywords would require current and old compilers to
recognize these keywords, if only to ignore them.
This has both philosophical and practical problems.
New preprocessor syntax suffers from similar problems.
.PP
What was finally done was to cause a number of words
to be recognized by
.I lint
when they were embedded in comments.
This required minimal preprocessor changes;
the preprocessor just had to agree to pass comments
through to its output, instead of deleting them
as had been previously done.
Thus,
.I lint
directives are invisible to the compilers, and
the effect on systems with the older preprocessors
is merely that the
.I lint
directives don't work.
.PP
The first directive is concerned with flow of control information;
if a particular place in the program cannot be reached,
but this is not apparent to
.I lint ,
this can be asserted by the directive
.DS
/* NOTREACHED */
.DE
at the appropriate spot in the program.
Similarly, if it is desired to turn off
strict type checking for
the next expression, the directive
.DS
/* NOSTRICT */
.DE
can be used; the situation reverts to the
previous default after the next expression.
The
.B \-v
flag can be turned on for one function by the directive
.DS
/* ARGSUSED */
.DE
Complaints about variable number of arguments in calls to a function
can be turned off by the directive
.DS
/* VARARGS */
.DE
preceding the function definition.
In some cases, it is desirable to check the
first several arguments, and leave the later arguments unchecked.
This can be done by following the VARARGS keyword immediately
with a digit giving the number of arguments which should be checked; thus,
.DS
/* VARARGS2 */
.DE
will cause the first two arguments to be checked, the others unchecked.
Finally, the directive
.DS
/* LINTLIBRARY */
.DE
at the head of a file identifies this file as
a library declaration file; this topic is worth a
section by itself.
.SH
Library Declaration Files
.PP
.I Lint
accepts certain library directives, such as
.DS
\-ly
.DE
and tests the source files for compatibility with these libraries.
This is done by accessing library description files whose
names are constructed from the library directives.
These files all begin with the directive
.DS
/* LINTLIBRARY */
.DE
which is followed by a series of dummy function
definitions.
The critical parts of these definitions
are the declaration of the function return type,
whether the dummy function returns a value, and
the number and types of arguments to the function.
The VARARGS and ARGSUSED directives can
be used to specify features of the library functions.
.PP
.I Lint
library files are processed almost exactly like ordinary
source files.
The only difference is that functions which are defined on a library file,
but are not used on a source file, draw no complaints.
.I Lint
does not simulate a full library search algorithm,
and complains if the source files contain a redefinition of
a library routine (this is a feature!).
.PP
By default,
.I lint
checks the programs it is given against a standard library
file, which contains descriptions of the programs which
are normally loaded when
a C program
is run.
When the
.B -p
flag is in effect, another file is checked containing
descriptions of the standard I/O library routines
which are expected to be portable across various machines.
The
.B -n
flag can be used to suppress all library checking.
.SH
Bugs, etc.
.PP
.I Lint
was a difficult program to write, partially
because it is closely connected with matters of programming style,
and partially because users usually don't notice bugs which cause
.I lint
to miss errors which it should have caught.
(By contrast, if
.I lint
incorrectly complains about something that is correct, the
programmer reports that immediately!)
.PP
A number of areas remain to be further developed.
The checking of structures and arrays is rather inadequate;
size
incompatibilities go unchecked,
and no attempt is made to match up structure and union
declarations across files.
Some stricter checking of the use of the
.B typedef
is clearly desirable, but what checking is appropriate, and how
to carry it out, is still to be determined.
.PP
.I Lint
shares the preprocessor with the C compiler.
At some point it may be appropriate for a
special version of the preprocessor to be constructed
which checks for things such as unused macro definitions,
macro arguments which have side effects which are
not expanded at all, or are expanded more than once, etc.
.PP
The central problem with
.I lint
is the packaging of the information which it collects.
There are many options which
serve only to turn off, or slightly modify,
certain features.
There are pressures to add even more of these options.
.PP
In conclusion, it appears that the general notion of having two
programs is a good one.
The compiler concentrates on quickly and accurately turning the
program text into bits which can be run;
.I lint
concentrates on issues
of portability, style, and efficiency.
.I Lint
can afford to be wrong, since incorrectness and over-conservatism
are merely annoying, not fatal.
The compiler can be fast since it knows that
.I lint
will cover its flanks.
Finally, the programmer can
concentrate at one stage
of the programming process solely on the algorithms,
data structures, and correctness of the
program, and then later retrofit,
with the aid of
.I lint ,
the desirable properties of universality and portability.
.SG MH-1273-SCJ-unix
.\".bp
.[
$LIST$
.]
.bp
.SH
Appendix:   Current Lint Options
.PP
The command currently has the form
.DS
lint\fR [\fB\-\fRoptions ] files... library-descriptors...
.DE
The options are
.IP \fBh\fR
Perform heuristic checks
.IP \fBp\fR
Perform portability checks
.IP \fBv\fR
Don't report unused arguments
.IP \fBu\fR
Don't report unused or undefined externals
.IP \fBb\fR
Report unreachable
.B break
statements.
.IP \fBx\fR
Report unused external declarations
.IP \fBa\fR
Report assignments of
.B long
to
.B int
or shorter.
.IP \fBc\fR
Complain about questionable casts
.IP \fBn\fR
No library checking is done
.IP \fBs\fR
Same as
.B h
(for historical reasons)
