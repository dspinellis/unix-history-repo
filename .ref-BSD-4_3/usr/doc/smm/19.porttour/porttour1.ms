.\"	@(#)porttour1.ms	6.3 (Berkeley) 5/30/86
.\"
.ds f \fIf77\fP
.ds U \s-2UNIX\s0
.ds V \s-2VAX\s0
.de Bl
.na
.fi
.ll 3i
..
.de Sp
.sp 0.25v
..
.TL
A Tour Through the Portable C Compiler
.AU
S. C. Johnson
.AI
AT&T Bell Laboratories
.sp
.AU
Donn Seeley
.AI
Department of Computer Science
University of Utah
.AB
.OH 'A Tour Through the Portable C Compiler''SMM:19-%'
.EH 'SMM:19-%''A Tour Through the Portable C Compiler'
.PP
Since its introduction,
the Portable C Compiler has become the standard
\*U C compiler for many machines.
Three quarters or more of the code
in the compiler is machine independent
and much of the rest can be generated easily
using knowledge of the target architecture.
This paper describes the structure
and organization of the compiler
and tries to further simplify the job
of the compiler porter.
.PP
This document originally appeared with the Seventh Edition of \*U,
and has been revised and extended for publication
with the Fourth Berkeley Software Distribution.
The new material covers changes which
have been made in the compiler since the Seventh Edition,
and includes some discussion of secondary topics
which were thought to be of interest
in future ports of the compiler.
.sp
.LP
Revised April, 1986
.AE
.SH
Introduction
.PP
A C compiler has been implemented that has proved to be quite portable,
serving as the basis for C compilers on roughly a dozen machines, including
the DEC \*V, Honeywell 6000, IBM 370, and Interdata 8/32.
The compiler is highly compatible with the C language standard.
.[
Kernighan Ritchie Prentice 1978
.]
.PP
Among the goals of this compiler are portability, high reliability,
and the use of state-of-the-art techniques and tools wherever practical.
Although the efficiency of the compiling process is not
a primary goal, the compiler is efficient enough, and produces
good enough code, to serve as a production compiler.
.PP
The language implemented is highly compatible with the current PDP-11 version of C.
Moreover, roughly 75% of the compiler, including
nearly all the syntactic and semantic routines, is machine independent.
The compiler also serves as the major portion of the program
.I lint ,
described elsewhere.
.[
Johnson Lint Program Checker
.]
.PP
A number of earlier attempts to make portable compilers are worth noting.
While on CO-OP assignment to Bell Labs in 1973, Alan Snyder wrote a portable C
compiler which was the basis of his Master's Thesis at M.I.T.
.[
Snyder Portable C Compiler
.]
This compiler was very slow and complicated, and contained a number of rather
serious implementation difficulties;
nevertheless, a number of Snyder's ideas appear in this work.
.PP
Most earlier portable compilers, including Snyder's, have proceeded by
defining an intermediate language, perhaps based
on three-address code or code for a stack machine,
and writing a machine independent program to
translate from the source code to this
intermediate code.
The intermediate code is then read by a second pass, and interpreted or compiled.
This approach is elegant, and has a number of advantages, especially if the
target machine is far removed from the host.
It suffers from some disadvantages as well.
Some constructions, like initialization and subroutine prologs,
are difficult or expensive to express in a
machine independent way that still allows them
to be easily adapted to the target assemblers.
Most of these approaches require a symbol table to be constructed
in the second (machine dependent) pass, and/or
require powerful target assemblers.
Also, many conversion operators may be generated
that have no effect on a given machine,
but may be needed on others (for example, pointer to pointer
conversions usually do nothing in C, but must be generated because
there are some machines where they are significant).
.PP
For these reasons, the first pass of the portable compiler
is not entirely machine independent.
It contains some machine dependent features, such as initialization, subroutine
prolog and epilog, certain storage allocation functions,
code for the
.I switch
statement, and code to throw out unneeded conversion operators.
.PP
As a crude measure of the degree of
portability actually achieved,
the Interdata 8/32 C compiler has
roughly 600 machine dependent lines of source out of 4600 in Pass 1, and 1000
out of 3400 in Pass 2.
In total, 1600 out of 8000, or 20%,
of the total source is machine dependent
(12% in Pass 1, 30% in Pass 2).
These percentages can be expected to rise slightly as the
compiler is tuned.
The percentage of machine-dependent code for the IBM is 22%, for
the Honeywell 25%.
If the assembler format and structure were the same for all
these machines, perhaps another 5-10% of the code
would become machine independent.
.PP
These figures are sufficiently misleading as to be almost
meaningless.
A large fraction of the machine dependent code can be converted
in a straightforward, almost mechanical way.
On the other hand, a certain amount of the code requires hard
intellectual effort to convert, since the algorithms
embodied in this part of the code are typically complicated
and machine dependent.
.PP
To summarize, however, if you need a C compiler written for a machine with
a reasonable architecture, the compiler is already three quarters finished!
.SH
Overview
.PP
This paper discusses the structure and organization of the portable compiler.
The intent is to give the big picture, rather than discussing the details of a particular machine
implementation.
After a brief overview and a discussion of the source file structure,
the paper describes the major data structures, and then delves more
closely into the two passes.
Some of the theoretical work on which the compiler is based, and
its application to the compiler, is discussed elsewhere.
.[
johnson portable theory practice
.]
One of the major design issues in any C compiler, the design of
the calling sequence and stack frame, is the subject of a separate
memorandum.
.[
johnson lesk ritchie calling sequence
.]
.PP
The compiler consists of two passes,
.I pass1
and
.I pass2 ,
that together turn C source code into assembler code for the target
machine.
The two passes are preceded by a preprocessor, that
handles the
.B "#define"
and
.B "#include"
statements, and related features (e.g.,
.B #ifdef ,
etc.).
The two passes may optionally be followed by
a machine dependent code improver.
.PP
The output of the preprocessor is a text file that is read as the standard
input of the first pass.
This
produces as standard output another text file that becomes the standard
input of the second pass.
The second pass produces, as standard output, the desired assembler language source code.
The code improver, if used, converts the assembler code
to more effective code,
and the result is passed to the assembler.
The preprocessor and the two passes
all write error messages on the standard error file.
Thus the compiler itself makes few demands on the I/O
library support, aiding in the bootstrapping process.
.PP
The division of the compiler into two passes is somewhat artificial.
The compiler can optionally be loaded
so that both passes operate in the same program.
This ``one pass'' operation eliminates the overhead of
reading and writing the intermediate file, so the compiler
operates about 30% faster in this mode.
It also occupies about 30% more space than the larger
of the two component passes.
This ``one pass'' compiler is the standard version
on machines with large address spaces, such as the \*V.
.PP
Because the compiler is fundamentally structured as two
passes, even when loaded as one, this document primarily
describes the two pass version.
.PP
The first pass does the lexical analysis, parsing, and
symbol table maintenance.
It also constructs parse trees for expressions,
and keeps track of the types of the nodes in these trees.
Additional code is devoted to initialization.
Machine dependent portions of the first pass serve to
generate subroutine prologs and epilogs,
code for switches, and code for branches, label definitions,
alignment operations,
changes of location counter, etc.
.PP
The intermediate file is a text file organized into lines.
Lines beginning with a right parenthesis are copied by the second
pass directly to its output file, with the
parenthesis stripped off.
Thus, when the first pass produces assembly code, such as subroutine prologs,
etc., each line is prefaced with a right parenthesis;
the second pass passes these lines to through to the assembler.
.PP
The major job done by the second pass is generation of code
for expressions.
The expression parse trees produced in the first pass are
written onto the
intermediate file in Polish Prefix form:
first, there is a line beginning with a period, followed by the source
file line number and name on which the expression appeared (for debugging purposes).
The successive lines represent the nodes of the parse tree,
one node per line.
Each line contains the node number, type, and
any values (e.g., values of constants)
that may appear in the node.
Lines representing nodes with descendants are immediately
followed by the left subtree of descendants, then the
right.
Since the number of descendants of any node is completely
determined by the node number, there is no need to mark the end
of the tree.
.PP
There are only two other line types in the intermediate file.
Lines beginning with a left square bracket (`[') represent the beginning of
blocks (delimited by { ... } in the C source);
lines beginning with right square brackets (`]') represent the end of blocks.
The remainder of these lines tell how much
stack space, and how many register variables,
are currently in use.
.PP
Thus, the second pass reads the intermediate files, copies the `)' lines,
makes note of the information in the `[' and `]' lines,
and devotes most of its effort to the
`.' lines and their associated expression trees, turning them
turns into assembly code to evaluate the expressions.
.PP
In the one pass version of the compiler, the expression
trees contain information useful to both logical passes.
Instead of writing the trees onto an intermediate file,
each tree is transformed in place into an acceptable
form for the code generator.
The code generator then writes the result of compiling
this tree onto the standard output.
Instead of `[' and `]' lines in the intermediate file, the information is passed
directly to the second pass routines.
Assembly code produced by the first pass is simply written out,
without the need for `)' at the head of each line.
.SH
The Source Files
.PP
The compiler source consists of 25 source files.
Several header files contain information
which is needed across various source modules.
.I Manifest.h
has declarations for node types, type manipulation
macros and other macros, and some global data definitions.
.I Macdefs.h
has machine-dependent definitions, such as the size and alignment of the
various data representations.
.I Config.h
defines symbols which control the configuration of the compiler,
including such things as the sizes of various tables
and whether the compiler is ``one pass''.
The compiler conditionally includes another file,
.I onepass.h ,
which contains definitions which are particular to
a ``one pass'' compiler.
.I Ndu.h
defines the basic tree building structure
which is used throughout the compiler
to construct expression trees.
.I Manifest.h
includes a file of opcode and type definitions named
.I pcclocal.h ;
this file is automatically generated from
a header file specific to the C compiler named
.I localdefs.h
and a public header file
.I /usr/include/pcc.h .
Another file,
.I pcctokens ,
is generated in a similar way
and contains token definitions for the compiler's Yacc
.[
Johnson Yacc Compiler cstr
.]
grammar.
Two machine independent header files,
.I pass1.h
and
.I pass2.h ,
contain the data structure and manifest definitions
for the first and second passes, respectively.
In the second pass, a machine dependent header file,
.I mac2defs.h ,
contains declarations of register names, etc.
.PP
.I Common.c
contains machine independent routines used in both passes.
These include routines for allocating and freeing trees, walking over trees,
printing debugging information, and printing error messages.
This file can be compiled in two flavors,
one for pass 1 and one for pass 2,
depending on what conditional compilation symbol is used.
.PP
Entire sections of this document are devoted to the detailed structure of the
passes.
For the moment, we just give a brief description of the files.
The first pass
is obtained by compiling and loading
.I cgram.y ,
.I code.c ,
.I common.c ,
.I local.c ,
.I optim.c ,
.I pftn.c ,
.I scan.c ,
.I stab.c ,
.I trees.c
and
.I xdefs.c .
.I Scan.c
is the lexical analyzer, which provides tokens to the
bottom-up parser which is defined by the Yacc grammar
.I cgram.y .
.I Xdefs.c
is a short file of external definitions.
.I Pftn.c
maintains the symbol table, and does initialization.
.I Trees.c
builds the expression trees, and computes the node types.
.I Optim.c
does some machine independent optimizations on the expression trees.
.I Common.c
contains service routines common to the two passes of the compiler.
All the above files are machine independent.
The files
.I local.c
and
.I code.c
contain machine dependent code for generating subroutine
prologs, switch code, and the like.
.I Stab.c
contains machine dependent code for
producing external symbol table information
which can drive a symbolic debugger.
.PP
The second pass
is produced by compiling and loading
.I allo.c ,
.I common.c ,
.I local2.c ,
.I match.c ,
.I order.c ,
.I reader.c
and
.I table.c .
.I Reader.c
reads the intermediate file, and controls the major logic of the
code generation.
.I Allo.c
keeps track of busy and free registers.
.I Match.c
controls the matching
of code templates to subtrees of the expression tree to be compiled.
.I Common.c
defines certain service routines,
as in the first pass.
The above files are machine independent.
.I Order.c
controls the machine dependent details of the code generation strategy.
.I Local2.c
has many small machine dependent routines,
and tables of opcodes, register types, etc.
.I Table.c
has the code template tables,
which are also clearly machine dependent.
.SH
Data Structure Considerations
.PP
This section discusses the node numbers, type words, and expression trees, used
throughout both passes of the compiler.
.PP
The file
.I manifest.h
defines those symbols used throughout both passes.
The intent is to use the same symbol name (e.g., MINUS)
for the given operator throughout the lexical analysis, parsing, tree building,
and code generation phases.
.I Manifest.h
obtains some of its definitions from
two other header files,
.I localdefs.h
and
.I pcc.h .
.I Localdefs.h
contains definitions for operator symbols
which are specific to the C compiler.
.I Pcc.h
contains definitions for operators and types
which may be used by other compilers
to communicate with a portable code generator
based on pass 2;
this code generator will be described later.
.PP
A token
like MINUS may be seen in the lexical analyzer before it is known
whether it is a unary or binary operator;
clearly, it is necessary to know this by the time the parse tree
is constructed.
Thus, an operator (really a macro) called
UNARY is provided, so that MINUS
and UNARY MINUS are both distinct node numbers.
Similarly, many binary operators exist in an assignment form (for example, \-=),
and the operator ASG may be applied to such node names to generate new ones, e.g. ASG MINUS.
.PP
It is frequently desirable to know if a node represents a leaf (no descendants), a unary operator (one
descendant) or a binary operator (two descendants).
The macro
.I optype(o)
returns one of the manifest constants LTYPE, UTYPE, or BITYPE, respectively, depending
on the node number
.I o .
Similarly,
.I asgop(o)
returns true if
.I o
is an assignment operator number (=, +=, etc. ), and
.I logop(o)
returns true if
.I o
is a relational or logical (&&, \(or\(or, or !) operator.
.PP
C has a rich typing structure, with a potentially infinite number of types.
To begin with, there are the basic types: CHAR, SHORT, INT, LONG, the unsigned versions
known as
UCHAR, USHORT, UNSIGNED, ULONG, and FLOAT, DOUBLE,
and finally
STRTY (a structure), UNIONTY, and ENUMTY.
Then, there are three operators that can be applied to types to make others:
if
.I t
is a type, we may potentially have types
.I "pointer to t" ,
.I "function returning t" ,
and
.I "array of t's"
generated from
.I t .
Thus, an arbitrary type in C consists of a basic type, and zero or more of these operators.
.PP
In the compiler, a type is represented by an unsigned integer; the rightmost four bits hold the basic
type, and the remaining bits are divided into two-bit fields, containing
0 (no operator), or one of the three operators
described above.
The modifiers are read right to left in the word, starting with the two-bit
field adjacent to the basic type, until a field with 0 in it is reached.
The macros PTR, FTN, and ARY represent the
.I "pointer to" ,
.I "function returning" ,
and
.I "array of"
operators.
The macro values are shifted so that they align with the first two-bit
field; thus PTR+INT
represents the type for an integer pointer, and
.DS
ARY + (PTR<<2) + (FTN<<4) + DOUBLE
.DE
represents the type of an array of pointers to functions returning \fBdouble\fPs.
.PP
The type words are ordinarily manipulated by macros.
If
.I t
is a type word,
.I BTYPE(t)
gives the basic type.
.I ISPTR(t) ,
.I ISARY(t) ,
and
.I ISFTN(t)
ask if an object of this type is a pointer, array, or a function,
respectively.
.I MODTYPE(t,b)
sets the basic type of
.I t
to
.I b .
.I DECREF(t)
gives the type resulting from removing the first operator from
.I t .
Thus, if
.I t
is a pointer to
.I t\(fm ,
a function returning
.I t\(fm ,
or an array of
.I t\(fm ,
then
.I DECREF(t)
would equal
.I t\(fm .
.I INCREF(t)
gives the type representing a pointer to
.I t .
Finally, there are operators for dealing with the unsigned types.
.I ISUNSIGNED(t)
returns true if
.I t
is one of the four basic unsigned types;
in this case,
.I DEUNSIGN(t)
gives the associated `signed' type.
Similarly,
.I UNSIGNABLE(t)
returns true if
.I t
is one of the four basic types that could become unsigned, and
.I ENUNSIGN(t)
returns the unsigned analogue of
.I t
in this case.
.PP
The other important global data structure is that of expression trees.
The actual shapes of the nodes are given in
.I ndu.h .
The information stored for each pass is not quite the same;
in the first pass, nodes contain
dimension and size information, while in the second pass
nodes contain register allocation information.
Nevertheless, all nodes contain fields called
.I op ,
containing the node number,
and
.I type ,
containing the type word.
A function called
.I talloc()
returns a pointer to a new tree node.
To free a node, its
.I op
field need merely be set to FREE.
The other fields in the node will remain intact at least until the next allocation.
.PP
Nodes representing binary operators contain fields,
.I left
and
.I right ,
that contain pointers to the left and right descendants.
Unary operator nodes have the
.I left
field, and a value field called
.I rval .
Leaf nodes, with no descendants, have two value fields:
.I lval
and
.I rval .
.PP
At appropriate times, the function
.I tcheck()
can be called, to check that there are no busy nodes remaining.
This is used as a compiler consistency check.
The function
.I tcopy(p)
takes a pointer
.I p
that points to an expression tree, and returns a pointer
to a disjoint copy of the tree.
The function
.I walkf(p,f)
performs a postorder walk of the tree pointed to by
.I p ,
and applies the function
.I f
to each node.
The function
.I fwalk(p,f,d)
does a preorder walk of the tree pointed to by
.I p .
At each node, it calls a function
.I f ,
passing to it the node pointer, a value passed down
from its ancestor, and two pointers to values to be passed
down to the left and right descendants (if any).
The value
.I d
is the value passed down to the root.
.I Fwalk
is used for a number of tree labeling and debugging activities.
.PP
The other major data structure, the symbol table, exists only in pass one,
and will be discussed later.
.SH
Pass One
.PP
The first pass does lexical analysis, parsing, symbol table maintenance,
tree building, optimization, and a number of machine dependent things.
This pass is largely machine independent, and the machine independent
sections can be pretty successfully ignored.
Thus, they will be only sketched here.
.SH
Lexical Analysis
.PP
The lexical analyzer is a conceptually simple routine that reads the input
and returns the tokens of the C language as it encounters them:
names, constants, operators, and keywords.
The conceptual simplicity of this job is confounded a bit by several
other simple jobs that unfortunately must go on simultaneously.
These include
.IP \(bu
Keeping track of the current filename and line number, and occasionally
setting this information as the result of preprocessor control lines.
.IP \(bu
Skipping comments.
.IP \(bu
Properly dealing with octal, decimal, hex, floating
point, and character constants, as well as character strings.
.PP
To achieve speed, the program maintains several tables
that are indexed into by character value, to tell
the lexical analyzer what to do next.
To achieve portability, these tables must be initialized
each time the compiler is run, in order that the
table entries reflect the local character set values.
.SH
Parsing
.PP
As mentioned above, the parser is generated by Yacc from the grammar
.I cgram.y.
The grammar is relatively readable, but contains some unusual features
that are worth comment.
.PP
Perhaps the strangest feature of the grammar is the treatment of
declarations.
The problem is to keep track of the basic type
and the storage class while interpreting the various
stars, brackets, and parentheses that may surround a given name.
The entire declaration mechanism must be recursive,
since declarations may appear within declarations of
structures and unions, or even within a
.B sizeof
construction
inside a dimension in another declaration!
.PP
There are some difficulties in using a bottom-up parser,
such as produced by Yacc, to handle constructions where a lot
of left context information must be kept around.
The problem is that the original PDP-11 compiler is top-down in
implementation, and some of the semantics of C reflect this.
In a top-down parser, the input rules are restricted
somewhat, but one can naturally associate temporary
storage with a rule at a very early stage in the recognition
of that rule.
In a bottom-up parser, there is more freedom in the
specification of rules, but it is more difficult to know what
rule is being matched until the entire rule is seen.
The parser described by
.I cgram.y
makes effective use of
the bottom-up parsing mechanism in some places (notably the treatment
of expressions), but struggles against the
restrictions in others.
The usual result is that it is necessary to run a stack of values
``on the side'', independent of the Yacc value stack,
in order to be able to store and access information
deep within inner constructions, where the relationship of the
rules being recognized to the total picture is not yet clear.
.PP
In the case of declarations, the attribute information
(type, etc.) for a declaration is carefully
kept immediately to the left of the declarator
(that part of the declaration involving the name).
In this way, when it is time to declare the name, the
name and the type information can be quickly brought together.
The ``$0'' mechanism of Yacc is used to accomplish this.
The result is not pretty, but it works.
The storage class information changes more slowly,
so it is kept in an external variable, and stacked if
necessary.
Some of the grammar could be considerably cleaned up by using
some more recent features of Yacc, notably actions within
rules and the ability to return multiple values for actions.
.PP
A stack is also used to keep track of the current location
to be branched to when a
.B break
or
.B continue
statement is processed.
.PP
This use of external stacks dates from the time when Yacc did not permit
values to be structures.
Some, or most, of this use of external stacks
could be eliminated by redoing the grammar to use the mechanisms now provided.
There are some areas, however, particularly the processing of structure, union,
and enumeration declarations, function
prologs, and switch statement processing, when having
all the affected data together in an array speeds later
processing; in this case, use of external storage seems essential.
.PP
The
.I cgram.y
file also contains some small functions used as
utility functions in the parser.
These include routines for saving case values and labels in processing switches,
and stacking and popping 
values on the external stack described above.
.SH
Storage Classes
.PP
C has a finite, but fairly extensive, number of storage classes
available.
One of the compiler design decisions was to
process the storage class information totally in the first pass;
by the second pass, this information must have
been totally dealt with.
This means that all of the storage allocation must take place in the first
pass, so that references to automatics and
parameters can be turned into references to cells lying a certain
number of bytes offset from certain machine registers.
Much of this transformation is machine dependent, and strongly
depends on the storage class.
.PP
The classes include EXTERN (for externally declared, but not defined variables),
EXTDEF (for external definitions), and similar distinctions for
USTATIC and STATIC, UFORTRAN and FORTRAN (for fortran functions) and
ULABEL and LABEL.
The storage classes REGISTER and AUTO are obvious, as
are STNAME, UNAME, and ENAME (for structure, union, and enumeration
tags), and the associated MOS, MOU, and MOE (for the members).
TYPEDEF is treated as a storage class as well.
There are two special storage classes: PARAM and SNULL.
SNULL is used to distinguish the case where no explicit
storage class has been given; before an entry is made
in the symbol table the true storage class is discovered.
Similarly, PARAM is used for the temporary entry in the symbol
table made before the declaration of function parameters is completed.
.PP
The most complexity in the storage class process comes from
bit fields.
A separate storage class is kept for each width bit field;
a
.I k
bit bit field has storage class
.I k
plus FIELD.
This enables the size to be quickly recovered from the storage class.
.SH
Symbol Table Maintenance
.PP
The symbol table routines do far more than simply enter
names into the symbol table;
considerable semantic processing and checking is done as well.
For example, if a new declaration comes in, it must be checked
to see if there is a previous declaration of the same symbol.
If there is, there are many cases.
The declarations may agree and be compatible (for example,
an \fBextern\fP declaration can appear twice) in which case the
new declaration is ignored.
The new declaration may add information (such as an explicit array
dimension) to an already present declaration.
The new declaration may be different, but still correct (for example,
an \fBextern\fP declaration of something may be entered,
and then later the definition may be seen).
The new declaration may be incompatible, but appear in an inner block;
in this case, the old declaration is carefully hidden away, and
the new one comes into force until the block is left.
Finally, the declarations may be incompatible, and an error
message must be produced.
.PP
A number of other factors make for additional complexity.
The type declared by the user is not always the type
entered into the symbol table (for example, if a formal parameter to a function
is declared to be an array, C requires that this be changed into
a pointer before entry in the symbol table).
Moreover, there are various kinds of illegal types that
may be declared which are difficult to
check for syntactically (for example,
a function returning an array).
Finally, there is a strange feature in C that requires
structure tag names and member names for structures and unions
to be taken from a different logical symbol table than ordinary identifiers.
Keeping track of which kind of name is involved is a bit of struggle
(consider typedef names used within structure declarations, for example).
.PP
The symbol table handling routines have been rewritten a number of times to
extend features, improve performance, and fix bugs.
They address the above problems with reasonable effectiveness
but a singular lack of grace.
.PP
When a name is read in the input, it is hashed, and the
routine
.I lookup
is called, together with a flag which tells which symbol table
should be searched (actually, both symbol tables are stored in one, and a flag
is used to distinguish individual entries).
If the name is found,
.I lookup
returns the index to the entry found; otherwise, it makes
a new entry, marks it UNDEF (undefined), and returns the
index of the new entry.
This index is stored in the
.I rval
field of a NAME node.
.PP
When a declaration is being parsed, this NAME node is
made part of a tree with UNARY MUL nodes for each \fB*\fP,
LB nodes for each array descriptor (the right descendant
has the dimension), and UNARY CALL nodes for each function
descriptor.
This tree is passed to the routine
.I tymerge ,
along with the attribute type of the whole declaration;
this routine collapses the tree to a single node, by calling
.I tyreduce ,
and then modifies the type to reflect the overall
type of the declaration.
.PP
Dimension and size information is stored in a table called
.I dimtab .
To properly describe a type in C, one needs not just the
type information but also size information (for structures and
enumerations) and dimension information (for arrays).
Sizes and offsets are dealt with in the compiler by
giving the associated indices into
.I dimtab .
.I Tymerge
and
.I tyreduce
call
.I dstash
to put the discovered dimensions away into the
.I dimtab
array.
.I Tymerge
returns a pointer to a single node that contains
the symbol table index in its
.I rval
field, and the size and dimension indices in
fields
.I csiz
and
.I cdim ,
respectively.
This information is properly considered part of the type in the first pass,
and is carried around at all times.
.PP
To enter an element into the symbol table, the routine
.I defid
is called; it is handed a storage class, and a pointer
to the node produced by
.I tymerge .
.I Defid
calls
.I fixtype ,
which adjusts and checks the given type depending on the storage
class, and converts null types appropriately.
It then calls
.I fixclass ,
which does a similar job for the storage class;
it is here, for example, that
\fBregister\fP declarations are either allowed or changed
to \fBauto\fP.
.PP
The new declaration is now compared against an older one,
if present, and several pages of validity checks performed.
If the definitions are compatible, with possibly some added information,
the processing is straightforward.
If the definitions differ, the block levels of the
current and the old declaration are compared.
The current block level is kept in
.I blevel ,
an external variable; the old declaration level is kept in the symbol table.
Block level 0 is for external declarations, 1 is for arguments to functions,
and 2 and above are blocks within a function.
If the current block level is the same as the old declaration, an error
results.
If the current block level is higher, the new declaration overrides the old.
This is done by marking the old symbol table entry ``hidden'', and making
a new entry, marked ``hiding''.
.I Lookup
will skip over hidden entries.
When a block is left, the symbol table is searched,
and any entries defined in that block are destroyed; if
they hid other entries, the old entries are ``unhidden''.
.PP
This nice block structure is warped a bit because labels
do not follow the block structure rules (one can do a
.B goto
into
a block, for example);
default definitions of functions in inner blocks also persist clear out to the outermost scope.
This implies that cleaning up the symbol table after block exit is more
subtle than it might first seem.
.PP
For successful new definitions,
.I defid
also initializes a ``general purpose'' field,
.I offset ,
in the symbol table.
It contains the stack offset for automatics and parameters, the register number
for register variables, the bit offset into the structure for
structure members, and
the internal label number for static variables and labels.
The offset field is set by
.I falloc
for bit fields, and
.I dclstruct
for structures and unions.
.PP
The symbol table entry itself thus contains
the name, type word, size and dimension offsets,
offset value, and declaration block level.
It also has a field of flags, describing what symbol table the
name is in, and whether the entry is hidden, or hides another.
Finally, a field gives the line number of the last use,
or of the definition, of the name.
This is used mainly for diagnostics, but is useful to
.I lint
as well.
.PP
In some special cases, there is more than the above amount of information kept
for the use of the compiler.
This is especially true with structures; for use in initialization,
structure declarations must have access to a list of the members of the
structure.
This list is also kept in
.I dimtab .
Because a structure can be mentioned long before the
members are known, it is necessary to have another
level of indirection in the table.
The two words following the
.I csiz
entry in
.I dimtab
are used to hold the alignment of the structure, and
the index in dimtab of the list of members.
This list contains the symbol table indices for the structure members, terminated by a
\-1.
.SH
Tree Building
.PP
The portable compiler transforms expressions
into expression trees.
As the parser recognizes each rule making up an
expression,
it calls
.I buildtree
which is given an operator number, and pointers to the
left and right descendants.
.I Buildtree
first examines the left and right descendants,
and, if they are both constants, and the operator
is appropriate, simply does the constant
computation at compile time, and returns
the result as a constant.
Otherwise,
.I buildtree
allocates a node for the head of the tree,
attaches the descendants to it, and ensures that
conversion operators are generated if needed, and that
the type of the new node is consistent with the types
of the operands.
There is also a considerable amount of semantic complexity here;
many combinations of types are illegal, and the portable
compiler makes a strong effort to check
the legality of expression types completely.
This is done both for
.I lint
purposes, and to prevent such semantic errors
from being passed through to the code generator.
.PP
The heart of
.I buildtree
is a large table, accessed by the routine
.I opact .
This routine maps the types of the left and right
operands into a rather smaller set of descriptors, and then
accesses a table (actually encoded in a switch statement) which
for each operator and pair of types causes
an action to be returned.
The actions are logical or's of a number of
separate actions, which may be
carried out by
.I buildtree .
These component actions may include
checking the left side to ensure that it
is an lvalue (can be stored into),
applying a type conversion to the left or right operand,
setting the type of the new node to
the type of the left or right operand, calling various
routines to balance the types of the left and right operands,
and suppressing the ordinary conversion
of arrays and function operands to pointers.
An important operation is OTHER, which causes
some special code to be invoked
in
.I buildtree ,
to handle issues which are unique to a particular operator.
Examples of this are
structure and union reference
(actually handled by
the routine
.I stref ),
the building of NAME, ICON, STRING and FCON (floating point constant) nodes,
unary \fB*\fP and \fB&\fP, structure assignment, and calls.
In the case of unary \fB*\fP and \fB&\fP,
.I buildtree
will cancel a \fB*\fP applied to a tree, the top node of which
is \fB&\fP, and conversely.
.PP
Another special operation is
PUN; this
causes the compiler to check for type mismatches,
such as intermixing pointers and integers.
.PP
The treatment of conversion operators is a rather
strange area of the compiler (and of C!).
The introduction of type casts only confounded this
situation.
Most of the conversion operators are generated by
calls to
.I tymatch
and
.I ptmatch ,
both of which are given a tree, and asked to make the
operands agree in type.
.I Ptmatch
treats the case where one of the operands is a pointer;
.I tymatch
treats all other cases.
Where these routines have decided on the
proper type for an operand, they call
.I makety ,
which is handed a tree, and a type word, dimension offset, and size offset.
If necessary, it inserts a conversion operation to make the
types correct.
Conversion operations are never inserted on the left side of
assignment operators, however.
There are two conversion operators used;
PCONV, if the conversion is to a non-basic type (usually a pointer),
and
SCONV, if the conversion is to a basic type (scalar).
.PP
To allow for maximum flexibility, every node produced by
.I buildtree
is given to a machine dependent routine,
.I clocal ,
immediately after it is produced.
This is to allow more or less immediate rewriting of those
nodes which must be adapted for the local machine.
The conversion operations are given to
.I clocal
as well; on most machines, many of these
conversions do nothing, and should be thrown away
(being careful to retain the type).
If this operation is done too early,
however,
later calls to
.I buildtree
may get confused about correct type of the
subtrees;
thus
.I clocal
is given the conversion operations only after the entire tree is built.
This topic will be dealt with in more detail later.
.SH
Initialization
.PP
Initialization is one of the messier areas in the portable compiler.
The only consolation is that most of the mess takes place
in the machine independent part, where it
is may be safely ignored by the implementor of the
compiler for a particular machine.
.PP
The basic problem is that the semantics of initialization
really calls for a co-routine structure; one collection
of programs reading constants from the input stream, while another,
independent set of programs places these constants into the
appropriate spots in memory.
The dramatic differences in the local assemblers also
come to the fore here.
The parsing problems are dealt with by keeping a rather
extensive stack containing the current
state of the initialization; the assembler
problems are dealt with by having a fair number of machine dependent routines.
.PP
The stack contains the symbol table number,
type, dimension index, and size index for the current identifier
being initialized.
Another entry has the offset, in bits, of the beginning
of the current identifier.
Another entry keeps track of how many elements have been seen,
if the current identifier is an array.
Still another entry keeps track of the current
member of a structure being initialized.
Finally, there is an entry containing flags
which keep track of the current state of the initialization process
(e.g., tell if a `}' has been seen for the current identifier).
.PP
When an initialization begins, the routine
.I beginit
is called; it handles the alignment restrictions, if
any, and calls
.I instk
to create the stack entry.
This is done by first making an entry on the top of the stack for the item
being initialized.
If the top entry is an array, another entry is made on the stack
for the first element.
If the top entry is a structure, another entry is made on the
stack for the first member of the structure.
This continues until the top element of the stack is a scalar.
.I Instk
then returns, and the parser begins collecting initializers.
.PP
When a constant is obtained, the routine
.I doinit
is called; it examines the stack, and does whatever
is necessary to assign the current constant to the
scalar on the top of the stack.
.I gotscal
is then called, which rearranges the stack so that the
next scalar to be initialized gets placed on top of the stack.
This process continues until the end of the initializers;
.I endinit
cleans up.
If a `{' or `}' is encountered in the
string of initializers, it is handled by calling
.I ilbrace
or
.I irbrace ,
respectively.
.PP
A central issue is the treatment of the ``holes'' that
arise as a result of alignment restrictions or explicit
requests for holes in bit fields.
There is a global variable,
.I inoff ,
which contains the current offset in the initialization
(all offsets in the first pass of the compiler are in bits).
.I Doinit
figures out from the top entry on the stack the expected
bit offset of the next identifier; it calls the
machine dependent
routine
.I inforce
which, in a machine dependent way, forces
the assembler to
set aside space if need be so that the
next scalar seen will go into the appropriate
bit offset position.
The scalar itself is passed to one of the machine dependent
routines
.I fincode 
(for floating point initialization),
.I incode
(for fields, and other initializations less than an int in
size),
and
.I cinit
(for all other initializations).
The size is passed to all these routines, and it is up to
the machine dependent routines to ensure that the
initializer occupies exactly the right size.
.PP
Character strings represent a bit of an exception.
If a character string is seen as the initializer for
a pointer, the characters making up the string must
be put out under a different location counter.
When the lexical analyzer sees the quote at the head
of a character string, it returns the token STRING,
but does not do anything with the contents.
The parser calls
.I getstr ,
which sets up the appropriate location counters
and flags, and calls
.I lxstr
to read and process the contents of the string.
.PP
If the string is being used to initialize a character array,
.I lxstr
calls
.I putbyte ,
which in effect simulates
.I doinit
for each character read.
If the string is used to initialize a character pointer,
.I lxstr
calls a machine dependent routine,
.I bycode ,
which stashes away each character.
The pointer to this string is then returned,
and processed normally by
.I doinit .
.PP
The null at the end of the string is treated as if it
were read explicitly by
.I lxstr .
.SH
Statements
.PP
The first pass addresses four main areas; declarations, expressions, initialization, and statements.
The statement processing is relatively simple; most of it is carried out in the
parser directly.
Most of the logic is concerned with allocating
label numbers, defining the labels, and branching appropriately.
An external symbol,
.I reached ,
is 1 if a statement can be reached, 0 otherwise; this is
used to do a bit of simple flow analysis as the program is being parsed,
and also to avoid generating the subroutine return sequence if the subroutine
cannot ``fall through'' the last statement.
.PP
Conditional branches are handled by generating an expression
node, CBRANCH,
whose left descendant is the conditional expression and the
right descendant is an ICON node containing the internal label
number to be branched to.
For efficiency, the semantics are that
the label is gone to if the condition is
.I false .
.PP
The switch statement is 
compiled by collecting the case entries, and an indication as to whether
there is a default case;
an internal label number is generated for each of these,
and remembered in a big array.
The expression comprising the value to be switched on is
compiled when the switch keyword is encountered,
but the expression tree is headed by
a special node, FORCE, which tells the code generator to
put the expression value into a special distinguished
register (this same mechanism is used for processing the
return statement).
When the end of the switch block is reached, the array
containing the case values is sorted, and checked for
duplicate entries (an error); if all is
correct, the machine dependent routine
.I genswitch
is called, with this array of labels and values in increasing order.
.I Genswitch
can assume that the value to be tested is already in the
register which is the usual integer return value register.
.SH
Optimization
.PP
There is a machine independent file,
.I optim.c ,
which contains a relatively short optimization routine,
.I optim .
Actually the word optimization is something of a misnomer;
the results are not optimum, only improved, and the
routine is in fact not optional; it must
be called for proper operation of the compiler.
.PP
.I Optim
is called after an expression tree is built, but
before the code generator is called.
The essential part of its job is to call
.I clocal
on the conversion operators.
On most machines, the treatment of
\fB&\fP is also essential:
by this time in the processing, the only node which
is a legal descendant of \fB&\fP is NAME.
(Possible descendants of \fB*\fP have been eliminated by
.I buildtree .)
The address of a static name is, almost by definition, a
constant, and can be represented by an ICON node on most machines
(provided that the loader has enough power).
Unfortunately, this is not universally true; on some machine, such as the IBM 370,
the issue of addressability rears its ugly head;
thus, before turning a NAME node into an ICON node,
the machine dependent function
.I andable
is called.
.PP
The optimization attempts of
.I optim
are quite limited.
It is primarily concerned with improving the behavior of
the compiler with operations one of whose arguments is a constant.
In the simplest case, the constant is placed on the right if the
operation is commutative.
The compiler also makes a limited search for expressions
such as
.DS
( \fIx\fP + \fIa\fP ) + \fIb\fP
.DE
where
.I a
and
.I b
are constants, and attempts to combine
.I a
and
.I b
at compile time.
A number of special cases are also examined;
additions of 0 and multiplications by 1 are removed,
although the correct processing of these cases to get
the type of the resulting tree correct is
decidedly nontrivial.
In some cases, the addition or multiplication must be replaced by
a conversion operator to keep the types from becoming
fouled up.
In cases where a relational operation is being done
and one operand is a constant, the operands are permuted and the operator altered, if necessary,
to put the constant on the right.
Finally, multiplications by a power of 2 are changed to shifts.
.SH
Machine Dependent Stuff
.PP
A number of the first pass machine dependent routines have been discussed above.
In general, the routines are short, and easy to adapt from
machine to machine.
The two exceptions to this general rule are
.I clocal
and
the function prolog and epilog generation routines,
.I bfcode
and
.I efcode .
.PP
.I Clocal
has the job of rewriting, if appropriate and desirable,
the nodes constructed by
.I buildtree .
There are two major areas where this
is important:
NAME nodes and conversion operations.
In the case of NAME nodes,
.I clocal
must rewrite the NAME node to reflect the
actual physical location of the name in the machine.
In effect, the NAME node must be examined, the symbol table
entry found (through the
.I rval
field of the node),
and, based on the storage class of the node,
the tree must be rewritten.
Automatic variables and parameters are typically
rewritten by treating the reference to the variable as
a structure reference, off the register which
holds the stack or argument pointer;
the
.I stref
routine is set up to be called in this way, and to
build the appropriate tree.
In the most general case, the tree consists
of a unary \fB*\fP node, whose descendant is
a \fB+\fP node, with the stack or argument register as left operand,
and a constant offset as right operand.
In the case of LABEL and internal static nodes, the
.I rval
field is rewritten to be the negative of the internal
label number; a negative
.I rval 
field is taken to be an internal label number.
Finally, a name of class REGISTER must be converted into a REG node,
and the
.I rval
field replaced by the register number.
In fact, this part of the
.I clocal
routine is nearly machine independent; only for machines
with addressability problems (IBM 370 again!) does it
have to be noticeably different.
.PP
The conversion operator treatment is rather tricky.
It is necessary to handle the application of conversion operators
to constants in
.I clocal ,
in order that all constant expressions can have their values known
at compile time.
In extreme cases, this may mean that some simulation of the
arithmetic of the target machine might have to be done in a
cross-compiler.
In the most common case,
conversions from pointer to pointer do nothing.
For some machines, however, conversion from byte pointer to short or long
pointer might require a shift or rotate operation, which would
have to be generated here.
.PP
The extension of the portable compiler to machines where the size of a pointer
depends on its type would be straightforward, but has not yet been done.
.PP
Another machine dependent issue in the first pass
is the generation of external ``symbol table'' information.
This sort of symbol table is used by programs such as symbolic debuggers
to relate object code back to source code.
Symbol table routines are provided in
the file \fIstab.c\fP,
which is included in the machine dependent sources
for the first pass.
The symbol table routines insert assembly code
containing assembly pseudo-ops directly into
the instruction stream generated by the compiler.
.PP
There are two basic kinds of symbol table operations.
The simplest operation is the generation of a source line number;
this serves to map an address in an executable image
into a line in a source file so that a debugger
can find the source code
corresponding to the instructions being executed.
The routine \fIpsline\fP is called by the scanner
to emit source line numbers when
a nonempty source line is seen.
The other variety of symbol table operation
is the generation of type and address information
about C symbols.
This is done through the \fIoutstab\fP routine,
which is normally called using the FIXDEF macro
in the monster \fIdefid\fP routine in \fIpftn.c\fP
that enters symbols into the compiler's internal symbol table.
.PP
Yet another major machine dependent issue involves function prolog and epilog
generation.
The hard part here is the design of the stack frame
and calling sequence; this design issue is discussed elsewhere.
.[
Johnson Lesk Ritchie calling sequence
.]
The routine
.I bfcode
is called with the number of arguments
the function is defined with, and
an array containing the symbol table indices of the
declared parameters.
.I Bfcode
must generate the code to establish the new stack frame,
save the return address and previous stack pointer
value on the stack, and save whatever
registers are to be used for register variables.
The stack size and the number of register variables is not
known when
.I bfcode
is called, so these numbers must be
referred to by assembler constants, which are
defined when they are known (usually in the second pass,
after all register variables, automatics, and temporaries have been seen).
The final job is to find those parameters which may have been declared
register, and generate the code to initialize
the register with the value passed on the stack.
Once again, for most machines, the general logic of
.I bfcode
remains the same, but the contents of the
.I printf
calls in it will change from machine to machine.
.I efcode
is rather simpler, having just to generate the default
return at the end of a function.
This may be nontrivial in the case of a function returning a structure or union, however.
.PP
There seems to be no really good place to discuss structures and unions, but
this is as good a place as any.
The C language now supports structure assignment,
and the passing of structures as arguments to functions,
and the receiving of structures back from functions.
This was added rather late to C, and thus to the portable compiler.
Consequently, it fits in less well than the older features.
Moreover, most of the burden of making these features work is
placed on the machine dependent code.
.PP
There are both conceptual and practical problems.
Conceptually, the compiler is structured around
the idea that to compute something, you put it into
a register and work on it.
This notion causes a bit of trouble on some machines (e.g., machines with 3-address opcodes), but
matches many machines quite well.
Unfortunately, this notion breaks down with structures.
The closest that one can come is to keep the addresses of the
structures in registers.
The actual code sequences used to move structures vary from the
trivial (a multiple byte move) to the horrible (a
function call), and are very machine dependent.
.PP
The practical problem is more painful.
When a function returning a structure is called, this function
has to have some place to put the structure value.
If it places it on the stack, it has difficulty popping its stack frame.
If it places the value in a static temporary, the routine fails to be
reentrant.
The most logically consistent way of implementing this is for the
caller to pass in a pointer to a spot where the called function
should put the value before returning.
This is relatively straightforward, although a bit tedious, to implement,
but means that the caller must have properly declared
the function type, even if the value is never used.
On some machines, such as the Interdata 8/32, the return value
simply overlays the argument region (which on the 8/32 is part
of the caller's stack frame).
The caller takes care of leaving enough room if the returned value is larger
than the arguments.
This also assumes that the caller declares the
function properly.
.PP
The PDP-11 and the \*V have stack hardware which is used in function calls and returns;
this makes it very inconvenient to
use either of the above mechanisms.
In these machines, a static area within the called function
is allocated, and
the function return value is copied into it on return; the function
returns the address of that region.
This is simple to implement, but is non-reentrant.
However, the function can now be called as a subroutine
without being properly declared, without the disaster which would otherwise ensue.
No matter what choice is taken, the convention is that the function
actually returns the address of the return structure value.
.PP
In building expression trees, the portable compiler takes a bit for granted about
structures.
It assumes that functions returning structures
actually return a pointer to the structure, and it assumes that
a reference to a structure is actually a reference to its address.
The structure assignment operator is rebuilt so that the left
operand is the structure being assigned to, but the
right operand is the address of the structure being assigned;
this makes it easier to deal with
.DS
.I "a = b = c"
.DE
and similar constructions.
.PP
There are four special tree nodes associated with these
operations:
STASG (structure assignment), STARG (structure argument
to a function call), and STCALL and UNARY STCALL
(calls of a function with nonzero and zero arguments, respectively).
These four nodes are unique in that the size and alignment information, which can be determined by
the type for all other objects in C, 
must be known to carry out these operations; special
fields are set aside in these nodes to contain
this information, and special
intermediate code is used to transmit this information.
.SH
First Pass Summary
.PP
There are may other issues which have been ignored here,
partly to justify the title ``tour'', and partially
because they have seemed to cause little trouble.
There are some debugging flags
which may be turned on, by giving the compiler's first pass
the argument
.DS
.B \-X [flags]
.DE
Some of the more interesting flags are
\fB\-Xd\fP for the defining and freeing of symbols,
\fB\-Xi\fP for initialization comments, and
\fB\-Xb\fP for various comments about the building of trees.
In many cases, repeating the flag more than once gives more information;
thus,
\fB\-Xddd\fP gives more information than \fB\-Xd\fP.
In the two pass version of the compiler, the
flags should not be set when the output is sent to the second
pass, since the debugging output and the intermediate code both go onto the standard
output.
.PP
We turn now to consideration of the second pass.
