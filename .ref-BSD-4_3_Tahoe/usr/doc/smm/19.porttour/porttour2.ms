.\"	@(#)porttour2.ms	6.3 (Berkeley) 5/30/86
.\"
.SH
Pass Two
.PP
Code generation is far less well understood than
parsing or lexical analysis, and for this reason
the second pass is far harder to discuss in a file by file manner.
A great deal of the difficulty is in understanding the issues
and the strategies employed to meet them.
Any particular function is likely to be reasonably straightforward.
.PP
Thus, this part of the paper will concentrate a good deal on the broader
aspects of strategy in the code generator,
and will not get too intimate with the details.
.SH
Overview
.PP
It is difficult to organize a code generator to be flexible enough to
generate code for a large number of machines,
and still be efficient for any one of them.
Flexibility is also important when it comes time to tune
the code generator to improve the output code quality.
On the other hand, too much flexibility can lead to semantically
incorrect code, and potentially a combinatorial explosion in the
number of cases to be considered in the compiler.
.PP
One goal of the code generator is to have a high degree of correctness.
It is very desirable to have the compiler detect its own inability to
generate correct code, rather than to produce incorrect code.
This goal is achieved by having a simple model of the job
to be done (e.g., an expression tree)
and a simple model of the machine state
(e.g., which registers are free).
The act of generating an instruction performs a transformation
on the tree and the machine state;
hopefully, the tree eventually gets
reduced to a single node.
If each of these instruction/transformation pairs is correct,
and if the machine state model really represents the actual machine,
and if the transformations reduce the input tree to the desired single node, then the
output code will be correct.
.PP
For most real machines, there is no definitive theory of code generation that
encompasses all the C operators.
Thus the selection of which instruction/transformations to generate, and in what
order, will have a heuristic flavor.
If, for some expression tree, no transformation applies, or, more
seriously, if the heuristics select a sequence of instruction/transformations
that do not in fact reduce the tree, the compiler
will report its inability to generate code, and abort.
.PP
A major part of the code generator is concerned with the model and the transformations.
Most of this is machine independent, or depends only on simple tables.
The flexibility comes from the heuristics that guide the transformations
of the trees, the selection of subgoals, and the ordering of the computation.
.SH
The Machine Model
.PP
The machine is assumed to have a number of registers, of at most two different
types:
.I A
and
.I B .
Within each register class, there may be scratch (temporary) registers and dedicated registers
(e.g., register variables, the stack pointer, etc.).
Requests to allocate and free registers involve only the temporary registers.
.PP
Each of the registers in the machine is given a name and a number
in the
.I mac2defs.h
file; the numbers are used as indices into various tables
that describe the registers, so they should be kept small.
One such table is the
.I rstatus
table on file
.I local2.c .
This table is indexed by register number, and
contains expressions made up from manifest constants describing the register types:
SAREG for dedicated AREG's, SAREG\(orSTAREG for scratch AREG's,
and SBREG and SBREG\(orSTBREG similarly for BREG's.
There are macros that access this information:
.I isbreg(r)
returns true if register number
.I r
is a BREG, and
.I istreg(r)
returns true if register number
.I r
is a temporary AREG or BREG.
Another table,
.I rnames ,
contains the register names; this is used when
putting out assembler code and diagnostics.
.PP
The usage of registers is kept track of by an array called
.I busy .
.I Busy[r]
is the number of uses of register
.I r
in the current tree being processed.
The allocation and freeing of registers will be discussed later as part of the code generation
algorithm.
.SH
General Organization
.PP
As mentioned above, the second pass reads lines from
the intermediate file, copying through to the output unchanged
any lines that begin with a `)', and making note of the
information about stack usage and register allocation contained on
lines beginning with `]' and `['.
The expression trees, whose beginning is indicated by a line beginning with `.',
are read and rebuilt into trees.
If the compiler is loaded as one pass, the expression trees are
immediately available to the code generator.
.PP
The actual code generation is done by a hierarchy of routines.
The routine
.I delay
is first given the tree; it attempts to delay some postfix
\fB+\|+\fP and \fB\-\|\-\fP computations that might reasonably be done after the
smoke clears.
It also attempts to handle comma (`,') operators by computing the
left side expression first, and then rewriting the tree
to eliminate the operator.
.I Delay
calls
.I codgen
to control the actual code generation process.
.I Codgen
takes as arguments a pointer to the expression tree,
and a second argument that, for socio-historical reasons, is called a
.I cookie .
The cookie describes a set of goals that would be acceptable
for the code generation: these are assigned to individual bits,
so they may be logically or'ed together to form a large number of possible goals.
Among the possible goals are FOREFF (compute for side effects only;
don't worry about the value), INTEMP (compute and store value into a temporary location
in memory), INAREG (compute into an A register), INTAREG (compute into a scratch
A register), INBREG and INTBREG similarly, FORCC (compute for condition codes),
and FORARG (compute it as a function argument; e.g., stack it if appropriate).
.PP
.I Codgen
first canonicalizes the tree by calling
.I canon .
This routine looks for certain transformations that might now be
applicable to the tree.
One, which is very common and very powerful, is to
fold together an indirection operator (UNARY MUL)
and a register (REG); in most machines, this combination is
addressable directly, and so is similar to a NAME in its
behavior.
The UNARY MUL and REG are folded together to make
another node type called OREG.
In fact, in many machines it is possible to directly address not just the
cell pointed to by a register, but also cells differing by a constant
offset from the cell pointed to by the register.
.I Canon
also looks for such cases, calling the
machine dependent routine 
.I notoff
to decide if the offset is acceptable (for example, in the IBM 370 the offset
must be between 0 and 4095 bytes).
Another optimization is to replace bit field operations
by shifts and masks if the operation involves extracting the field.
Finally, a machine dependent routine,
.I sucomp ,
is called that computes the Sethi-Ullman numbers
for the tree (see below).
.PP
After the tree is canonicalized,
.I codgen
calls the routine
.I store
whose job is to select a subtree of the tree to be computed
and (usually) stored before beginning the
computation of the full tree.
.I Store
must return a tree that can be computed without need
for any temporary storage locations.
In effect, the only store operations generated while processing the subtree must be as a response
to explicit assignment operators in the tree.
This division of the job marks one of the more significant, and successful,
departures from most other compilers.
It means that the code generator can operate
under the assumption that there are enough
registers to do its job, without worrying about
temporary storage.
If a store into a temporary appears in the output, it is always
as a direct result of logic in the
.I store
routine; this makes debugging easier.
.PP
One consequence of this organization is that
code is not generated by a treewalk.
There are theoretical results that support this decision.
.[
Aho Johnson Optimal Code Trees jacm
.]
It may be desirable to compute several subtrees and store
them before tackling the whole tree;
if a subtree is to be stored, this is known before the code
generation for the subtree is begun, and the subtree is computed
when all scratch registers are available.
.PP
The
.I store
routine decides what subtrees, if any, should be
stored by making use of numbers,
called
.I "Sethi-Ullman numbers" ,
that give, for each subtree of an expression tree,
the minimum number of scratch registers required to
compile the subtree, without any stores into temporaries.
.[
Sethi Ullman jacm 1970
.]
These numbers are computed by the machine-dependent
routine
.I sucomp ,
called by
.I canon .
The basic notion is that, knowing the Sethi-Ullman numbers for
the descendants of a node, and knowing the operator of the
node and some information about the machine, the
Sethi-Ullman number of the node itself can be computed.
If the Sethi-Ullman number for a tree exceeds the number of scratch
registers available, some subtree must be stored.
Unfortunately, the theory behind the Sethi-Ullman numbers
applies only to uselessly simple machines and operators.
For the rich set of C operators, and for machines with
asymmetric registers, register pairs, different kinds of registers,
and exceptional forms of addressing,
the theory cannot be applied directly.
The basic idea of estimation is a good one, however,
and well worth applying; the application, especially
when the compiler comes to be tuned for high code
quality, goes beyond the park of theory into the
swamp of heuristics.
This topic will be taken up again later, when more of the compiler
structure has been described.
.PP
After examining the Sethi-Ullman numbers,
.I store
selects a subtree, if any, to be stored, and returns the subtree and the associated cookie in
the external variables
.I stotree
and
.I stocook .
If a subtree has been selected, or if
the whole tree is ready to be processed, the
routine
.I order
is called, with a tree and cookie.
.I Order
generates code for trees that
do not require temporary locations.
.I Order
may make recursive calls on itself, and,
in some cases, on
.I codgen ;
for example, when processing the operators \fB&&\fP, \fB\(or\(or\fP, and comma (`,'), that have
a left to right evaluation, it is
incorrect for
.I store
examine the right operand for subtrees to be stored.
In these cases,
.I order
will call
.I codgen
recursively when it is permissible to work on the right operand.
A similar issue arises with the \fB? :\fP operator.
.PP
The
.I order
routine works by matching the current tree with
a set of code templates.
If a template is discovered that will
match the current tree and cookie, the associated assembly language
statement or statements are generated.
The tree is then rewritten,
as specified by the template, to represent the effect of the output instruction(s).
If no template match is found, first an attempt is made to find a match with a
different cookie; for example, in order to compute
an expression with cookie INTEMP (store into a temporary storage location),
it is usually necessary to compute the expression into a scratch register
first.
If all attempts to match the tree fail, the heuristic part of
the algorithm becomes dominant.
Control is typically given to one of a number of machine-dependent routines
that may in turn recursively call
.I order
to achieve a subgoal of the computation (for example, one of the
arguments may be computed into a temporary register).
After this subgoal has been achieved, the process begins again with the
modified tree.
If the machine-dependent heuristics are unable to reduce the tree further,
a number of default rewriting rules may be considered appropriate.
For example, if the left operand of a \fB+\fP is a scratch
register, the \fB+\fP can be replaced by a \fB+=\fP operator;
the tree may then match a template.
.PP
To close this introduction, we will discuss the steps in compiling
code for the expression
.DS
\fIa\fR += \fIb\fR
.DE
where
.I a
and
.I b
are static variables.
.PP
To begin with, the whole expression tree is examined with cookie FOREFF, and
no match is found.  Search with other cookies is equally fruitless, so an
attempt at rewriting is made.
Suppose we are dealing with the Interdata 8/32 for the moment.
It is recognized that the left hand and right hand sides of the \fB+=\fP operator
are addressable, and in particular the left hand side has no
side effects, so it is permissible to rewrite this as
.DS
\fIa\fR = \fIa\fR + \fIb\fR
.DE
and this is done.
No match is found on this tree either, so a machine dependent rewrite is done; it is recognized
that the left hand side of the assignment is addressable, but the right hand side is not
in a register, so
.I order
is called recursively, being asked to put the right
hand side of the assignment into a register.
This invocation of
.I order
searches the tree for a match, and fails.
The machine dependent rule for \fB+\fP
notices that the right hand operand is addressable;
it decides to put the left operand into a scratch register.
Another recursive call to
.I order
is made, with the tree
consisting solely of the leaf
.I a ,
and the cookie asking that the value be placed into a scratch register.
This now matches a template, and a load instruction is emitted.
The node consisting of
.I a
is rewritten in place to represent the register into which
.I a
is loaded,
and this third call to
.I order
returns.
The second call to
.I order
now finds that it has the tree
.DS
\fBreg\fR + \fIb\fR
.DE
to consider.
Once again, there is no match, but the default rewriting rule rewrites
the \fB+\fP as a \fB+=\fP operator, since the left operand is a scratch register.
When this is done, there is a match: in fact,
.DS
\fBreg\fR += \fIb\fR
.DE
simply describes the effect of the add instruction
on a typical machine.
After the add is emitted, the tree is rewritten
to consist merely of the register node, since the result of the add
is now in the register.
This agrees with the cookie passed to the second invocation of
.I order ,
so this invocation
terminates, returning to the first level.
The original tree has now
become
.DS
\fIa\fR = \fBreg\fR
.DE
which matches a template for the store instruction.
The store is output, and the tree rewritten to become
just a single register node.
At this point, since the top level call to
.I order
was
interested only in side effects, the call to
.I order
returns, and the code generation is completed;
we have generated a load, add, and store, as might have been expected.
.PP
The effect of machine architecture on this is considerable.
For example, on the Honeywell 6000, the machine dependent heuristics recognize that there is an ``add to storage''
instruction, so the strategy is quite different;
.I b
is loaded in to
a register, and then an add to storage instruction generated
to add this register in to
.I a .
The transformations, involving as they do the semantics of C,
are largely machine independent.
The decisions as to when to use them, however, are
almost totally machine dependent.
.PP
Having given a broad outline of
the code generation process, we shall next consider the
heart of it: the templates.
This leads naturally into discussions of template matching and register allocation,
and finally a discussion of the machine dependent interfaces and strategies.
.SH
The Templates
.PP
The templates describe the effect of the target machine instructions
on the model of computation around which the compiler is organized.
In effect, each template has five logical sections, and represents an assertion
of the form:
.IP
.B If
we have a subtree of a given shape (1), and we have a goal (cookie) or goals to
achieve (2), and we have sufficient free resources (3),
.B then
we may emit an instruction or instructions (4), and
rewrite the subtree in a particular manner (5),
and the rewritten tree will achieve the desired goals.
.PP
These five sections will be discussed in more
detail later.  First, we give an example of a
template:
.DS
.ta 1i 2i 3i 4i 5i
ASG PLUS,	INAREG,
	SAREG,	TINT,
	SNAME,	TINT,
		0,	RLEFT,
		"	add	AL,AR\en",
.DE
The top line specifies the operator (\fB+=\fP) and the cookie (compute the
value of the subtree into an AREG).
The second and third lines specify the left and right descendants,
respectively,
of the \fB+=\fP operator.
The left descendant must be a REG node, representing an
A register, and have integer type, while the right side must be a NAME node,
and also have integer type.
The fourth line contains the resource requirements (no scratch registers
or temporaries needed), and the rewriting rule (replace the subtree by the left descendant).
Finally, the quoted string on the last line represents the output to the assembler:
lower case letters, tabs, spaces, etc. are copied
.I verbatim .
to the output; upper case letters trigger various macro-like expansions.
Thus,
.B AL
would expand into the \fBA\fRddress form of the \fBL\fReft operand \(em
presumably the register number.
Similarly,
.B AR
would expand into the name of the right operand.
The
.I add
instruction of the last section might well be
emitted by this template.
.PP
In principle, it would be possible to make separate templates
for all legal combinations of operators, cookies, types, and shapes.
In practice, the number of combinations is very large.
Thus, a considerable amount of mechanism is present to
permit a large number of subtrees to be matched
by a single template.
Most of the shape and type specifiers are individual bits, and can
be logically
or'ed
together.
There are a number of special descriptors for matching classes of
operators.
The cookies can also be combined.
As an example of the kind of template
that really arises in practice, the
actual template for the Interdata 8/32
that subsumes the above example is:
.DS
.ta 1i 2i 3i 4i 5i
ASG OPSIMP,	INAREG\(orFORCC,
	SAREG,	TINT\(orTUNSIGNED\(orTPOINT,
	SAREG\(orSNAME\(orSOREG\(orSCON,	TINT\(orTUNSIGNED\(orTPOINT,
		0,	RLEFT\(orRESCC,
		"	OI	AL,AR\en",
.DE
Here, OPSIMP represents the operators
+, \-, \(or, &, and ^.
The
.B OI
macro in the output string expands into the
appropriate \fBI\fRnteger \fBO\fRpcode for the operator.
The left and right sides can be integers, unsigned, or pointer types.
The right side can be, in addition to a name, a register,
a memory location whose address is given by a register and displacement (OREG),
or a constant.
Finally, these instructions set the condition codes,
and so can be used in condition contexts:
the cookie and rewriting rules reflect this.
.SH
The Template Matching Algorithm
.PP
The heart of the second pass is the template matching
algorithm, in the routine
.I match .
.I Match
is called with a tree and a cookie; it attempts to match
the given tree against some template that will transform it
according to one of the goals given in the cookie.
If a match is successful, the transformation is
applied;
.I expand
is called to generate the assembly code, and then
.I reclaim
rewrites the tree, and reclaims the resources, such
as registers, that might have become free as a result
of the generated code.
.PP
This part of the compiler is among the most time critical.
There is a spectrum of implementation techniques available
for doing this matching.
The most naive algorithm simply looks at the templates one by one.
This can be considerably improved upon by restricting the search
for an acceptable template.
It would be possible to do better than this if the templates were given
to a separate program that ate them and generated a template
matching subroutine.
This would make maintenance of the compiler much more
complicated, however, so this has not been done.
.PP
The matching algorithm is actually carried out by restricting
the range in the table that must be searched for each opcode.
This introduces a number of complications, however, and needs a
bit of sympathetic help by the person constructing the
compiler in order to obtain best results.
The exact tuning of this algorithm continues; it
is best to consult the code and comments in
.I match
for the latest version.
.PP
In order to match a template to a tree,
it is necessary to match not only the cookie and the
operator of the root, but also the types and shapes of the
left and right descendants (if any) of the tree.
A convention is established here that is carried out throughout
the second pass of the compiler.
If a node represents a unary operator, the single descendant
is always the ``left'' descendant.
If a node represents a unary operator or a leaf node (no descendants)
the ``right'' descendant is taken by convention to be the node itself.
This enables templates to easily match leaves and conversion operators, for example,
without any additional mechanism in the matching program.
.PP
The type matching is straightforward; it is possible to specify any combination
of basic types, general pointers, and pointers to one or more of
the basic types.
The shape matching is somewhat more complicated, but still pretty simple.
Templates have a collection of possible operand shapes
on which the opcode might match.
In the simplest case, an
.I add
operation might be able to add to either a register variable
or a scratch register, and might be able (with appropriate
help from the assembler) to add an integer constant (ICON), a static
memory cell (NAME), or a stack location (OREG).
.PP
It is usually attractive to specify a number of such shapes,
and distinguish between them when the assembler output is produced.
It is possible to describe the union of many elementary
shapes such as ICON, NAME, OREG,
AREG or BREG
(both scratch and register forms), etc.
To handle at least the simple forms of indirection, one can also
match some more complicated forms of trees: STARNM and STARREG
can match more complicated trees headed by an indirection operator,
and SFLD can match certain trees headed by a FLD operator.
These patterns call machine dependent routines that match the
patterns of interest on a given machine.
The shape SWADD may be used to recognize NAME or OREG
nodes that lie on word boundaries: this may be of some importance
on word addressed machines.
Finally, there are some special shapes: these may not
be used in conjunction with the other shapes, but may be
defined and extended in machine dependent ways.
The special shapes SZERO, SONE, and SMONE are predefined and match
constants 0, 1, and \-1, respectively; others are easy to add
and match by using the machine dependent routine
.I special .
.PP
When a template has been found that matches the root of the tree,
the cookie, and the shapes and types of the descendants,
there is still one bar to a total match: the template may
call for some resources (for example, a scratch register).
The routine
.I allo
is called, and it attempts to allocate the resources.
If it cannot, the match fails; no resources are
allocated.
If successful, the allocated resources are given numbers
1, 2, etc. for later reference when the assembly code is
generated.
The routines
.I expand
and
.I reclaim
are then called.
The
.I match
routine then returns a special value, MDONE.
If no match was found, the value MNOPE is returned;
this is a signal to the caller to try more cookie
values, or attempt a rewriting rule.
.I Match
is also used to select rewriting rules, although
the way of doing this is pretty straightforward.
A special cookie, FORREW, is used to ask
.I match
to search for a rewriting rule.
The rewriting rules are keyed to various opcodes; most
are carried out in
.I order .
Since the question of when to rewrite is one of the key issues in
code generation, it will be taken up again later.
.SH
Register Allocation
.PP
The register allocation routines, and the allocation strategy,
play a central role in the correctness of the code generation algorithm.
If there are bugs in the Sethi-Ullman computation that cause the
number of needed registers to be underestimated,
the compiler may run out of scratch registers;
it is essential that the allocator keep track of those registers that
are free and busy, in order to detect such conditions.
.PP
Allocation of registers takes place as the result of a template
match; the routine
.I allo
is called with a word describing the number of A registers,
B registers, and temporary locations needed.
The allocation of temporary locations on the stack is relatively
straightforward, and will not be further covered; the
bookkeeping is a bit tricky, but conceptually trivial, and requests
for temporary space on the stack will never fail.
.PP
Register allocation is less straightforward.
The two major complications are
.I pairing
and
.I sharing .
In many machines, some operations (such as multiplication
and division), and/or some types (such as longs or double precision)
require even/odd pairs of registers.
Operations of the first type are exceptionally difficult to
deal with in the compiler; in fact, their theoretical
properties are rather bad as well.
.[
Aho Johnson Ullman Multiregister
.]
The second issue is dealt with rather more successfully;
a machine dependent function called
.I szty(t)
is called that returns 1 or 2, depending on the
number of A registers required to hold an object of type
.I t .
If
.I szty
returns 2, an even/odd pair of A registers is allocated
for each request.
As part of its duties, the routine
.I usable
finds usable register pairs for various operations.
This task is not as easy as it sounds;
it does not suffice to merely use
.I szty
on the expression tree,
since there are situations in which
a register pair temporary is needed even though
the result of the expression requires only one register.
This can occur with assignment operator expressions
which have \fBint\fP type but a \fBdouble\fP right hand side,
or with relational expressions where
one operand is \fBfloat\fP and the other \fBdouble\fP.
.PP
The other issue, sharing, is more subtle, but
important for good code quality.
When registers are allocated, it
is possible to reuse registers that hold address
information, and use them to contain the values
computed or accessed.
For example, on the IBM 360, if register 2 has
a pointer to an integer in it, we may load the
integer into register 2 itself by saying:
.DS
L	2,0(2)
.DE
If register 2 had a byte pointer, however, the sequence for
loading a character involves clearing the target
register first, and then inserting the desired character:
.DS
SR	3,3
IC	3,0(2)
.DE
In the first case, if register 3 were used as the target,
it would lead to a larger number of registers
used for the expression than were required; the compiler would
generate inefficient code.
On the other hand, if register 2 were used as the target in the second
case, the code would simply be wrong.
In the first case, register 2 can be 
.I shared
while in the second, it cannot.
.PP
In the specification of the register needs in the templates,
it is possible to indicate whether required scratch registers
may be shared with possible registers on the left or the right of the input tree.
In order that a register be shared, it must be scratch, and it must
be used only once, on the appropriate side of the tree being compiled.
.PP
The
.I allo
routine thus has a bit more to do than meets the eye;
it calls
.I freereg
to obtain a free register for each A and B register request.
.I Freereg
makes multiple calls on the routine
.I usable
to decide if a given register can be used to satisfy
a given need.
.I Usable
calls
.I shareit
if the register is busy, but might be shared.
Finally,
.I shareit
calls
.I ushare
to decide if the desired register is actually in the appropriate
subtree, and can be shared.
.PP
Just to add additional complexity, on some machines (such as the IBM 370) it
is possible to have ``double indexing'' forms of
addressing; these are represented by OREG's
with the base and index registers encoded into the register field.
While the register allocation and deallocation
.I "per se"
is not made more difficult by this phenomenon, the code itself
is somewhat more complex.
.PP
Having allocated the registers and expanded the assembly language,
it is time to reclaim the resources; the routine
.I reclaim
does this.
Many operations produce more than one result.
For example, many arithmetic operations may produce
a value in a register, and also set the condition
codes.
Assignment operations may leave results both in a register and in memory.
.I Reclaim
is passed three parameters; the tree and cookie
that were matched, and the rewriting field of the template.
The rewriting field allows the specification of possible results;
the tree is rewritten to reflect the results of the operation.
If the tree was computed for side effects only (FOREFF),
the tree is freed, and all resources in it reclaimed.
If the tree was computed for condition codes, the resources
are also freed, and the tree replaced by a special
node type, FORCC.
Otherwise, the value may be found in the left
argument of the root, the right argument of the root,
or one of the temporary resources allocated.
In these cases, first the resources of the tree,
and the newly allocated resources,
are
freed; then the resources needed by the result
are made busy again.
The final result must always match the shape of the input cookie;
otherwise, the compiler error
``cannot reclaim''
is generated.
There are some machine dependent ways of
preferring results in registers or memory when
there are multiple results matching multiple goals in the cookie.
.PP
.I Reclaim
also implements, in a curious way,
C's ``usual arithmetic conversions''.
When a value is generated into a temporary register,
.I reclaim
decides what the type and size of the result will be.
Unless automatic conversion is specifically suppressed
in the code template with the \fBT\fP macro,
\fIreclaim\fP converts \fBchar\fP and \fBshort\fP
results to \fBint\fP,
\fBunsigned char\fP and \fBunsigned short\fP results
to \fBunsigned int\fP,
and \fBfloat\fP into \fBdouble\fP
(for double only floating point arithmetic).
This conversion is a simple type pun;
no instructions for converting the value
are actually emitted.
This implies that registers must always
contain a value that is at least as wide
as a register,
which greatly restricts the range of possible templates.
.SH
The Machine Dependent Interface
.PP
The files
.I order.c ,
.I local2.c ,
and
.I table.c ,
as well as the header file
.I mac2defs ,
represent the machine dependent portion of the second pass.
The machine dependent portion can be roughly divided into
two: the easy portion and the hard portion.
The easy portion
tells the compiler the names of the registers, and arranges that
the compiler generate the proper assembler formats, opcode names, location counters, etc.
The hard portion involves the Sethi\-Ullman computation, the
rewriting rules, and, to some extent, the templates.
It is hard because there are no real algorithms that apply;
most of this portion is based on heuristics.
This section discusses the easy portion; the next several
sections will discuss the hard portion.
.PP
If the compiler is adapted from a compiler for a machine
of similar architecture, the easy part is indeed easy.
In
.I mac2defs ,
the register numbers are defined, as well as various parameters for
the stack frame, and various macros that describe the machine architecture.
If double indexing is to be permitted, for example, the symbol
R2REGS is defined.
Also, a number of macros that are involved in function call processing,
especially for unusual function call mechanisms, are defined here.
.PP
In
.I local2.c ,
a large number of simple functions are defined.
These do things such as write out opcodes, register names,
and address forms for the assembler.
Part of the function call code is defined here; that is nontrivial
to design, but typically rather straightforward to implement.
Among the easy routines in
.I order.c
are routines for generating a created label,
defining a label, and generating the arguments
of a function call.
.PP
These routines tend to have a local effect, and depend on a fairly straightforward way
on the target assembler and the design decisions already made about
the compiler.
Thus they will not be further treated here.
.SH
The Rewriting Rules
.PP
When a tree fails to match any template, it becomes
a candidate for rewriting.
Before the tree is rewritten,
the machine dependent routine
.I nextcook
is called with the tree and the cookie; it suggests
another cookie that might be a better candidate for the
matching of the tree.
If all else fails, the templates are searched with the cookie
FORREW, to look for a rewriting rule.
The rewriting rules are of two kinds;
for most of the common operators, there are
machine dependent rewriting rules that may be applied;
these are handled by machine dependent functions
that are called and given the tree to be computed.
These routines may recursively call
.I order
or
.I codgen
to cause certain subgoals to be achieved;
if they actually call for some alteration of the tree,
they return 1, and the
code generation algorithm recanonicalizes and tries again.
If these routines choose not to deal with the tree, the
default rewriting rules are applied.
.PP
The assignment operators, when rewritten, call the routine
.I setasg .
This is assumed to rewrite the tree at least to the point where there are
no side effects in the left hand side.
If there is still no template match,
a default rewriting is done that causes
an expression such as
.DS
.I "a += b"
.DE
to be rewritten as
.DS
.I "a = a + b"
.DE
This is a useful default for certain mixtures of strange types
(for example, when
.I a
is a bit field and
.I b
an character) that
otherwise might need separate table entries.
.PP
Simple assignment, structure assignment, and all forms of calls
are handled completely by the machine dependent routines.
For historical reasons, the routines generating the calls return
1 on failure, 0 on success, unlike the other routines.
.PP
The machine dependent routine
.I setbin
handles binary operators; it too must do most of the job.
In particular, when it returns 0, it must do so with
the left hand side in a temporary register.
The default rewriting rule in this case is to convert the
binary operator into the associated assignment operator;
since the left hand side is assumed to be a temporary register,
this preserves the semantics and often allows a considerable
saving in the template table.
.PP
The increment and decrement operators may be dealt with with
the machine dependent routine
.I setincr .
If this routine chooses not to deal with the tree, the rewriting rule replaces
.DS
.I "x ++"
.DE
by
.DS
.I "( (x += 1) \- 1)"
.DE
which preserves the semantics.
Once again, this is not too attractive for the most common
cases, but can generate close to optimal code when the
type of x is unusual.
.PP
Finally, the indirection (UNARY MUL) operator is also handled
in a special way.
The machine dependent routine
.I offstar
is extremely important for the efficient generation of code.
.I Offstar
is called with a tree that is the direct descendant of a UNARY MUL node;
its job is to transform this tree so that the combination of
UNARY MUL with the transformed tree becomes addressable.
On most machines,
.I offstar
can simply compute the tree into an A or B register,
depending on the architecture, and then
.I canon
will make the resulting tree into an OREG.
On many machines,
.I offstar
can profitably choose to do less work than computing
its entire argument into a register.
For example, if the target machine supports OREG's
with a constant offset from a register, and
.I offstar
is called
with a tree of the form
.DS
.I "expr + const"
.DE
where
.I const
is a constant, then
.I offstar
need only compute
.I expr
into the appropriate form of register.
On machines that support double indexing,
.I offstar
may have even more choice as to how to proceed.
The proper tuning of
.I offstar ,
which is not typically too difficult, should be one of the
first tries at optimization attempted by the
compiler writer.
.SH
The Sethi-Ullman Computation
.PP
The heart of the heuristics is the computation of the Sethi-Ullman numbers.
This computation is closely linked with the rewriting rules and the
templates.
As mentioned before, the Sethi-Ullman numbers are expected to
estimate the number of scratch registers needed to compute
the subtrees without using any stores.
However, the original theory does not apply to real machines.
For one thing, the theory assumes that all registers
are interchangeable.
Real machines have general purpose, floating point, and index registers,
register pairs, etc.
The theory also does not account for side effects;
this rules out various forms of pathology that arise
from assignment and assignment operators.
Condition codes are also undreamed of.
Finally, the influence of types, conversions, and the
various addressability restrictions and extensions of real
machines are also ignored.
.PP
Nevertheless, for a ``useless'' theory,
the basic insight of Sethi and Ullman is amazingly
useful in a real compiler.
The notion that one should attempt to estimate the
resource needs of trees before starting the
code generation provides a natural means of splitting the
code generation problem, and provides a bit of redundancy
and self checking in the compiler.
Moreover, if writing the
Sethi-Ullman routines is hard, describing, writing, and debugging the
alternative (routines that attempt to free up registers by stores into
temporaries ``on the fly'') is even worse.
Nevertheless, it should be clearly understood that these routines exist in a realm
where there is no ``right'' way to write them;
it is an art, the realm of heuristics, and, consequently, a major
source of bugs in the compiler.
Often, the early, crude versions of these routines give little trouble;
only after
the compiler is actually working
and the
code quality is being improved do serious problem have to be faced.
Having a simple, regular machine architecture is worth quite
a lot at this time.
.PP
The major problems arise from asymmetries in the registers: register pairs,
having different kinds of registers, and the related problem of
needing more than one register (frequently a pair) to store certain
data
types (such as longs or doubles).
There appears to be no general way of treating this problem;
solutions have to be fudged for each machine where the problem arises.
On the Honeywell 66, for example, there are only two general purpose registers,
so a need for a pair is the same as the need for two registers.
On the IBM 370, the register pair (0,1) is used to do multiplications and divisions;
registers 0 and 1 are not generally considered part of the scratch registers, and so
do not require allocation explicitly.
On the Interdata 8/32, after much consideration, the
decision was made not to try to deal with the register pair issue;
operations such as multiplication and division that required pairs
were simply assumed to take all of the scratch registers.
Several weeks of effort had failed to produce
an algorithm that seemed to have much chance of running successfully
without inordinate debugging effort.
The difficulty of this issue should not be minimized; it represents one of the
main intellectual efforts in porting the compiler.
Nevertheless, this problem has been fudged with a degree of
success on nearly a dozen machines, so the compiler writer should
not abandon hope.
.PP
The Sethi-Ullman computations interact with the
rest of the compiler in a number of rather subtle ways.
As already discussed, the
.I store
routine uses the Sethi-Ullman numbers to decide which subtrees are too difficult
to compute in registers, and must be stored.
There are also subtle interactions between the
rewriting routines and the Sethi-Ullman numbers.
Suppose we have a tree such as
.DS
.I "A \- B"
.DE
where
.I A
and
.I B
are expressions; suppose further that
.I B
takes two registers, and
.I A
one.
It is possible to compute the full expression in two registers by
first computing
.I B ,
and then, using the scratch register
used by
.I B ,
but not containing the answer, compute
.I A .
The subtraction can then be done, computing the expression.
(Note that this assumes a number of things, not the least of which
are register-to-register subtraction operators and symmetric
registers.)
If the machine dependent routine
.I setbin ,
however, is not prepared to recognize this case
and compute the more difficult side of the expression
first, the
Sethi-Ullman number must be set to three.
Thus, the
Sethi-Ullman number for a tree should represent the code that
the machine dependent routines are actually willing to generate.
.PP
The interaction can go the other way.
If we take an expression such as
.DS
* ( \fIp\fP + \fIi\fP )
.DE
where
.I p
is a pointer and
.I i
an integer,
this can probably be done in one register on most machines.
Thus, its Sethi-Ullman number would probably be set to one.
If double indexing is possible in the machine, a possible way
of computing the expression is to load both
.I p
and
.I i
into registers, and then use double indexing.
This would use two scratch registers; in such a case,
it is possible that the scratch registers might be unobtainable,
or might make some other part of the computation run out of
registers.
The usual solution is to cause
.I offstar
to ignore opportunities for double indexing that would tie up more scratch
registers than the Sethi-Ullman number had reserved.
.PP
In summary, the Sethi-Ullman computation represents much of the craftsmanship and artistry in any application
of the portable compiler.
It is also a frequent source of bugs.
Algorithms are available that will produce nearly optimal code
for specialized machines, but unfortunately most existing machines
are far removed from these ideals.
The best way of proceeding in practice is to start with a compiler
for a similar machine to the target, and proceed very
carefully.
.SH
Register Allocation
.PP
After the Sethi-Ullman numbers are computed,
.I order
calls a routine,
.I rallo ,
that does register allocation, if appropriate.
This routine does relatively little, in general;
this is especially true if the target machine
is fairly regular.
There are a few cases where it is assumed that
the result of a computation takes place in a particular register;
switch and function return are the two major places.
The expression tree has a field,
.I rall ,
that may be filled with a register number; this is taken
to be a preferred register, and the first temporary
register allocated by a template match will be this preferred one, if
it is free.
If not, no particular action is taken; this is just a heuristic.
If no register preference is present, the field contains NOPREF.
In some cases, the result must be placed in
a given register, no matter what.
The register number is placed in
.I rall ,
and the mask MUSTDO is logically or'ed in with it.
In this case, if the subtree is requested in a register, and comes
back in a register other than the demanded one, it is moved
by calling the routine
.I rmove .
If the target register for this move is busy, it is a compiler
error.
.PP
Note that this mechanism is the only one that will ever cause a register-to-register
move between scratch registers (unless such a move is buried in the depths of
some template).
This simplifies debugging.
In some cases, there is a rather strange interaction between
the register allocation and the Sethi-Ullman number;
if there is an operator or situation requiring a
particular register, the allocator and the Sethi-Ullman
computation must conspire to ensure that the target
register is not being used by some intermediate result of some far-removed computation.
This is most easily done by making the special operation take
all of the free registers, preventing any other partially-computed
results from cluttering up the works.
.SH
Template Shortcuts
.PP
Some operations are just too hard or too clumsy
to be implemented in code templates
on a particular architecture.
.PP
One way to handle such operations is to replace them with function calls.
The intermediate file reading code in
.I reader.c
contains a call to an implementation dependent macro MYREADER;
this can be defined to call various routines
which walk the code tree and
perform transformations.
On the \*V, for example,
unsigned division and remainder operations
are far too complex to encode in a template.
The routine
.I hardops
is called from a tree walk in
.I myreader
to detect these operations and
replace them with calls to the C runtime functions
.I udiv
and
.I urem .
(There are complementary functions
.I audiv
and
.I aurem
which are provided as support for unsigned assignment operator expressions;
they are different from \fIudiv\fP and \fIurem\fP
because the left hand side of an assignment operator expression
must be evaluated only once.)
Note that arithmetic support routines are always expensive;
the compiler makes an effort to notice common operations
such as unsigned division by a constant power of two
and generates optimal code for these inline.
.PP
Another escape involves the routine
.I zzzcode .
This function is called from
.I expand
to process template macros which start with the character
.B Z .
On the \*V,
many complex code generation problems
are swept under the rug into \fIzzzcode\fP.
Scalar type conversions are a particularly annoying issue;
they are primarily handled using the macro \fBZA\fP.
Rather than creating a template for each possible
conversion and result,
which would be tedious and complex given
C's many scalar types,
this macro allows the compiler to take shortcuts.
Tough conversions such as \fBunsigned\fP into \fBdouble\fP
are easily handled using special code under \fBZA\fP.
One convention which makes scalar conversions
somewhat more difficult than they might otherwise be
is the strict requirement that
values in registers must have a type
that is as wide or wider than a single register.
This convention is used primarily to implement
the ``usual arithmetic conversions'' of C,
but it can get in the way when
converting between (say) a \fBchar\fP value
and an \fBunsigned short\fP.
A routine named \fIcollapsible\fP is used
to determine whether one operation or two
is needed to produce a register-width result.
.PP
Another convenient macro is \fBZP\fP.
This macro is used to generate an appropriate
conditional test after a comparison.
This makes it possible to avoid
a profusion of template entries
which essentially duplicate each other,
one entry for each type of test
multiplied by the number of different
comparison conditions.
A related macro, \fBZN\fP,
is used to normalize the result
of a relational test by
producing an integer 0 or 1.
.PP
The macro \fBZS\fP does the unlovely job
of generating code for structure assignments.
It tests the size of the structure
to see what \*V instruction can be used to move it,
and is capable of emitting a block move
instruction for large structures.
On other architectures this macro
could be used to generate a function call
to a block copy routine.
.PP
The macro \fBZG\fP was recently introduced
to handle the thorny issue of
assignment operator expressions which
have an integral left hand side
and a floating point right hand side.
These expressions are passed to the code generator
without the usual type balancing so that
good code can be generated for them.
Older versions of the portable compiler computed
these expressions with integer arithmetic;
with the \fBZG\fP operator,
the current compiler can convert the left hand side
to the appropriate floating type,
compute the expression with floating point arithmetic,
convert the result back to integral type
and store it in the left hand side.
These operations are performed by
recursive calls to \fIzzzcode\fP
and other routines related to \fIexpand\fP.
.PP
An assortment of other macros finish the job
of interpreting code templates.
Among the more interesting ones:
\fBZC\fP produces the number of words
pushed on the argument stack,
which is useful for function calls;
\fBZD\fP and \fBZE\fP produce
constant increment and decrement operations;
\fBZL\fR and \fBZR\fP produce
the assembler letter code (\fBl\fP, \fBw\fP or \fBb\fP)
corresponding to the size and type
of the left and right operand respectively.
.SH
Shared Code
.PP
The
.I lint
utility shares sources with the portable compiler.
\fILint\fP uses all of the machine independent pass 1 sources,
and adds its own set of ``machine dependent'' routines,
contained mostly in \fIlint.c\fP.
\fILint\fP uses a private intermediate file format
and a private pass 2 whose source is \fIlpass2.c\fP.
Several modifications were made to the C scanner
in \fIscan.c\fP, conditionally compiled with
the symbol LINT,
in order to support \fIlint\fP's convention
of passing ``pragma'' information inside special comments.
A few other minor modifications were also
made, \fIe.g.\fP to skip over \fIasm\fP statements.
.PP
The \*f and \fIpc\fP compilers use a code generator
which shares sources with pass 2 of the portable compiler.
This code generator is very similar to pass 2
but uses a different intermediate file format.
Three source files are needed in addition to the pass 2 sources.
.I fort.c
is a machine independent source file which
contains a pass 2 main routine that replaces the equivalent routine in
.I reader.c ,
together with several routines for reading the binary intermediate file.
.I fort.c
includes the machine dependent file
.I fort.h ,
which defines two trivial label generation routines.
A header file
.I /usr/include/pcc.h
defines opcode and type symbols which
are needed to provide a standard intermediate file format;
this file is also included by the Fortran and Pascal compilers.
The creation of this header file
made it necessary to make
some changes in the way the portable C compiler is built.
These changes were made with the aim
of minimizing the number of lines changed
in the original sources.
Macro symbols in
.I pcc.h
are flagged with a unique prefix to avoid
symbol name collisions in the Fortran and Pascal compilers,
which have their own internal opcode and type symbols.
A
.I sed (1)
script is used to strip these prefixes,
producing an include file named
.I pcclocal.h
which is specific to the portable C compiler
and contains opcode symbols which
are compatible with the original opcode symbols.
A similar
.I sed
script is used to produce a file of Yacc tokens for the C grammar.
.PP
A number of changes to existing source files
were made to accommodate the Fortran-style pass 2.
These changes are conditionally compiled
using the symbol FORT.
Many changes were needed to implement
single-precision arithmetic;
other changes concern such things as
the avoidance of floating point move instructions,
which on the \*V can cause floating point faults
when a datum is not a normalized floating point value.
In earlier implementations of the Fortran-style pass 2 there were
a number of stub files which served only
to define the symbol FORT in a particular
source file; these files have been removed for 4.3BSD
in favor of a new compilation strategy which
yields up to three different objects from
a single source file, depending on what
compilation control symbols are defined for that file.
.PP
The Fortran-style pass 2 uses a Polish Postfix intermediate file.
The file is in binary format,
and is logically divided into a stream of 32-bit records.
Each record consists of an (\fIopcode, value, type\fP) triple,
possibly followed inline by more descriptive information.
The
.I opcode
and
.I type
are selected from the list in
.I pcc.h ;
the type encodes a basic type,
around which may be wrapped type modifiers
such as ``pointer to'' or ``array of'' to
produce more complex types.
The function of the
.I value
parameter depends on the opcode;
it may be used for a flag,
a register number
or the value of a constant,
or it may be unused.
The optional inline data is often a null-terminated string,
but it may also be a binary offset from a register
or from a symbolic constant;
sometimes both a string and an offset appear.
.PP
Here are a few samples of intermediate file records
and their interpretation:
.KS
.TS
expand, tab(:);
c  c  c  c  c
^  ^  ^  c  ^
l  lB l  l  l.
Opcode:Type:Value:Optional:Interpretation
:::Data:
.sp 0.5v
=
.sp 0.5v
ICON:int:flag=0:binary=5:T{
.Bl
the integer constant 5
T}
.Sp
NAME:char:flag=1:T{
.nf
binary=1,
string="_foo_"
T}:T{
.Bl
a \fBcharacter*1\fP element in
a Fortran common block \fIfoo\fP
at offset 1
T}
.Sp
OREG:char:reg=11:T{
.nf
offset=1,
string="v.2-v.1"
T}:T{
.Bl
the second element
of a Fortran \fBcharacter*1\fP array,
expressed as an offset from a static base register
T}
.Sp
PLUS:float:::T{
.Bl
a single precision add
T}
.Sp
FTEXT::size=2:string=".text 0":T{
.Bl
an inline assembler directive
of length 2 (32-bit records)
T}
.TE
.KE
.SH
Compiler Bugs
.PP
The portable compiler has an excellent record of generating correct code.
The requirement for reasonable cooperation between the register allocation,
Sethi-Ullman computation, rewriting rules, and templates builds quite a bit
of redundancy into the compiling process.
The effect of this is that, in a surprisingly short time, the compiler will
start generating correct code for those
programs that it can compile.
The hard part of the job then becomes finding and
eliminating those situations where the compiler refuses to
compile a program because it knows it cannot do it right.
For example, a template may simply be missing; this may either
give a compiler error of the form ``no match for op ...'' , or cause
the compiler to go into an infinite loop applying various rewriting rules.
The compiler has a variable,
.I nrecur ,
that is set to 0 at the beginning of an expressions, and
incremented at key spots in the
compilation process; if this parameter gets too large, the
compiler decides that it is in a loop, and aborts.
Loops are also characteristic of botches in the machine-dependent rewriting rules.
Bad Sethi-Ullman computations usually cause the scratch registers
to run out; this often means
that the Sethi-Ullman number was underestimated, so
.I store
did not store something it should have; alternatively,
it can mean that the rewriting rules were not smart enough to
find the sequence that
.I sucomp
assumed would be used.
.PP
The best approach when a compiler error is detected involves several stages.
First, try to get a small example program that steps on the bug.
Second, turn on various debugging flags in the code generator, and follow the
tree through the process of being matched and rewritten.
Some flags of interest are
\fB\-e\fP, which prints the expression tree,
\fB\-r\fP, which gives information about the allocation of registers,
\fB\-a\fP, which gives information about the performance of
.I rallo ,
and \fB\-o\fP, which gives information about the behavior of
.I order .
This technique should allow most bugs to be found relatively quickly.
.PP
Unfortunately, finding the bug is usually not enough; it must also
be fixed!
The difficulty arises because a fix to the particular bug of interest tends
to break other code that already works.
Regression tests, tests that compare the performance of
a new compiler against the performance of an older one, are very
valuable in preventing major catastrophes.
.SH
Compiler Extensions
.PP
The portable C compiler makes a few extensions
to the language described by Ritchie.
.PP
.I "Single precision arithmetic."
``All floating arithmetic in C is carried out in double-precision;
whenever a
.B float
appears in a an expression it is lengthened to
.B double
by zero-padding its fraction.''
\*-Dennis Ritchie.
.[
kernighan ritchie prentice 1978
.]
Programmers who would like to use C
to write numerical applications
often shy away from it because
C programs cannot perform single precision arithmetic.
On machines such as the \*V
which can cleanly support arithmetic on two (or more)
sizes of floating point values,
programs which can take advantage of
single precision arithmetic will run faster.
A very popular proposal for the ANSI C standard states that
implementations may perform single precision computations
with single precision arithmetic;
some actual C implementations already do this,
and now the Berkeley compiler joins them.
.PP
The changes are implemented in the compiler with a set of
conditional compilation directives based on the symbol SPRECC;
thus two compilers are generated,
one with only double precision arithmetic
and one with both double and single precision arithmetic.
The
.I cc
program uses a flag
.B \-f
to select the single/double version of the compiler (\fI/lib/sccom\fP)
instead of the default double only version (\fI/lib/ccom\fP).
It is expected that at some time in the future
the double only compiler will be retired
and the single/double compiler will become the default.
.PP
There are a few implementation details
of the single/double compiler which
will be of interest to users and compiler porters.
To maintain compatibility with functions compiled by the double only compiler,
single precision actual arguments are still coerced to double precision,
and formal arguments which are declared single precision
are still ``really'' double precision.
This may change if function prototypes of the sort
proposed for the ANSI C standard are eventually adopted.
Floating point constants are now classified into single precision
and double precision types.
The precision of a constant is determined from context;
if a floating constant appears in an arithmetic expression
with a single precision value,
the constant is treated as having single precision type
and the arithmetic expression is computed
using single precision arithmetic.
.PP
Remarkably little code in the compiler
needed to be changed to implement the single/double compiler.
In many cases the changes overlapped with
special cases which are used for the Fortran-style
pass 2 (\fI/lib/f1\fP).
Most of the single precision changes were implemented by Sam Leffler.
.PP
.I "Preprocessor extensions."
The portable C compiler is normally distributed
with a macro preprocessor written by J. F. Reiser.
This preprocessor implements the features described
in Ritchie's reference manual;
it removes comments, expands macro definitions
and removes or inserts code based on conditional compilation directives.
Two interesting extensions are provided by this version of the preprocessor:
.IP \(bu
When comments are removed, no white space is necessarily substituted;
this has the effect of \fIre-tokenizing\fP code,
since the PCC will reanalyze the input.
Macros can thus create new tokens by clever use of comments.
For example, the macro definition ``#define foo(a,b) a/**/b''
creates a macro \fIfoo\fP which
concatenates its two arguments, forming a new token.
.IP \(bu
Macro bodies are analyzed for macro arguments
without regard to the boundaries of string or character constants.
The definition ``#define bar(a) "a\en"'' creates a macro
which returns the literal form of its argument embedded
in a string with a newline appended.
.PP
These extensions are not portable to
a number of other C preprocessors.
They may be replaced in the future by corresponding ANSI C features,
when the ANSI C standard has been formalized.
.SH
Summary and Conclusion
.PP
The portable compiler has been a useful tool for providing C
capability on a large number of diverse machines,
and for testing a number of theoretical
constructs in a practical setting.
It has many blemishes, both in style and functionality.
It has been applied to many more machines than first
anticipated, of a much wider range than originally dreamed of.
Its use has also spread much faster than expected, leaving parts of
the compiler still somewhat raw in shape.
.PP
On the theoretical side, there is some hope that the
skeleton of the
.I sucomp
routine could be generated for many machines directly from the
templates; this would give a considerable boost
to the portability and correctness of the compiler,
but might affect tunability and code quality.
There is also room for more optimization, both within
.I optim
and in the form of a portable ``peephole'' optimizer.
.PP
On the practical, development side,
the compiler could probably be sped up and made smaller
without doing too much violence to its basic structure.
Parts of the compiler deserve to be rewritten;
the initialization code, register allocation, and
parser are prime candidates.
It might be that doing some or all of the parsing
with a recursive descent parser might
save enough space and time to be worthwhile;
it would certainly ease the problem of moving the
compiler to an environment where
.I Yacc
is not already present.
.SH
Acknowledgements
.PP
I would like to thank the many people who have
sympathetically, and even enthusiastically, helped me grapple
with what has been a frustrating program to write, test, and install.
D. M. Ritchie and E. N. Pinson provided needed early
encouragement and philosophical guidance;
M. E. Lesk,
R. Muha, T. G. Peterson,
G. Riddle, L. Rosler,
R. W. Mitze,
B. R. Rowland,
S. I. Feldman,
and
T. B. London
have all contributed ideas, gripes, and all, at one time or another,
climbed ``into the pits'' with me to help debug.
Without their help this effort would have not been possible;
with it, it was often kind of fun.
\*-S. C. Johnson
.sp
.PP
Many people have contributed fixes and improvements
to the current Berkeley version of the compiler.
A number of really valuable fixes were contributed by
Ralph Campbell, Sam Leffler, Kirk McKusick, Arthur Olsen, Donn Seeley,
Don Speck and Chris Torek,
but most of the bugs were spotted by
the legions of virtuous C programmers
who were kind enough to let us know
that the compiler was broken
and when the heck were we going to get it fixed?
Thank you all.
\*-Donn Seeley
.sp 2
.LP
.[
$LIST$
.]
.LP
