.ND
.nr LL 75n
.nr LT 80n
.rm CF
.ds LH PEARL Documentation
.ds CH Update
.ds RH Page %
.po 1.50i
.ls 1
.hy 14
.LP
.bp 76
.DS C
.LG
\fBUpdate of Changes
Through
PEARL 3.9
April 1983\fR
.SM
.DE
.SH
1. Introduction
.PP
This appendix describes the changes that have been made to PEARL since
the original manual was produced.
It is designed to parallel the sections of the manual so that the
original index can be used to find changes.
.PP
PEARL is now distributed with Franz Lisp (starting with Opus 38.58).
The earliest version of PEARL distributed (with 38.58) was PEARL 3.6.
The current update corresponds to version 3.9 and is being distributed
with the Franz Opus 38.69 available with 4.2 BSD Unix.
The current major and minor version numbers for PEARL are stored in
the special variables \fIpearlmajorversion\fR and \fIpearlminorversion\fR
respectively.
.PP
With the change in mail protocols and addition of new machines at
Berkeley, the form of addresses for bugs and suggestions have been
simplified.
Bugs, suggestions or queries should be sent to 
\fIPearl\-Bugs@Berkeley\fR or \fIucbvax!pearl\-bugs\fR.
.SH
2. Running PEARL
.PP
PEARL is currently only maintained under Franz Lisp.
The current version could be moved back to UCI Lisp (or to other
Lisps) fairly easily but has not been for lack of need.
Lisp Machine Lisp is the most likely Lisp that PEARL will be moved
to next but it has not been done, mostly because of conflicts in the
use of the colon character and lack of access to a Lisp Machine.
.SH
2.1 Under Franz Lisp
.PP
Since PEARL is now part of Franz Lisp, it should be available as
\fI/usr/ucb/pearl\fR or wherever you find \fIlisp\fR on your system.
.PP
The \fI.start.pearl\fR and \fI.init.pearl\fR files are actually
called \fIstart.prl\fR and \fIinit.prl\fR and may optionally be
prefixed with a dot "." and/or suffixed with either ".o" or ".l" just
as in Franz.
The use of the dot prefix and of the ".o" or ".l" is preferred
and fastest.
Thus PEARL will read the first file found in the following
sequence: \fI.init.prl.o\fR, \fI.init.prl.l\fR, \fI.init.prl\fR,
\fIinit.prl.o\fR, \fIinit.prl.l\fR, or \fIinit.prl\fR and similarly
for \fIstart.prl\fR.
Franz's special variable \fI$ldprint\fR is lambda-bound to \fInil\fR during
the reading of these two files to disable the printing of
"[load .init.prl]".
.SH
5. Accessing Slots of Structures
.PP
Doing a "\fIpath \fBput\fR" on a slot containing a variable will
not set the variable.
Rather it replaces the variable with the value provided. 
.SH
10. Printing Structures, Symbols and Other PEARL Objects
.PP
The various printing functions still exist but all call a single
formatting function with various options controlled by special atoms.
The principle functions are \fBallform\fR which does the building of a
printable list form for internal PEARL structures and \fBallprint\fR
which calls \fIallform\fR.
\fIAllform\fR uses the following global variables to determine what
form to build:
.IP 1.
\fB*abbrevprint*\fR -- a non-\fInil\fR value causes abbreviations
to be used whenever possible for any structure except the top level
structure passed to a print function.
Abbreviations are described at the end of this section.
The new functions \fBabbrevform\fR and \fBabbrevprint\fR
lambda-bind this to \fIt\fR and then call \fIallform\fR.
\fIfullform\fR binds this to \fInil\fR.
.IP 2.
\fB*fullprint*\fR -- a non-\fInil\fR value causes complete information
including hooks and predicates to be given when present.
\fIFullform\fR (and thus \fIfullprint\fR) lambda-binds this to \fIt\fR
and calls \fIallform\fR.
\fIAbbrevform\fR binds this to \fInil\fR.
.LP
\fIValform\fR lambda-binds both to \fInil\fR.
The default value of both is are also \fInil\fR, so that the default action of
\fIallform\fR when used by itself will be like \fIvalform\fR unless these
special variables are changed.
All the default print functions automatically use \fIallprint\fR so that
they can all be changed by changes to the default values of
\fI*abbrevprint*\fR and \fI*fullprint*\fR.
.LP
Two other special atoms which affect the behavior of all the printing
functions are:
.IP 3.
\fB*uniqueprint*\fR -- a non-\fInil\fR value causes a structure which is
encountered more than once during the same top-level call to a
print function to be translated into exactly the same cons-cells.
This saves on cons-cells and also makes it possible for the \fI\-\-form\fR
functions to handle circular structures, although \fIsprint\fR and thus
the \fI\-\-print\fR functions cannot handle the result.
Since most people seldom have duplications within a structure,
the default is \fInil\fR (off).
The assoc\-list of already translated structures is stored in the
special atom \fB*uniqueprintlist*\fR.
.IP 4.
\fB*quiet*\fR -- a non-\fInil\fR value disables all printing by any of
PEARL's print functions, providing an easy way to disable printing
all at once.
There is also a function called \fBquiet\fR which behaves like
\fIprogn\fR, except that it lambda-binds \fI*quiet*\fR to \fIt\fR
during the evaluation of its arguments, providing a local island
of "quiet".
.PP
The standard print functions are designed to handle any Lisp structure.
Thus, they spend a significant amount of time determining what kind of
object they have been passed.
For situations in which you know exactly what type of object you want
printed, the functions 
\fBstructureform/structureprint\fR, \fBsymbolform/symbolprint\fR,
and \fBstreamform/streamprint\fR are provided.
They assume you know what you are doing and do not ensure that
you give them the right type of value.
.PP
Adapting PEARL to fit an improvement in Franz, the atoms
\fIshowstack-printer\fR and \fItrace-printer\fR are bound to
the functions \fBpearlshowstackprintfn\fR and \fBpearltraceprintfn\fR.
\fBNote\fR the addition of "pearl" to the beginning of these.
The name of \fIbreakprintfn\fR was also changed to
\fBpearlbreakprintfn\fR but it is not currently lambda-bindable.
.SH
10.1. Abbreviations
.PP
As people build larger deeper structures it becomes useful to have
some of them abbreviated during printing if they are internal to the
structure being printed.
When an individual (including default instance) structure is created,
an abbreviation atom is stored in it.
This abbreviation is chosen as follows:
.IP 1.
If the option in \fIcreate\fR of having a structure automatically
stored in an atom is used, then that atom is the one used as an
abbreviation.
Thus the structure created by \fI(create individual x Pete)\fR will
be given the abbreviation \fIPete\fR.
.IP 2.
If that option is not used, then default instances will be
given the abbreviation \fIi:x\fR (where x is the structure type name)
and individuals at the top level will be given a name \fInewsym\fR-ed
from the name of their type.
Thus \fI(create base x)\fR will make a default instance abbreviated
\fIi:x\fR and the first structure created with \fI(create individual x)\fR
will be abbreviated \fIx0\fR.
.IP 3.
\fIScopy\fR and related functions that create new structures from old
ones \fIgensym\fR the new structure's abbreviation from that of the
old structure.
.SH
11. Error Messages, Bugs, and Error Handling Abilities
.PP
Bugs, complaints and suggestions of useful features (to be added to
the current list of 30 or so things on the wish list) should be
mailed by electronic mail to \fBPearl\-Bugs@Berkeley\fR or
\fBucbvax!pearl\-bugs\fR.
.SH
12. Short-Circuiting and Redirecting Create Using !, $ and Atoms
.PP
If an atom is encountered where a value-description was expected in
any type of slot, and it is bound to a value of the right type,
its value is inserted into the slot.
For \fIsymbols\fR, this is done if the atom is not a symbol name.
For \fIstructures\fR, the atom must evaluate to a structure.
For \fILisp\fR slots, it must simply be bound.
For \fIsetof\fR slots, its value is checked for being of the appropriate
type, including depth of nesting.
.PP
Note also that a change in the internal representation has made it possible
to allow \fBeven atoms\fR in slots of type \fIlisp\fR.
.SH
13. More Flexible Hash Selection
.PP
Because we have never gotten around to adding fetch functions to take
advantage of colon and colon-colon hashing and these two methods
really are not useful in normal fetching, they are currently ignored.
.PP
For situations in which you wish to create an expanded structure and
add new hashing marks to an old slot (rather than replace them),
preceding new hash marks with a plus ("+") will cause the old
hashing information to be copied before processing the new hashing.
.PP
Thus, the sequence
.DS
(cb x (* a int))
(ce x y (a ^))
(ce x z (+ : a ^))
(ce x w (: + a ^)) ; anomalous use of +
.DE
will result in:
.DS
* hashing in x,
no hashing in y,
both * and : hashing in z, and
only * hashing in w (because of misplacement of +).
.DE
.PP
Several new hashing methods have been added to PEARL.
.PP
A hashing mechanism using the label \fB***\fR has been added called
"triple-star hashing".
If slots are labeled with *** and \fBall\fR slots so marked are filled
with useful values, then the item is hashed under the type of structure
plus the values of all these slots.
During fetching, this is considered the most useful (that is,
specific) hash method.
.PP
A hashing mechanism using the label \fB&&\fR has been added called
"hash focusing".
It is designed for people using a data base all of
whose entries are of the same type (not required, just common
for this application) and enables the contents of a single slot
to be used to better discriminate them.
Examples of such structures are "planfors", inference rules,
or almost any other such extremely-common binary predicates.
If a slot labeled && is found when inserting into the database then
the item is hashed as if it were the item in the slot so labeled.
At fetching time, && is considered less useful than *** or **
and more useful than * or nothing.
.PP
This differs from & (hash aliasing) in that hash focusing
affects how a structure itself is inserted and fetched, while
& simply affects how structures containing this type of
structure are treated.
For example, suppose the unique numbers of A, B, and C
respectively are 1, 2, and 3.
C is a symbol.
A has one slot X with * and && hashing.
B has one slot Y of type symbol with * hashing.
Then a structure like (A (X (B (Y C)))) will be indexed the
following ways and \fIfetcheverywhere\fR (see below) will find
it in the following order: the && method will be used first
which uses the 2 and 3 from B and its C, (ignoring the 1 of A),
and also simply 2 from B;
the * on A uses the type of B thus using 1 and 2;
it is also looked for under the 1 of A without using 2 or 3.
If B had an & in its slot then the * on A is affected by & on B thus
using 1 and 3 (ignoring the 2 of B).
.PP
Thus, if you consider A, B, and C to be three levels of information
in the structure, an item can be hashed under any combination of two
of those levels.
The normal * method uses levels 1 and 2,
the aliasing & method ignores level 2 and uses levels 1 and 3,
and the new focussing && method ignores level 1 and uses levels 2 and 3.
In addition, the item can be put under 1, 2 or 3 individually by
various combinations of marks (1 = none, 2 = :, 3 = :+&).
The only unavailable combination of the three is all of them.
.SH
16. Attaching Hooks to Structures (If-Added Demons)
.PP
Slot hooks are now always inherited and added to, rather than replaced.
If the hooks and predicates of a slot are preceded by \fBinstead\fR
then inheriting does not happen and hooks and predicates are replaced.
.PP
The atoms for path hooks were misnamed in such a way that you could not
use \fIhidden\fR and \fIvisible\fR.
Instead of \fI*rungethooks*\fR, and other \fI*run...hooks*\fR forms,
they are now \fB*rungetpathhooks*\fR and other \fB*run...pathhooks*\fR.
Note that they must be called as (\fIXXX\fRpath ...) and not
(path\ \fIXXX\fR ...) when used with \fIhidden\fR and \fIvisible\fR.
.SH
17. Creating and Manipulating Multiple Data Bases
.PP
The function \fIsetdbsize\fR can now be done at any time and
will remove all current databases before changing the size,
warn the user (if \fI*warn*\fR is set) and recreate \fI*maindb*\fR
with the special variable \fI*db*\fR pointing to it.
.PP
The function \fIcleardb\fR is now a local database clearer
and its effects do not extend up the database hierarchy.
.SH
19. Creating Expanded Subtypes of Previously Defined Objects
.PP
Hashing in old slots inherited by new expanded structures can now be
added to by preceding the new hash marks with plus ("+").
See section 13 above.
.PP
The name of an old slot inherited by a new expanded structure may be
changed by following the new name by the old slotname preceded with
an equal sign.
Thus for example:
.DS
pearl> (create base X (A struct))
    (X (A (nilstruct)))
pearl> (create expanded X Y (B =A) (C .....))
    (Y (B (nilstruct)) (C .....)))
.DE
Note that there may not be a space between the equal sign and the slot
name since \fI=\fR is a read macro which expands \fI=A\fR into
\fI(*slot* A)\fR but leaves a single space-surrounded equal sign alone.
The actual effect is to add another name to the slot so that it can be
later referenced with either name.
.SH
20. Fetching Expanded Structures
.PP
A fetching function called \fBfetcheverywhere\fR exists which gathers
\fBall\fR the buckets the object could have been hashed into and
builds a stream out of all of them (potentially five buckets).
There is currently no "expanded" counterpart, since it has the potential
of returning \fI5 times the-depth-of-the-hierarchy\fR buckets.
.SH
21.2 The Matching Process
.PP
During matching, if an unbound global variable is set and
the match later fails, the value is restored to \fI*pearlunbound*\fR.
The names of variables that are set are saved in the special variable
\fB*globalsavestack*\fR.
.PP
Formerly, there was only one match function which was used by both
\fIstandardfetch\fR and \fIexpandedfetch\fR and which therefore would
match two structures if they were hierarchically related.
This is really inappropriate for the standard fetching, so 
there are now two regular match functions, \fIstandardmatch\fR and
\fIbasicmatch\fR, which will only match two structures of the same type,
and two expanded match functions, \fIstandardexpandedmatch\fR and
\fIbasicexpandedmatch\fR, which will match two structures which are
related hierarchically (one above the other) on the slots they have
in common.
Streams built by \fIstandardfetch\fR use the regular versions and
and streams built by \fIexpandedfetch\fR use the expanded versions.
.PP
There are now two functions \fBmemmatch\fR and \fBmemstrequal\fR which
are like \fImemq\fR except that they use \fImatch\fR and \fIstrequal\fR
respectively instead of \fIeq\fR.
.PP
As of version 3.8, PEARL will now do \fBunification\fR of variables in
pattern matching.
To turn it on, call the function \fBuseunification\fR.
(The current implementation precludes turning it off once it is on but
this may be remedied in later versions if we can figure out what it
means to stop unifying.)
.SH
26. Looping and Copying Functions
.PP
The function \fIscopy\fR no longer deletes bound adjunct variables.
.PP
The standard Franz function \fIcopy\fR is no longer redefined since
the standard version now avoids the copying of hunks.
.PP
The functions \fIscopy\fR and \fIpatternize\fR are now exprs rather
than macros.
.PP
The new function \fBvarreplace\fR permanently "freezes" the values
of slots containing bound variables by replacing all bound variables
in an item with their values.
.PP
A variation on \fIscopy\fR called \fBintscopy\fR ("internal scopy")
is designed to do the copying as if the copied item were internal to
another outer item, thus sharing its local and block variables.
Its arguments are the item to be copied and the outer item in whose
scopy the copying should be done.
.SH
29. Appendix of UCI Lisp functions added to Franz PEARL
.PP
The definitions of \fIde\fR, \fIdf\fR, \fIdm\fR, \fIdrm\fR and \fIdsm\fR
have been modified so that if the special variable \fB*savedefs*\fR
is \fInil\fR then old definitions of functions are not saved.
This is especially useful in compiling (and as a result, assembly
and loading) since it will speed them up quite a bit.
This also disables the saving of the name of the file that the
definition was in.
The variable \fI*savedefs*\fR is normally \fIt\fR which causes these
macros to act as before, saving the definition, etc.
If \fI*savedefs*\fR is \fInil\fR, then they simply expand into the
appropriate \fIdefun\fR or \fIsetsyntax\fR.
The following lines should be included in a file to have this effect
only at compile time:
.DS
(eval-when (compile)
	(declare (special *savedefs*))
	(setq *savedefs* nil))
.DE
.LP
If you also want to permanently disable this feature in a lisp, that
loads \fIucisubset.l\fR, simply put a \fI(setq *savedefs* nil)\fR
in your \fI.lisprc\fR file AFTER the loading of \fIucisubset.l\fR.
.PP
The function \fIremove\fR is no longer made equivalent to Franz's
\fIdelete\fR so that Franz's \fIremove\fR can be used.
The functions \fInth\fR, \fIpush\fR and \fIpop\fR are no longer
defined by PEARL, since the new Franz versions are better.
(UCI Lisp users note: This switches the arguments to \fIpush\fR.)
.SH
32. Index of Global Variables and Functions With Their Arguments
.PP
All special variables in PEARL are now defined with \fIdefvar\fR so
that \fIfasl\fR'ing in \fIpearl.o\fR at compile time will automatically
declare them special again.
.PP
All the exprs whose names were of the form \fIXXXX1\fR where
\fIXXXX\fR was the name of a lexpr which was a principle function
of PEARL were eliminated (i.e., absorbed by the other form).
.SH
34. Compiling Lisp+PEARL Files.
.PP
To compile a file of mixed Lisp and PEARL functions with \fIliszt\fR,
you must first load in the function definitions and special
declarations of PEARL by loading the object code.  
This is the file \fIpearl.o\fR which is normally kept in the
\fI/usr/lib/lisp\fR directory and will found automatically by
\fIload\fR.
.PP
Thus, the following should normally be included at the
beginning of a PEARL file you wish to compile:
.DS
(eval-when (compile)
	(declare (special defmacro-for-compiling))
	(setq defmacro-for-compiling t)
	(load 'pearl.o))
(declare (macros t))
.DE
.rm CF
.rm LH
.rm CH
.rm RH
.bp
.DS C
.LG
\fBUpdate of Changes
Through
PEARL 3.9
April 1983
.sp 1
Table of Contents\fR
.SM
.DE
.DS L
1.  Introduction                                                                                           \ka76
2.  Running PEARL               \h'|\nau'76
      2.1.  Under Franz Lisp    \h'|\nau'76
5.  Accessing Slots of Structures \h'|\nau'76
10. Printing Structures, Symbols and Other PEARL Objects \h'|\nau'76
      10.1.  Abbreviations \h'|\nau'77
11. Error Messages, Bugs, and Error Handling Abilities \h'|\nau'78
12. Short-Circuiting and Redirecting \fICreate\fR Using !, $ and Atoms \h'|\nau'78
13. More Flexible Hash Selection \h'|\nau'78
16. Attaching Hooks to Structures (If-Added Demons) \h'|\nau'79
17. Creating and Manipulating Multiple Data Bases \h'|\nau'80
19. Creating Expanded Subtypes of Previously Defined Objects \h'|\nau'80
20. Fetching Expanded Structures \h'|\nau'80
21.2  The Matching Process \h'|\nau'80
26. Looping and Copying Functions \h'|\nau'81
29. Appendix of UCI Lisp functions added to Franz PEARL \h'|\nau'81
32. Index of Global Variables and Functions With Their Arguments \h'|\nau'81
34. Compiling Lisp+PEARL Files \h'|\nau'82
.DE
