.RS
.ds CF "\(co 1982 UCLA Computer Club
.TL
A Brief Description of UCLA
Dungeon Definition Language (DDL)
(Second Edition)
.AU
Bruce Adler
Chris Kostanick
Michael Stein
Michael Urban
.AI
University of California
Los Angeles, CA 90024
.AB
This document describes Dungeon Definition Language, a meta-adventure
specification language.  It is designed primarily for the programmer
who wishes to create a 
\s-2DDL\s+2 
"world", and secondarily for the programmer
attempting to implement 
\s-2DDL\s+2 
on a new host machine.  
.AE
.bp 1
.ds CF "\(co 1982 UCLA Computer Club
.NH 
Introduction.
.PP
\s-2DDL\s+2
is a system of notation for the specification of "worlds".  Using
\s-2DDL\s+2,
a programmer may create Objects, Verbs to act upon those objects,
and Routines to describe the behavior of Objects and Verbs.  The user
of a 
\s-2DDL\s+2
program, known as the Player, types these verbs and the names of
objects to manipulate those objects at a high level.  Thus, a Player's
dialogue with a 
\s-2DDL\s+2
program will appear something like:
.IP
.DS
.SM

  You are standing outside the north entrance of a large
  brick building.  Inscribed above the doorway, appear the
  text: 'AARDVARK'S MUSEUM -- GATEWAY TO ADVENTURELAND'.
  There is a coil of rope here.
  There is a shovel here.
  There is a carbide-flame lamp here.
  There is a copy of a newspaper here.
 >take rope
  OK
 >south
  You are in a large rotunda of an old museum.  Doors lead
  to the north, south, east, and west, and a narrow stairway
  in the north-east corner of the room leads down.
  There is a ball-point pen here.
  There is a slip of paper here.
 >take paper
  OK
 >take pen
  OK
 >e
  You are in a dimly lit room containing an empty display case.
  A portion of a vandalized sign above the case reads:
  'ARTIFACTS OF ANCIENT INDIA -- Several of these items,
  including the sacred rhinoceros horn, the deadly ...'.
  The rest of the sign is unreadable.
  To the west, you can look through a large door into the rotunda
  of the museum. On the east wall of the hall there is an outline
  of an arch.
 >sign paper
  In a blinding flash of light, a stone archway appears in the east wall!
.NL
.DE
.PP
This sort of behavior will be familiar to users of the celebrated programs,
.I "Adventure" 
and 
.I "Dungeon" 
(AKA 
.I "Zork"
), of Crowther, Woods, Anderson
and Blank.
While not as sophisticated in many ways as some of these programs,
the primary function of 
\s-2DDL\s+2 
is to allow a number of interesting
puzzles and games to be exchanged among users of disparate machines
with a minimum of portability problem.
.NH
Processor Structure
.PP
\s-2DDL\s+2 consists of two processors.  The first,
.I ddlcomp,
reads a program in the \s-2DDL\s+2 language, analyzes it,
notes syntax errors, and produces a data file representing
the game described by the program.  The second,
.I ddlrun,
takes one such data file as input and presents a Player with
the scenario represented by that file.
.NH
General Constructs
.PP
A \s-2DDL\s+2 program will consist of a series of declarations.
Some declarations will define properties of Objects and Verbs;
others will define Routines to be executed at various stages
of play.  These routines are roughly similar to \s-2LISP\s+2
functions in that they consist of a series of functional
expressions.  Certain identifiers, such as DWIMI and START,
are expected to be defined as routines by the programmer,
and have special meaning to the processor.
.NH
General Flow of Execution.
.PP
When the 
\s-2DDL\s+2
program begins execution, a special routine which has been
coded by the 
\s-2DDL\s+2
programmer is executed.  This routine must be given the
name START.  START will normally initialize demons and set certain initial
values.  Execution then proceeds in the cyclic fashion described below.
.PP
When a 
\s-2DDL\s+2
scenario is running,
execution proceeds in a series of cycles known as "turns".  On
each turn, a number of actions takes place.
.IP "(1) Demons: " 10
Each of the currently active Demon routines
(routines set up to do some work on each turn)
is run in order
of activation.
Demon routines are specified and activated by a 
\s-2DDL\s+2
program through 
the 
.I $sdem 
function.
.B Note: 
The normal action of Looking (executing description routines) which
one expects to occur on each turn must be coded by the 
\s-2DDL\s+2
programmer
as a Demon.
.IP "(2) Fuses: " 10
All active Fuse routines are checked to see if they
are to be executed on this turn.  Those Fuses which have thus "burned down"
are then executed (in reverse order of activation) and removed from the
Fuse list.
.IP "(3) Parse: " 10
The player types a line of input,
and an attempt is made to resolve that input into a Verb, an Indirect Object,
and a Direct Object, by means of attendant Prepositions, Articles,
and Adjectives.  Unambiguous abbreviations for words are recognized
by the parser.

If an input Noun is ambiguous (for example, if the user says
"take book" when "red book" and "blue book" have both been
\fIdefined\fR),
\s-2DDL\s+2
routines called DWIMD and DWIMI are used to disambiguate
direct and indirect objects respectively.  DWIMD and DWIMI,
which must be defined by the \s-2DDL\s+2 programmer, should return
nonzero if the direct or indirect object is "possibly the one he means"
(e.g. if it is in the room, etc); only if exactly one such object
exists with the given Noun name can the parse complete successfuly.
If
any of the input components is found to be missing, the value zero is
assumed for that object (and no associated routines are executed).

If a syntax error or unknown word is detected, a hopefully informative
error message is printed.  In addition, unknown words encountered
in the input
may be saved in a file for perusal by the DDL programmer.

The direct object may be enclosed in double-quotes by the Player.
Such a direct object is returned as a String to the program.  Strings
may be detected by the program as having "numeric values" less than
zero.  Strings may be operated on with the 
.I $eqst, 
.I $subs, 
and 
.I $leng
functions, and the 
.I $say 
procedure.
.IP "(4) Pre-action: " 10
The PREACT routine (if any)
that the 
\s-2DDL\s+2
programmer has associated
with the input Verb is executed.  These routines typically will check
for the availability of the object in question, and so on.
.IP "(5) Indirect Object: " 10
The ACTION routine associated with the Indirect Object
that the Player typed (if any) is executed.
.IP "(6) Direct Object: " 10
The ACTION routine associated with the Direct Object
that the Player typed (if any) is executed.
For many specialized actions (like "rub lamp") the particular code
is best attached to the object.
If the Direct Object is a String, the ACTION routine (if any)
associated with the object STRING (if such is defined by the
programmer) is executed.
.IP "(7) Room Action: " 10
The ACTION routine associated with the room the Player is
in (actually, the LOC of .ME) is executed.  Normally, this will be
a "transition" routine which will check if the verb is "north", and so on.
.B Note:
This is the ONLY aspect of "built-in" action which depends in ANY
WAY upon the actual state of variables within the "dungeon" itself.
.IP "(8)  Verb: " 10
The ACTION routine associated with the input Verb (if any)
is executed.  ACTION routines for most Verbs will often be
default routines.  For example the Action routine for the Verb "rub"
might print "Rubbing that object is not useful."
.LP
If any of these routines terminates with ($exit 1), the remainder of
the current turn is skipped.  Furthermore, the 
\s-2DDL\s+2
programmer is responsible
for incrementing the Turn Counter (normally in a Demon routine) if Fuses
are to be used.
.NH
Data types.
.NH 2
Objects.
.PP
Player machinations are in terms of Objects.  All Objects are nodes in
a "containment" tree, the root node of which is labelled ".ALL".  
A second special
object, ".ME" is considered to represent the Player.  Objects will
normally be treated either as rooms or portable-type objects, but 
\s-2DDL\s+2
itself
does not distinguish these functions; all objects are stored and treated
uniformly.  It is therefore possible, in principal, to write a 
\s-2DDL\s+2
scenario in which the Player may pick up a room, carry it, and
later enter it.  Each object possesses the following attributes.  If
any of these is not specified, it is given the default value of zero.
.IP "LOC: " 6
The object ID of the parent (location) of the object.
.IP "CONT: " 6
The object ID of the first child (contents) of the object.
.IP "LINK: " 6
The object ID of the next sibling (others in the same place) of the
object
.IP "ADJ: " 6
The Adjective ID which uniquely distinguishes this object from others
of the same name (if any).
.IP "OTHERS: " 6
The Object ID of another object with the same name as this object,
though with a different adjective.  The DWIMx routines are called
for each of the objects in this chain associated with 
an ambiguous direct
or indirect object.  For example, if the player types "take book",
and both "red book" and "blue book" are defined, DWIMD will be
called once with "red book" as its parameter, and once with "blue book"
as its parameter.  If the DWIMD is coded correctly for this example,
DWIMI will return TRUE (nonzero) if and only if the parameter book
can be taken.  If zero or both books satisfy the DWIMD criteria,
an error message is printed.
.IP "NAME: " 6
The unqualified Noun by which the Player names the object.
.IP "PROPS: " 6
Up to
25
numeric values can be arbitrarily associated with an object by the 
\s-2DDL\s+2
programmer.  Properties
1-16
may only possess the values 0 or 1.  The others may range in value from
-32768 to +32767.
The last three of these properties have special usages.  Their indices
are predefined by the compiler.
.RS
.IP "LDESC (23)" 6
The Routine ID of a "Long Description" routine
.IP "SDESC (24)" 6
The Routine ID of a "Short Description" routine
.IP "ACTION (25)" 6
The Routine ID of a "Action" routine, to be called if the Player
either attempts to do something with that object (specifies it as a
Direct or Indirect Object), or while inside that object.
.RE
.NH 2
Verbs.
.PP
Each "command" typed by the Player must begin with a Verb which 
has been
defined by the 
\s-2DDL\s+2
programmer.  Each Verb has two Routines associated with it:
.IP "PREACT: " 6
The Routine ID of a routine to execute when the verb has been
recognized and the remaining input identified, but before any "Action"
routines associated with the Objects in that input have been executed.
For example, the PREACT routine of "take" might check to see if
the direct object is in the room.
.IP "ACTION: " 6
The Routine ID of a routine to execute after all input object action
routines have been called.
Our experience has been that such routines end up being "default" routines
that typically only say things like "Rubbing that object does nothing."
.NH 2
Strings.
.PP
Simple strings may be defined by the 
\s-2DDL\s+2
programmer to be printed.  Strings
may be up to 255 bytes in length, delimited by double-quote marks.
Carriage returns may be embedded in strings freely, or the sequence \en
may be used to represent a carriage return at any point.
Additionally, strings may be generated by the
.I $subs
and
.I $read
functions at run time, or typed by the player
as a "direct object."
.NH 2
Numbers.
.PP
\s-2DDL\s+2
programers may only specify nonnegative integers up to 32767.
However, a routine may compute any integer value from -32768 to +32767.
.NH 2
Adjectives.
.PP
Adjectives possess no data, but are uniquely numbered by the 
\s-2DDL\s+2
compiler
so as to have unique internal IDs (which begin at the value 1).
Adjectives are normally only used to distinguish various objects which
have the same Noun name (e.g. the "red book" and the "blue book").
.NH 2
Routines
.PP
Routines represent the actual logical behavior of the Dungeon.  A routine
consists of one or more calls to builtin or user-defined functions.
Internally, a routine may be stored as an interpretive program for a
very simple stack machine.  The internal representation is up to the
implementer.
Routines may call one another, and a single
routine may call itself recursively.
.NH 2
Globals
.PP
50
globals (numbered
0-49)
are available to the 
\s-2DDL\s+2
programmer and may contain any integer value.  They are named by
numeric constants.  Such constants are conveniently assigned
symbolic names by means of the VAR declaration described below.
The last three globals are set each turn to contain the Indirect
Object, Direct Object, Preposition, 
and Verb typed by the player.  The constants
.I Iobj, 
.I Dobj, 
.I Prep,
and 
.I Verb 
are predefined by the compiler to refer to those
globals.
.B "Implementor's Note:"
If \s-2DDL\s+2 is implemented on a system that
does not permit case distinctions, these constants
should be renamed as
.I iobj,
.I dobj,
.I prepg,
and
.I verbg
to avoid conflict with the
.I VERB
and
.I PREP
declarations described below.
.NH
\s-2DDL\s+2
Programs
.PP
.B Note:
In the syntactic descriptions below, metavariables such as
.I varname
refer to user-defined identifiers.  These identifiers consist
of an arbitrary-length string of characters.
These characters may be alphabetic (upper or lower case; case
is distinguished), numeric, or one of the special characters
'#', '$', '_', or '.'.  
.PP
A 
\s-2DDL\s+2
specification consists of one or more 
\s-2DDL\s+2 
statements, each terminated
by a semicolon.  The following statements exist:
.sp
.IP "VAR \fIvarname, varname\fR,..." 8
.sp
Declares each 
.I varname
as a new symbol.  The symbol
is defined as a constant with a value different from each
previously declared \fIvarname\fR.  \fIvarname\fR must not
be previously declared.
.PP
.B "Example:  "
VAR strength, intell, wisdom;
.sp
.IP "VERB \fIverbname, verbname\fR,..." 8
.sp
Declares each 
.I verbname 
as a new verb.  
.I verbname
must
not be previously assigned.
.PP
.B "Example:  "
VERB north, south, east, west;
.sp
.IP "ADJECTIVE \fIadjectivename, adjectivename\fR,..." 8
.sp
Creates a new adjective with name 
.I adjectivename,
which must not be previously assigned.
.PP
.B Example: 
ADJECTIVE red, green, blue;
.sp
.IP "NOUN \fInoun\fR [(\fIcontainer\fR)]" 8
.sp
Creates a new object named 
.I noun
whose
initial location is 
.I 
container.  noun 
.R
may not
be previously assigned; 
.I container
must be of
type NOUN.  If the "(\fIcontainer\fR)" clause is omitted,
the new object is placed in object .ALL .
The 
.I noun
may actually be a adjective-noun pair;
if so, the
.I adjective
must have been previously defined.
.PP
.B Examples: 
.DS
NOUN red book, blue book;
NOUN gem(red book);
.DE
.sp
.IP "ROUTINE \fIroutinename, routinename, ...\fR" 8
.sp
Declares that the \fIroutinename\fRs listed will be used
for Routines later in the program.  This is to allow \s-2DDL\s+2,
which is intended to be easily implementable, to deal with
recursive routines (which have not yet been declared at the
time of their definitions).  Only routines which are used
before being defined need to be declared with this statement.
.sp
.IP "ARTICLE \fIarticle, article,\fR..." 8
.sp
Creates each \fIarticle\fR as an article.  Articles are recognized
by the run-time parser, but are basically "noise" words.
.PP
.B Example: 
ARTICLE the;
.IP "PREP \fIprep, prep\fR,..." 8
.sp
Creates each 
.I prep
as a preposition.  Prepositions are basically
used by the parser to recognize the presence of
indirect objects in the Player's input.
However, a global named
.I Prep
contains the preposition typed by the player on each turn
(or zero if none).  The DDL program can thus distinguish
"put red book on shelf" from "put red book in shelf" if
it is so desired.
.PP
.B Example: 
PREP into,on,using,to,at;
.sp
.IP "\fInoun\fR (\fInumexp\fR) = \fIexp2\fR" 8
.sp
Property \fInumexp\fR of \fInoun\fR is set to the
value of \fIexp2\fR.
.I exp2
may be a number, a string, a routine name, or a new routine;
the numeric value or ID of
.I exp2
is always placed into the specified property.
.PP
.B Examples: 
.DS
gem(11)=1;  		{ 11 == Luminous }
gem(LDESC) = ($say "There is a bright gem here!");
gem(SDESC) = ($say "a bright gem");
gem(ACTION) = GemAction;  { Earlier-defined routine }
.DE
.sp
.IP "\fIverb\fR (PREACT | ACTION) = \fIroutine\fR" 8
.sp
Assigns \fIroutine\fR as the pre-object action or default action of
the given \fIverb\fR.  The routine may be a predefined routine name or
an actual routine.
.PP
.B Example: 
.DS
rub(ACTION) =	($say "Rubbing ")
		(($sdesc ($dobj)))
		($say " seems silly!");
.DE
.sp
.IP "\fIname\fR = \fInumber\fR" 8
.sp
Assigns \fIname\fR as equivalent to \fInumber\fR.  \fIname\fR
must not be previously assigned.
.PP
.B Example: 
OPEN=11; TRUE=1;
.sp
.IP "\fIname1\fR = \fIname2\fR" 8
.sp
Assigns 
.I name1
as a synonym for 
.I name2.
.PP
.B Example: 
n=north;s=south;se=southeast;
.sp
.IP "(\fInumexp\fR) = \fInumexp2\fR" 8
.sp
Assigns the global (or VAR) named by \fInumexp\fR to the value
given by \fInumexp2\fR.
.PP
.B Example: 
(Maxpt)=450;
.IP "\fIname\fR = "
"\fIstring\fR"
.sp
Assigns 
.I name
as equivalent to "\fIstring\fR".
Frequently, it is just
as easy to assign a routine to Say the given string
as it is to define that string separately.
However, there are other string functions, such as 
.I $eqst
and 
.I $substr, 
for which it may be useful to predefine strings.
.PP
.B Example: 
.br
err="Nothing happens.\en";
.br
MagicWord = "ShaZam";
.sp
.IP "\fIname\fR = \fIroutine\fR" 8
.sp
Assigns 
.I name
as equivalent to 
.I routine
.PP
.B Example: 
sayer=($say "Nothing happens.\en");
.IP "INCLUDE ""\fIfilename\fR""" 8
.sp
.B
(\s-2UNIX\s+2 implementation only)
.R
Causes input to be read from the named file.
.RE
.NH 
Routines
.PP
A routine is a list of one or more "forms".  Forms are of three types:
.RS
.NH 2
Conditional Forms
.IP "(\fIform1\fB : \fIform form\fR ... [: \fIelseform elseform\fR ...])" 8
.PP
If 
.I form1
evaluates to
nonzero, the subsequent \fIform\fRs are executed in
sequence.  Otherwise, the list of \fIelseform\fRs is executed in sequence.
Note that
the second colon, and the subsequent \fIelseform\fRs, are optional.
.PP
.B Example: 
.PP
.DS
(TRUE : ($say "Always do me") : ($say "Never do me"))
.DE
.NH 2
Simple Looping Forms
.IP "(WHILE \fIform1\fR : \fIform form ... \fR)" 8
.PP
If \fIform1\fR evaluates
to nonzero, the subsequent \fIform\fRs are evaluated
in sequence.  This process is repeated until such
a time as \fIform1\fR is found to evaluate to zero.
.PP
.B Example:
.PP
.DS
(WHILE ($eq ($loc .ME) JewlRoom) : (TRYmv .ME Prison))
.DE
.NH 2
Basic Function Forms
.IP "(\fIfunction arg1 arg2\fR ...)" 8
.PP
This is the basic function call (note that all builtin functions
begin with the character $).  The \fIfunction\fR is applied
to the \fIarg\fRs.  An argument may be a number,
string, declared name, or another form.  However, the function must
be a simple identifier, or a form which evaluates to a function
identifier (
.I 
e.g.
.R
($ldesc xxx)).
In addition, three special argument types are recognized:
.PP
An argument such as "@\fInumber\fR" is interpreted as 
"contents of global \fInumber\fR".
.PP
An argument such as "%\fInumber\fR" is interpreted as "the value of the 
\fInumber\fR
argument to this function".
.PP
An argument such as "[\fIadj noun\fR]" must be used if the programmer wishes to
refer to an object with an associated adjective.
.RE
.NH
Program Comments
.PP
Comments may be placed freely anywhere in a DDL program.  Comments
are surrounded by "curly braces" { like these }, but may NOT
be nested.  A single right brace will close any and all open
comments.  
.NH
The Parser
.PP
The \s-2DDL\s+2 run-time parser is the Player's only
interface to the world created by the \s-2DDL\s+2
programmer.  The parser recognizes four basic
forms of input "sentence":
.DS
	VERB  (e.g. "inventory")
	VERB DIRECT-OBJECT  (e.g. "take rock")
	VERB DIRECT-OBJECT PREP INDIRECT-OBJECT
		(e.g. "plant flower in vase")
	VERB INDIRECT-OBJECT DIRECT-OBJECT
		(e.g. "give the troll the red blanket"
		 or  "Turn the lamp off" where "off" is an object)
.DE
.PP
Either a direct or indirect object may consist of
a simple noun, an adjective-noun pair, or either
type of noun phrase preceded by an article.  Additionally,
a direct object may be a string delimited by double-quotes.
The parser attempts to resolve all ambiguous noun references,
and then sets up the globals,
.I Dobj,
.I Iobj,
.I Prep,
and
.I Verb
with the symbol-table values associated with the appropriate
verb or object.  For an object this is an index into the
Object Table; for a verb it is an index into the Verb table.
A string typed as a direct object will be stored as an index
into an internal temporary-string table, but its value
will be negated so that the programmer can detect that a
string has been typed, knowing that all strings (and only strings)
have a numeric value less than zero.
.PP
When a syntactically invalid line is typed, the parser
prints a (hopefully) helpful error message and accepts
new input.  A new turn is
.I not
begun.  A similar action is taken when a nonsense word
is typed.
.PP
Several commands may be typed on one line, separated by
commas.  However, this is considered as identical to
separating them by new-lines; they are dealt with on
separate turn cycles (and extra prompts may be printed).
.bp
.NH
Built-in Functions
.PP
The following functions are built-in functions available to the 
\s-2DDL\s+2
programmer.  These functions are the heart of the 
\s-2DDL\s+2 
system and are
the means whereby the 
\s-2DDL\s+2 
routines manipulate all system data.  Thus,
these functions completely describe the facilities of the 
\s-2DDL\s+2 
system.
.PP
The arguments to functions are here shown as "\fIobj\fR and
the like.  In fact, any function may take any value as an argument.
Mentioning the name of a symbol simply gives its symbol-table
value.  For an object, for example, this is its index in the object
table.  So, while it may be valid to say "($say window)", this will
only print the message whose message number happens to be the
same as the object index of the "window".  Note that the parser correctly
assigns such symbol-table values to the variables
.I
Dobj, Iobj, Prep,
.R
and
.I Verb.
.NH 2
Functions on objects
.IP "\fB$loc    \fR" 8
($loc \fIobj\fR) \(-> The container of \fIobj\fR.
.IP "\fB$cont   \fR" 8
($cont \fIobj\fR) \(-> First item contained in \fIobj\fR.
.IP "\fB$link   \fR" 8
($link \fIobj\fR) \(-> The next object in the same node as \fIobj\fR.
.IP "\fB$ldesc  \fR" 8
($ldesc \fIobj\fR) \(-> The routine ID for the long description of \fIobj\fR.
.IP "\fB$sdesc  \fR" 8
($sdesc \fIobj\fR) \(-> The routine ID for the short description of \fIobj\fR.
.IP "\fB$rtn    \fR" 8
($rtn \fIobj\fR) \(-> The ACTION routine for \fIobj\fR.
.IP "\fB$prop   \fR" 8
($prop \fIobj\fR \fIpropnum\fR) \(-> returns the value of the \fIpropnum\fR'th property
of \fIobj\fR.
.NH 2
Arithmetic Funcions
.IP "\fB$plus   \fR" 8
($plus \fIarg1\fR \fIarg2\fR) \(-> \fIarg1\fR+\fIarg2\fR
.IP "\fB$minus  \fR" 8
($minus \fIarg1\fR \fIarg2\fR) \(-> \fIarg1\fR\-\fIarg2\fR
.IP "\fB$times  \fR" 8
($times \fIarg1\fR \fIarg2\fR) \(-> \fIarg1\fR*\fIarg2\fR
.IP "\fB$quotient \fR" 8
($quotient \fInum den\fR) \(-> [\fInum\fR/\fIden\fR]
.IP "\fB$remainder \fR" 8
($remainder \fInum den\fR) \(-> \fInum\fB mod \fIden\fR
.IP "\fB$rand \fR" 8
($rand \fIarg\fR) \(-> Random integer between 1 and \fIarg\fR inclusive
.NH 2
Boolean Functions
.IP "\fB$and    \fR" 8
($and \fIa b\fR) \(-> \fIa\fB (bitwise AND) \fIb\fR
.IP "\fB$or     \fR" 8
($or \fIa b\fR) \(-> \fIa\fB (bitwise OR) \fIb\fR
.IP "\fB$not    \fR" 8
($not \fIx\fR) \(->  \s-2IF\s+2 \fIx\fR nonzero \s-2THEN\s+2 zero \s-2ELSE\s+2 one.
.IP "\fB$yorn   \fR" 8
($yorn) \(-> Waits for the Player to type a line of input.  Returns
one if the Player types "yes" or "y" and zero otherwise.
.IP "\fB$pct    \fR" 8
($pct \fIprob\fR) \(-> Returns one, \fIprob\fR% of the time, zero otherwise.
.IP "\fB$eq     \fR" 8
($eq \fIarg1\fR \fIarg2\fR) \(-> 1 if \fIarg1\fR equals \fIarg2\fR, zero otherwise.
.IP "\fB$ne     \fR" 8
($ne \fIarg1\fR \fIarg2\fR) \(-> IF \fIarg1\fR ~= \fIarg2\fR THEN  one  ELSE  zero.
.IP "\fB$lt     \fR" 8
($lt \fIarg1\fR \fIarg2\fR) \(-> 1 if \fIarg1\fR < \fIarg2\fR, zero otherwise.
.IP "\fB$gt     \fR" 8
($gt \fIarg1\fR \fIarg2\fR) \(-> 1 if \fIarg1\fR > \fIarg2\fR, zero otherwise.
.IP "\fB$le     \fR" 8
($le \fIarg1\fR \fIarg2\fR) \(-> 1 if \fIarg1\fR <= \fIarg2\fR, zero otherwise.
.IP "\fB$ge     \fR" 8
($ge \fIarg1\fR \fIarg2\fR) \(-> 1 if \fIarg1\fR >= \fIarg2\fR, zero otherwise.
.NH 2
Builtin Procedures (no return value)
.IP "\fB$setg    \fR" 8
($setg \fIglobalnumber value\fR) \(-> No return value.  Sets the
contents of global #\fIglobalnumber\fR to \fIvalue\fR.
.IP "\fB$setp   \fR" 8
($setp \fIobj propnum value\fR) \(-> No return value.  Sets the \fIpropnum\fR'th
property of \fIobj\fR to \fIvalue\fR.  Note that properties 1-16 may only contain 0 or 1.
.IP "\fB$move   \fR" 8
($move \fIobj dest\fR) \(-> No return value.  Causes \fIobj\fR to be placed
inside \fIdest\fR, and adjusts pointers accordingly.  \fBDanger\fR: No checking is
performed to verify that $move is not being used to violate the tree structure
of the object list (eg ($move obj obj)).
Bad results are likely if this occurs.
.IP "\fB$say    \fR" 8
($say \fImsg\fR) \(-> No return value.  Types \fImsg\fR.
.IP "\fB$name   \fR" 8
($name \fIobj\fR) \(-> No return value.  Types the (5-letter) name of \fIobj\fR.
.IP "\fB$num    \fR" 8
($num \fIx\fR) \(-> No return value.  Types the number \fIx\fR.
.IP "\fB$exit   \fR" 8
($exit \fIn\fR) \(-> Leave present routine.  ($exit 1) causes the current
"turn" to be prematurely terminated and the next turn to be initiated
at the Demon phase.  ($exit 0) returns to the driver to begin the next phase.
.IP "\fB$rtrn   \fR" 8
($rtrn \fIn\fR) \(-> Exits to the calling routine, returning value '\fIn\fR' TO
THE CALLING FUNCTION.
.IP "\fB$spec   \fR" 8
($spec \fIcode arg1 arg2 arg3 arg4\fR) \(-> Performs a special function as
follows:
.TS
center box;
c | c.
code	function
=
3	Terminate this run of DDL
_
4	Save a game
_
5	Restore a game
_
6	Fork a shell with arguments \fIarg1 \- arg4\fR
_
7	Preserve unknown words in file \fIarg1\fR
.TE
.PP
Functions 4 and 5 prompt for a file name in which the saved game is
kept.  
Function 6 is a \s-2UNIX\s+2-specific function.
Function 7 causes any unknown words encountered by the parser
to be preserved in a file for later perusal by the 
\s-2DDL\s+2 
programmer.  It
would be used to learn about things players have tried unsuccessfully
that should be dealt with.  The file must already exist, and must
be specified as a string.
.PP
ALL arguments must be specified, even if zero.
.NH 2
Global-value functions
.IP "\fB$glob   \fR" 8
($glob \fIn\fR) \(-> Value of global \fIn\fR.  Equivalent to @\fIn\fR.
.IP "\fB$verb   \fR" 8
($verb) \(-> The ID of the verb returned by the parser (zero if none).
Typically used in comparisons, it is equivalent to @Verb.
.IP "\fB$dobj   \fR" 8
($dobj) \(-> The ID of the direct object returned by the parser
(zero if none).  Equivalent to @Dobj.
.IP "\fB$iobj   \fR" 8
($dobj) \(-> The ID of the indirect object returned by the parser
(zero if none).  Equivalent to @Iobj.
.sp
.B Note:
There is no ($prep) corresponding to @Prep.
.NH 2
Transition Procedures
.IP "\fB$setv   \fR" 8
($setv \fIv1 v2 v3 v4 v5 v6 v7 v8 v9 v10\fR) \(-> sets the values in
the internal vector VECVERB to the values \fIv1\fR thru \fIv10\fR.  These are
used by routines $hit and $miss.
.IP "\fB$hit    \fR" 8
($hit \fImover d1 d2 d3 d4 d5 d6 d7 d8 d9 d10\fR) \(-> No return value.
Compares ($verb) with the values in builtin vector VECVERB.  When ($verb)
is found to match the nth entry in VECVERB, ($move \fImover d[n]\fR) is executed.
Note that \fImover\fR is what gets moved to d[n]; this argument is naturally
absent from $setv and $miss.
.IP "\fB$miss   \fR" 8
($miss \fIr1 r2 r3 r4 r5 r6 r7 r8 r9 r10\fR) \(-> no return value.
Compares ($verb) to VECVERB as $hit does.  When a match to the nth
entry in VECVERB is found, routine \fIr\fR[n] is called.  An attempt to
call routine 0 does nothing.
.NH 2
String Functions
.PP
There are two varieties of strings.  Constant strings defined
by the \s-2DDL\s+2 programmer are permanent, and have a numeric "value"
greater than zero (which is in fact a table index).  Strings
typed by the Player as a direct object, and strings produced
by the functions $subs and $read are temporary, have a numeric
"value" less than zero (which allows the programmer to determine
if the direct object is in fact a string), and are purged by
having their index values recycled at the beginning of every turn.
No more than 200 such strings may be generated on a given turn.
String functions (including
.B $say
) automatically understand both varieties of strings; the 
\s-2DDL\s+2 programmer should not attempt to un-negate
direct-object-type strings.
.IP "\fB$eqst\fR" 8
($eqst \fIarg1 arg2\fR) \(-> 1 iff the strings specified by the
two \fIarg\fRs are equal, zero otherwise.
.IP "\fB$subs\fR" 8
($subs \fIstr index length\fR) \(-> a string consisting of the
substring of \fIstr\fR, starting at character \fIindex\fR 
(with an origin of Zero for the beginning of the string), for
the specified \fIlength\fR.  A \fIlength\fR of zero causes
all the remaining characters starting at \fIindex\fR to be
taken.
.IP "\fB$leng\fR" 8
($leng \fIstr\fR) \(-> The length of string \fIstr\fR.
.IP "\fB$read\fR" 8
($read) \(-> Causes \s-2DDL\s+2 to pause and wait for input from
the Player.  Returns the string the player typed, without the
trailing newline.
.NH 2
Demons and Fuses
.IP "\fB$sdem   \fR" 8
($sdem n) \(-> Activates routine n as a Demon, to be executed every
turn.  At least one such Demon should exist, to Look at the Player's
current location, and to increment the turn counter
.IP "\fB$ddem   \fR" 8
($ddem n) \(-> Removes routine n from the active Demon list.  For
example, ($ddem Kount) undoes the action of ($sdem Kount).
.IP "\fB$sfus   \fR" 8
($sfus rout n) \(-> Causes routine "rout" to be executed (one
time only) after n turns.  Such a routine is called a Fuse.
.IP "\fB$dfus   \fR" 8
($dfus rout) \(-> Causes routine rout to be taken off the
pending fuse list.
.IP "\fB$itun   \fR" 8
($itun) \(-> Increments the turn counter.  This is a builtin function
because fuses depend upon the turn counter.  The 
\s-2DDL\s+2 
programmer has the
option to "slow time" by refraining from incrementing the turn counter.
.IP "\fB$gtun   \fR" 8
($gtun) \(-> Returns the current turn counter value.
