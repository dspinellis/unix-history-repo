.RS
.ds CF "\(co 1981 UCLA Computer Club
.TL
A Brief Description of UCLA
Dungeon Definition Language (DDL)
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
.ds CF "\(co 1981 UCLA Computer Club
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
Each of the Demon routines currently active is run in order
of activation.
Demon routines are specified and activated by the 
\s-2DDL\s+2
program by executing
the $sdem function.
.B Note: 
The normal action of Looking (executing description routines) which
one expects to occur on each turn must be coded by the 
\s-2DDL\s+2
programmer
as a Demon.
.IP "(2) Fuses: " 10
All active Fuse routines are checked to see if they
are to be executed on this turn.  Those Fuses which have thus "burned down"
are then executed (in reverse order of activation) and removed.
.IP "(3) Parse: " 10
The player types a line of input,
and an attempt is made to resolve that input into a Verb, an Indirect Object,
and a Direct Object, by means of attendant Prepositions, Articles,
and Adjectives.  Unambiguous abbreviations for words are recognized
by the parser.
If an input Noun is ambiguous (because of two objects distinguished by only
adjectives), 
\s-2DDL\s+2
routines called DWIMD and DWIMI are used to disambiguate
direct and indirect objects respectively.  DWIMD and DWIMI each return
nonzero if the direct or indirect object is "possibly the one he means"
(e.g. if it is in the room, etc)); only if exactly one such object
exists with the given Noun name can the parse complete successfuly.
any of the input components are found to be missing, the value zero is
assumed for that object (and no associated routines are executed).
.PP
If a syntax error or unknown word is detected, a hopefully informative
error message is printed.  In addition, unknown words encountered
in the input
may be saved in a file for perusal by the DDL programmer.
.PP
The direct object may be enclosed in double-quotes by the Player.
Such a direct object is returned as a String to the program.  Strings
may be detected by the program as having "numeric values" less than
zero.  Strings may be operated on with the $eqst, $subs, and $leng
functions, and the $say procedure.
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
For most specialized actions (like "rub lamp") the particular code
is frequently attached to the object.
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
If any of these routines terminates with an ($exit 1), the remainder of
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
a tree, the root node of which is labelled ".ALL".  A second special
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
though with a different adjective.
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
.IP "LDESC (23)" 6
The Routine ID of a "Long Description" routine
.IP "SDESC (24)" 6
The Routine ID of a "Short Description" routine
.IP "ACTION (25)" 6
The Routine ID of a "Action" routine, to be called if the Player
either attempts to do something with that object (specifies it as a
Direct or Indirect Object), or while inside that object.
.NH 2
Verbs.
.PP
The "commands" typed by the Player must name Verbs which have been
defined by the 
\s-2DDL\s+2
programmer.  Each Verb is associated with two Routine
ID's:
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
Carriage returns may be embedded in strings freely, or the sequence \\n
may be used to represent a carriage return at any point.
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
have the same Noun name (eg the "red book" and the "blue book").
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
Object, Direct Object, and Verb typed by the player.  The constants
Iobj, Dobj, and Verb are predefined by the compiler to refer to those
globals.
.NH
\s-2DDL\s+2
Programs
.PP
.B Note:
In the syntactic descriptions below, metavariables such as
.I varname
refer to user-defined identifiers.  These identifiers consist
of a string of alphameric characters of arbitrary length.
A 
\s-2DDL\s+2
specification consists of one or more 
\s-2DDL\s+2 
statements, each terminated
by a semicolon.  The following statements exist:
.sp
.IP "VAR \fIvarname, varname\fR,..." 8
.PP
Declares each 
.I varname
as a new symbol.  The symbol
is defined as a constant with a value different from each
previously declared <varname>.  <varname> must not
be previously declared.
.PP
.B "Example:  "
VAR strength, intell, wisdom;
.sp
.IP "VERB \fIverbname, verbname\fR,..." 8
.PP
Declares each 
.I verbname 
as a new verb.  
.I verbname
must
not be previously assigned.
.PP
.B "Example:  "
VERB north,south,east,west;
.sp
.IP "ADJEC \fIadjectivename, adjectivename\fR,..." 8
.PP
Creates a new adjective with name 
.I adjectivename,
which must not be previously assigned.
.PP
.B Example: 
ADJEC red,green,blue;
.sp
.IP "NOUN \fInoun\fR[(\fIcontainer\fR)]" 8
.PP
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
type NOUN.  If the (\fIcontainer\fR) clause is omitted,
the new object is placed in object .ALL .
the 
.I noun
may actually be a adjective-noun pair.
.PP
.B Examples: 
.DS
NOUN red book, blue book;
NOUN worm(red book);
.DE
.sp
.IP "ROUTINE \fIroutinename, routinename, ...\fR" 8
Declares that the \fIroutinename\fRs listed will be used
for Routines later in the program.  This is to allow \s-2DDL\s+2,
which is intended to be easily implementable, to deal with
recursive routines (which have not yet been declared at the
time of their definitions).  Only routines which are used
before being defined need to be declared with this statement.
.sp
.IP "ARTICLE \fIarticle, article,\fR..." 8
.PP
Creates each \fIarticle\fR as an article.  Articles are recognized
by the run-time parser, but are basically "noise" words.
.PP
.B Example: 
ARTICLE the;
.IP "PREP \fIprep, prep\fR,..." 8
.PP
Creates each 
.I prep
as a preposition.  Prepositions are basically
noise words, but are used by the parser to recognize the presence of
indirect objects in the Player's input.
.PP
.B Example: 
PREP into,on,using,to,at;
.sp
.IP "\fInoun\fR (\fInumexp\fR) = \fIexp2\fR" 8
.PP
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
gem(11)=0;  		{ 11 == Luminous }
gem(LDESC) = ($say "There is a bright gem here!");
gem(SDESC) = ($say "a bright gem");
gem(ACTION) = GmAct;
.DE
.sp
.IP "\fIverb\fR (PREACT | ACTION) = \fIroutine\fR" 8
.PP
Assigns \fIroutine\fR as the pre-object action or default action of
the given \fIverb\fR.  The routine may be a predefined routine name or
an actual routine.
.PP
.B Example: 
.DS
rub(ACTION) =	($say "Rubbing ")
		($sdisc ($dobj))
		($say " seems silly.\\n");
.DE
.sp
.IP "\fIname\fR = \fInumber\fR" 8
.PP
Assigns \fIname\fR as equivalent to \fInumber\fR.  \fIname\fR
must not be previously assigned.
.PP
.B Example: 
OPEN=11; TRUE=1;
.IP "\fIname1\fR = \fIname2\fR" 8
.PP
Assigns 
.I name1
as a synonym for 
.I name2.
.PP
.B Example: 
n=north;s=south;se=southeast;
.IP "(\fInumexp\fR) = \fInumexp2\fR" 8
.PP
Assigns the global (or VAR) named by \fInumexp\fR to the value
given by \fInumexp2\fR.
.PP
.B Example: 
(Maxpt)=450;
.IP "\fIname\fR = "
"\fIstring\fR"
.PP
Assigns 
.I name
as equivalent to "\fIstring\fR".
.B Note:
This seems to be rarely, if ever, used.  Usually it's just
as easy to assign a routine to Say the given string.
However, there are other string functions, such as $eqst
and $substr, for which it may be useful to predefine strings.
.PP
.B Example: 
err="Nothing happens.\\n";
MagicWord = "ShaZam";
.IP "\fIname\fR = \fIroutine\fR" 8
.PP
Assigns 
.I name
as equivalent to 
.I routine
.PP
.B Example: 
sayer=($say "Nothing happens.\\n");
.IP "INCLUDE ""\fIfilename\fR""" 8
.PP
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
.IP "(\fIform1\fB : \fIform, form\fR ... [: \fIelseform, elseform\fR ...])" 8
.PP
Conditional expression.  If 
.I form1
evaluates to
nonzero, the subsequent \fIform\fRs are executed in
sequence.  Otherwise, the list of \fIelseform\fRs is executed in sequence.
.B Note:
The second colon, and the subsequent \fIelseform\fRs, are optional.
.PP
.B Example: 
.PP
.DS
(TRUE : ($say "Always do me") : ($say "Never do me"))
.DE
.IP "(WHILE \fIform1\fR : \fIform, form ... \fR)" 8
.PP
Simple looping construct.  If \fIform1\fR evaluates
to nonzero, the subsequent \fIform\fRs are evaluated
in sequence.  This process is repeated until such
a time as \fIform1\fR is found to evaluate to zero.
.PP
.B Example:
.PP
.DS
(WHILE ($eq ($loc .ME) JewlRoom) : (TRYmv .ME Prison))
.DE
.IP "(\fIfunction arg1 arg2\fR ...)" 8
.PP
Function call (note that all builtin functions
begin with the character $).  The \fIfunction\fR is applied
to the \fIarg\fRs.  An argument may be a number,
string, declared name, or another form.  However, the function must
be a simple identifier, or a form which evaluates to a function
identifier (
.I 
e.g.
.R
($ldisc xxx)).
In addition, three special argument types are recognized:
.PP
An argument such as "@\fInumber\fR" is interpreted as 
"contents of global \fInumber\fR".
.PP
An argument such as "%\fInumber\R" is interpreted as "the value of the \fInumber\R
argument to this function".
.PP
An argument such as "[\fIadj noun\fR]" must be used if the programmer wishes to
refer to an object with an associated adjective.
.RE
.PP
.B Examples:
.DS
.SM
VERB north,south,east,west,ne,nw,se,sw,up,down;
n=north; s=south; e=east; w=west; u=up; d=down;

NOUN rm001,rm002,rm003,rm004,rm005,rm006;
NOUN .ME(rm001);

ADJECTIVE red,blue;
NOUN red ball(rm002),blue ball(rm003);

red ball(LDESC) = ($say "There is a red ball here.");
red ball(SDESC) = ($say "Red ball.");

VAR score;
(score) = 0;

TAKBT = 16;
TRUE = 1; FALSE=0;
red ball(TAKBT) = TRUE;

ROUTINE takeR;			{ Declared later }

VERB take;
take(ACTION) = 	( ($and ($prop ($dobj) TAKBT)
	 	  	($eq ($loc .ME)($loc ($dobj)))):
				(takeR ($dobj))
		);
takeR = 	($move %1 .ME)
		(($eq %1 [red ball]):
			($say "The ball is glowing!")
			($setg score ($plus 10 @score)));
.NL
.DE
