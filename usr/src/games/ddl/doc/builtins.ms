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
.NH 2
Functions on objects
.IP "\fB$loc    \fR" 8
($loc \fIobj\fR) \(-> The container of \fIobj\fR.
.IP "\fB$cont   \fR" 8
($cont \fIobj\fR) \(-> First item contained in \fIobj\fR.
.IP "\fB$link   \fR" 8
($link \fIobj\fR) \(-> The next object in the same node as \fIobj\fR.
.IP "\fB$ldisc  \fR" 8
($ldisc \fIobj\fR) \(-> The routine ID for the long description of \fIobj\fR.
.IP "\fB$sdisc  \fR" 8
($sdisc \fIobj\fR) \(-> The routine ID for the short description of \fIobj\fR.
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
($minus \fIarg1\fR \fIarg2\fR) \(-> \fIarg1\fR-\fIarg2\fR
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
7	Preserve unknown words in file \fIarg1\fR
.TE
.PP
Functions 4 and 5 prompt for a file name in which the saved game is
kept.  Function 7 causes any unknown words encountered by the parser
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
by the DDL programmer are permanent, and have a numeric "value"
greater than zero (which is in fact a table index).  Strings
typed by the Player as a direct object, and strings produced
by the functions $eqst and $read are temporary, have a numeric
"value" less than zero (which allows the programmer to determine
if the direct object is in fact a string), and are purged by
having their index values recycled at the beginning of every turn.
No more than 200 such strings may be generated on a given turn.
.IP "\fB$eqst\fR" 8
($eqst \fIarg1 arg2\fR) \)-> 1 iff the strings specified by the
two \fIarg\fRs are equal, zero otherwise.
.IP "\fB$subs\fR" 8
($subs \fIstr index length\fR) \)-> a string consisting of the
substring of \fIstr\fR, starting at character \fIindex\fR 
(with an origin of Zero for the beginning of the string), for
the specified \fIlength\fR.  A \fIlength\fR of zero causes
all the remaining characters starting at \fIindex\fR to be
taken.
.IP "\fB$leng\fR" 8
($leng \fIstr\fR) \)-> The length of string \fIstr\fR.
.IP "\fB$read\fR" 8
($read) \)-> Causes \s-2DDL\s+2 to pause and wait for input from
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
