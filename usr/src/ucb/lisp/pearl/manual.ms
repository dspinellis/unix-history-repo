.ND
.nr LL 75n
.nr LT 80n
.rm CF
.ds LH PEARL Documentation
.rm CH
.ds RH Page %
.po 1.00i
.ls 1
.hy 14
.RP
.TL
.LG
.LG
Using the PEARL AI Package
.sp 1
.SM
\fR(\fIP\fRackage for \fIE\fRfficient \fIA\fRccess to \fIR\fRepresentations in \fIL\fRisp)*
.NL
.FS
* This research was sponsored in part by the Office of Naval Research
under contract N00014-80-C-0732 and the National Science Foundation
under grant MCS79-06543.
.FE
.AU
Michael Deering
Joseph Faletti
Robert Wilensky
.AI
Computer Science Division
Department of EECS
University of California, Berkeley
Berkeley, California 94720
.sp 1
February 1982
.AB
This document is a tutorial and manual for PEARL
(Package for Efficient Access to Representations in Lisp),
an AI language developed with space and time efficiencies in mind.
PEARL provides a set of functions for creating hierarchically-defined
slot-filler representations and for efficiently and flexibly inserting
and fetching them from a forest of associative data bases.
In addition to providing the usual facilities such as demons and matching,
PEARL introduces stronger typing on slots and user-assisted hashing
mechanisms.
.AE
.NH 0
Introduction
.PP
PEARL (Package for Efficient Access to Representations in Lisp) is
a set of functions for creating hierarchically-defined slot-filler
representations and for efficiently and flexibly inserting and fetching
them from a forest of data bases.
Its intended use is in AI programming and it has been used at Berkeley
in the development of several AI programs including PAM [7] and
PANDORA [8].
.PP
PEARL has the expressive power found in other AI knowledge
representation languages, but is extremely time-space efficient.
For example, using a data base of 4000 entries, PEARL takes only
about 4.2 CPU milliseconds for an average unsuccessful query and
7.3 CPU milliseconds of an average successful query on a PDP-10.
.PP
This document describes PEARL's use and is intended for the
beginning user.
(A description of the implementation of PEARL will be available
shortly.)
The best way to approach PEARL is to read this document up through
section 11 and then to take it to a terminal and reread it, typing
in the examples and observing their effects.
.PP
PEARL was implemented by Michael Deering and Joseph Faletti.
It was originally developed on a DEC PDP-10 under UCI Lisp
and was subsequently moved to a DEC VAX 11/780 under Franz Lisp
with help from Douglas Lanam and Margaret Butler.
Both PEARL and its documentation are still
being developed, improved, and debugged.
Any comments or suggestions will be appreciated.
Send them to Joe Faletti via Arpanet or Unix mail
(Kim\fB.\fRFaletti\fB@\fRBerkeley or ucbvax\fB!\fRkim\fB.\fRfaletti).
.bp
.DS
.sp 5
.DE
.NH
Running PEARL
.PP
PEARL is implemented as a set of functions compiled and
loaded into Lisp.
Thus the full power of Lisp is available in addition to the added
power of PEARL.
.PP
Since PEARL runs under two different Lisps on two different machines,
there are a few differences between versions.
Most of these differences are in the method of starting PEARL up
and in the names of external files accessed by PEARL.
The two parts of this section describe how to start up PEARL either
under Franz Lisp or under UCI Lisp.
You need only read the section which is applicable to your Lisp.
.NH 2
Under Franz Lisp
.PP
To access PEARL, simply run the core version of Lisp
containing PEARL. On Berkeley Unix, this is available by typing
the command:
.DS
  %  ~bair/bin/pearl
.DE
or, if ~bair/bin is in your search path, simply:
.DS
  %  pearl
.DE
During the startup process, PEARL will read in two
files, \fB.init.prl\fR and \fB.start.prl\fR, if they exist.
These files are designed for purposes similar to those
of \fI.lisprc\fR.
However, they split these functions into two groups.
In your \fI.init.prl\fR file you should include any
expressions which change the user-settable parameters to PEARL.
(For example, methods for setting the size of data bases,
the print function, and the prompt are described below.)
.PP
When you wish to have other files read in at startup time,
this usually needs to be done after PEARL's parameters are set.
PEARL is set up so that after the reading of \fI.init.prl\fR, it sets
any necessary parameters which you have not set in your .init.prl
and then reads in the file \fI.start.prl\fR if you have one.
This is where any processing which requires the
attention of PEARL (such as the existence of its data bases)
should be placed.
Thus \fI.init.prl\fR is primarily for initializing PEARL
and \fI.start.prl\fR is for starting up your use of PEARL.
\fBNote:\fR unlike most Unix programs which look for startup files
only in your home directory, thereby limitting you to only one
environment for each program, PEARL looks for each file first in
the current directory and if there is none, then it looks in your
home directory.
This allows you to tailor invocations of PEARL to the kind of work
you do in a particular directory.
.bp
.PP
After reading in these two files, PEARL will then place you in a
modified prompt-read-eval-print loop, with a default prompt of "PEARL>  ".
This can be changed by setting the special variable
\fB*pearlprompt*\fR to the desired value.
If you want the standard Lisp prompt "->  " to be used by PEARL,
you must set \fI*pearlprompt*\fR to \fInil\fR in your \fI.init.prl\fR
and PEARL will do the right thing.
.PP
The primary feature of the PEARL prompt-read-eval-print loop is that
it uses a different print function.
The default function is
.DS
(lambda (*printval*)
        (valprint  *printval*  4) )
.DE
but this can be changed to whatever you desire by giving 
a new function definition to \fBpearlprintfn\fR.
The PEARL prompt-read-eval-print loop also contains a number of
features to improve upon the standard Lisp top level.
These include a history mechanism and are described in chapter 25.
.PP
There are quite a few functions from UCI Lisp which have been added
to PEARL to make it easier to move programs to Franz Lisp.
A list of these with brief documentation of differences is
included in an appendix.
.NH 2
Under UCI Lisp
.PP
To access PEARL, simply run the core version of Lisp containing PEARL. 
On the Berkeley KL-10 system, this is available by typing the system call
.DS
RU  PEARL[5500,504,PEARL]
.DE
During the startup process, PEARL will read in two files,
INIT.PRL and START.PRL, if they exist.
The file INIT.PRL is designed for purposes similar to those
of INIT.LSP.
In this file you should include any expressions which
change the user-settable parameters to PEARL.
(For example, methods for setting the size of data bases,
the print function, and the prompt are described below.)
If you wish to use the REALLOC function to
enlarge your memory space, this call should be the
last call in your INIT.PRL file.
.PP
When you wish to have other files read in at startup
time, this usually needs to be done after the REALLOC.
The common kludge with UCI Lisp to solve this is to define
an INITFN (initialization function) which does this and then
to reset the INITFN to \fInil\fR which returns you to the
standard Lisp prompt-read-eval-print loop.
However, PEARL sets the INITFN for its own purposes so
that this common "solution" will not work.
Instead, PEARL is set up so that after the reading of INIT.PRL,
it sets any necessary parameters which you have not set in your
INIT.PRL and then reads in the file START.PRL if you have one.
This is where any processing which requires the
attention of PEARL should be placed.
Thus INIT.PRL is primarily for initializing PEARL and
START.PRL is for starting up your use of PEARL.
.PP
After reading in these two files, PEARL will then place you in a
modified prompt-read-eval-print loop, with a default prompt of "PEARL>  ".
The ">" portion is the (modified) Lisp prompt which is printed
whenever \fIread\fR is invoked and can be changed
with the UCI Lisp function INITPROMPT.
The "PEARL" is PEARL's addition and can be set by
setting the special variable \fB*pearlprompt*\fR
to the desired value.
If you do not want any prompt added by PEARL other than the Lisp
prompt you must set \fI*pearlprompt*\fR to \fInil\fR in your
INIT.PRL and PEARL will do the right thing.
.PP
The main feature of the PEARL prompt-read-eval-print loop is
that it uses a different print function.
The default function is
.DS
(lambda (*printval*)
        (valprint  *printval*  4) )
.DE
but this can be changed to whatever you desire by giving the
function \fBpearlprintfn\fR a new definition.
Note that \fIdskin\fR and the break package have been
changed slightly to also use of this print function.
Also, although the functions names and examples below are in
lower case, PEARL in UCI Lisp expects them all in upper
case, just as the rest of the UCI Lisp functions.
.NH
Creating Simple Objects.
.PP
PEARL allows four basic types of objects.
The first two are integers and arbitrary Lisp objects
and are created in the usual Lisp fashion.
The second two are structured types provided by PEARL,
and are stored in an internal form as blocks of memory.
These latter types are called \fBsymbols\fR and \fBstructures\fR.
.NH 2
Defining Symbols
.PP
\fBSymbol\fRs are PEARL's internal atomic symbols.
Semantically they are like Lisp atoms, but are represented
and used differently to make PEARL more efficient.
Before they are used, symbols must
be declared (and thus defined to PEARL) by a call to the function
\fBsymbol\fR, which takes as arguments any number of atoms
whose names will be used to create symbols.
For example,
.DS
(symbol  John)
.DE
creates one symbol called John and
.DS
(symbol  Bob  Carol  Ted  Alice  Home
         Office  School  Healthy  NewYork)
.DE
creates several symbols at one time.
\fISymbol\fR is an nlambda (fexpr) and returns
a list containing the names of the symbols it created.
A one-argument lambda (expr) version is available as \fBsymbole\fR.
.PP
There are two ways to get at the actual (unique) symbol:
you can use the function \fBgetsymbol\fR or you can evaluate the
atom whose name is built out of the symbol name with the characters
\fBs:\fR on the front.
The function \fBsymatom\fR will build this atom for you when
given a symbol name.
For example, to set B to the symbol Bob use any of:
.DS
(setq  B  (getsymbol  'Bob) )
(setq  B  s:Bob)
(setq  B  (eval (symatom 'Bob))
.DE
.LP
Given an internal symbol, you can find out its print name by passing
it to the function \fBpname\fR (which also will return the print name
of other types of PEARL objects).
.NH 2
Defining Structures
.PP
\fBStructure\fRs in PEARL provide the ability to define and manipulate
logical groupings of heterogeneous data and are essentially objects
with slots and slot fillers.
As such, they act more like "records"
in Pascal or "structures" in C than Lisp lists.
In reality they are more than both, but for the moment the reader
should keep records in mind.
.PP
Just as you must define the form
of a record in Pascal before defining the value of a variable whose
type is that kind of record, it is necessary to define each particular
form of structure you wish to use in PEARL before creating an
object with that form.
PEARL provides one function called \fBcreate\fR which
is used both to define kinds of structures and to
create individual instances of these structures.
(One function is provided for both because a special individual
is created as a side effect of each definition.
More on this is provided in section 7 on defaults.)
The first argument to \fIcreate\fR distinguishes
between a call which defines and one which creates an individual.
There are three kinds of defining calls (\fIbase\fR, \fIexpanded\fR
and \fIfunction\fR) and two kinds of instance-creating calls
(\fIindividual\fR, \fIpattern\fR) to \fIcreate\fR.
Only one of each (\fIbase\fR and \fIindividual\fR) is described
in this section.
The rest are left for later.
.PP
To start off with an example, let us suppose that you wish to represent
the conceptual act "PTrans" from the Conceptual Dependency (CD) notation
of Schank.
(The examples in this documentation assume a passing
familiarity with CD but lack of this should not hurt you too badly
and PEARL itself does not restrict you in any way to CD.
PTrans stands for Physical Transfer which has four "cases": actor doing
the transfer, object being transferred, original location and final
location.)
First we must define the form which PTrans structures will take.
In C this would be a type definition for the type PTrans as
follows (assuming a system-provided definition of the type \fIsymbol\fR):
.DS
struct  PTrans {
        symbol  Actor;
        symbol  Object;
        symbol  From;
        symbol  To;
};
.DE
In Pascal this would be
.DS
type  PTrans  =  record
                     Actor  :  symbol;
		     Object  :  symbol;
	             From  :  symbol;
		     To  :  symbol
                 end;
.DE
.LP
In PEARL,
.DS
(create base  PTrans
	(Actor  symbol)
	(Object  symbol)
	(From  symbol)
	(To  symbol) )
.DE
does the job.
Note first of all that in order to define a new form
of structure, the first argument to \fIcreate\fR must be \fBbase\fR.
Note also that the second argument to \fIcreate\fR is the name of the
structure form to be created.
Following this is a list of (<slotname> <type>) pairs.
Structures are currently allowed to have up to 32 slots
in Franz PEARL or 18 in UCI Lisp PEARL as long as all slots
within a particular structure have mutually distinct names.
Different structures may have slots of the same name.
Thus in applications of PEARL to CD twenty different structure
types might all have an Actor slot.
.PP
Five types are allowed for slots: \fBsymbol\fR, \fBstruct\fR,
\fBint\fR, \fBlisp\fR, and \fBsetof <type>\fR.
\fISymbol\fR and \fIstruct\fR are the types just described.
\fIInt\fR is a normal Lisp integer value.
The type \fIlisp\fR allows arbitrary \fBnon-atomic\fR Lisp values.
Finally, \fIsetof <type>\fR allows you to define sets consisting
of all symbols (\fIsetof symbol\fR) or all structures (\fIsetof struct\fR)
and can be done recursively (\fIsetof setof struct\fR).
.NH
Creating Individual Instances of Defined Structures
.PP
Once you have defined a specific form of structure like PTrans, you
can create an individual PTrans using \fBindividual\fR as the first
argument to \fIcreate\fR and the name of the base structure you want
an individual instance of as the second argument.
The rest of the arguments are (<slotname> <value>) pairs in which
the <value> must be of the type that the slot was declared to be.
The slots may be listed in any order and need not be in the same
order as defined.
For example, to create an instance of John going home
from the office (i.e., John PTransing himself from the office to
home) you would use this call to \fIcreate\fR:
.DS
(create individual  PTrans
	(Actor  John)
	(Object  John)
	(From  Office)
	(To  Home) )
.DE
\fICreate\fR will return an object of type PTrans, with the slots filled
in as indicated.
The object returned has been created and stored as
a \fIhunk\fR of memory in Franz Lisp or a block of memory in Binary
Program Space in the UCI Lisp (rather than Free Storage where most
Lisp objects are stored).
Since you are using the PEARL prompt-read-eval-print loop,
the object returned by \fIcreate\fR will be printed in an external list
form, something like the above.
However, if you print a structure using the standard Lisp print
functions (as for example some break packages will do), it will
be printed by Franz Lisp in the normal way it prints hunks.
(Warning: Since the structure actually contains a circular
reference to another hunk, this will cause problems with programs
which do not set \fIprinlevel\fR in Franz Lisp so general packages which
you wish to add to PEARL should be modified to use some PEARL
print function.)
With UCI Lisp's normal print function, it will show up as an
address in Binary Program Space, looking something like "#31534".
.PP
As with any Lisp function that returns an object,
we must store (a pointer to) the result of \fIcreate\fR somewhere
(for example, in the atom Trip)
if we wish to reference it in the future.
Otherwise, the created object will be inaccessible.
(This point is clearer if you consider
that Pascal would insist that you do something with the result
of the function call, although PEARL and many languages like Lisp
and C in which every subprogram is a value-returning function allow
you to construct a value and then blithely go on your way without
using it.)
.PP
To store (a pointer to) the instance returned by \fIcreate\fR in
the atom Trip, you could do the following:
.DS
(setq  Trip  (create individual  PTrans
		     (Actor  John)
		     (Object  John)
		     (From  Office)
		     (To  Home) ) )
.DE
Since this is a common operation, \fIcreate\fR provides the option of
having (a pointer to) the newly created instance automatically
assigned to a Lisp atom.
This is accomplished by including the
name of the atom as the third argument to \fIcreate\fR.
If the third argument to \fIcreate\fR is an atom rather than a
(<slotname> <value>) pair, \fIcreate\fR stores the new
object in this atom.
Thus the effect of the previous example can be achieved by:
.DS
(create individual  PTrans  Trip
	(Actor  John)
	(Object  John)
	(From  Office)
	(To  Home) )
.DE
(In addition, when \fIcreate base PTrans\fR is used, an assignment is
automatically made to the atom PTrans, thus making the defaultinstance
of a structure easily available.
To preserve this, calls to create of the form
\fI(create individual PTrans PTrans ...)\fR are disallowed (that
is, ignored).
In case you should actually wish to use the atom PTrans for other
purposes, evaluating the atom built by prepending \fBi:\fR onto
the structure name will give you the default instance of a base
structure and evaluating the atom built by prepending \fBd:\fR
will give you the actual definition.
Changing the value of these atoms is \fBvery dangerous\fR.
Given the name of a structure, the functions \fBinstatom\fR and
\fBdefatom\fR will construct these atoms for you.
For more information about the item assigned to \fIPTrans\fR
and \fIi:PTrans\fR, see the section 7 on defaults.)
.PP
PTrans is an example of a structure whose slots are all
of the type \fIsymbol\fR.
A more complex example is that of MTrans (Mental Transfer:
an actor transfering a concept (Mental Object) from one place
to another (usually from himself to someone else).
The MObject slot is some other act and so would be of
type \fIstruct\fR resulting in the following definition:
.DS
(create base  MTrans
	(Actor  symbol)
	(MObject  struct)
	(From  symbol)
	(To  symbol) )
.DE
A sample instance of MTrans is \fIJohn told Carol that he
was going home from the office\fR and would be created with
.DS
(create individual  MTrans  InformLeaving
	(Actor  John)
	(MObject  (PTrans  Leaving
		           (Actor  John)
		           (Object  John)
		           (From  Office)
		           (To  Home) ) )
	(From  John)
	(To  Carol) )
.DE
.LP
Note that to fill a slot of type \fIstruct\fR (or \fIsetof struct\fR)
with a structure value within a
\fIcreate\fR one just embeds the appropriate arguments for a recursive
call to \fIcreate\fR, \fIexcept\fR that you \fBmay\fR leave out
\fIindividual\fR since it would just be repetitive.
If you should want to create an object of another type within
an individual or base structure, you must include the alternative
argument (\fIindividual\fR, \fIbase\fR, \fIpattern\fR, \fIexpanded\fR,
or \fIfunction\fR) before the type name.
This is particularly useful when you wish to create a pattern
with an individual instance in one of its slots.
.PP
The optional third argument of an atom name for storing
in may be included at each level if you wish.
In the example above, \fIcreate\fR actually will create two
new instances, an MTrans which will be stored in InformLeaving,
and a PTrans which is pointed to by the MObject slot of the
MTrans and is also pointed to by Leaving.
In this case, neither InformLeaving nor Leaving is required.
If Leaving were left out, then one would still have a way
to get at the PTrans via the MObject slot of the MTrans that 
InformLeaving points to.
However, if InformLeaving were left out and the
result of the call to \fIcreate\fR were not stored any other way,
there is one more way that the MTrans would be accessible.
The value of the most recently created object is always
stored in the special variable \fB*lastcreated*\fR by
\fIcreate\fR so the value of the MTrans would remain
accessible until the next call to \fIcreate\fR.
Note that if there are recursive calls to \fIcreate\fR during this
time in order to process structure values in slots, the value of
\fI*lastcreated*\fR is continually changing to the most recent
one and the setting of \fI*lastcreated*\fR is the last thing
\fIcreate\fR does.
There is also a special variable called \fB*currenttopcreated*\fR
which is set by \fIcreate\fR at the top level call as soon as
the space for an individual or default instance is allocated.
Since it is sometimes handy for a piece of user code which
runs during \fIcreate\fR (see the sections on !, $, predicates and
demons) to be able to access the topmost object,
\fI*currenttopcreated*\fR is sometimes quite useful.
.PP
As in C and Pascal, one can embed to any level.
\fICreate\fR does not have facilities
for more complex networks of structures, as there are other
functions in PEARL which allow their construction.
\fICreate\fR is mainly used to create objects for other
functions to manipulate.
.NH
Accessing Slots of Structures
.PP
In C and Pascal one can access the slots of a record or structure by
using dot notation.
For example, in Pascal the To slot of the MObject slot of
the MTrans pointed to by InformLeaving would be accessed
with the expression InformLeaving.MObject.To (or perhaps
more accurately InformLeaving\fB^\fR.MObject\fB^\fR.To
since slots really contain pointers to objects).
In Pascal and C, there are essentially only two things that
one can do to a slot of a record or structure: access it
(get its value) and assign to it (give it a new value).
In PEARL the macro \fBpath\fR provides
a large number of ways to access and/or change the values
in slots of individual structures.
(In the UCI Lisp version this is called \fIppath\fR
to distinguish it from the system function \fIpath\fR.)
A call to \fIpath\fR is of the following general form:
.DS
(path  <Selector>  <Structure>  <Slot-Name-or-List>  <Value>)
.DE
<Selector> determines the action to be performed
and is not evaluated.
<Structure> should evaluate to the object in which the slot
occurs (or in whose depths the object occurs).
<Slot-Name-or-List> should evaluate either to the atom name of the
slot desired in <Structure> or a list of the slot names
which one must follow to get down to the slot.
<Value> (which is only needed when it makes sense) 
should evaluate to the value to be put into the slot
(or otherwise used in performing the function).
At this point, we will only describe the two <Selector>s
corresponding to accessing and assigning.
These are \fBget\fR and \fBput\fR respectively.
Thus to access the value of a slot, you would use
.DS
(path  get  <Structure>  <Slot-Name-or-List>)
.DE
(No value is needed;
the purpose of this call is to get the value.)
To assign a value to a slot, you would use
.DS
(path  put  <Structure>  <Slot-Name-or-List>  <Value>)
.DE
For example, to access the Actor slot of the PTrans in Trip, either of
the following is appropriate:
.DS
(path  get  Trip  'Actor)
(path  get  Trip  '(Actor) )
.DE
This is essentially equivalent to a reference to
\fITrip\fB^\fI.Actor\fR in Pascal.
.PP
To access a slot within a structure in a slot of type \fIstruct\fR, 
add the slot names to the <Slot-Name-or-List>, just as we access
embedded fields within fields in Pascal by adding more slots
to the accessing expression.
For example, to access the place
John told Carol he was going in our MTrans example above, we
want the To slot of the MObject slot of the MTrans stored in
InformLeaving:
.DS
(path  get  InformLeaving  '(MObject To) )
.DE
This is essentially equivalent to a reference to
\fIInformLeaving\fB^\fI.MObject\fB^\fI.To\fR in Pascal.
PEARL will check each slot reference, and will indicate if
a slot name is not found (perhaps due to a misspelling or an
unbound slot).
.PP
Similarly, to change the Actor of our PTrans in Trip to be Bob:
.DS
(path  put  Trip  '(Actor)   (getsymbol  'Bob) )
.DE
and to change the To slot within the MObject of the MTrans:
.DS
(path  put  InformLeaving  '(MObject  To)   (getsymbol  'School) )
.DE
which is essentially equivalent to assigning a value to
\fIInformLeaving\fB^\fI.MObject\fB^\fI.To\fR in Pascal.
Note that the order of the arguments to these functions is in
\fBnot like\fR the argument ordering of \fIputprop\fR.
.PP
\fBCAUTION\fR:
\fIPath\fR does not check values to ensure that they are of the
correct type before putting them in a slot.
Also, a change in a structure with \fIpath\fR
does not cause it to be reinserted in the data base
(see the next section).
Thus, before changing a structure, one should remove it from
the data base and then reinsert it after the change.
.PP
These functions were gathered under the macro \fIpath\fR because of
their similarity.
However, if you prefer to have the action being performed lead off the
function name in keeping with \fIputprop\fR, \fIget\fR, \fIputd\fR,
\fIgetd\fR, etc., these two functions are also available as
\fBputpath\fR and \fBgetpath\fR with similar names also provided for
all the other forms of path described below.
Thus the name "path" may be tacked onto the end of one of the action
selectors to \fIpath\fR but the rest of the arguments to these
functions remain the same.
.PP
There are quite a few other operations which are allowed through
\fIpath\fR which you will not need or understand until you have read
the rest of this documentation.
We describe them here for completeness but suggest you skip
to the next section during your first reading.
If you feel there is one missing, feel free to suggest it since
they are easy to add.
.IP
\fIpath \fBclear\fR or \fBclearpath\fR -- sets the selected path to
the standard default value for its type (\fInilsym\fR,
\fInilstruct\fR, zero or \fInil\fR).
Note that this is only the standard default and does
not inherit a default from above.
See section 7 for more on default values.
.IP
\fIpath \fBaddset\fR or \fBaddsetpath\fR -- add the specified value to
a slot of type \fIsetof\fR.
.IP
\fIpath \fBdelset\fR or \fBdelsetpath\fR -- delete the specified value
from a slot of type \fIsetof\fR.
.IP
\fIpath \fBgetpred\fR or \fBgetpredpath\fR -- get the list of
predicates on the slot.
.IP
\fIpath \fBaddpred\fR or \fBaddpredpath\fR -- add the specified
predicate to the predicates on the slot.
.IP
\fIpath \fBdelpred\fR or \fBdelpredpath\fR -- delete the specified
predicate from the predicates on the slot.
.IP
\fIpath \fBgethook\fR or \fBgethookpath\fR -- get the assoc-list of 
hooks (demons) on the slot.
.IP
\fIpath \fBapply\fR or \fBapplypath\fR -- arguments to this function
are a <Function-or-Lambda-Body>, followed by the <Structure>, and
<Slot-Name-or-List>.
The <Function-or-Lambda-Body> is applied to the value of the slot.
(In the UCI Lisp version, \fIapply#\fR is used so that macros will work.
In the Franz Lisp version, a PEARL-supplied version of \fIapply\fR
called \fBapply*\fR is used which also handles macros "right".)
.PP
(Skip this next paragraph until you have read about hashing and
the data bases.)
The method of processing the path in \fBpath\fR functions allows a
form of indirection through the data base that is sometimes
helpful when you use symbols in slots as unique pointers to
other structures.
Suppose you had the following definitions:
.DS
(create base  Person
	(* Identity  symbol)
	(   Name  lisp) )
.DE
.DS
(dbcreate individual  Person
	  (Identity  John)
	  (Name  (John  Zappa) )
.DE
and you want to ask \fI"what is the Name of the Person in the
Actor slot of Trip (above)"\fR which you might normally write as:
.DS
(getpath  (fetch  (create pattern  Person
			  (Identity  !  (getpath  Trip  'Actor) ) ) )
	   'Name)
.DE
This is very hard to understand.
A shorthand for this is the following:
.DS
(getpath  Trip  '(Actor  Person  Name) )
.DE
which behaves like this:  when \fIpath\fR gets to the symbol in
the Actor slot of Trip, it notices that there is still more path
to follow.
It then interprets the next symbol in the path as the
name of a type and does a quick equivalent of fetch of a Person
with its first slot set to John.
It then continues to follow the path specified in this new
structure, finishing up with the value of the Name slot
of the structure.
(Note that this depends on Person structures being hashed by the
relevant slot and will fail otherwise.
Also note that the tendency of most users of PEARL has been away
from the use of symbols as indirections to larger structures and
toward actually putting the larger structure in the slot.
In this case this would imply putting the Person structure in the
Actor slot of PTrans and eliminate the need for "Person" in the
path list.)
.NH
Storing In and Retrieving From the Data Base -- The Simplest Way
.PP
So far we have shown how to create structures and have treated
them pretty much like C structures or Pascal records.
However, PEARL's most important departures from these languages
involve its data base facilities.
PEARL's data base can be thought of as one large
bag into which any structure can be placed.
The data base can hold hundreds, even thousands of separate objects.
There are two basic operations that can be performed upon
the data base, inserting with the function \fIinsertdb\fR
and retrieving with a combination of the functions
\fIfetch\fR and \fInextitem\fR.
.NH 2
Storing In the Data Base: \fIInsertdb\fR and \fIRemovedb\fR
.PP
While the simplest forms of these actions are
relatively straightforward, the power and efficiency of PEARL
derives from the nuances and controls available with these
functions which take up much of the rest of this document.
Much of the power develops from knowledge provided by the user about
how each kind of structure is likely to be retrieved (and therefore
how it should be stored).
Thus, the data base benefits from knowing as much as possible
in advance about the objects that will be placed within it.
This information is provided by using a large variety
of extra flags during definition calls to \fIcreate\fR.
It is used by \fIinsertdb\fR to hash objects into a specific
\fIhash bucket\fR in the data base, by \fIfetch\fR to retrieve the
correct hash bucket from the data base, and by \fInextitem\fR
to filter the desired objects from this bucket.
.PP
PEARL allows the construction and use of multiple data bases which are
described in detail later.
Without exerting any effort, a data base is automatically created
called \fB*maindb*\fR and pointed to by the special variable \fB*db*\fR.
In general, all PEARL functions which deal with a data base have an
optional last argument which specifies which data base to use.
If it is missing, then the default data base pointed to by
\fI*db*\fR is assumed.
Thus you can change the default data base
simply by assigning the desired data base to \fI*db*\fR.
For simplicity, this optional data base argument is not
mentioned in the following discussion.
.PP
The function \fBinsertdb\fR takes a single structure argument and
inserts it into the data base.
In its simplest form \fIinsertdb\fR requires no user flags on the
definitions of structures.
In this case, \fIinsertdb\fR simply hashes on the type of the
structure regardless of its specific contents so that each
entry ends up in a bucket with all other entries of that type.
For example, to insert into the data base the PTrans which was
saved in the Lisp variable Trip above, you simply provide it as an
argument to \fIinsertdb\fR:
.DS
(insertdb  Trip)
.DE
We could also put the PTrans (saved in Leaving whose To slot
was changed to School) which was the MObject of the MTrans above
in the data base with:
.DS
(insertdb  Leaving)
.DE
Since no information has been provided by the user about how to
efficiently distinguish PTranses in general, these two will be stored
in the same bucket (as will all PTranses).
When inserting an item into a bucket, \fIinsertdb\fR will check
to ensure that this specific item is not already in that bucket
(using \fIeq\fR) and will only insert it if
it is not already there, thus avoiding duplicates.
.PP
The function \fBremovedb\fR takes a single structure argument
and removes it from any place in the data base where it has been
put using \fIeq\fR to determine equality.
.PP
Since one often wants to create an individual and then insert it into
the data base, there is a macro \fBdbcreate\fR provided whose
arguments are precisely like \fIcreate\fR.
Thus, \fI(dbcreate individual PTrans ....)\fR expands into
\fI(insertdb (create individual PTrans ....) )\fR.
.NH 2
Retrieving Hash Buckets From the Data Base: Fetch
.PP
.hy next-item
The simplest case of fetching from the data base is
equivalent to asking if a particular, completely defined
object is in the data base.
This is performed by a combination of the functions 
fIfetch\fR and \fInextitem\fR.
The first step is to retrieve the hash bucket(s) for the object.
For example, to determine whether the object stored in Trip is in the
data base, the first step is to call the function \fBfetch\fR and
store what it returns (the form of what is returned is described
below):
.DS
(setq  PotentialTrips  (fetch  Trip) )
.DE
.PP
The function \fIfetch\fR takes a single structure argument which is
called the \fBpattern\fR.
What \fIfetch\fR returns includes this pattern and the hash bucket(s)
from the data base which contain those structures which are
most likely to "match".
The rules of "matching" are fairly complex and are described in
detail in section 20, but for the moment it is enough to know that
two structures match if their respective slots contain equal values.
Thus matching is closer to Lisp's \fIequal\fR than to \fIeq\fR.
.NH 2
Accessing the Results of a Fetch: Nextitem.
.PP
Conceptually, what \fIfetch\fR returns is a restricted type of \fBstream\fR.
A stream is a "virtual" list, whose items are computed only as needed.
When a fetch from the data base is performed, the pattern provided
is only used to construct a stream containing that pattern and the
appropriate hash bucket from the data base;
no matching (comparing)
between the pattern and objects in the data base occurs.
Thus the stream contains pointers to all data base items in the
same hash bucket, regardless of their likelihood of matching the pattern.
Therefore, the \fIstream\fR or "virtual list" returned by \fIfetch\fR is
in fact bigger than the list of actual items which match.
(For this reason, the default PEARL print function only prints how
many potential items are in the stream.)
.PP
For our purposes, you should regard the object that \fIfetch\fR
returns to be a funny sort of black box, whose only use is as
an argument to the function \fBnextitem\fR.
\fINextitem\fR will compute the next element to be removed from the stream.
When elements are extracted from the stream with the function \fInextitem\fR,
the pattern is "matched" against successive items from the hash bucket
until one matches (and is returned) or until the potential items run out
(and \fInil\fR is returned).
.PP
\fINextitem\fR is very much like the function \fIpop\fR in Lisp because it
updates the status of the stream to reflect the
extraction of the "topmost element" similar to the way \fIpop\fR replaces
its list argument with its \fIcdr\fR.
\fINextitem\fR does this by destructively modifying the stream
(but not the hash bucket);
once the top item
has come off it is no longer a part of the stream.
Like \fIpop\fR, \fInextitem\fR returns \fInil\fR if the stream is empty.
.PP
A stream, as returned by \fIfetch\fR in PotentialTrips,
will \fBnever\fR be \fInil\fR but instead will be a list.
In all cases, the first element will be the atom \fB*stream*\fR.
In most cases, the second element (\fIcadr\fR) is the pattern (object
being fetched) and the rest (\fIcddr\fR) is
the hash bucket that the object hashes to.
However, it is entirely possible for the hash bucket to either
fail to contain any instances of the object, or to contain
multiple instances of the object.
The form that is printed by PEARL's default print function is:
the atom \fB*stream:*\fR, the object being fetched,
and the number of potential items in the stream,
avoiding the prining of a lengthy result.
(If the pattern is actually a function structure, then the atom
used is \fB*function-stream:*\fR.)
.PP
Thus, to actually determine whether the object in Trip is in the data
base, it is necessary to ask for the \fInextitem\fR in the bucket of
the stream PotentialTrips (that is, in the \fIcddr\fR)
which matches the object being fetched (that is, the \fIcadr\fR
of PotentialTrips):
.DS
(setq  FirstResult  (nextitem  PotentialTrips) )
(setq  SecondResult  (nextitem  PotentialTrips) )
.DE
If nothing matching Trip occurred in the data base, FirstResult would
contain \fInil\fR;
otherwise, it would contain an object in the data base
which matches Trip.
If you have typed in all the examples we have shown you above,
then FirstResult will contain the same value as Trip.
SecondResult will be \fInil\fR.  (The only other object in the same
bucket is the value of Leaving, but that does not match because
its To slot contains School after the \fIpath put\fR above.)
If the two structures in Trip and Leaving both contained
the same slot fillers, they would both match Trip and each
would be returned by \fInextitem\fR on successive calls.
.PP
This is essentially the only type of fetching that is
useful with the information presented so far,
but more powerful types will be described shortly.
.PP
Since the functions \fIcreate\fR, \fIfetch\fR, and \fInextitem\fR
are often used in combination, several macros combining them are
provided by PEARL:
.IP
When you wish to create a pattern only long enough to use it as an
argument to \fIfetch\fR, you can use the macro \fBfetchcreate\fR
which is defined in such a way that \fI(fetchcreate blah)\fR is
equivalent to \fI(fetch (create blah) )\fR ).
.IP
If you want to do a \fIfetchcreate\fR in a function definition and
you wish the pattern to be created only once but used each time
this function is called (a potential savings in space and time),
the macro \fBinlinefetchcreate\fR will call \fIcreate\fR when it
expands and then expand to a call to fetch with this pattern.
.IP
If you want to do a \fIcreate\fR in a function definition and
you wish the object to be created only once,
the macro \fBinlinecreate\fR will call \fIcreate\fR when it
expands and effectively replace itself with the result.
.IP
When you wish to fetch but only need the resulting stream long
enough to use it as an argument to \fInextitem\fR, you can use
the macro \fBfirstfetch\fR which is defined in such a way that
\fI(firstfetch blah)\fR is equivalent to \fI(nextitem (fetch blah) )\fR ).
.IP
If your only goal in fetching some fully-specified object is to
test for its existence in the data base, the function \fBindb\fR
which expects a single structure argument will return \fInil\fR
if it is not there, and non-\fInil\fR if it is.
(Note that \fIindb\fR uses \fIstrequal\fR rather than \fImatch\fR.)
.IP
It is sometimes convenient to have a list of all the items which would be
returned by successive calls to \fInextitem\fR on a stream.
The function \fBstreamtolist\fR expects a stream argument and
returns a list of the items which the stream would produce.
.NH
The Default Values for Unspecified Slots
.PP
When creating an instance of a given type, one may not always
wish to fill in all the slots of the structure, either because
the slot value is unknown or immaterial.
PEARL has a mechanism for filling unfilled slots with default values.
The simplest form of defaulting involves two
predefined objects, \fBnilsym\fR and \fBnilstruct\fR.
\fINilsym\fR is a \fIsymbol\fR, and roughly corresponds to Lisp's
\fInil\fR when \fInil\fR is viewed as an atom.
\fINilstruct\fR is a structure without any slots,
and corresponds to a null structure.
In the absence of a specified value, PEARL will fill in slots
of an individual instance of a structure being created
with \fInilsym\fR if the slot type is \fIsymbol\fR,
\fInilstruct\fR if the slot type is \fIstruct\fR, zero if the slot
is of type \fIint\fR, and Lisp \fInil\fR
if the slot is of type \fIlisp\fR or \fIsetof <any type>\fR.
Note that it is up to the user to decide upon the meaning of
\fInilsym\fR and \fInilstruct\fR during further processing.
For example, you must decide for your own application whether
a \fInilstruct\fR filling the MObject slot of a MTrans indicates
that nothing was said or that what was said is unknown.
.PP
Often you may desire closer control over the default values of
a particular slot within individual instances.
For example, suppose you had a definition of Person that
includes several pieces of information about a person:
.DS
(create base  Person
	(Identity  symbol)
	(Age  int)
	(Salary  int)
	(Health  symbol) )
.DE
The Identity slot would be filled in with a unique symbol for
that person (such as the symbol John), the Age slot would contain
the age in years, the Salary slot would get the person's monthy salary
in dollars, and the Health slot would contain a \fIsymbol\fR indicating
their state of health.
Now in creating an individual instance of a Person
the Identity slot should be always filled in, but we may desire the
other slots to be defaulted to 30 (years), 20000 (dollars) and Healthy.
However, under the default system described so far, these would be
defaulted to zero, zero and \fInilsym\fR.
PEARL provides the ability to specify individual defaults for
each slot of a particular structure type.
This is done at \fIbase\fR creation time by following the type
within a slot with the new default value.
Thus our definition of Person would be:
.DS
(create base  Person
	(Identity  symbol)
	(Age  int  30)
	(Salary  int  20000)
	(Health  symbol  Healthy) )
.DE
Although the main purpose of a call to \fIcreate base\fR is to define
a structure, it also creates a special individual of the type
being defined which has its slots filled with the default values.
For this reason this individual is called the \fBdefault instance\fR
of that type.
It is a structure instance like any other, only a
pointer to it is kept with the type definition, and it is consulted
whenever an instance of that type is constructed.
Thus the default values (either the user-defined defaults or
the standard defaults) will always be used whenever the user
declines to fill in a slot of a structure instance.
For more on defaults, see the discussion of inheriting in
section 19 on creating expanded structures.
.NH
Using Patterns For More Flexible and Powerful Retrieval
.PP
If the fetching mechanisms described so far were the
only sort of fetching that we could do, \fIfetch\fR
(and PEARL) would not be very useful.
What is needed is a way to only partially specify the
values in the structure to be fetched.
Note that the default mechanism does not accomplish this,
since all slots are specified at creation time, even if they
get \fInilsym\fR, \fInilstruct\fR, or \fInil\fR for a value.
What is needed is the ability to specify a \fIdon't care\fR
value for slots whose values should not affect the matching
process during retrieval.
The easiest way to accomplish this in PEARL is to create
a type of object called a \fBpattern\fR.
A \fIpattern\fR is similar to an \fIindividual\fR instance of
a structure except that a special pattern-matching variable
called \fB?*any*\fR which means \fIdon't care\fR or \fImatch anything\fR
is used as the default value for unspecified slots.
(The reason for its name will be clear after the description
of pattern-matching variables later in this section.
Even more detail on pattern-matching variables and more powerful
patterns is provided in sections 21-23 on the matching process,
blocks, lexically scoped variables, and the
unbinding of variables.)
.PP
Patterns are created with \fIcreate\fR using \fIpattern\fR
as the first argument.
Other than this, their syntax is exactly the same as individuals.
An example of a \fIpattern\fR creation is:
.DS
(create pattern  PTrans  JohnWentSomewhere
	(Actor  John)
	(Object  John) )
.DE
This pattern would match any instance of PTrans in which John
was both the actor and the object being moved.
(Note that this pattern is stored in the Lisp variable
JohnWentSomewhere in the same way as other individuals.)
The main use of patterns is as arguments to \fIfetch\fR, as in:
.DS
(setq  PotentialGoings  (fetch  JohnWentSomewhere) )
.DE
\fIFetch\fR will return a stream containing all PTranses in the
data base in which John was the actor and object, regardless
what the From and To slots contain.
More complex examples can be created.
Patterns can be embedded as in:
.DS
(create pattern  MTrans  InformJohnGoingSomewhere
	(MObject  (PTrans  (Actor  John)
			   (Object  John) ) ) )
.DE
Since all unspecified slots are filled with ?*any*, this pattern
will return any MTranses concerning any of John's PTranses
when passed to \fIfetch\fR.
Thus, if we insert InformLeaving from above in the data base:
.DS
(insertdb  InformLeaving)
.DE
then the following will fetch that object:
.DS
(nextitem  (fetch  InformJohnGoingSomewhere) )
.DE
.PP
Usually one is interested in a more specific piece of information.
For example, if you knew that John told Carol something and wanted
to find out what it was, then you could do this two ways.
One is to create a pattern, fetch it and look at the MObject slot of
the result:
.DS
(create pattern  MTrans  WhatDidJohnTellCarol
	(Actor  John)
	(From  John)
	(To  Carol) )
(setq  Result  (firstfetch  WhatDidJohnTellCarol) )
(path  get  Result  'MObject)
.DE
However, you might prefer to use a pattern which explicitly
shows that you want that value and gives you a slightly easier
way to get at it.
In this case, you can specify a pattern-matching variable
in the MObject slot of the pattern.
A pattern-matching variable is created by preceding an atom with a
question mark \fB?\fR as in \fI?What\fR.
The question mark is a read macro character which reads the next
atom and builds the list \fI(*var* What)\fR out of it (or
\fI(*global* What)\fR if \fIWhat\fR has previously been declared
global to PEARL;
see below for more on global variables.).
During matching, this variable will get bound to the value
of the slot it gets matched against:
.DS
(create individual  MTrans  WhatDidJohnTellCarol
	(Actor  John)
	(MObject  ?What)
	(From  John)
	(To  Carol) )
(firstfetch  WhatDidJohnTellCarol)
.DE
To access the value of a pattern-matching variable in
a structure, one uses either the function \fBvalueof\fR
(which is an expr) or the fexpr \fBvarvalue\fR.
Both functions have two arguments: the name of the
pattern-matching variable whose value you want and
the structure it occurs in (which is evaluated internally
by \fIvarvalue\fR).
Thus both of the following are equivalent:
.DS
(valueof   \fB'\fRWhat  WhatDidJohnTellCarol)
(varvalue  What  WhatDidJohnTellCarol)
.DE
.NH
Marking Structures During Creation For More Efficient Retrieval
.PP
Besides specifying what type each structure is, the simplest
piece of information that \fIinsertdb\fR would like the user
to give it through \fIcreate\fR concerns which slot(s) of
a type would be most likely to contain unique information
about a particular instance of that type.
This information is used to differentiate instances of the
type from each other, so that they will be hashed into
different hash buckets.
This is similar to the "keys" that many data base systems ask for.
For PTrans, the Actor slot is often the best choice for this role.
For Person, the Identity slot would be the best choice for this role.
Such unique slots are indicated to \fIcreate\fR when defining a
type by placing an asterisk '*' before the slotname in a
(<slotname> <type>) pair.
For example, our new definitions of PTrans and Person
to take advantage of this would look like:
.DS
(create base  PTrans
	(* Actor  symbol)
	(   Object  symbol)
	(   From  symbol)
	(   To  symbol) )
(create base  Person
	(* Identity  symbol)
	(   Age  int  30)
	(   Salary  int  20000)
	(   Health  symbol  Healthy) )
.DE
If you execute this when you have already executed the other examples
in this document, PEARL will warn you that you are redefining the
base structures PTrans and Person.
This is all right, since that is precisely what we want to happen.
However, the previous instances of PTrans will remain hashed in the
more inefficient way and will not match any later PTrans structures
that are defined. 
If you find these warnings annoying when redefining structures,
they may be turned off by setting the special variable
\fB*warn*\fR to \fInil\fR instead of the initial \fIt\fR.
(Note that the Lisp scanner requires a space (or other
white space) to separate the asterisk from the slotname.
Otherwise one would have the slotname \fI*Actor\fR).
.PP
Any number of starred slots may be provided within a structure
definition, but usually only one or two are necessary.
How one decides which slots should be starred is an
art, and depends significantly on your application and the nature
of your data.
The basic rule of thumb is to choose those slots
whose values vary the most widely from instance to instance.
A bad choice will not usually cause the system to bomb or
operate incorrectly in any way, but when it comes time to
fetch an object back out of the data base the system may have
to take the time to scan a large amount of the data base
rather than finding the desired object very rapidly.
Thus execution time is usually the only penalty one pays
for an improper choice of slots to star.
.PP
However, there is one type of use of a slot which can cause
problems in combination with hashing information.
It involves the use of pattern-matching variables and will
serve as a useful example of how to use variables and of how
\fIinsertdb\fR and \fIfetch\fR use the hashing
information to insert and find objects.
The key difference between them is that while \fIinsertdb\fR
inserts an object in as many places as it can, \fIfetch\fR only
looks for it in the \fBbest\fR place.
(What we mean by best will be more obvious after section 13.)
.PP
The problem situation occurs when you wish to insert items
into the data base which contain a variable in the starred
slot (representing general knowledge) and then use,
as a pattern, a structure with that slot filled.
Thus, the following sequence will fail to find Thing
in the data base and instead will return \fInil\fR:
.DS
(create base  Thing
	(*  One  int) )
.DE
.DS
(dbcreate individual  Thing  DBThing
	  (One  ?O)
	  (Two  2) )
.DE
.DS
(nextitem (fetchcreate individual  Thing  PatThing
	               (One  1)
	               (Two  2) )
.DE
This fails \fIsimply because of the hashing\fR.
Let us see why.
When \fIinsertdb\fR is asked to put something into the data base,
it seeks to put it as many places as the hashing information
indicates are good places to want to look for it.
With no hashing information at all, the only thing \fIinsertdb\fR
can do is to put the object with all other objects of its type.
Thus, with no hashing information, all Things are put together in
the same hash bucket.
With the hashing information, \fIinsertdb\fR would like to put
DBThing in a second (and better) place based on the fact that it
is a Thing \fIand\fR on the value of its One slot.
Unfortunately, its One slot has an unbound variable in it and does
not provide any information which is useful.
Thus the hashing process puts DBThing into the data base in 
only one place.
However, when \fIfetch\fR indexes PatThing, it uses the hashing
information to determine that it should look in the data base
under the best combination which is \fIThing + 1\fR.
Since DBThing is not there, it is not found.
If we remove the asterisk, this sequence will return
DBThing with ?O bound to 1 because both DBThing and PatThing will
get indexed into the same spot (along with all other Things).
Thus you should be very careful when determining how to hash 
types of structures when you intend to insert them into
the data base with variables in them.
(An alternative solution which could be more efficient if used
carefully is to use the function \fIfetcheverywhere\fR which is
described in section 13.
This problem can also sometimes be solved with the use of adjunct
variables, described in section 14.)
.PP
After more of the system has been described and examples of fetching
and inserting have been given the user will have a better
understanding of this process.
.PP
As another example, let us now create and insert an instance
of our new PTrans which has the Actor slot starred:
.DS
(dbcreate individual  PTrans  Trip
	  (Actor  John)
	  (Object  John)
	  (From  Office)
	  (To  Home) ) 
.DE
This would insert Trip with all other PTranses and, because of the
starred slot Actor, also with any other PTranses with a value of
John in the Actor slot.
Next we redefine and recreate the MTrans:
.DS
(create base  MTrans 
	(*  Actor  symbol)
	(    MObject  struct)
	(    From  symbol)
	(    To  symbol) )
.DE
.DS
(create individual  MTrans  InformLeaving
	(Actor  John)
	(MObject  (PTrans  Leaving
		           (Actor  John)
		           (Object  John)
		           (From  Office)
		           (To  Home) ) )
	(From  John)
	(To  Carol) )
.DE
reinsert the PTrans from the MTrans:
.DS
(insertdb  Leaving)
.DE
and finally create and insert two other instances of a PTrans,
one with different values in the From and To slots
and one with different values in the Actor and Object slot:
.DS
(create individual  PTrans  Trip
	(Actor  John)
	(Object  John)
	(From  Home)
	(To  School) ) 
.DE
.DS
(create individual  PTrans
	(Actor  Ted)
	(Object  Ted)
	(From  Office)
	(To  Home) )
.DE
Note that this last PTrans will be indexed under the combination
of PTrans and Ted and thus will not be in the same hash bucket we
look in when fetching Trip (which is indexed by PTrans and John):
.DS
(setq  PotentialTrips  (fetch  Trip) )
(setq  Result  (nextitem  PotentialTrips)
PotentialTrips
.DE
Notice the form of the stream PotentialTrips at this point.
.NH
Printing Structures, Symbols and Other PEARL Objects
.PP
As mentioned in the beginning, PEARL stores symbols and
structures (and their definitions) in hunks of memory
that are circularly linked.
Lisp cannot print out the contents of these blocks in a useful way.
Franz Lisp sometimes goes into an infinite loop trying to print them
and the best UCI Lisp can do is tell you its address,
like #2934, which is not very informative.
As mentioned above, the PEARL prompt-read-eval-print loop knows how
to print these in symbolic form.
However, when you want your own programs to print
them, PEARL provides you with two pairs of functions
to convert these blocks into more readable form.
The first we will discuss is the function \fBvalform\fR.
\fIValform\fR takes a \fIstruct\fR, a \fIsymbol\fR or any other type
of PEARL or Lisp object as an argument and returns a
Lisp S-expression describing the object.
Thus if one calls \fIvalform\fR on our PTrans in Trip:
.DS
(setq  TripAsList  (valform  Trip) )
.DE
the Lisp variable TripAsList will contain the S-expression:
.DS
(PTrans (Actor John) (Object John) (From Home) (To School) )
.DE
Note that \fIvalform\fR does not cause the PTrans to be printed out
at the user's terminal, it is merely a function that returns an
S-expression (just as the Lisp function \fIlist\fR does.)
PEARL functions will operate upon structures and symbols only
when they are in their internal form, so the primary reason
for converting structures to S-expressions is to print them
(or to modify them for use as new input to \fIcreate\fR).
So PEARL provides a more useful function \fBvalprint\fR
that is effectively \fI(sprint (valform <argument>) )\fR.
(\fBSprint\fR is a function provided by UCI Lisp or Franz PEARL
which prints a Lisp expression in a nicely indented form.
There are more details on \fIsprint\fR in the appendix on UCI Lisp
functions added to PEARL.)
\fIValprint\fR is normally used within a Lisp program to
print out any PEARL construct onto the user's terminal
and it is also used by the default print function in the
PEARL prompt-read-eval-print loop.
Try typing the following and notice that they are the
same except that the second value is slightly indented.
.DS
(valprint  Trip)
Trip
.DE
Like \fIsprint\fR, \fIvalprint\fR will accept a second integer
argument telling it which column to start printing in.
The default \fIpearlprintfn\fR uses a value of 4 for this argument
to make the items typed by the user more distinguisable from the 
results typed by PEARL.
.PP
There is one other form of each of these two functions.
The functions \fBfullform\fR and \fBfullprint\fR are
like \fIvalform\fR and \fIvalprint\fR but they print
more complete information.
If you type
.DS
(fullprint  Trip)
.DE
you will notice that the result has two mysterious
\fInil\fRs in each slot.
These represent other forms of information (predicates
and hooks or demons) which can be added to structures
and which will be described later.
At the moment therefore, \fIvalform\fR and \fIvalprint\fR
are all that the user need remember.
.PP
Note also from above that when a pattern with \fI?*any*\fR is printed,
only the name of that variable is printed, and not its value.
(Try typing JohnWentSomewhere and InformJohnGoingSomewhere
if you do not remember what this looked like.)
If a PEARL pattern-matching variable has not been bound,
PEARL indicates this by printing no value.
If a variable is bound, both the variable name and
its value are printed.
Later when you learn how to put your own variables
in slots, this will become more useful.
.PP
When given a data base, these functions assume that the user does
not really want the complete contents of the data base printed out
and so simply print \fI(database: <databasename>)\fR.
To actually have the complete contents of a data base printed out,
use the function \fBprintdb\fR.
With no argument, it prints the default data base.
Otherwise, it expects its argument to evaluate to a data base.
A print function which prints all the internal information contained
in a structure or its definition is \fBdebugprint\fR.
.NH
Error Messages, Bugs, and Error Handling Abilities
.PP
Due to the complex implemention of PEARL and the lack
of facilities in Lisp for easily distinguishing between
types of input, a user's error in using PEARL will not
show up as soon as it occurs, but may instead cause some
unfathomable part of PEARL to bomb out sometime later.
If this should happen, the user might try using the Lisp
trace facilities, but will often find little useful information.
This sad state of affairs is currently unavoidable due to the 
difficulty of catching user errors where they first occur.
This is partly due to our inability to predict what kinds of
errors users are most likely to make.
.PP
PEARL checks as much as it can, but many features are impossible
or prohibitively expensive to check.
The best strategy for the
user to follow is to examine his last interaction with PEARL.
If the error occurred in the bowels of \fIcreate\fR, then there is a
good chance that the user did something wrong in the details of
a slot description in the call to \fIcreate\fR, since gross structural
errors in such calls are checked for.
Inspect your call closely.
.PP
Other errors can be even more difficult, since a function call
may blow up or fail to produce the desired result due to bad
data passed to \fIcreate\fR several calls ago.
A general rule of thumb to use in tracking down mystifying
errors is to check out the definitions of the structures
involved in the function that failed.
Thus if \fIpath\fR blows up, one should determine the type of
the structure passed to \fIpath\fR, and then check the 
\fIcreate\fR call that defined that type.
.PP
Sometimes PEARL may appear to the user to be doing the wrong thing,
but due to PEARL's complex semantics, the user is really at fault.
To make matters worse, there is of course always the chance that
the error really \fBis\fR in PEARL.
Every effort has been made to minimize this chance, and at the
moment there are no known major errors (except those indicated
in this documentation), but as with any complex evolving
software system there is always the chance of obscure errors.
It has been our experience that most errors
are due to the user (including the implementors) not completely
understanding the semantics of some PEARL feature.
This documentation is an effort to minimize this type of error.
For any error which you commit in which PEARL gives what
you consider an unreasonable response, feel free to report
it and we will consider trying to catch it.
These or any other complaints, bugs or suggestions should be
mailed to Joe Faletti via Arpanet or Unix mail
(Kim.Faletti@Berkeley or ucbvax!kim.faletti).
.NH
Short-Circuiting and Redirecting Create Using !, $ and Atoms
.PP
Sometimes, when creating an individual structure,
one may want to fill a slot with an already created structure
that is pointed to by some atom or returned by some function
(or with whatever type of value the slot requires).
In this case, one does not wish to (or cannot) describe the
value for a slot as a list of atoms.
To handle this situation, PEARL allows you to list a Lisp expression
which evaluates to the desired internal form (that is, a form
which needs no processing by \fIcreate\fR), preceding it with
an exclamation point \fB"!"\fR.
The structure (or other object) resulting from evaluating
the Lisp expression will be tested to ensure it is the right type
of value and, if it is, inserted in the newly created structure
as the value of that slot.
(The mnemonic idea of this symbol is as a sort
of "barrier" meaning \fIStop processing here!!! and take this
(almost) literally!!!\fR)  For example, after using
.DS
(create individual  PTrans  Ptrans23
	(Actor  John)
	(Object  John)
	(To  Home) )
.DE
to create an individual PTrans, leaving it in internal form in the
atom Ptrans23, you can then insert this PTrans into a new MTrans
with:
.DS
(create individual  MTrans
	(Actor  Bob)
	(MObject  !  Ptrans23)
	(To  Carol) )
.DE
.PP
At other times the user may want to take the result of evaluating
some Lisp code and splice it into the Lisp expression describing the
structure being created at the point where the description of the
value of a slot would occur.
In this case, you wish some Lisp code to be evaluated and then
you wish \fIcreate\fR to build a value for this slot
by further processing (scanning) the result of this evaluation.
To this end, PEARL will evaluate any slot value preceded by a
\fB"$"\fR and insert its result into the \fIcreate\fR call,
proceeding to process it just as if
the user had typed it in directly.
So if one stores the atom Alice in Name with
.DS
(setq  Name  'Alice) ; the atom Alice, not the symbol Alice
		     ;   (or the value of s:Alice).
.DE
or possibly
.DS
(setq  Name  (read) )
.DE
with the user having typed \fIAlice\fR, then \fI$  Name\fR in
.DS
(create individual  PTrans
	(Actor  $  Name)
	(Object  $  Name)
	(From  Home)
	(To  NewYork) )
.DE
is equivalent to having Alice typed as the Actor and Object
values: \fIcreate\fR evaluates Name and then processes its
value \fIAlice\fR as input.
Thus, the PTrans will be equivalent to
.DS
(create individual  PTrans
	(Actor  Alice)
	(Object  Alice)
	(From  Home)
	(To  NewYork) )
.DE
The power of this construct occurs when Name is a atom whose
value changes at run time (as when it is read above) or the
\fIcreate\fR call is within a loop in which Name takes on many
different values.
.PP
In summary, both ! and $ cause the evaluation of the Lisp
expression following them.
However, ! stops the usual processing and expects an
internal value, whereas $ continues the usual
processing and expects a Lisp description of the value.
When you need either ! or $, you will know it!
Until then, do not worry if you do not understand them very well!
.PP
If the expression you want evaluated is simply an atom
bound to a value of the appropriate type, you need not use the !.
Whenever \fIcreate\fR is looking for a value of a particular
type, and finds a bound atom instead, it evaluates the atom and
if it is bound to the correct type of value, that value is used.
This is only done in \fIsymbol\fR slots when the atom is not a
symbolname and in \fIinteger\fR slots if the atom is not from
the ordinal type (if any) associated with the slot.
.NH
More Flexible Hash Selection
.PP
The use of stars (asterisks *) to indicate useful slots for hashing
described earlier is only one of many hashing schemes that PEARL allows.
This section describes the others, and how the user can control them.
The first point to note is that even with the star hashing a single
structure can be hashed in several different ways.
Thus if one thinks that in a particular program PTrans will be
frequently fetched from the data base given only the
Actor \fBor\fR only the Object (that is, the program might only
know the Actor and desire the whole PTrans, or know only
the Object and desire the whole PTrans) the user should
star \fBboth\fR the Actor and Object slots within
the definition of PTrans.
When PEARL stores a PTrans into the data base, it will index
it under both (PTrans + Actor) and (PTrans + Object) in addition
to the usual indexing with all other PTranses.
In general, any number of slots can be starred.
.PP
Another type of hashing does not use the type of the structure in
creating a hash index.
If the symbol colon (:) is used before the
name of a slot, objects of that type will be hashed under that slot
value only.
Thus if the Actor slot of the PTrans definition was
preceded by a colon instead of a star, then instances of PTrans
would be hashed under the value of the Actor slot alone rather the
value of the (PTrans + Actor).
This would be useful in the case in which one were interested in
fetching any structure in which a particular value, say the
symbol John, appered in a coloned slot.
For example all structures in which John appeared in the Actor slot
could be fetched at once (and very efficiently).
.PP
A third type of hashing is \fBstar-star\fR or \fBdouble-star (**)
hashing\fR.
If a double star is used instead of a single star,
PEARL will use \fBtriple hashing\fR.
Only one triple hashing is allowed per structure.
Triple hashing requires that two, and only two slots be double starred.
If PTrans were to be defined in the following way:
.DS
(create base  PTrans
	(**  Actor  symbol)
	(**  Object  symbol)
	(      From  symbol)
	(      To  symbol) )
.DE
then when an instance of a PTrans is created, it will be hashed
into the data base under a combination of the three values
(PTrans + Actor + Object).
As with all hashing, if a slot is necessary to a particular type
of hashing but is unfilled (or filled with \fInilsym\fR or
\fInilstruct\fR) the hashing will not occur.
Triple hashing is used when one wants fast access to all
individuals of a particular type with two slots likely to have
fairly unique values.
In the case of PTrans, this would allow one fast access to all
PTranses in which John PTranses Mary somewhere.
Distinctions this fine are not usually necessary, and as it
is slightly more expensive at creation and fetching time,
it should only be used when the user is sure of the need for it.
.PP
A fourth type of hashing is \fBcolon-colon\fR or \fBdouble colon (::)
hashing\fR.
It has the same relation to colon hashing as double star
hashing has to star hashing.
If the **'s in the above are replaced with ::, the hashing will be
on (Actor + Object) ignoring the fact that the structure is a PTrans.
This might be useful in fetching all structures involving John and Mary.
As with double star hashing, double colon hashing should be used
sparingly and only one such hashing pair may be used in a type.
.PP
However, it is possible to combine the use of any of these
hashing methods in a single structure.
Thus one could have both double colon hashing and double star
hashing, as well as several * and : hashings as well.
Given several ways, PEARL uses the one
which the most complex one is used during
fetching, since that should provide the greatest degree of
discrimination between items (that is, most likely to narrow
down the choices).
If the value in a slot intended to take part in hashing is unbound
or otherwise not useful, then the next most specific method it used.
Given the values which are considered useful and the hashing
information for the type of structure, the hierarchy of buckets
to be chosen is as follows:
.DS
**  hashing
::  hashing
*  hashing
:  hashing
.DE
.PP
In section 9 we discussed a problem that arises when the pattern
you are using is more specific than the structures in the data base.
In this case, \fIfetch\fR looks in the data base in the most
specific place and does not find what it is looking.
One alternative is to eliminate the hashing that causes this problem
but this will force \fIfetch\fR to look through a large number of items.
If you do not intend to look all the way through the stream
returned by \fIfetch\fR, there is a version of fetch which will
build the stream out of all the ways the pattern could be fetched.
This function is called \fBfetcheverywhere\fR and will return a
stream made up of the (up to five) hash buckets that its pattern
could be -- potentially expensive if you intend to process the
whole thing.
.PP
In addition to these four methods of hashing, and the simplest one
based on the type of structure only, there are several
hashing labels which are modifiers on these methods and
affect what values are used to compute the index.
.PP
The remaining hashing flags do not introduce any new types
of hashing, but rather modify the way the existing types work.
To motivate these, consider the implementation of Goal withing CD.
In early versions of CD, there were several different types of
goals, including Delta-Prox (goal of being near something),
Delta-Poss (goal of possessing something), and so on.
In general these delta goals were of the form
(Delta-<some CD primitive> (Actor ...) (Objective ...) ).
This lead to an explosion of Delta-goals
(e.g. Delta-Move-Fingers-Within-Telephone-Dial),
and a new way of handling goals was established.
This was simply that all Goals were to have the form:
.DS
(create base  Goal
	(Planner  symbol)
	(Objective  struct) )
.DE
where the Objective would be filled with the appropriate structure,
whether it was a simple Poss or the $DialPhone script.
This change makes CD much cleaner, but poses somewhat of
a problem for hashing.
One of the major uses of hashing within some AI programs
written in PEARL is to associate plans with goals.
So it is best if this process is efficient.
.PP
As an example of this problem (using the early form of Delta-goals):
.DS
; Declaration of PlanFor rules.
(create base  PlanFor
	(*  Objective  struct)
	(*  Plan  struct) )
.DE
.DS
(create base  Delta-Prox
	(Planner  symbol)
	(Location  symbol) )
.DE
.DS
(create base  Walk-Plan
	(Planner  symbol)
	(From  symbol)
	(To  symbol) )
.DE
.DS
; Store in the data base the fact that walking is a way of accomplishing
;    a Delta-Prox goal.
(dbcreate individual  PlanFor
          (Goal  (Delta-Prox  (Planner  ?X)
			      (Location  ?Y) ) )
          (Plan  (Walk-Plan  (Planner  ?X)
 			     (From  nilsym)
 			     (To  ?Y) ) ) )
.DE
This structure simply says the fact that if one has a goal of being
somewhere, then one plan for doing this is to walk.
Or, using the rule in reverse, if you note that someone is
walking to some location, then you might infer that they had
a goal of being at that location.
Note that after being put into the data base, the rule can be easily
fetched by presenting either half of it as a pattern.
.PP
Thus if a planning program has a goal of doing the action in
the atom GoalAct, then it can query the data base for
any direct plans for doing Act by:
.DS
(fetchcreate individual  PlanFor
	     (Goal  !  GoalAct)
	     (Plan  ?*any*) )
.DE
So if GoalAct happened to be a Delta-Prox goal, then the
rule above would be fetched.
However the revised form of goals hides the unique nature of
the Delta-goal, and the best one could do is fetch all PlanFor rules
that have a structure of type Goal in their Goal slot.
This is a serious loss since \fIall PlanFors\fR have a Goal
in their Goal slot;
thus the system would have to look through all
PlanFors whenever it was trying to fetch one.
What is needed is a way of telling PEARL that when hashing on Goals,
never hash the structure type Goal, but rather use the
item that fills the Objective slot of the Goal.
This would solve our problem nicely, as now all
PlanFors would be hashed on the name of the Objective (Prox,
Dial-Phone, etc.), and a list of all PlanFors would not have to be
searched to find a particular one, rather the system could just hash
directly to it.
.PP
To indicate to PEARL that this \fBhash aliasing\fR is desired,
place an ampersand '&' before the slot name to be substituted
for the structure name when defining the structure.
Thus Goal would be declared:
.DS
(create base  Goal
	(    Planner  symbol)
	(&  Objective  struct) )
.DE
Naturally only one slot can be selected for hash aliasing.
.PP
In this way, Goals change the way in which other structures
use them to index but the way in which Goals themselves
are indexed will not be affected.
Since many other types of structures are likely to contain Goals,
we must be careful about how this affects the hashing of all of them.
It might be the case that PlanFor was the only structure
indexed based on Goals which would benefit from hash aliasing
and that some structures would actually be hurt by this
because they expected Goals to be only one of many types
of values.
In this case, putting the control of how Goals get used by
other structures into the definition of Goal is a bad idea.
Instead, the control can be moved up into only the
problematic structures.
These structures can simply add the \fB">"\fR hash label to
a starred slot, causing PEARL to use the first starred
slot of the slot-filling structure instead of its type.
For example, when we put a both \fB"*"\fR and \fB">"\fR on the Goal
slot of PlanFor then it will always use the first starred
slot of the Goal in its Goal slot:
.DS
(create base  Goal
	(    Planner  symbol)
	(*  Objective  struct))
.DE
.DS
(create base  PlanFor
	(*  >  Goal  struct)
	(        Plan  struct))
.DE
Thus, the use of \fB">"\fR hashing is called \fBforced aliasing\fR since
the structure filling a slot has very little control over it.
.PP
However, there is one way for a structure to affect
how forced aliasing happens.
If the user wanted to also star the Planner slot of Goal,
but wanted the Objective slot to be used in cases of forced
aliasing, then the use of an \fB"^"\fR on the Objective slot will
allow that:
.DS
(create base  Goal
	(*       Planner  symbol)
	(*  ^  Objective  struct))
.DE
thus allowing Goals inserted directly into the data base to be
indexed by the combinations \fIGoal + Planner\fR and
\fIGoal + Objective\fR while other objects containing Goals would
use the Objective slot rather than Goal \fIOtherObject + Objective\fR.
.PP
On the other hand, if most structures containing Goals would
benefit from the use of the hash aliasing label \fB"&"\fR in Goal,
but only one or two are hurt by it, the use of \fB"&"\fR in Goal
can be overridden by the structures which will contain Goals
by adding the \fB"<"\fR hash label to the starred slot to produce
\fBanti-aliasing\fR.
This gives the controlling structure the last word
over how it is hashed.
.DS
(create base  Goal
	(    Planner  symbol)
	(&  Objective  struct))
.DE
.DS
(create base  OffendedStructure
	(*  <  Slot  struct))
.DE
Thus, the anti-aliasing \fB"<"\fR means \fIjust for this hashing, turn
off hash aliasing (if any) of any structure filling this slot\fR.
.PP
The proper use of hash aliasing and anti-aliasing,
like all the hashing specifiers is an art that must be learned by
applying them to real systems, and the correct hash directives
for a particular system rely critically upon the statistics of
that particular system operating upon a particular set of data.
The hashing mechanism was designed to give the user benefit in
proportion to the effort expended in determining hash labels.
With no effort, the structure type provides some help.
With the addition of each label or pair of labels,
an item to be inserted into the data base is indexed into
another location in the hash table.
Thus the cost of \fIextra\fR labels is simply the time to
find another hash bucket (a few adds and multiplies), and add
the item to the front of the list implying the time and
space incurred by one cons-cell.
.NH
Using Predicates to Constrain Fetching
.PP
Sometimes when you are creating a pattern to fetch a structure,
giving the overall form of the structure is not specific enough.
In particular, it is often desirable to restrict the value of a
slot to a subrange.
For example, using the structure Health:
.DS
(create base  Health
	(Actor  symbol)
	(Level  int) )
.DE
one might want to find out who is sick by creating a pattern
that only matches those Health structures in which the Level
is less than -1 (on a scale from -10 to 10 perhaps).
This can be done by simply writing a predicate (say Sick)
which expects to be given the value of the slot being matched
against as its one argument:
.DS
(de Sick  (Num)
    (lessp  Num  -1) )
.DE
Then you simply add its name after the value
within the <slotname filler> pair of the pattern:
.DS
(create pattern  Health  HealthPattern
	(Actor  ?Person)
	(Level  ?Level  Sick) )
.DE
Given these definitions, a (fetch HealthPattern) would pass
the Level slotfiller of each Health structure it
found in the data base to the predicate Sick.
If Sick returned true (non-\fInil\fR) then it would
consider the slot to have matched whereas a
\fInil\fR from Sick would be considered a mismatch.
There are no standard predicates for users to use for these
purposes, but they are relatively easy to create as needed.
.PP
However, one often has a predicate which has more than one
argument only one (or none) of which are the slot value.
For example, one might want to include a special variable
or the value of some other slot of the structure or the
structure itself.
To make this easy PEARL allows predicates to be arbitrary
s-expressions which may contain any of several special forms
for which PEARL substitutes the current slot or structure.
.PP
If a predicate includes an asterisk \fB*\fR, this is replaced by
the value of the current slot (in the structure being matched
against).
If it includes a double asterisk \fB**\fR, this is replaced
by the whole structure being matched against.
If you want the value of another slot in the current structure,
precede its name with an equal sign (as in \fB=SlotName\fR to
have the value of the slot named SlotName inserted).
There is a readmacro \fB"="\fR which converts \fI=S\fR into 
\fI(*slot* S)\fR, just as the readmacro \fB"?"\fR converts ?X into
\fI(*var* X)\fR (or \fI(*global* X)\fR) for pattern-matching variables.
While processing predicates before executing them, PEARL will
look for these three constructs and replace any of them with the
appropriate value, so pattern-matching variables can also be
used in predicates.
.PP
If there are several predicates on a slot, they are run in
succession until one returns nil or they have all been run.
Thus, a list of predicates provides the effect of a conditional
\fIand\fR.
Thus, although PEARL knows nothing special about logical
connectives like \fIor\fR and \fIand\fR, the effect of a
the usual Lisp \fIand\fR is automatically implied and
the conditional \fIor\fR of Lisp can be had by using the
s-expression type of predicate.
If you wish things to run regardless of their results,
providing the effect of unconditional \fIand\fR, use hooks (demons).
.PP
The above was one of two types of predicates available.
To motivate the other type, consider the case of wanting
to fetch all MTranses about the occurence of a PTrans.
This could be accomplished in one of two ways.
The first is:
.DS
; In this pattern example, all slots are automatically filled
;     with ?*any* except the MObject which must be a PTrans.
(create pattern  MTrans
	(MObject  (PTrans) ) )
.DE
Since this method actually results in \fI?*any*\fR being
matched against the fillers in each of the PTrans's
slots, it is a bit inefficient.
.PP
The second way uses \fBstructure predicates\fR
to avoid this matching by specifying merely that the filler
of the MObject slot must be a PTrans structure.
This is done by listing the name of a previously
defined structure after a pattern-matching variable:
.DS
(create pattern  MTrans
	(MObject  ?Obj  PTrans) )
.DE
PEARL will then bind Obj to any structure that is a PTrans
(or expanded PTrans) and match successfully without
examining any of the slots of that PTrans.
PEARL can tell the difference between these two types of
predicates since one will have some sort of function declaration
and the other will be the name of a defined structure.
In the case of a function with the
same name as a structure (which the user should never do as it
invites errors) the name's structure role takes precedence.
.PP
Since a similar effect is sometimes desired on slots of type
\fIsymbol\fR, a similar but more complex mechanism is provided
with symbols and with structures which failed the above test.
If the name of a predicate on a slot of type symbol or structure
is the name of a type of structure, PEARL will assume that what
you want to know about the value in this slot is whether there
is anything in the data base of the type specified by the structure
predicate with the slot value in its first slot.
Thus, if the data base contains an item saying that the symbol
John represents a person:
.DS
(symbol  John)
(dbcreate individual  Person
	  (Identity  John))
.DE
then fetching a pattern with a symbol slot which has a Person
predicate on it:
.DS
(fetchcreate pattern  Thing
	     (Slot  ?X  Person))
.DE
will cause the equivalent of a fetch from the (default) data base
of the pattern (Person (Identity John)).
Note that this implies that the first slot of a structure enjoys
somewhat of a pre-eminence and that this means that one should
carefully choose which slot to put first.
For efficiency however, \fIfetch\fR is not actually used.
The function actually used is \fBdisguisedas\fR which expects
the slot filler, the structure definition (not default instance)
and an optional data base to look in.
Slot filler may be either a symbol or structure.
.PP
This second type of predicate can also result in a kind of
inefficiency which you might like to avoid.
By putting a variable in the MObject slot of the MTrans along with
a PTrans structure predicate, we preclude PEARL from hashing the
object in any useful way, forcing it to look through all MTranses
instead of only MTranses with PTranses in their MObject slot.
Since patterns are most often less specific than the objects in
the data base, this can make a big difference.
Another problem with a variable plus a structure predicate is that the
structure predicate is either based on fetches and the first slot or it
is limitted to matching the type only.
We might sometimes want a more complicated structure to be used
as a predicate.
However, if we opt instead for the more efficient fetching and
matching by putting a structure in the slot, we have lost the
ability to have a variable bound during the match.
.PP
To allow you both to help improve the hashing and matching of a
structure and also to bind a variable as a side effect, PEARL
provides a mechanism to attach an \fBadjunct variable\fR to the slot.
This adjunct variable in a slot is bound as a side effect whenever the
values in the slot of the two structures were already bound, have
already been matched successfully and all predicates and slot hooks
have been run.
Adjunct variables may be local, lexically scoped or global, just
as any other variable.
To use an adjunct variable, include the variable \fIafter\fR the
value preceded by a colon and preceding any predicates or slot hooks.
For example, 
.DS
(create pattern  MTrans
	(MObject  (PTrans  (Actor  John) )  :  ?Obj) )
.DE
would match any MTrans about John PTransing something, and also
bind the adjunct variable ?Obj to the actual PTrans structure
that applied.
.PP
Since PEARL uses hunks to create so many types of values of its
own, it also provides a set of predicates to test an item to see
what type it is.
Many of them are quite definitely kludges since they depend upon
certain bizarre structures existing only in PEARL-created items
and not in user-created items and thus should not be depended
upon totally.
These functions are \fBstreamp\fR, \fBdatabasep\fR, \fBblockp\fR,
\fBdefinitionp\fR, \fBpsymbolp\fR (to distinguish from Franz Lisp
\fIsymbolp\fR), \fBstructurep\fR,
\fBsymbolnamep\fR, and \fBstructurenamep\fR.
.NH
More Useful Slot Types
.PP
These last few examples begin to show the restricted nature of basic
integer values and of labelling slots as being of type \fIstruct\fR.
If the values in an integer slot will range between -10 and 10,
then you would like to say that.
If the values which will fill a slot of type structure will
be Events or Acts or States, you would like to specify that.
PEARL provides mechanisms to fill both of these needs.
.PP
In the case of an integer slot to be filled with values from a range
of -10 to 10, these integer values do not represent "levels of health"
very well either.
Rather than saying that a person's "health level"
is -2, you might like to say it was "Sick".
In fact, you would
probably like to say that the values of the slot will be one from
among the set of values "Dead, Critical, Sick, OK, Healthy and InThePink".
Moreover, you might like to specify that these values are to be
associated with integer values in such a way that the ordering
you specified holds and you may or may not want to specify precisely
what integer values should be associated with these atoms.
In other words, you would like a type which consists of a set of
values with a linear ordering on them, similar to the Pascal scalar or
enumeration type.
.PP
Such a type exists in PEARL and is created by a call to
the function \fBordinal\fR.
For example, to create an ordered set of values to represent
levels of various states when you want the actual
integer values to be created by PEARL, you would say:
.DS
(ordinal  Levels  (Low  Middle  High))
.DE
which would associate the numbers 1, 2, and 3 with Low, Middle and
High respectively.
If you want to specify the values to be associated with each name,
you simply list the value after each name.
Thus, to create a set of values for use in the integer Level
slot of Health above, you might say the following (the values need
not be listed in order):
.DS
(ordinal  HealthLevels  (Dead -10  Critical -6  Sick -2  OK 2
				   Healthy 6  InThePink 10))
.DE
Among the actions that \fIordinal\fR performs are the following:
.IP 1.
The assoc-list of names and values for the ordinal type can be
accessed by evaluating the atom built by prepending \fBo:\fR to
the name of the ordinal type.
Given the name of an ordinal type, the function \fBordatom\fR builds
this atom.
Thus \fIo:Levels\fR contains (and \fI(eval (ordatom 'Levels))\fR returns)
the value \fI((Low . 1) (Middle . 2) (High . 3))\fR.
.IP 2.
Atoms consisting of the name of the ordinal type concatenated
with a colon and the value name are created and set to the value
they represent.
Thus \fILevels:Low\fR is set to 1, \fILevels:Middle\fR is set to 2, etc.
.IP 3.
Two atoms with \fB:min\fR and \fB:max\fR concatenated to the
name of the ordinal type are created and set to the lowest
and highest integer values in the type.
Thus \fIHealthLevels:min\fR is -10, and \fIHealthLevels:max\fR is 10.
.IP 4.
The name of the ordinal type is added the list of all ordinal type
names kept in the special variable \fB*ordinalnames\fR*.
.IP 5.
The name of the ordinal type is stored with the slot
so that the print functions can convert from the
integer value back into the name.
Since the default value for integers is zero but most
ordinals will not have a zero value, the print functions will
print \fB*zero-ordinal-value*\fR instead of zero.
.PP
Having created an ordinal type, it is then possible to declare in
a structure definition that a slot will contain values of that type.
The use of values from this type is \fBnot enforced\fR
by PEARL but allows the definitions of integer slots to be
more readable, allows the use of the names of values instead
of their associated integers when creating individuals and
allows PEARL to print the more readable information when
printing an integer slot.
The special atoms created allow predicates, hooks (demons) and
other functions to refer to these values without knowing
their associated integers.
We can now redefine Health to use HealthLevels:
.DS
(create base  Health
	(Actor  symbol)
	(Level  HealthLevels) )
.DE
and create an individual which says that John is in
the pink of health:
.DS
(create individual  Health
	(Actor  John)
	(Level  InThePink) )
.DE
.PP
Declaring a slot to be of type \fIstruct\fR is similarly
unenlightening, so PEARL will accept the name of a
structure type in its place.
For example, we can make the following definitions:
.DS
(create base  Person
	(*  Identity  symbol) )
(create base  Health
	(Actor  Person)
	(Level  HealthLevels) )
.DE
and the Actor slot of Health will be of type \fIstruct\fR.
However, there is currently no extra type checking implied
by this declaration (although it is being considered), but
again it improves the readability of declarations tremendously.
.NH
Attaching Hooks to Structures (If-Added Demons)
.PP
A fairly old construct within AI is that of demons.
In their pure form they could be thought of as asynchronous
parallel processes that watch everything going on within a
system, lying in wait for a particular set of conditions to occur.
These conditions might be a block-manipulating program stacking
some blocks too high to be stable, or a data base program violating
a consistency constraint.
The main problem with classical demons was that in their most flexible
form they gobble up far too much system time, as well as being very
hard to program as it was hard to see just when they might pop up
during the execution of a program.
.PP
In an attempt to control the implementation of demons and at the same
time provide the user with increased control over the built-in PEARL
functions, PEARL allows the user to attach pieces of code to
structures that will be run when specific PEARL (or user) functions
access particular types of data or pieces of data at particular
places in the code.
Thus, PEARL provides a general but restricted and fairly efficient
ability to control the operation of specific functions on specific
pieces of data by providing \fBhooks\fR in the PEARL functions
which check for requests within structures that certain functions
be run when they are accessed in certain ways.
Thus PEARL has two useful sub-breeds of \fBhooks\fR which
watch over either
.IP a.
the value of a particular slot of a particular individual structure,
referred to as \fIslot hooks\fR.
.IP b.
operations upon all individuals of a particular base structure type
referred to as \fIbase hooks\fR.
.PP
Like predicates, hooks can either be the name of a function to
run or a Lisp s-expression to be evaluated.
If an s-expression, they can include the special forms 
\fB**\fR representing the current structure or \fB*\fR representing
the value of the current slot on slot hooks and of the current
structure on base hooks.
Variables or slot names preceded by \fB=\fR are also allowed
(just as in predicates), referring to variables or slots in
the current structure.
If hooks are run by functions which take two items as arguments,
like \fImatch\fR, then the special form \fB>**\fR may
be used to represent the \fBother\fR structure (which \fB>\fR is
meant to suggest) and \fB>*\fR may be used for the value in this
slot of the other structure.
(In the case of functions of only one argument, \fI>*\fR and
\fI>**\fR are the same as \fI**\fR and \fI*\fR.)
In functions which take two arguments, the special form \fB?\fR
may be used to represent the result that the function intends to
return.
(This will be \fI*pearlunbound*\fR in hooks which run before the
function has done its job.)
.PP
When hooks run in the context of a call to \fIpath\fR,
two special variables are available: \fB*pathtop*\fR which
is the topmost structure passed to path and \fB*pathlocal*\fR
which is the current innermost structure whose slot is
being accessed.
When hooks are run in the context of a call to a function which
deals with a data base, then the special variable \fBdb\fR
will contain the data base currently being used.
.PP
The functions used to fill in the special forms like *, **, =slot,
and variables before evaluation come in two flavors and are
called \fBfillin1\fR and \fBfillin2\fR.
\fIFillin1\fR is designed for hooks which run on single structures
and expects as arguments:
.IP a.
the function (s-expression) to fill in,
.IP b.
the slot value (or item if a base hook) to use for \fI*\fR,
.IP c.
the structure to use for \fI**\fR, and
.IP d.
the definition for the item provided as the third argument
(for interpretation of \fI=slot\fR forms).
.PP
\fIFillin2\fR is designed for hooks which run on two structures and
produce a result and expects as arguments:
.IP a.
the function (s-expression) to fill in,
.IP b-c.
the slot values (or structures if a base hook) to use for \fI*\fR and \fI>*\fR,
.IP d-e.
the structures to use for \fI**\fR and \fI>**\fR,
.IP f.
the definition for the structure provided as the fourth argument, and
.IP g.
the result the function intends to return to use for \fI?\fR.
.PP
Four functions for running hooks are provided for the user, two
for running slot hooks and base hooks for single items and two for
running slot hooks and base hooks for pairs of items.
\fBRunslothooks1\fR expects to be given the invoking function's
name, the structure and name of the slot on which to run the slot
hooks, and the value to be used for \fI*\fR.
\fBRunslothooks2\fR expects to be given the invoking function's
name, the two structures and name of the slot in them on which to
run the slot hooks, and the values to be used for \fI*\fR and \fI>*\fR.
\fBRunbasehooks1\fR expects to be given the invoking function's name
and the structure whose base hooks are to be run.
\fBRunbasehooks2\fR expects the invoking function's name, the two
structures whose base hooks are to be run and the result the
calling function plans to return.
.PP
If present, base hooks are run by most major PEARL functions.
If a base hook is labelled with \fI<foo\fR then the function
\fIfoo\fR will execute the hook just after entry and whatever
initialization is necessary.
If a base hook is labelled with \fI>foo\fR then the function \fIfoo\fR
will execute the hook just before exitting.
Slot hooks are run by most major PEARL functions which look through
the slots of a structure.
If a slot hook is labelled with \fI<foo\fR then the function \fIfoo\fR
will execute the hook just before processing the slot.
If a slot hook is labelled with \fI>foo\fR then the function \fIfoo\fR
will execute the hook just after processing the slot.
.PP
However, hooks can be turned off selectively or completely.
By setting the atoms \fB*runallslothooks*\fR and
\fB*runallbasehooks*\fR to nil, you can completely disable
the running of all hooks.
This is useful for debugging and also helps improve efficiency
a bit if you do not use hooks at all.
There is also an atom to go with each PEARL function (of the form
\fB*run...hooks*\fR) which can be used to disable hooks for selected
functions.
The following is a complete table of what PEARL functions run hooks
and the names of the labels that invoke them and the atoms that
control their running:
.LD
Base hooks are run by:			\kminvoked by hooks labelled:
    create expanded			\h'|\nmu'<expanded or >expanded 
    create individual			\h'|\nmu'<individual or >individual 
    create pattern			\h'|\nmu'<pattern or >pattern 
    smerge				\h'|\nmu'<smerge or >smerge 
    nextitem				\h'|\nmu'<nextitem or >nextitem 
    standardfetch *			\h'|\nmu'<fetch or >fetch 
    expandedfetch *			\h'|\nmu'<fetch or >fetch 
    fetcheverywhere *			\h'|\nmu'<fetch or >fetch 
    insertdb				\h'|\nmu'<insertdb or >insertdb 
    removedb				\h'|\nmu'<removedb or >removedb 
    nextequal				\h'|\nmu'<nextequal or >nextequal 
    indb				\h'|\nmu'<indb or >indb 
    standardmatch			\h'|\nmu'<match or >match 
    basicmatch				\h'|\nmu'<match or >match 
    strequal				\h'|\nmu'<strequal or >strequal 
_________
* \fIfetch\fR does not run hooks on function structures.
.sp 2
Slot hooks are run by:			\h'|\nmu'invoked by hooks labelled:
    standardmatch			\h'|\nmu'<match or >match
    basicmatch				\h'|\nmu'<match or >match
    strequal				\h'|\nmu'<strequal or >strequal
    path put				\h'|\nmu'<put or >put
    path clear				\h'|\nmu'<clear or >clear
    path addset				\h'|\nmu'<addset or >addset
    path delset				\h'|\nmu'<delset or >delset
    path addpred			\h'|\nmu'<addpred or >addpred
    path delpred			\h'|\nmu'<delpred or >delpred
    path get				\h'|\nmu'<get or >get
    path getpred			\h'|\nmu'<getpred or >getpred
    path gethook			\h'|\nmu'<gethook or >gethook
    path apply				\h'|\nmu'<apply or >apply
.sp 2
Hooks of both kinds are controlled by these atoms, initially t:
    *runallslothooks*  -- controls all slot hooks.
    *runallbasehooks*  -- controls all base hooks.
    *runputpathhooks*			\h'|\nmu'*runclearpathhooks*
    *runaddsetpathhooks*		\h'|\nmu'*rundelsetpathhooks*
    *runaddpredpathhooks*		\h'|\nmu'*rundelpredpathhooks*
    *rungetpathhooks*			\h'|\nmu'*rungetpredpathhooks*
    *rungethookpathhooks*		\h'|\nmu'*runapplypathhooks*
    *runmatchhooks*			\h'|\nmu'*runsmergehooks*
    *runindividualhooks*		\h'|\nmu'*runexpandedhooks*
    *runpatternhooks*			\h'|\nmu'*runnextitemhooks*
    *runfetchhooks*			\h'|\nmu'*runinsertdbhooks*
    *runremovedbhooks*			\h'|\nmu'*runindbhooks*
    *runnextequalhooks*			\h'|\nmu'*runstrequalhooks*
.DE
.PP
It is likely that hooks attached to a particular function would like to run
the same function in such a way that hooks will not be invoked.
Or in general, it is possible that you will want to run some PEARL function
in such a way that it is "hidden" from hooks.
To make this easy, a macro is provided called \fBhidden\fR which temporarily
sets the atom \fI*run...hooks*\fR to nil, runs a command and then restores
the former value of that atom.
For this to work correctly, you \fBmust\fR invoke the function you wish hidden
with the name corresponding to the "..." in its \fI*run...hooks*\fR atom.
Thus, you can hide the creation of an individual from hooks by executing:
.DS
(hidden (individual PTrans ....) )
.DE
(see Section 27 for the macro \fIindividual\fR) but \fBnot\fR by executing:
.DS
(hidden (create individual PTrans ....) )
.DE
A parallel function \fBvisible\fR temporarily sets the associated
atom to \fIt\fR before evaluating the function.
.PP
One of the reasons that hooks are checked for both before and after
a PEARL function does its job is to provide the user with the
opportunity to affect the result of the particular task.
In the simplest case, a hook simply executes a piece of code
and does not directly affect the function it is labelled with.
However, if the value returned by a hook is a list whose \fIcar\fR
is either \fB*done*\fR, \fB*fail*\fR, and \fB*use*\fR, then the action
of that function will be modified.
If the result of a hook which runs before the task starts with
\fI*done*\fR, then the hook is presumed to have accomplished what the
PEARL function was supposed to have done and the function will return
immediately with the \fIcadr\fR of the hook's result if there is
one, or else with the structure being operated on (for base hooks)
or the value in the slot (for slot hooks).
If the result of a hook which runs after the task starts with
\fI*done*\fR, then the function will return immediately with the
\fIcadr\fR of the hook's result if there is one, or else with
the result that was going to be return anyway.
.PP
If the result of a hook which runs before the task starts with
\fI*fail*\fR, then the hook is presumed to have determined that the
PEARL function should quit and the function will return
immediately with the \fIcadr\fR of the hook's result if there is one,
or else with the atom \fI*fail*\fR.
If the result of a hook which runs after the task starts with
\fI*fail*\fR, then the function will return immediately with the
\fIcadr\fR of the hook's result (which may be nil).
.PP
If the result of a hook which runs before the task starts with
\fI*use*\fR, then the hook is presumed to have determined that the
PEARL function should use a different value instead of the originally
provided one and the function will use the \fIcadr\fR of the hook's
result for the rest of the task.
If the result of a hook which runs after the task starts with
\fI*use*\fR, then the function will replace its intended result with
the \fIcadr\fR of the hook's result (which may be nil).
Thus, for example, a slot hook labelled with \fI<match\fR can
short-circuit the matching of a slot and one labelled with
\fI<match\fR can reverse the decision made by matching of a slot.
Similarly, a base hook labelled with \fI<match\fR can use its own matching
algorithm and one labelled with \fI>match\fR can modify the result of the
whole match.
.PP
Obviously, these all should be used with great care.
Note that \fIreturn immediately\fR means without even running
any other slot hooks on that slot for slot hooks or without
running any other base hooks on that structure for base hooks.
.PP
For example consider the case of a structure representing someone's
order in a Chinese restaurant.
As items are added to the order, it would be nice if there was a
magical slot TotalBill that contained the current
running total of the cost of the items ordered.
Demons, being such magical creatures, fill the bill nicely.
However, we only wish to have our demon-like hooks
activated when particular slots are filled (added to or accessed).
First consider the simple case in which an order consists of
three items only, the name of the soup and one or two entrees:
.DS
(create base  Chinese-Food-Entree
	(Name  lisp)
	(Price  int) )
.DE
.DS
(create base  Chinese-Dinner-Order
	(Soup  Chinese-Food-Entree)
	(Entree1  Chinese-Food-Entree)
	(Entree2  Chinese-Food-Entree)
	(TotalBill  int) )
.DE
.DS
(create individual  Chinese-Food-Entree
	(Name  (Hot And Sour Soup) )
	(Price  323) )
.DE
.DS
(create individual  Chinese-Food-Entree
	(Name  (Sizzling Rice Soup) )
	(Price  349) )
.DE
.DS
(create individual  Chinese-Food-Entree
	(Name  (Lingnan Beef) )
	(Price  399) )
.DE
.DS
(create individual  Chinese-Food-Entree
	(Name  (Mandarin Chicken) )
	(Price  367) )
.DE
.DS
(create individual  Chinese-Food-Entree
	(Name  (Shrimp Cantonese) )
	(Price  479) )
.DE
.DS
; an undetermined meal is created.
(create individual  Chinese-Dinner-Order Meal
	(Soup  ^  if  >put  (Maintain-Total  *  **  =TotalBill) )
	(Entree1  ^  if  >put  (Maintain-Total  *  **  =TotalBill) )
	(Entree2  ^  if  >put  (Maintain-Total  *  **  =TotalBill) )
	(TotalBill  0) )
.DE
Note that a slot hook is put after the value in a slot by using
the word \fBif\fR (or \fBhook\fR) followed by the appropriate label
for the invoking function followed by the function name or
s-expression to be evaluated.
Note also that when you want to put hooks on slots of an individual but
do not want to specify a value, the use of \fB"^"\fR will instruct
\fIcreate\fR to copy the default value instead.
If the Maintain-Total function is properly specified, whenever
one replaces one of the food slots with a real dish using
the \fIputpath\fR function, the Maintain-Total function would be
activated and would add the price of that meal to the running total
in the TotalBill slot.
If one changed one's mind a lot, it would be necessary to include
another hook Remove-Price which would be activated by a \fIclearpath\fR.
This would require adding the \fIif-cleared\fR hook
\fI"if >clear Remove-Price"\fR after the \fIif-put\fR hook:
.DS
(create individual  Chinese-Dinner-Order  ChangingMeal
	(Soup  ^  if  >put  (Maintain-Total  *  **  =TotalBill)
	          if  >clear  (Remove-Price  *  **  =TotalBill) )
	(Entree1  ^ if  >put  (Maintain-Total  *  **  =TotalBill)
		    if  >clear  (Remove-Price  *  **  =TotalBill) )
	(Entree2  ^ if  >put  (Maintain-Total  *  **  =TotalBill)
		    if  >clear  (Remove-Price  *  **  =TotalBill) )
	(TotalBill  0) )
.DE
The code for the two hooks follows:
.DS
(de Maintain-Total  (Food  Meal  CurrentMealTotal)
    (putpath  Meal  '(TotalBill)
	      (*plus  CurrentTotal
		      (getpath  Food  '(Price) ) ) ) )
.DE
.DS
(de Remove-Price  (Food  Meal  CurrentMealTotal)
    (putpath  Meal  '(TotalBill)
	      (*plus  CurrentTotal
		      (getpath  Food  '(Price) ) ) ) )
.DE
.PP
A more flexible meal order structure would not have three slots
for food, but rather a single slot of type \fIsetof struct\fR.
Then entries would be added by the \fIaddsetpath\fR functions,
and the \fIif-put\fR hook would be an \fIif-addset\fR hook but the
code would essentially be the same.
.PP
To attach a base hook to a structure, the first "slot" in its definition
must start with one of the atoms \fBif\fR or \fBhook\fR.
The rest of the slot must then contain a sequence of labels for invoking
functions and function names or s-expressions to be evaluated.
For example, to invoke \fIvalprint\fR before and a user function called
\fIverify\fR afterwards whenever a PTrans is inserted into the data base,
you would define PTrans as follows:
.DS
(create base  PTrans
	(if <insertdb (valprint * 5)
	    >insertdb (verify *))
	(* Actor  symbol)
	(   Object  symbol)
	(   From  symbol)
	(   To  symbol) )
.DE
.PP
Recall that PEARL provides a print function called \fBfullprint\fR
which for most structures seen so far printed two extra \fInil\fRs
in each slot.
If a slot has predicates, the first \fInil\fR will be replaced by
a list of them.
If the slot has hooks, the second \fInil\fR will be
replaced by a list of cons-cells with the invoking function in the
\fIcar\fR and the hook in the \fIcdr\fR.
.PP
The invocation of hooks labelled with other forms of \fIpath\fR are similar
except for \fIapply\fR.
If \fI(path <apply Fcn ...)\fR or \fI(path >apply Fcn ...)\fR is executed,
then any hooks which are labelled with Fcn will be run.
.PP
At this point the syntax of a slot in a definition or individual has become
quite complicated, so we summarize with the following BNF grammar:
.DS
{ a b c }	means select one of a, b, or c.
[ XXX ]		means optionally XXX.
XXX *		means zero or more XXX's
x | y		means x or y
.DE
.ID
<BaseSlot> ::=  (
		<HashLabels>
		<SlotName>
		<SlotType>
		<InheritOrValue>
		<AdjunctVariable>
		<PredicatesAndHooks>
		)
<IndividualSlot> ::=  (
		      <SlotName>
		      <InheritOrValue>
		      <AdjunctVariable>
		      <PredicatesAndHooks>
		      )
<ExpandedSlot> ::= <BaseSlot> | <IndividualSlot>
.sp 1
<HashLabels>  ::=  { "&" "^" "*" "**" ":" "::" ">" "<" } * 
<SlotType>  ::=  { "struct" "symbol" "int" "lisp" }  |
		 "setof" <SlotType>  |  <OrdinalName>  |
		 <StructureName>
<InheritOrValue>  ::=  <Value>  |  "^"  |  "nil"  |
                     "==" <Value>  |  ":=" <Value>
<Value> ::= <integer>  |  <atom>  |  <list>  |  <Variable>
<AdjunctVariable>  ::=   [ ":" <Variable> ]
<Variable>  ::=  ?<atom>
<PredicatesAndHooks>  ::=  { <Predicate>  |  <Hook> } *
<Predicate>  ::=  <StructureName>  |  <S-Expression>
<Hook>  ::=  "if" <atom> <HookFunction>
<HookFunction>  ::=  <atom>  |  <S-Expression>
.DE
.NH
Creating and Manipulating Multiple Data Bases
.PP
Without any effort on the user's part, a single data base of a
default size exists in PEARL when it starts up.
It is called \fB*maindb*\fR and is pointed to by the special
variable \fB*db*\fR which is assumed by all functions which use a
data base to point to the default data base (that is, the data
base to be used when an expected data base argument is missing).
.PP
To build another data base, choose a name for it and call the
function \fBbuilddb\fR which is an nlambda (fexpr) expecting
the name of the new data base.
You may build as many as you wish and store whichever one you want
in \fI*db*\fR.
.PP
Sometimes one may wish to clear out the data base and start out with a
clean slate.
To make this easy, there is a special function \fBcleardb\fR
which expects either zero or one data bases as arguments
and does the job.
If it receives no arguments, then the default data base is cleared.
\fICleardb\fR removes everything from the data base,
but does not actually delete (or reclaim the storage space of) the
objects within the data base.
But if the objects inside are not pointed to by any program
variables, they are gone for good.
(\fICleardb\fR clears out \fIonly\fR the named data base and not
data bases that it may be built upon as described in the next section.)
.PP
Data bases contain two parts, referred to as \fIdb1\fR and \fIdb2\fR.
\fIDb1\fR contains items which are indexed under only their type
or using single-colon hashing.
Its default size is 29.
\fIDb2\fR contains items which are indexed under two or three
values.
Its default size is 127.
These sizes are chosen to be prime numbers which are just barely
smaller than a power of two.
(This choice was made to take full advantage of hunks in Franz Lisp
which are always allocated to be a power of two.)
The ratio between the two sizes is approximately 1 to 4.
The size for data bases may be chosen by specifying the
power of two that you wish \fIdb2\fR to close to.
.PP
The function \fBsetdbsize\fR expects an integer between 2 and 13
representing the power to which two should be raised.
The default data base size is thus the result of calling
\fIsetdbsize\fR with an argument of 7.
To change the default size, you should call \fIsetdbsize\fR
in your \fI.init.prl\fR file, before creating any data bases of your
own.
\fISetdbsize\fR rebuilds \fI*maindb*\fR (without putting
anything into the new one) and releases all other data bases.
Thus, it should not \fInormally\fR be used at any time after the
processing of the \fI.init.prl\fR file.
(In the Franz Lisp version, although this full range of values is
accepted, the largest a data base in the 1 to 4 ratio can be
is 29 + 127 since hunks are limitted to 128 words.
However, an argument of 9 to \fIsetdbsize\fR will set the sizes
of both data bases to 127.)
Related special variables are \fB*db1size*\fR and
\fB*db2size*\fR which are set by \fIsetdbsize\fR and
\fB*availablesizes*\fR which contains the assoc-list used
to associate the power of two to a size.
.NH
Creating a Forest of Data Bases
.PP
Although having multiple data bases which are unconnected is often
enough, it is sometimes convenient to build onto an already
existing data base in a tree-like fashion.
For example, in a story understanding program, one might want
to have the default data base containing long-term knowledge
and then add a data base to contain the knowledge specific to a
particular story being processed.
In large applications, it can also help to split up special kinds
of knowledge to improve efficiency even more than PEARL's hashing
already does.
With only the ability to build separate data bases, searching for
a fact which might be either general knowledge or specific
knowledge learned from the story would require two fetches, one
from each data base.
However, if the story data base is built on top of the main data
base then simply fetching an item from the story data base will
also include fetching from the main data base.
To build another data base upon an existing one, use the function
\fBbuilddb\fR with two arguments, the name of the new data base
and the name of the old one to build onto:
.DS
(builddb  *story*  *maindb*)
(builddb  *future*  *maindb*)
.DE
These two statements will build two data bases on top of the main
one such that fetching from *story* will look both in it and in
*maindb* but not in *future*.
You can then build further upon any of these if you wish.
Note however, that the second argument must be \fIthe name of the
data base to build upon\fR and cannot be \fI*db*\fR to build upon
the default data base.
Also, if the second argument is missing, then the new data base is
isolated, not built on top of the default data base.
.PP
If your program builds many data bases, it is likely that some of
them will be temporary ones.
If this is so, it is possible to release a data base so that the
space can be garbage collected or reused for a later data base.
To release a data base, pass the actual data base (not its name)
to the function \fBreleasedb\fR.
If the data base is not a leaf of the data base tree, then the
space will not actually be released until all its children
are released also but PEARL will no longer accept it as a data
base argument.
.PP
A list of the names of the currently active data bases is
maintained by PEARL in the special variable \fB*activedbnames*\fR.
.NH
Creating Expanded Subtypes of Previously Defined Objects
.PP
Within CD, as in many applications, you may have many different structures
with some slots with the same name.
PEARL allows this, as it can always tell which type of structure
you are using, and thus it behaves just as if you had used
unique names for all slots.
But sometimes the fact that two different structure types have
slots with the same names is more than a coincidence:
there may be various semantic similarities
between the similar parts of the two structures.
PEARL has a mechanism for creating such structures using the
\fBexpanded\fR selector to \fIcreate\fR.
Basically, you must first define a base structure that contains
all the identical parts of two or more structures, and then you
must define the structures themselves as \fIthe base plus the differences\fR.
A good example of this from CD involves Acts.
All Acts within CD have an Actor slot, and all of
these slots have the same meaning.
That is, whatever is going on, the person in the actor slot is the
motivating force.
So we may first define this common part as a normal
base structure:
.DS
(create base  Act
	(*  Actor  symbol) )
.DE
and then we can define the various acts as expansions upon this base:
.DS
(create expanded  Act  PTrans
	(Object  symbol)
	(From  symbol)
	(To  symbol) )
.DE
.DS
(create expanded  Act  MTrans
	(MObject  struct)
	(From  symbol)
	(To  symbol) )
.DE
.DS
(create expanded  Act  ATrans
	(Object  symbol)
	(From  symbol)
	(To  symbol) )
.DE
.DS
(create expanded  Act  Injest
	(Object  symbol)
	(Through  symbol) )
.DE
Note that we did \fBnot\fR have to list the Actor slot,
it was \fBinherited\fR from the base structure Act.
The structure to be expanded need not be a base structure,
but could itself be an \fIexpanded\fR structure.
Thus we can capture the similarities of the various Transfers with:
.DS
(create expanded  Act  Trans
	(From  symbol)
	(To  symbol) )
.DE
followed by
.DS
(create expanded  Trans  PTrans
	(Object  symbol) )
.DE
.DS
(create expanded  Trans  MTrans
	(MObject  symbol) )
.DE
.DS
(create expanded  Trans  ATrans
	(Object  symbol) )
.DE
In expanded definitions as in base definitions one can
specify hashing and default information in the usual way.
However one can selectively inherit some of this
information from the structure being expanded.
Thus in our first Act example, since we specified star hashing on the
Actor slot, all the structures that we defined in terms of Act
have star hashing on their Actor slot by default.
If we had not wanted this for ATrans, we could have specified this
simply by listing the Actor slot over again without the asterisk.
However, since PEARL requires old slots in expanded structures
to also provide a new value, we need some way to say \fIinherit the
same old value\fR.
This is done by putting an up-arrow \fB"^"\fR where PEARL expects
to find a value, just as when you want to inherit the default
value but add hooks or predicates when creating individuals.
.DS
(create expanded  Act  ATrans
	(Actor  ^)
	(From  symbol) )
.DE
We also could have added colon hashing to the Actor slot by
listing it above as normal.
However, we cannot change the type of a slot and including a type
name after \fIActor\fR will cause PEARL to try to interpret that
type name as a value, (resulting in any of several errors,
depending on the type).
Thus, the hashing information for any slot is inherited from
above, \fIunless\fR it the slot appears in the expanded structure.
.PP
Default values are inherited in almost the same way.
The exception is that if in the original structure
the default is preceded by the symbol \fB":="\fR (rather than being
preceded by either nothing or the symbol \fB"=="\fR), expansions of that
structure will not inherit this value, but instead will get the
standard default for that type.
So if one defines:
.DS
(symbol  Pandora)
.DE
.DS
(create base  Act
	(Actor  symbol  Pandora) )

		or
.DE
.DS
(create base  Act
	(Actor  symbol  ==  Pandora) )
.DE
.DS
(create expanded  Act  PTrans
	(From  symbol) )
.DE
then all PTranses will have Pandora as their default Actor, whereas with:
.DS
(create base  Act
	(Actor  symbol  :=  Pandora) )
.DE
.DS
(create expanded  Act  PTrans
	(From  symbol) )
.DE
only the default instance of Act will have Pandora in its Actor
slot and the default Actor of PTrans will just be the usual
default for \fIsymbol\fR-valued slots which is \fInilsym\fR.
Which type of default inheritance to use depends upon the
application, and must be decided on a case by case basis.
.PP
Given this hierarchy, it is often useful to check whether an
object is of a certain type or an expanded version of it.
Two functions provide this ability with slightly different
arguments.
\fBIsa\fR expects an item and the name of the type you want to
check for.
\fBIsanexpanded\fR expects two instances.
Thus the following are always true for any structure X:
.DS
(isa  X  (pname X))
(isanexpanded  X  X)
.DE
Two related functions are \fBnullstruct\fR and \fBnullsym\fR which
are functions for testing for \fInilstruct\fR and \fInilsym\fR
(similar to \fInull\fR for \fInil\fR).
.NH
Fetching Expanded Structures
.PP
To make the extra information that \fIexpanded\fR structures provide
more useful, a special version of \fIfetch\fR called \fBexpandedfetch\fR
is provided which takes the hierarchy of structures defined into
account when fetching.
For example, using the above hierarchical
definitions of Act, Trans, PTrans, MTrans, and ATrans, you can insert
three different Transes into the data base:
.DS
(dbcreate individual  PTrans
	  (Actor  Pandora)
	  (Object  Pandora) )
.DE
.DS
(dbcreate individual  MTrans
	  (Actor  Pandora)
	  (To  Pandora) )
.DE
.DS
(dbcreate individual  ATrans
	  (Actor  Pandora)
	  (From  Pandora) )
.DE
and then to fetch all Transes performed by Pandora, you could use:
.DS
(create pattern  Trans  TransPattern
	(Actor  Pandora) )
.DE
.DS
(expandedfetch  TransPattern)
.DE
Once you start using expanded structures, you usually want to be
able to use the function name \fIfetch\fR and mean \fIexpandedfetch\fR. 
To this end, the standard fetch function is actually called
\fBstandardfetch\fR.
This leaves the function \fBfetch\fR to be bound to whichever
fetch function you wish.
It is initially given the same function definition as
\fIstandardfetch\fR.
.NH
How Two Objects Match
.PP
When a fetch from the data base is performed, the pattern provided
is only used to construct a stream containing that pattern and the
appropriate hash bucket from the data base;
no matching (comparing)
between the pattern and objects in the data base occurs.
Thus the stream contains pointers to all data base items in the
same hash bucket, regardless of their likelihood of
matching the pattern.
When elements are extracted from the stream with the function
\fInextitem\fR, the pattern is "matched" against successive
items from the hash bucket until one matches (and is returned)
or until the potential items run out (and \fInil\fR is returned).
.NH 2
When Is a Pattern Not a Pattern?
.PP
To understand the process with which two objects are
matched, it is necessary to understand what is meant by
a \fIpattern\fR in the context of matching.
The term \fIpattern\fR has been used in two ways in PEARL.
It has been used previously in this documentation in
a specialized sense which is only relevant in the context
of creating a \fIpattern\fR.
The use of the \fIpattern\fR selector to \fIcreate\fR is simply a
variation on \fIcreate individual\fR which uses the match-anything
variable ?*any* as the default for unspecified slots instead
of the usual default values (either the one inherited from the
base definition or the default for the type of slot).
It is called creating a \fIpattern\fR because the
change of default is usually only useful for constructing a pattern.
.PP
However, the use of the function \fIcreate\fR with object
selector \fIpattern\fR is \fBnot\fR the only way to create a
pattern which can be matched;
in fact, it is only useful for
forming simple patterns.
\fBAny\fR individual structure in PEARL can be used as a pattern.
If a fully specified structure (that is, one with an actual value
in all of its slots) is used as a pattern for fetching, it will
only match objects which are equal to it in a manner similar to
\fIequal\fR (versus \fIeq\fR) in Lisp.
(An exception to this occurs when patterns with pattern-matching
variables are stored in the data base.)
Thus a fully specified pattern is only useful for
determining whether a particular fact (object) is in the data base.
Any object is a pattern but the interesting patterns will not
be fully specified;
rather, they will have unspecified slots
which contain pattern-matching variables instead of values.
The details of the matching process will now be described.
.NH 2
The Matching Process
.PP
In general, the matching procedure takes two structures and either,
neither or both may contain pattern-matching variables.
So conceptually, both are patterns.
If the structures are not definitionally the same type
then the match fails automatically.
Otherwise, each structure is viewed as a sequence of slots
which are successively "matched" between the two structures.
Two structures of the same type match if and only if each of
their slots "matches" the corresponding slot of the other structure.
Each slot is of one of four types (\fIstruct\fR, \fIsymbol\fR, \fIint\fR,
or \fIlisp\fR), or is a \fIsetof\fR one of these types.
Regardless of its type, each slot is filled in one of four ways:
.IP (1)
The slot may contain an actual value of its type (for example,
a slot of type \fIstruct\fR may contain a PTrans).
.IP (2)
The slot may contain a variable which is local to the structure
(pattern-matching variables are local unless otherwise specified).
.IP (3)
The slot may contain a global variable, declared previously by a
call to the function \fIglobal\fR with the variable's name as argument.
.IP (4)
The slot may contain the special match-anything variable ?*any*.
.LP
If the slot contains a variable (other than ?*any*) which has not
been bound then it may become bound as a side effect of the
matching process.
All local pattern-matching variables are unbound at the start
of the matching process.
When a local variable is bound to a real
value during the matching process (it will never be bound to a
variable), it will not be unbound again but for the purposes of
matching will be treated as if the slot were filled with that value.
.PP
Let us now examine each of the pairings of slot values
which may occur and how they are matched.
If either of the two slots being matched contains the
special variable ?*any*, then the slots match by definition,
regardless of the contents of the other slot.
If both slots contain variables that are unbound, the slots
do not normally match, (even if the two variables are textually
the same name).
(Since some users want two unbound variables to match,
the value to be returned in this case is stored in the
special variable \fB*matchunboundsresult*\fR whose
initial value is \fInil\fR.
Setting this variable to non-\fInil\fR will cause two unbound
variables to match immediately but will not cause their
predicates to be run.)
If one slot contains an unbound variable (and the other
a bound variable or a value), then the predicates and
restrictions of the slot with the unbound variable are
tested, and hooks on that slot labelled
with \fImatch\fR are run to see if the unbound variable
should be bound to the bound value.
If so, then the unbound variable is bound to the value
of the other slot, and the two slots match.
Note that only the predicates and hooks on the
structure containing the unbound variable are run while
the symbols *, **, and =<slotname> refer to the other
structure (with the bound value in it).
If the predicates or restrictions return \fInil\fR,
the two slots do not match, the variable
is not bound, and the entire match fails.
.PP
If both slots contain either bound variables or values, then the values
of the two slots are compared.  If the slot is of type \fIstruct\fR,
then the entire matching algorithm is recursively applied.
If the slot is of types \fIint\fR or \fIlisp\fR,
then \fIequal\fR is used.
If the type is \fIsymbol\fR, then the two values must
be the same symbol.
Regardless of the type, restrictions associated with the slot
are executed until one fails or there are no more to run.
All must succeed for the match to succeed.
If the match succeeds, then any hooks
with the label \fImatch\fR are run.
.PP
The difference between the two types of variables is one of scope.
Normal variables (for PEARL) do not need to be declared, and
may be used in any structure by typing in \fI?<var>\fR during a
\fIcreate\fR (note that \fIputpath\fR is incapable of
installing variables).
The scope of these variables is only over the structure
in which they are typed.
Thus the variable \fI?V\fR typed into two different creations of
structures are in no way connected (in the same manner as two
local variables V in different Pascal subroutines are unrelated.)
If one becomes bound, the other is unaffected.
On the other hand, if a variable name is previously declared
as \fBglobal\fR:
.DS
(global  G)
.DE
then all instances of the variable name ?G are the same
(similar to global variables in Pascal).
The list of global variables is kept in the special variable \fB*globallist*\fR.
.PP
As mentioned before, when two structures are matched, all
normal (local) variables in both structures are unbound
(bound to the value \fI*pearlunbound*\fR) before any
slots are compared.
This is to ensure that any bindings induced by a previous
unsuccessful (or successful for that matter) match are removed.
This rule is useful because the type of matching that
early PEARL users have needed is in matching most
patterns against fully-specified values (that is, cases
in which one slot is always bound and the other either
bound or unbound).
Global variables are \fBnot\fR unbound before each match,
so they can be used to reflect global contexts.
They are given the value *\fIpearlunbound*\fR at the
time they are declared and remain bound thereafter unless
explicitly unbound by the user.
To unbind a global variable, you may use use the function
\fBunbind\fR, a fexpr which requires
the name of a (previously declared) global variable:
.DS
(unbind  G)
.DE
or use \fIsetq\fR and the function \fBpunbound\fR which
simply returns the atom \fI*pearlunbound*\fR:
.DS
(setq  G  (punbound) )
.DE
The function \fBpboundp\fR will test the value of a Lisp
(not PEARL) variable to see if it is \fI*pearlunbound*\fR.
The function \fBglobalp\fR will determine whether the variable
passed to it has been declared global.
.PP
Global variables should be used with care so that
they are not set by unsuccessful matches.
Generally this is achieved by first collecting the value
desired into a local variable via a series of matches
(only the last of which succeed), and then using the result of
this success to cause a further action which is guaranteed to
correctly bind the value of the global variable.
(These actions may be hooks which rebind the global
variable every time the local one is bound.
Effectively, this is a way to say \fIalways unbind this particular
global variable before matches\fR.
The action also could be performed by the user's program
when the right value is found.)
.PP
Each structure or tree of structures built by a call to \fIcreate\fR
constructs an individual assoc(association)-list of all the local
variables in that structure.
This assoc-list is stored with the root of the tree, thus
achieving local uniqueness of variables within a structure.
Global variables are bound values of the Lisp atom of
the same name and are accessed in the usual way.
To access the value of a local variable in a structure,
one uses either the function \fBvalueof\fR (which is an expr)
or the fexpr \fBvarvalue\fR both of which have two
arguments: the name of the variable whose value
you want and the structure it occurs in (evaluated internally by
\fIvarvalue\fR).
For example, to get the value of ?G in X, use either of:
.DS
(valueof  'G  X)
(varvalue  G  X)
.DE
Thus PEARL uses both deep and shallow binding.
.PP
The match algorithm is available to the user as a
separate function by the name \fBstandardmatch\fR.
This function unbinds all local variables before
proceeding with the match (using the macro \fBunbindvars\fR)
and again afterwards if the match failed.
A function which assumes that all local variables have been
unbound already and proceeds just as \fIstandardmatch\fR
would is \fBbasicmatch\fR.
The function name used to access the matching function by
\fInextitem\fR and all other built-in PEARL functions is
\fBmatch\fR which is normally given the same function definition
as \fIstandardmatch\fR but can be bound to whichever match function
you wish.
A function which compares two structures for equality without
affecting the values of their variables is available as
\fBstrequal\fR.
Since it does not bind variables, it also does not execute
predicates although it does run base hooks and slot hooks labelled
with \fIstrequal\fR.
A function parallel to \fInextitem\fR which uses \fIstrequal\fR
instead of \fImatch\fR is available as \fBnextequal\fR.
.PP
This rest of this section covers other ways to access and affect
the values of variables.
It will make more sense after reading the next section on blocks
but fits in better here so you should probably leave it for your
second reading.
.PP
Recall that the question mark read macro expands into either
\fI(*var* <varname>)\fR or \fI(*global* <varname>)\fR.
These two forms are not normally meant to be evaluated.
However, for convenience, there are two functions \fB*var*\fR and
\fB*global*\fR which return the value of the variable whose name
is their argument.
That is, if \fI?X\fR expands into \fI(*global* X)\fR, executing it
will returned the value of the atom X.
Thus \fIX\fR and \fI?X\fR are equivalent for a global variable.
For a local or lexically scoped variable, in which \fI?X\fR
expands into \fI(*var* X), the function \fI*var*\fR looks in
three places for a variable with the name \fIX\fR.
.IP 1.
First it looks to see if the special variable
\fB*currentstructure*\fR has been bound to a structure by
the user, and if so, looks in its variable list.
.IP 2.
If this fails, it looks in the special variable
\fB*currentpearlstructure*\fR for a structure.
This variable is set by various PEARL functions like
\fIcreate\fR, \fIfetch\fR, \fIpath\fR, and \fInextitem\fR
to the top level structure they last operated on.
.IP 3.
If this fails, it looks in the currently open block on
top of \fI*blockstack*\fR if there is one.
.IP 4.
If this fails, it returns \fInil\fR.
.LP
Note that the atom \fI*currentstructure*\fR is there simply for
the use of the user and is never set by PEARL.
.PP
A related function is \fBsetv\fR which takes a question-mark
variable, a value and an optional environment and sets that
variable in that environment or else in the default environment
described above to that value.
The environment can be either a structure or a block.
This stops with an error message if it fails to find a variable 
by that name in the specified or default environment.
.NH
Binding Blocks of Structures Together Via Common Variables
.PP
It is sometimes the case that you wish to create a group of
structures which are closely related in some way and which you
wish to tie together via pattern-matching variables.
For example, a \fIframe\fR might be considered such a loosely
connected group of structures.
In this case what is desired is for the pattern-matching variables
to \fIactually be the same\fR.
Normally however, if you create several structures in PEARL with
variables having the same name, each has its own local variable
with that name and they are totally unrelated.
If on the other hand, you declared them to be global, then all
structures having variables with that name would refer to the same
variable and it would no be unbound before matching.
For this purpose, PEARL provides variables of an intermediate
nature which are local to only a small group of structures and
which are all unbound before any one of the structures takes
parting in matching.
.PP
These variables are called \fBlexically scoped\fR (although if
the related functions \fIblock\fR and \fIendblock\fR are called
dynamically, they also provide a breed of dynamic scoping).
To declare a set of lexically-scoped variables, thus opening a
(nested) scope for them, use the function \fBblock\fR,
so named because of the similarity to the concept of a block
in Algol-like languages.
The function \fIblock\fR is a fexpr which in its simplest form
expects one argument which should be a list of new variables:
.DS
(block  (A  B  C))
.DE
Such a call to \fIblock\fR creates an unnamed block containing
these variables and any occurrences of variables with these
names in any structures \fIcreated\fR after this call will
refer to these lexically-scoped variables.
Thus, no structure created after the above call to \fIblock\fR
can contain a local variable called A, B, or C.
(However, if a variable has been previously declared to be global
this overrides \fBall\fR future declarations with \fIblock\fR.
Once again, global pattern-matching variables are to be
used with \fIextreme caution\fR.)
.PP
If you use several blocks, especially nested blocks,
it is helpful to give them names.
For this purpose, \fIblock\fR will accept two arguments, the first
an atom to name the block and the second the list of new variables.
For example:
.DS
(block  Name  (A  B  C))
.DE
.PP
To end the most recent block, use the fexpr \fBendblock\fR.
This function accepts any of three types of arguments.
If last block was unnamed, simply use:
.DS
(endblock)
.DE
If the last block was named, you must provide \fIendblock\fR
with this name:
.DS
(endblock  Name)
.DE
This is provided as a protection against unbalanced calls to
\fIblock\fR and \fIendblock\fR.
If you wish to end the most recent block, regardless of what
its name is, use
.DS
(endblock  *)
.DE
To end several blocks at once, you can use the fexpr
\fBendanyblocks\fR which ends all blocks back through
the one whose name matches its argument.
Again no argument (\fInil\fR) means the last unnamed block.
An argument of \fB"*"\fR causes PEARL to end all currently
open blocks.
A shorthand for \fI(endanyblocks *)\fR is \fB(endallblocks)\fR.
.PP
The function \fIblock\fR builds an assoc-list of
the variables listed.
If the block is nested, the assoc-list of the enclosing block is
hooked to the end of its assoc-list, thus providing a complete
assoc-list of all the variables available in the block.
A side effect of \fIblock\fR is that this assoc-list is bound to
the name of the block.
The block itself (the block's name plus this assoc-list) is available
as \fIb:<blockname>\fR so that the above call to block binds
\fIName\fR to
.DS L
((A  .  *pearlunbound*)  (B  .  *pearlunbound*)  (C  .  *pearlunbound*))
.DE
and \fIb:Name\fR to
.DS
(Name  (A  .  *pearlunbound*)  (B  .  *pearlunbound*)
       (C  .  *pearlunbound*))
.DE
If a block is unnamed, PEARL calls it \fIunnamedblock\fR and the
corresponding variables are set.
The special variable \fB*blockstack*\fR contains a stack of all the
currently active blocks.
The effect of ending a block is to pop it off this stack.
Once a block is closed, it is still accessible through the Lisp
variable \fIb:<blockname>\fR.
Given the name of a block, the function \fBblockatom\fR will build
this atom for you.
.PP
It is possible to return to the scope of an earlier block with the
fexpr \fBsetblock\fR which expects the name of a named block.
This will have the effect of ending all currently open blocks and
setting the current block stack to contain this block.
Note that this block will contain all the variables of any blocks
it is nested in but that it is not possible to close off these
block selectively.
Thus, the block stack will contain only one block with all the
variables in its complete assoc-list.
.NH
Controlling the Unbinding of Variables by Match
.PP
It is sometimes desireable to use the filled-in result pattern
of a \fIfetch\fR or \fImatch\fR as a pattern for a further
\fIfetch\fR (or \fImatch\fR) or to otherwise store and restore
the current values of variables (for example, to allow
backtracking algorithms and/or hypothetical assertions).
Since all bound local variables would normally be unbound during this
further fetching or matching, this would not be possible given the
mechanism described so far.
To accomplish this action, which can be considered as "pushing"
the context of the current assoc-list,
you should use one of several functions provided for this purpose.
The function \fBfreezebindings\fR takes a structure as argument
and moves all bound variables from its normal assoc-list to a
backup so that \fIfetch\fR will not unbind them.
The function \fBthawbindings\fR takes a structure as argument and
will undo this action, restoring the assoc-list to its complete state.
These two functions affect the structure plus any bound variables
in all enclosing blocks.
To freeze or thaw only a single structure, use \fBfreezestruct\fR
and \fBthawstruct\fR.
To freeze or thaw only a single block, use \fBfreezeblock\fR
and \fBthawblock\fR which expect the name of a block as an
argument.
.PP
Above it was mentioned that two structures will match if
and only if they both are of the same type.
Actually the system has been extended to allow the matching
of a structure of one type with another of a type derived
from the first via a \fIcreate expanded\fR.
The extra slots of the larger (expanded)
structure are ignored during the match.
.PP
Lastly it should be mentioned that the matching rules are
an evolving system, and may be amended as experience
with their use is accumulated.
The rules may seem a bit complex at first, but in use they
are fairly natural.
The rules are biased towards efficiency (like much of PEARL).
The designers felt that hiding exponential time-complexity
processing within the language would lead users to
construct inefficient programs without realizing it.
Thus several "features" of other complex AI matchers are not built in.
The user must implement these individually at a higher level.
It has been our experience that this leads to much cleaner designs.
.NH
Function Structures
.PP
In using PEARL, it is sometimes handy to escape into Lisp in
a "\fIstructure\fRd" way.
Although PEARL allows ad hoc escapes by way of its hooks
and the ! and $ evaluation operators defined above,
the philosophy in PEARL \fBfunction structures\fR
is to allow structured escapes that restrict the generality
of the escape to the minimum necessary for the task.
At times you may wish to equate Lisp functions with their expected
arguments with PEARL structures with their associated slots.
For example while you may wish to describe an action in a program
as fetching an item from the data base, you may actually be
unable to describe the item as a structure and/or be unable or
unwilling to actually store it in the data base.
Instead, you will sometimes want the value to be provided by
a function called at fetching time instead of a structure in the
data base.
.PP
Take as an example the case of keeping track of whether any two
objects are near each other.
One possible way to do this is to keep structures in the data base
which record for each pair of objects that are near each other the
fact that they are near each other:
.DS
(create base  Near
	(Object1  struct)
	(Object2  struct))
.DE
Then determining whether two objects are near each other would
require a simple fetch.
However, if you are dealing with a large number of objects which
are moving around quite a bit but only want to know about nearness
once in a while, it might be easier or more efficient to compute
whether two objects are near each other only on demand.
In this case, you might like to write a function called Near
which expects two arguments.
However, for consistency, you may not want to design your program
so that it knows what things can be fetched and what things need
computing.
So you would like to define a structure which looks like our
definition of Near above but which actually invokes the
function Near.
.PP
To do this, one may create the function Near (which must be an
expr) and also a structure of type \fIfunction\fR named Near:
.DS
(de Near  (x  y)
    ... mechanism to actually determine nearness ... )

(create function  Near
	(Object1  struct)
	(Object2  struct))
.DE
and then can create an individual of it for fetching:
.DS
(create individual  Near  IsNear
	(Object1  John)
	(Object2  Office))

(fetch  IsNear)
.DE
Note that the format of function structures within PEARL
is the same as that of structures.
However, the name of the actual Lisp function to be called must
match the type name of the \fIfunction\fR structure, and the
arguments must occur in the same order and be of the same types
as the slots which will contain the actual arguments to the function.
.PP
As another simple example, to define a \fIfunction\fR structure
to correspond to the function \fIgetpath\fR, we would use the following:
.DS
(create function  getpath
	(Item  struct)
	(Path  lisp) )
.DE
and then an actual instance:
.DS
(create individual  getpath  Minst
	(Item  !  Mtrans1)
	(Path  '(MObject) ) )
.DE
This example is not too useful.
As a more realistic use, consider a program to return all
the MObjects of all MTranses that are in the data base:
.DS
(create function  nextitem
	(Stream  lisp) )
.DE
.DS
(create pattern  MTrans  MPat1
	(MObject  ?X) )
.DE
.DS
(global  MStream)
(setq  MStream  (fetch  MPat1) )
.DE
.DS
(create individual  getpath  Minst2
	(Item  (nextitem  (Stream  ?MStream) ) )
	(Path  '(MObject) )
.DE
.DS
(setq  Stream1  (fetch  Minst2) )
.DE
Note the recursive use of the data base: the \fIfetch\fR of
Minst2 will cause a \fIgetpath\fR to be executed.
But PEARL must first get the two arguments to pass on to
\fIgetpath\fR which causes the function \fInextitem\fR
to be evaluated, getting the next MTrans in MStream to
pass to \fIgetpath\fR. 
.PP
Thus, function structures provide a way to describe a function and
its arguments through a PEARL structure and then to include,
in a pattern to fetch or in a structure slot,
a function call which will provide the desired value
at fetching time.
However, this only works during fetching.
.PP
The function used by PEARL to execute a function
structure is \fBevalfcn\fR.
It takes an item as its argument and returns the result of
applying the associated expr to its slot values if the item
is a function structure.
If the item is a single structure it returns the item untouched.
If the item is a list of structures, it applies itself
recursively with \fImapcar\fR.
No other PEARL functions currently know about function structures
as being any different than other individual structures.
.NH
More About the PEARL Top Level Loop and History Mechanism
.PP
The PEARL prompt-read-eval-print loop includes two features which
make PEARL easier to work with than the usual top level of Lisp.
Both features were designed in imitation of the Berkeley Unix
shell program \fIcsh\fR.
.PP
The first is an aliasing mechanism which provides the ability to
use various atoms as aliases for commonly executed s-expressions.
If you type an atom to the top level and it has the property
\fBalias\fR, the value of its \fIalias\fR property will be
evaluated instead.
Thus, if you do a
.DS
(putprop  'dir  '(dir)  'alias)  ; in UCI Lisp
	  or
(putprop  'ls  '(exec  ls)  'alias)  ; in Franz Lisp
.DE
then if you type the atom \fIdir\fR or \fIls\fR repectively
to the top level, you will get the contents of your
directory printed out.
Two such built-in atoms are \fBhistory\fR which will
run the function \fIhistory\fR and print out your last
64 commands (see below) and \fBh\fR which will print the last 22
commands (one crt screenful).
The aliasing mechanism can be turned off (saving a \fIget\fR for
each atom you use at the top level) by setting the special
variable \fB*usealiases*\fR to \fInil\fR.
.PP
PEARL's top level also includes a simplified command-history mechanism.
As you type in expressions to the top level of PEARL, they are
stored away for future reference.
The results of evaluating each expression are also kept.
The commands and their results are kept in two hunks
whose default size is 64.
The hunk containing the commands is kept in the special
variable \fB*history*\fR and the hunk containing the results
is kept in the special variable \fB*histval*\fR
To change the number of commands remembered, set the special
variable \fB*historysize*\fR to something other than 64
in your \fI.init.prl\fR.
It cannot be changed later.
(If you are a novice user of PEARL, we recommend that you not
change it to be smaller, since the history command can sometimes
be helpful to someone helping you to debug something after you
have fiddled with it a while.)
.PP
The commands you type are squirrelled away so that you can ask
PEARL to re-execute them, thus saving the pain of retyping
a complicated expression.
To access the previous commands, the readmacro \fB"!"\fR is
provided.
To access the results of the previous commands,
the readmacro \fB"$"\fR is provided.
(The exclamation point is in imitation of the cshell;
the dollar sign is meant to suggest "value".)
These readmacros peek at the next character to determine what to do.
We discuss the variations available on these two readmacros in
parallel, since many of them coincide.
.PP
The simplest and most useful forms are \fB"!!"\fR and \fB"$$"\fR
which effectively re-execute and reprint the last command or its result.
Actually, both forms are executed, but the dollard sign macro
always returns its value quoted so that its effect is usually to
just reprint the result of the previous command.
Note that since these are readmacros which simply return the
last s-expression typed or its value, you can use them to build up
more complex commands.
For example:
.DS
pearl>  (fetch  Item)
    (*stream:*  . . .)
pearl>  (nextitem  !!)
.DE
will cause the fetch to be repeated and then do a \fInextitem\fR on it.
However, it is much more efficient to use the \fI$$\fR form in
this case, since what you really want is to do a \fInextitem\fR
on the result of the \fIfetch\fR in the last command:
.DS
pearl>  (fetch  Item)
    (*stream:*  . . .)
pearl>  (nextitem  $$)
.DE
.PP
The commands are numbered as you type them, starting with zero.
Although the values wrap around in the hunks, the \fIhistory number\fR
continues to climb.
The current history number is available in the special
variable \fB*historynumber*\fR.
To access a particular command or its value, you may type you may
follow an exclamation point or dollar sign with the number of the
command.
Thus \fB!23\fR and \fB$23\fR are the 23rd command and its result.
If you don't remember the command's number you can use the
function name or a prefix of it.
Thus \fB!fetch\fR and \fB$fetch\fR will access the last \fIfetch\fR
or its value.
Or \fB!fe\fR and \fB$fe\fR will access the last command starting
with \fIfe\fR or its value.
If there was a reference to an atom (instead of a list) with that
name or with that as a prefix somewhere, then the atom will be
evaluated again.
For exclamation point, this is a waste of typing except for long
atom names.
For dollar sign, it provides you a way of recovering the value of
a variable that has since changed.
(As a side effect of implementing this, PEARL contains a function
\fBprefix\fR which expects two lists and determines whether the
first is a prefix of the second, considered as a list of atoms.
Thus, PEARL just calls \fIprefix\fR on the results of \fIexplode\fRing
two atoms.)
.PP
Here the parallel between the two macros ends.
.PP
There are five forms which work only with exclamation point and
refer only to the last s-expression typed.
They are essentially ways to pick individual top-level elements
out of the last command:
.DS
\fB!^\fR	the first argument
\fB!$\fR	the last argument
\fB!*\fR	the complete set of arguments
\fB!:0\fR	the function name
\fB!:n\fR	the nth argument
.DE
Both macros are splicing macros so that their values may be
spliced into the current s-expression.
\fB!*\fR is designed so that the following will work:
.DS
pearl>  (add  1  2  3  4)
    10
pearl>  (times  !*)
(times  1  2  3  4)
    24
.DE
.PP
To see the last 64 commands you gave printed out, use the function
\fBhistory\fR (or type the atom \fBhistory\fR).
If you don't want all 64 commands, \fIhistory\fR will accept an
integer argument telling how many you want.
Thus the aliases on \fIhistory\fR and \fIh\fR are:
.DS
(putprop  'history  '(history)  'alias)
(putprop  'h  '(history  22)  'alias)
.DE
If you use the command numbers often, you might like to have the
history number printed out before each command.
To have the history number printed just before the PEARL prompt,
set the special variable \fB*printhistorynumber*\fR to a
non-\fInil\fR value.
The default value is f\Inilf\R.
.PP
Whenever you use the ! or $ history mechanisms, the line you type in
will be reprinted in its expanded form on the next line using
the current \fIpearlprintfn\fR.
If you wish to modify your own read macros so that they also will
cause this reprinting, simply have them set the special
variable \fB*readlinechanged*\fR to a non-\fInil\fR value.
.PP
It is sometimes useful to have a function return no value.
That is, you often do not want the value of the function to be
printed by the top level loop.
In particular, functions which print values often return ugly
values afterward.
To get around this problem, the PEARL top level disables printing
of the value returned by a function if it returns the atom
\fB*invisible*\fR.
All of the PEARL print functions return this value.
.PP
It is sometimes useful to be able to save the current state of a
PEARL run for later.
There are two functions to allow this.
If you wish to save a version which will continue exactly where
you left off (at the top level), use the function
\fBsavecontinue\fR which expects zero, one or two arguments.
If you wish to save a version which will read in
the \fI.start.prl\fR file when it starts up, use \fBsavefresh\fR.
(If you also want \fI.init.prl\fR read in, change the value of the
special variable \fB*firststartup*\fR to \fIt\fR beforehand but
be careful not to put functions which may only be run once in it.)
Note however that you cannot save Franz PEARL on top of the file
you are running;
trying to will result in the \fIDumplisp failed\fR
error message from Franz Lisp.
Note also that a saved PEARL uses about 1500 blocks or 750kbytes on
the disk so this should be used sparingly.
(Exceeding the disk quota will result in the same error message.)
In the Franz Lisp version, if the number of arguments to either of
these functions is:
.IP 0:
It will be saved as \fIpearl\fR in the current directory.
.IP 1:
The argument is assumed to be a (relative) file name to save under.
.IP 2:
The result of concatenating the two arguments together with a
\fB/\fR between them will be the file name used.
(This is for UCI Lisp compatibility.)
.LP
In the UCI Lisp version, if the number of arguments is:
.IP 0:
It will be saved as \fIpearl\fR in the current directory.
.IP 1:
The argument is assumed to be a file name for the current directory.
.IP 2:
They must be a directory and a file name to save in.
.NH
Looping and Copying Functions
.PP
PEARL includes several loop macros.
The first two were included simply for use by the implementation but
might be useful to the user.
They are the \fBfor\fR and \fBwhile\fR macros which both expand
into a \fIprog\fR wrapped around a \fIprogn\fR.
A call to the \fIwhile\fR macro should be of the form:
.DS
(while <test>
       EXPR1
       EXPR2
       ...
       EXPRn)
.DE
The <test> is evaluated before each execution of the loop.
If it is non-\fInil\fR, the EXPRi are evaluated in sequence.
This continues until <test> return nil in which case the last
value returned by EXPRn is returned.
Since the while expands into a \fIprog\fR, any of the EXPRi may
call the function \fIreturn\fR, terminating the loop prematurely
and returning the value given to \fIreturn\fR.
.PP
A call to the \fIfor\fR macro should be of the form:
.DS
(for <var> <initial> <final>
     EXPR1
     EXPR2
     ...
     EXPRn)
.DE
<initial> and <final> should evaluate to integers.
The EXPRi are repeatedly evaluated in sequence with <var> being
set to the values ascending from <initial> to <final>.
If <initial> is greater than <final>, nothing is done.
<var> is a prog variable which disappears after the \fIfor\fR
executes.
The value returned is the last value of EXPRn and \fIreturn\fR
provides a premature exit with a value as in \fIwhile\fR.
.PP
The fexpr \fBforeach\fR expects a stream and a function (or macro)
and applies the function to each element returned by successive
calls to \fInextitem\fR on the stream.
Unfortunately it only returns \fInil\fR at this time.
Eventually, other useful looping structures may be provided.
.PP
Since PEARL provides several new types of values, it provides a
few functions to copy them.
In particular, the standard Lisp function \fBcopy\fR has been
redefined to avoid trying to copy anything that is not a cons-cell.
There are several ways to copy structures, described below.
The rest of PEARL values either are too complicated to copy
(data bases), can be copied with \fIcopy\fR (streams) or else
make no sense to copy (symbols, blocks).
.PP
For copying structures, there are currently two functions.
The one you are most likely to want is \fBscopy\fR which expects a
single structure argument and returns a new structure with the
same values in it.
However, the new structure will differ from the old in several
important ways.
First of all, copying a bound variable will result in the actual
value being inserted in the new copy.
When copying an unbound variable, the new structure will receive
a local variable with the same name and this variable will
be installed in the slot.
All variables so installed will be installed in the top level
structure regardless of where they came from in the original.
The only exception to this is lexically-scoped variables.
When the new structure is built, it will be built within any
currently open blocks and any of its unbound variables whose names
match variables from the current block(s) will be identified with
those block variables.
Global variables are similarly reinstalled only if they are unbound.
Adjunct variables are also installed \fIonly if\fR they are
unbound, since if they are bound their purpose will already have
been served and their bound values installed in other slots
referring to them.
.PP
A variation on \fIscopy\fR which replaces all unbound
variables from the original with \fI?*any*\fR is called
\fBpatternize\fR.
After (and during) the running of these copying functions, the
resulting top-level structure is kept in the special variable
\fB*currenttopcopy*\fR.
.PP
The situation sometimes arises where you have already built a
structure and have a new structure with information that should be
merged into the old one.
Rather than use \fIpath\fR to copy each relevant slot, you can use
\fBsmerge\fR which expects as arguments the old structure to merge
into and the new structure from which to take values.
All unfrozen variables in the old structure are unbound first and
then any unbound variable whose counterpart in the new structure
is bound gets replaced (\fBnot set\fR) with this value.
The old structure being merged into must be of the same type or
an expanded version of the new structure.
.NH
Miscellaneous Variations and Abbreviations
.PP
People very quickly get tired of typing the relatively long
function names that PEARL uses.
As a result, a large number of abbreviations and macros have
been included in PEARL.
We recommend that the shortest ones be used primarily at
the top level, since they are easily subject to typographic
errors.
Most the abbreviations are in \fIcreate\fR and are summarized by
the following table:
.DS
   The function or atom:       May \kmbe abbreviated:
	create			   \h'|\nmu'cr
	individual		   \h'|\nmu'ind
	pattern			   \h'|\nmu'pat
	expanded		   \h'|\nmu'exp
	function		   \h'|\nmu'fn
.DE
Thus, \fI(cr pat ....)\fR is equivalent to
\fI(create pattern ....)\fR.
.PP
In addition, a large number of macros for popular combinations of
functions are included:
.ID
    The s-expression:               Is exp\kmanded into by the macro:
(create base ...)		\h'|\nmu'(cb  ...)
				\h'|\nmu'(base  ...)
(create individual ...)		\h'|\nmu'(ci  ...)
				\h'|\nmu'(individual  ...)
				\h'|\nmu'(ind  ...)
(create expanded ...)		\h'|\nmu'(ce  ...)
				\h'|\nmu'(expanded  ...)
				\h'|\nmu'(pexp  ...)
(create pattern ...)		\h'|\nmu'(cp  ...)
				\h'|\nmu'(pattern  ...)
				\h'|\nmu'(pat  ...)
(create function ...)		\h'|\nmu'(cf  ...)
				\h'|\nmu'(pfunction  ...)
				\h'|\nmu'(fn  ...)
.sp 1
(insertdb  (create  ...)  nil)	\h'|\nmu'(dbcreate  ...)
				\h'|\nmu'(dbcr  ...)
`(quote  ,(create  ...))	\h'|\nmu'(inlinecreate  ...)
(fetch  (create  ...) nil)	\h'|\nmu'(fetchcreate  ...)
`(fetch  (quote  ,(create  ...))  nil)	\h'|\nmu'(inlinefetchcreate  ...)
(nextitem  (fetch  ...) )	\h'|\nmu'(firstfetch  ...)
.sp 1
(valprint  ...)				\h'|\nmu'(vp  ...)
(fullprint  ...)			\h'|\nmu'(fp  ...)
.DE
(\fIpexp\fR and \fIpfunction\fR are so named to avoid conflict
with the exponential function \fIexp\fR and the function quoting
function \fIfunction\fR.)
.PP
The automatic setq feature of \fIcreate\fR that causes an atom
to be bound to the item created is available throughout
\fIcreate\fR.
In all cases, the special variable \fB*lastcreated*\fR is
set to the item.
In addition:
.DS
This combination:          Causes \kmthis atom to be set:
(create base  X ...		\h'|\nmu'X
(create base  X Y ...		\h'|\nmu'Y
(create expanded  X Y ...	\h'|\nmu'Y
(create expanded  X Y Z ...	\h'|\nmu'Z
(create individual  X ...	\h'|\nmu'(none)
(create individual  X Y ...	\h'|\nmu'Y
(create individual  X X ...	\h'|\nmu'(none, the second X is ignored)
(create pattern  X ...		\h'|\nmu'(none)
(create pattern  X Y ...	\h'|\nmu'Y
(create pattern  X X ...	\h'|\nmu'(none, the second X is ignored)
.DE
.PP
When creating an object, wherever a recursive call to \fIcreate\fR
is implied by a structure in a slot of type structure, you may start
with one of the types \fIindividual\fR, \fIpattern\fR, \fIbase\fR,
\fIexpanded\fR, \fIfunction\fR to change the type of object
being created.
Whenever it isn't given, the type of the toplevel \fIcreate\fR,
which is kept in the special variable
\fB*currentcreatetype*\fR is used.
For example, in
.DS
(create pattern  x
	(a  (individual  y))
	(b  (base  z  (s1  ...)  ...))
	(c  (w)))
.DE
where a, b, and c are all slots of type structure, slot a
will contain an individual y which the attendant defaults
filled in, slot b will contain the default instance of a
newly created type z, and slot c will contain a pattern w
with \fI?*any*\fR as defaults.
.PP
Since each Lisp stores its functions in a different place, PEARL
includes a macro \fBaliasdef\fR which expects the names of an new
and a old function name and copies the function definition of the
old one to the new one.
In the case of Lisps which store the function definition on the
property list, \fIaliasdef\fR requires a third argument which is
the name of the property that the definition is kept under.
.NH
Low Level Access Functions.
.PP
There are a large number of functions for setting and accessing
the various part of structures, symbols, and data bases which are
primarily intended for the use of PEARL.
In general, the access functions are called \fBget...\fR where
"..." is the name of the information about the structure.
The functions which change information are called \fBput...\fR.
It is not generally safe to use the \fIput...\fR functions but the
\fIget...\fR functions can sometimes be useful to the user.
For a complete list of the functions, see the index.
If you don't recognize the function by name, you don't need it so
we don't bother to further document them.
Since most of them expect a slot number, it is useful to know
about the macro \fBnumberofslot\fR which requires the name of a
slot and the definition of a structure (which can be accessed
with \fIdefatom\fR or \fId:<structurename>\fR.) and returns the
corresponding slot number.
.bp
.NH
Appendix of UCI Lisp functions added to Franz PEARL
.PP
Since PEARL was originally written in UCI Lisp, there are many functions
from UCI Lisp that it needed.
We also wrote others to move our other programs.
The number is too great to document each one.
If the function is described with an equal sign, as in
\fI"fn = other"\fR then the function definition of the Franz Lisp
function \fIother\fR has been put under \fIfn\fR.
Thus it might not behave quite the same as in UCI Lisp.
If no equivalence is given, it was written from scratch which is
slightly more likely to mimic UCI Lisp.
In this case, see the UCI Lisp manual for details.
.PP
The functions used for the PEARL top level loop in the Franz Lisp
version plus changes to the fixit debugger and the trace package
are briefly described here also.
.PP
The Franz Lisp version of PEARL is normally loaded with both the Fixit
debugger and the trace package already loaded.
This is done to avoid getting the versions which do not know how to print
PEARL objects.
In addition, the Fixit debugger is attached to all available hooks for
going into the break package, since it is much more similar to the UCI Lisp
break package than the standard Franz Lisp break package is.
Both the debugger and trace package use the function
\fBbreakprintfn\fR to print values.
The \fImsg\fR function uses the function \fBmsgprintfn\fR
to print values.
Either can be bound to whatever function you wish.
To disengage the Fixit debugger, read the Franz manual chapter on exception
handling.
See Note 4 below for more on features added to the Fixit debugger.
.LP
.nf
Atoms and Variables:
*dskin* -- special variable -- initial value: t.  See Note 1 below.
*file* -- special variable -- initial value: nil.  Used by \fIdskin\fR
       and function definition functions.
*invisible* -- special atom -- not printed by \fIdskin\fR if returned
	    by a value when it is evaluated.

Functions:
*append = append
(breakprintfn value lmar rmar) -- used by \fItrace\fR and \fIdebug\fR.
*dif = diff
*eval = eval
*great = greaterp
*less = lessp
*max = max
(msgprintfn value lmar rmar) -- used by \fImsg\fR.
*nconc = nconc
*plus = plus
*times = times
(addprop 'id 'value 'prop)
(allsym itemorpair) -- fexpr
(apply* 'fcn 'args) -- macro -- This is provided to act like UCI Lisp's
	\fIapply#\fR.  The asterisk is used because of the special meaning
	of # in Franz Lisp.  Unlike Franz Lisp's \fIfuncall\fR and
	\fIapply\fR, this does what you would expect with macros!
atcat = concat
(boundp 'item)
clrbfi = drain
consp = dtpr
(de fcnname arglist &rest body) -- macro -- See Note 2 below.
(debug-replace-function-name 'cmd 'frame) -- Used by the modified
	Fixit debugger to handle the "> newfcnname" facility.
(defp 'to 'from [prop]) -- macro -- Ignores \fIprop\fR and just
      copies the function definition.
(defv var val) -- fexpr
(df fcnname arglist &rest body) -- macro -- See Note 2 below.
(dm fcnname arglist &rest body) -- macro -- See Note 2 below.
(dremove 'elmt 'l)
(drm char lambda) -- macro -- See Note 2 below.
(dskin filename1 filename2 ....) -- See Note 1 below.
(dskin1 '*file*)
(dskin2 'port)
(dsm char lambda) -- macro -- See Note 2 below.
(enter 'v 'l)
(every 'fcn 'args) -- macro -- Potential problem when compiled.
expandmacro = macroexpand
(funl &rest body) -- macro -- Expands into (function (lambda ...)).
(ge 'x) -- macro
(gensym1 'ident 'val)
gt = >
(initsym atomorpair1 ...) -- fexpr
(intersection 'set1 'set2)
(islambda 'fcn) -- Is \fIfcn\fR a lambda (expr)?
(le 'x) -- macro
(length '*u*)
lineread = readl (below)
(litatom 'x) -- macro
lt = <
mapcl = mapcar
memb = member
(msg ...) -- macro -- Some features may be missing.  The function
	  used to print is \fImsgprintfn\fR, initially bound to 
	       (or (eq '*invisible* ...)
	           (patom (valform ...)))
(nconc1 'l 'elmt)
(nequal 'arg1 'arg2)
(newsym atom) -- fexpr
noduples = union (below)
(nth 'l 'num)
(oldsym atomorpair) -- fexpr
(pearl-break-err-handler) -- Should be tied to ER%tpl if you want the
	standard Franz Lisp break (not much of a) package.
	Same as standard Franz Lisp \fIbreak-err-handler\fR except
	that it uses the function \fIbreakprintfn\fR.
(pearl-top-level) -- The PEARL top level loop.
(pearl-top-level-init) -- The initial function called when PEARL starts up.
	This is the code that reads in the init files and sets any unset
	PEARL parameters.
peekc = tyipeek
(pop q) -- macro
(push var 'val) -- macro
(readl ['flag]) -- fexpr
(readl1 'flag)
remove = delete
(remprops 'item 'proplist)
(remsym atomorpairlist) -- fexpr
(save fcnname) -- fexpr -- Saves function or macro definition under 
	the property \fIolddef\fR.  Saves macro character definitions
	under \fIoldmacro\fR.
(selectq ...) -- macro
(some 'fcn 'list) -- macro -- Potential problem when compiled.
(sprint 'item ['lmar ['rmar]]) -- See Note 3 below.
(subset 'fcn 'list) -- macro
(timer (defun timer fexpr (request)$?
(unbound) -- macro
(union 'list1 ['list2 ...])
(unsave fcnname) -- fexpr -- See \fIsave\fR.
.fi
.PP
\fBNote 1:\fR A simplified but extended imitation of the UCI Lisp function
\fBdskin\fR is provided in PEARL.
It is an nlambda which requires the file extensions to be provided.
There is a special variable \fB*dskin*\fR which controls whether
the expression read in is printed and/or whether the result of
evaluating it is printed.
.DS L
*dskin* = nil	means neither
*dskin* = t	means result only
*dskin* = 'name	means the name of the variable in setq \fIor\fR the name
		of the function in de, df, dm, dsm, drm, defmacro,
		defun, or def \fIor\fR the name of the type in create.
*dskin* = 'both	means both t and 'name.
.DE
The default value of *dskin* is t.
.PP
File names are always printed before they are opened.
The print function used for values is the current function
definition of \fBdskprintfn\fR.
The default function definition in PEARL is:
.DS
(de dskprintfn  (*printval*)
    (cond  ((atom  *printval*)  (patom  *printval*))
	   (  t  (print  (valform  *printval*)))))
.DE
.PP
\fBNote 2:\fR  For better compatibility with UCI Lisp, PEARL contains
macros for the function and read macro definition functions 
\fBde, df, dm, dsm,\fR and \fBdrm\fR.
They have been defined to save the old definitions automatically
and to return \fI(fcnname Redefined)\fR when this is the case.
\fIDe, df,\fR and \fIdm\fR save the old definition under the
property '\fIolddef\fR.
\fIDsm\fR and \fIdrm\fR save the old definition under the
property '\fIoldmacro\fR.
(The current definition of a readmacro is kept by Franz under the
property '\fImacro\fR.)
If the function definition is read in by \fIdskin\fR,
then the current file name which is in the special variable
\fB*file*\fR is put under the property '\fIsourcefile\fR.
.PP
\fBNote 3:\fR  A function similar to the UCI Lisp \fBsprint\fR is included,
including the printmacro facility and the optional second argument
saying which column to start in.
In addition, there is an optional third argument saying which column
to try not to go beyond (that is a right margin).
A slight addition has been made to the printmacro feature (feature 1 below).
During \fIsprinting\fR, if the atom in the function position in a list
has the printmacro property one of four things will happen during
\fIsprinting\fR:
.IP 1.
If the printmacro property value is a string and the item to be
printed has a nil \fIcdr\fR, then the string will be printed instead
of the item.
.IP 2.
If the printmacro property value is a string and the item to be
printed has two items in it, then the string will be printed followed
immediately by the \fIcadr\fR of the item.
.IP 3.
If the printmacro property value is a string but the item to be
printed is longer than two elements, then it will be \fIsprinted\fR in
the normal fashion (i.e., the printmacro will be ignored).
.IP 4.
Otherwise, the printmacro property value will be applied
to the rest of the arguments.
It should be a function which expects three arguments, the item
to be printed, a left column to start in and a right column to
try not to go beyond.
A good default value for the right column argument seems to be zero.
If the function under the printmacro property returns nil,
then \fIsprint\fR assumes that it decided not to print the item
and prints it in the usual way.
.PP
\fBNote 4:\fR  The Fixit debugger now accepts a command of the
form \fB>  newname\fR whenever either an undefined function or
unbound variable error occurs.  As in UCI Lisp, newname is not
evaluated in the case of an undefined function but is evaluated
in the case of an unbound variable.
Note that the blank is required (unlike UCI Lisp).
This is not guaranteed to work if you move around the stack first.
.bp
.NH
Appendix of Franz Lisp functions added to UCI Lisp PEARL
.PP
The following is a summary of the functions added to the UCI Lisp
version of PEARL to make it compatible with Franz Lisp.
Where the details are not obvious, see the Franz Lisp manual.
\fBNote:\fR Most \fImacros\fR listed in the index which are
labelled with asterisks are not available in UCI Lisp PEARL, since
the implementor must specifically request that they stick around.
.PP
\fIDskin\fR, the break package, and \fImsg\fR have been changed
to use the functions \fBdskprintfn\fR, \fBbreakprintfn\fR,
\fBmsgprintfn\fRfor printing.
.LP
.nf
(addtoaddress 'n 'address) -- expr -- Used by \fIcxr\fR and
	\fIrplacx\fR.  Written in LAP code.
(apply* 'fcn 'args) -- macro -- Equivalent to \fIapply#\fR.
(buildalist ...) --- expr --- Used by \fIdefmacro\fR.
(combineskels ...) -- expr -- Used by \fIquasiquote\fR.
(convert ...) --- expr --- Used by \fIdefmacro\fR.
(cxr 'index 'hunk) -- expr -- A hunk is a block of memory.  Provides
	random access to a single cell of a hunk.  (Uses
	\fIaddtoaddress\fR and \fIeven\fR.)
(defmacro macroname arglist body) -- macro -- \fIDefmacro\fR provides
	a slightly more intelligent macro facility.  \fIBody\fR is
	processed to look for occurrences of the arguments in
	\fIarglist\fR which are replaced with the appropriate form
	of \fIca..r\fR.  If an argument is preceded by \fI&rest\fR,
	then it gets the list of the rest of the arguments.
	The Franz Lisp version has many more features not included
	in the PEARL version.
(even 'x) -- expr -- Is \fIx\fR even? Used by \fIcxr\fR and
	\fIrplacx\fR to determine which half of a cons-cell to use.
(isconst ...) -- expr -- Used by \fIquasiquote\fR.
(makhunk 'size) -- expr -- Calls the UCI Lisp function \fIgetblk\fR,
	requesting a block of memory which is half of \fIsize\fR, since
	each piece of a UCI Lisp block of core is a cons-cell.
(msg ...) -- fexpr -- Modified to use \fImsgprintfn\fR to print
	values of evaluated elements of the print list.
(pearl-top-level) -- the PEARL top level loop. 
(pearl-top-level-init) -- The initial function called when PEARL starts up.  
(rplacx 'index 'hunk 'val) -- expr -- Provides random access storage into
	a block of memory.  (Uses \fIaddtoaddress\fR and \fIeven\fR.)
(quasiquote 'skel) -- expr -- called by the quasi-quote readmacro
	character backquote \fB`\fR.  Equivalent to the quasiquote
	functions defined in Charniak[2] with different invoking
	characters to match those of Franz Lisp.
	Unquote is comma \fB","\fR and splice-unquote is \fB",@"\fR.
	Uses \fIcombineskels\fR and \fIisconst\fR.
.fi
.bp
.NH
Bibliography
.SM
.IP [1]
Bobrow, D., and Winograd, T. "An Overview of KRL, a Knowledge
Representation Language."
\fICognitive Science\fR 1:1 (1977).
.IP [2]
Charniak, E., Riesbeck, C., and McDermott, D.
\fIArtificial Intelligence Programming\fR.
Hillsdale, New Jersey: Lawrence Erlbaum Associates, 1980.
.IP [3]
Faletti, J., and Wilensky, R. "The Implementation of PEARL:
A Package for Efficient Access to Representations In Lisp",
forthcoming ERL technical report, UCB.
.IP [4]
Greiner, R., and Lenat, D. "A Representation Language Language."
In \fIProc. First NCAI\fR. Stanford, CA, August, 1980,
165-169.
.IP [5]
Roberts, I., and Goldstein, R.
"NUDGE, A Knowledge-Based Scheduling Program."
In \fIProc. IJCAI-77\fR. Cambridge, MA, August, 1977, 257-263.
.IP [6]
Schank, R. \fIConceptual Information Processing\fR.
Amsterdam: North Holland, 1975.
.IP [7]
Wilensky, R. "Understanding Goal-Based Stories",
Technical Report 140, Computer Science Department,
Yale University, New Haven, CT, September 1978.
.IP [8]
Wilensky, R.
"Meta-Planning: Representing and Using Knowledge about Planning in Problem
Solving and Natural Language Understanding."
\fICognitive Science\fR 5:3 (1981).
.bp
.nr PS 9
.nr VS 11p
.ps 9
.vs 11p
.NH
Index of Global Variables and Functions With Their Arguments
.PP
All functions are exprs (or lexprs) unless otherwise listed.
Functions with one or more asterisks for a page number are not
documented other than in this index because they were not
actually intended for use by the PEARL user.
A single asterisk * means it is primarily intended for use by
PEARL but might be useful and will generally work right.
A double asterisk ** means it will generally only work
within PEARL's code, since it expects certain
external prog variables to exist and be set correctly.
A triple asterisk *** means it is dangerous to use.
Note that it is dangerous to redefine any functions in this list,
although it should be all right to redefine any macros.
.LP
.nr PS 8
.nr VS 10p
.ps 8
.vs 10p
.nf
*activedbnames* -- special variable -- initial value: nil			    \ki40
*any*conscell* -- special variable -- value: '(*any* . *pearlunbound*)	\h'|\niu'*
*availablesizes* -- special variable -- value:	\h'|\niu'39
	((-1. . 1.) (0. . 1.) (1. . 1.) (2. . 3.) (3. . 7.)
	 (4. . 13.) (5. . 29.) (6. . 61.) (7. . 127.) . . . .
		Franz Lisp: . . . (8. . 127.) (9. . 127.) (10. . 127.)
				  (11. . 127.) (12. . 127.) (13. . 127.))
		UCI Lisp:  . . .  (8. . 251.) (9. . 509.) (10. . 1021.)
				  (11. . 2039.) (12. . 4093.) (13. . 8191.))
*blockstack* -- special variable -- initial value: nil	\h'|\niu'48
*currentcreatetype* -- special variable -- initial value: base	\h'|\niu'56
*currentpearlstructure* -- special variable -- initial value: nil	\h'|\niu'46
*currentstructure* -- special variable -- initial value: nil	\h'|\niu'46
*currenttopcopy* -- special variable -- initial value: <UNBOUND>	\h'|\niu'55
*currenttopcreated* -- special variable -- initial value: (nilstruct)	\h'|\niu'8
.sp
db -- special variable -- default initial value: <UNBOUND>	\h'|\niu'33
*db* -- special variable -- default value: the *maindb* data base	\h'|\niu'12
*db1size* -- special variable -- default initial value: 29	\h'|\niu'39
*db2size* -- special variable -- default initial value: 127	\h'|\niu'39
*done* -- special atom	\h'|\niu'35
.sp
*fail* -- special atom	\h'|\niu'35
*file* -- special variable -- initial value: nil	\h'|\niu'60
*firstartup* -- special variable -- initial value: t	\h'|\niu'53
*function-stream:* -- special atom	\h'|\niu'13
*globallist* -- special variable -- initial value: nil	\h'|\niu'45
.sp
*history* -- special variable -- value: command history hunk	\h'|\niu'51
*historynumber* -- special variable -- initial value: 0	\h'|\niu'52
*historysize* -- special variable -- default value: 64	\h'|\niu'51
*histval* -- special variable -- value: value history hunk	\h'|\niu'51
*invisible* -- special atom	\h'|\niu'53
.sp
*lastcreated* -- special variable -- initial value: (nilstruct)	\h'|\niu'8
*lastsymbolnum* -- special variable -- initial value: -1	\h'|\niu'*
*maindb* -- special variable -- default value: the main data base	\h'|\niu'11
*matchunboundsresult* -- special variable -- initial value: nil	\h'|\niu'44
*ordinalnames* -- special variable -- initial value: nil	\h'|\niu'31
.sp
*pathlocal* -- special variable -- initial value: <UNBOUND>	\h'|\niu'33
*pathtop* -- special variable -- initial value: <UNBOUND>	\h'|\niu'33
*pearlprompt* -- special variable -- default value: "pearl> "	\h'|\niu'3, 4
*pearlunbound* -- special atom	\h'|\niu'45
*printhistorynumber* -- special variable -- initial value: nil	\h'|\niu'53
.sp
*readlinechanged* -- special variable -- initial value: nil	\h'|\niu'53
*runaddpredpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runaddsetpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runallbasehooks* -- special variable -- initial value: t	\h'|\niu'33
*runallslothooks* -- special variable -- initial value: t	\h'|\niu'33
.sp
*runapplypathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runclearpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*rundelpredpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*rundelsetpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runexpandedhooks* -- special variable -- initial value: t	\h'|\niu'34
*runfetchhooks* -- special variable -- initial value: t	\h'|\niu'34
*rungethookpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*rungetpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*rungetpredpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runindbhooks* -- special variable -- initial value: t	\h'|\niu'34
*runindividualhooks* -- special variable -- initial value: t	\h'|\niu'34
*runinsertdbhooks* -- special variable -- initial value: t	\h'|\niu'34
*runmatchhooks* -- special variable -- initial value: t	\h'|\niu'34
*runnextequalhooks* -- special variable -- initial value: t	\h'|\niu'34
*runnextitemhooks* -- special variable -- initial value: t	\h'|\niu'34
*runpatternhooks* -- special variable -- initial value: t	\h'|\niu'34
*runputpathhooks* -- special variable -- initial value: t	\h'|\niu'34
*runremovedbhooks* -- special variable -- initial value: t	\h'|\niu'34
*runsmergehooks* -- special variable -- initial value: t	\h'|\niu'34
*runstrequalhooks* -- special variable -- initial value: t	\h'|\niu'34
.sp
*stream* -- special atom	\h'|\niu'13
*stream:* -- special atom	\h'|\niu'13
*toplevelp* -- special variable -- initial value: <UNBOUND>	\h'|\niu'*
*unhashablevalues* -- special variable -- initial value:	\h'|\niu'*
	(0 unbound *pearlunbound* nilsym (nilstruct))
*use* -- special atom	\h'|\niu'35
*usealiases* -- special variable -- initial value: t	\h'|\niu'51
*warn* -- special variable -- initial value: t	\h'|\niu'17
*zero-ordinal-value* -- special variable -- initial value: 0	\h'|\niu'31
.sp
! -- splicing macro	\h'|\niu'52
$ -- splicing macro	\h'|\niu'52
= -- read macro	\h'|\niu'28
? -- read macro	\h'|\niu'16
.sp
(addalist 'var 'inst) -- macro	\h'|\niu'*
(addbasehook 'conscell 'item) -- macro	\h'|\niu'*
(addhistory 'line)	\h'|\niu'*
(addpredpath 'item 'path 'pred)	\h'|\niu'10
(addsetpath 'item 'path 'value)	\h'|\niu'10
.sp
(addtoexpansionlists) -- macro	\h'|\niu'**
(adjvarset 'var 'val) -- macro	\h'|\niu'*
(allocdef numofslots) -- macro	\h'|\niu'*
(allocval numofslots) -- macro	\h'|\niu'*
(applypath 'fcn 'item 'path)	\h'|\niu'10
.sp
(base name [storage] slot1 ...) -- macro	\h'|\niu'56
(basicmatch 'item1 'item2)	\h'|\niu'46
(block [blockname] varlist) -- fexpr	\h'|\niu'47
(blockatom 'symbol)	\h'|\niu'48
(blockp 'potblock)	\h'|\niu'30
.sp
(breakprintfn '*printval*)	\h'|\niu'58, 59
(builddb newdb [olddb]) -- fexpr	\h'|\niu'38
(buildintvalue 'intval 'bppset) -- macro	\h'|\niu'*
(buildslot) -- macro	\h'|\niu'**
(buildstructvalue 'structdesc) -- macro	\h'|\niu'*
(buildsymbolvalue 'symname) -- macro	\h'|\niu'*
(buildvalue 'value 'typenum 'ppset)	\h'|\niu'*
.sp
(cb name [storage] slot1 ...) -- macro	\h'|\niu'56
(ce basename newname [storage] slot1 ...) -- macro	\h'|\niu'56
(cf name [storage] slot1 ...) -- macro	\h'|\niu'56
(checkandrunbasehooks2 'fcn 'item1 'item2) -- macro	\h'|\niu'**
(checkandrunslothooks2 'fcn 'hooks 'val1 'val2 'item1 'item2) -- macro	\h'|\niu'**
(checkrunhandlebasehooks1 'fcn 'runhooksatom) -- macro	\h'|\niu'**
(checkrunhandleslothooks1 'fcn 'runhooksatom) -- macro	\h'|\niu'**
(ci basename [storage] slot1 ...) -- macro	\h'|\niu'56
.sp
(cleardb ['db])	\h'|\niu'39
(cleardb1 'db)	\h'|\niu'39
(cleardbactive 'db) -- macro	\h'|\niu'*
(clearhashandformat 'slotnum 'defblock) -- macro	\h'|\niu'*
(clearpath 'item 'path)	\h'|\niu'10
.sp
(compatible 'slotnum 'item1 'item2) -- macro	\h'|\niu'*
(connectdb 'newdb 'olddb)	\h'|\niu'*
(consistentvalue 'val 'predlist 'typenum 'item) -- macro	\h'|\niu'*
(constructvalue) -- macro	\h'|\niu'**
(convertpreds 'pred)	\h'|\niu'*
(copy 'list)	\h'|\niu'54
(copypatternslot) -- macro	\h'|\niu'**
(copyslice) -- macro	\h'|\niu'**
(copyslot 'nameblock) -- macro	\h'|\niu'**
.sp
(cp basename [storage] slot1 ...) -- macro	\h'|\niu'56
(cr selector ...) -- fexpr	\h'|\niu'55
(create selector ...) -- fexpr	\h'|\niu'5
(createbase 'newname 'slots)	\h'|\niu'*
(createexpanded 'oldname 'newname 'slots)	\h'|\niu'*
(createfunction 'fcnname 'slots)	\h'|\niu'*
(createindividual 'basename 'slots)	\h'|\niu'*
(createpattern 'basename 'slots)	\h'|\niu'*
.sp
(databasep 'potdb)	\h'|\niu'30
(dbcr selector ...) -- macro	\h'|\niu'56
(dbcreate selector ...) -- macro	\h'|\niu'12, 56
(debugprint 'item)	\h'|\niu'21
(defatom 'symbol)	\h'|\niu'7
.sp
(defaultfortype 'typenum) -- macro	\h'|\niu'*
(definitionp 'potdef)	\h'|\niu'30
(delpredpath 'item 'path 'pred)	\h'|\niu'10
(delsetpath 'item 'path 'value)	\h'|\niu'10
(disguisedas 'filler 'struct ['db])	\h'|\niu'29
(disguisedas1 'filler 'struct 'db)	\h'|\niu'29
.sp
(dobasehooks2< 'fcn 'runhookatom) -- macro	\h'|\niu'**
(dobasehooks2> 'fcn 'runhookatom) -- macro	\h'|\niu'**
(doslothooks2< 'fcn 'runhookatom) -- macro	\h'|\niu'**
(doslothooks2> 'fcn 'runhookatom) -- macro	\h'|\niu'**
(dskprintfn '*printval*)	\h'|\niu'60
.sp
(endallblocks)	\h'|\niu'48
(endanyblocks blockname) -- fexpr	\h'|\niu'48
(endblock [blockname]) -- fexpr	\h'|\niu'47
(enforcetype 'value 'typenum)	\h'|\niu'*
(equalvalue 'xval 'yval 'typenum) -- macro	\h'|\niu'*
(evalfcn 'item)	\h'|\niu'51
.sp
(executehook1 fcn value item defblock) -- macro	\h'|\niu'**
(executehook2 fcn val1 val2 item1 item2 defblock result) -- macro	\h'|\niu'**
(expanded basename newname [storage] slot1 ...) -- macro	\h'|\niu'56
(expandedfetch 'item ['db])	\h'|\niu'42
(expandedfetch1 'item 'db)	\h'|\niu'42
.sp
(fcnslot) -- macro	\h'|\niu'**
(fetch 'item ['db])	\h'|\niu'12, 43
(fetch1 'item 'db)	\h'|\niu'12, 43
(fetcheverywhere 'item ['db])	\h'|\niu'19, 25
(fetcheverywhere1 'item 'db)	\h'|\niu'19, 25
(fetchcreate selector ...) -- macro	\h'|\niu'14, 56
(fillbaseslot) -- macro	\h'|\niu'**
(fillin1 'fcn 'value 'item 'defblock)	\h'|\niu'33
(fillin2 'fcn 'val1 'val2 'item1 'item2 'defblock 'result)	\h'|\niu'33
(fillindivslot) -- macro	\h'|\niu'**
.sp
(findnextblockstart) -- macro	\h'|\niu'**
(findslotnum) -- macro	\h'|\niu'**
(findstructsymbolpair 'defblock 'symbol) -- macro	\h'|\niu'**
(firstfetch pattern) -- macro	\h'|\niu'14, 56
(fn name [storage] slot1 ...) -- macro	\h'|\niu'56
.sp
(followpath 'item 'path)	\h'|\niu'*
(for val 'init 'final &rest 'body) -- macro	\h'|\niu'54
(foreach 'stream fcn) -- fexpr	\h'|\niu'54
(fp 'item ['lmar ['rmar]])	\h'|\niu'56
(freezebindings 'struct)	\h'|\niu'48
(freezeblock 'blockname)	\h'|\niu'49
(freezestruct 'struct)	\h'|\niu'49
.sp
(fullform 'item)	\h'|\niu'20
(fullprint 'item ['lmar ['rmar]])	\h'|\niu'20, 37
(fullprint1 'item 'lmar 'rmar)	\h'|\niu'20, 37
(fullslotform) -- macro	\h'|\niu'**
.sp
(getalist 'inst) -- macro	\h'|\niu'57
(getalistcp 'inst) -- macro	\h'|\niu'57
(getbasehooks 'defblock) -- macro	\h'|\niu'57
(getdb1 'db) -- macro	\h'|\niu'57
(getdb2 'db) -- macro	\h'|\niu'57
(getdbactive 'db) -- macro	\h'|\niu'57
(getdbchildren 'db) -- macro	\h'|\niu'57
(getdbname 'db) -- macro	\h'|\niu'57
(getdbparent 'db) -- macro	\h'|\niu'57
(getdefaultinst 'defblock)	\h'|\niu'57
(getdefinition 'valblock)	\h'|\niu'57
(getenforce 'slotnum 'defblock) -- macro	\h'|\niu'57
(getexpansionlist 'defblock) -- macro	\h'|\niu'57
(getformatinfo 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash* 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash** 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash1 'num1 'db1) -- macro	\h'|\niu'57
(gethash2 'num1 'num2 'db2) -- macro	\h'|\niu'57
(gethash3 'num1 'num2 'num3 'db2) -- macro	\h'|\niu'57
(gethash: 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash:: 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash< 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethash> 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethashalias 'defblock) -- macro	\h'|\niu'57
(gethashinfo 'slotnum 'defblock) -- macro	\h'|\niu'57
(gethashvalue 'slotnum 'item 'defblock)	\h'|\niu'*
.sp
(gethookpath 'item 'path)	\h'|\niu'10
(getisa 'valblock) -- macro	\h'|\niu'57
(getpath 'item 'path)	\h'|\niu'10
(getpname 'defblock) -- macro	\h'|\niu'57
(getppset 'slotnum 'defblock) -- macro	\h'|\niu'57
(getpred 'slotnum 'inst) -- macro	\h'|\niu'57
(getpredpath 'item 'path)	\h'|\niu'10
.sp
(getsinglevalue 'slotnum 'item)	\h'|\niu'*
(getslot 'slotnum 'inst) -- macro	\h'|\niu'57
(getslothooks 'slotnum 'inst) -- macro	\h'|\niu'57
(getslotname 'slotnum 'defblock) -- macro	\h'|\niu'57
(getslottype 'slotnum 'defblock) -- macro	\h'|\niu'57
(getstructlength 'defblock) -- macro	\h'|\niu'57
(getstructorsymnum 'strsym) -- macro	\h'|\niu'57
.sp
(getsymbol 'symname)	\h'|\niu'4
(getsymbolpname 'symbolitem) -- macro	\h'|\niu'57
(getuniquenum 'defblock) -- macro	\h'|\niu'57
(getvalue 'slotnum 'inst)	\h'|\niu'57
(getvarandvalue 'slotnum 'inst 'var)	\h'|\niu'57
(getvarval 'slotnum 'inst) -- macro	\h'|\niu'57
.sp
(*global* varname) -- fexpr	\h'|\niu'46
(global variable) -- fexpr	\h'|\niu'45
(globalp 'variable)	\h'|\niu'45
(handlehookresult 'oldval 'newval) -- macro	\h'|\niu'**
(hashablevalue 'slotnum 'item 'defblock) -- macro	\h'|\niu'**
(hashslot) -- macro	\h'|\niu'**
.sp
(hidden 'command) -- macro	\h'|\niu'35
(higheroreq 'item1 'item2) -- macro	\h'|\niu'*
(history ['num])	\h'|\niu'53
(ind basename [storage] slot1 ...) -- macro	\h'|\niu'56
(indb 'item ['db])	\h'|\niu'14
(indb1 'item 'db)	\h'|\niu'14
(individual basename [storage] slot1 ...) -- macro	\h'|\niu'56
.sp
(inheritvalue 'structdef) -- macro	\h'|\niu'**
(inlinecreate selector ...) -- macro	\h'|\niu'14, 56
(inlinefetchcreate selector ...) -- macro	\h'|\niu'14, 56
(insertdb 'item ['db])	\h'|\niu'12
(insertdb1 'item 'db)	\h'|\niu'12
.sp
(insidecreate selector ...) -- fexpr	\h'|\niu'**
(insidefetch patdef expdefs) -- macro	\h'|\niu'**
(insidefetcheverywhere patdef expdefs) -- macro	\h'|\niu'**
(insidepatternize 'item)	\h'|\niu'**
(insidescopy 'item)	\h'|\niu'**
(installadjunct 'adjunctvar) -- macro	\h'|\niu'**
(installglobal 'globalvar) -- macro	\h'|\niu'**
(installvar 'varname) -- macro	\h'|\niu'**
.sp
(instatom 'symbol)	\h'|\niu'7
(isa 'item1 'name)	\h'|\niu'42
(isanexpanded 'item1 'item2)	\h'|\niu'42
(islambda 'fcnname)	\h'|\niu'*
.sp
(match 'item1 'item2)	\h'|\niu'46
(msgprintfn '*printval*)	\h'|\niu'58, 62
(newnum) -- macro	\h'|\niu'*
(nextequal 'stream)	\h'|\niu'46
(nextitem 'stream)	\h'|\niu'13
(noalias) -- macro	\h'|\niu'**
.sp
(nullstruct 'item)	\h'|\niu'42
(nullsym 'item)	\h'|\niu'42
(numberofslot 'slotname 'defblock) -- macro	\h'|\niu'57
(onesymbol) -- macro	\h'|\niu'**
(ordatom 'symbol)	\h'|\niu'31
(ordinal name vallist) -- fexpr	\h'|\niu'30
.sp
(pat basename [storage] slot1 ...) -- macro	\h'|\niu'56
(path fcn 'item 'pathlist ['val]) -- macro	\h'|\niu'9
(pattern basename [storage] slot1 ...) -- macro	\h'|\niu'56
(patternize 'item) -- macro	\h'|\niu'55
(patternizeslot) -- macro	\h'|\niu'**
(pboundp 'a)	\h'|\niu'45
.sp
(pearlprintfn '*printval*)	\h'|\niu'3, 4
(pexp basename newname [storage] slot1 ...) -- macro	\h'|\niu'56
(pfunction name [storage] slot1 ...) -- macro	\h'|\niu'56
(pname 'item)	\h'|\niu'4
(ppsetform 'slotval 'ppsetname)	\h'|\niu'*
.sp
(prefix 'item1 'item2)	\h'|\niu'52
(prefixcommandhistory)	\h'|\niu'*
(prefixcommandvalue)	\h'|\niu'*
(printdb ['db])	\h'|\niu'21
(printdb1 'db)	\h'|\niu'21
(psymbolp 'potsymbol)	\h'|\niu'30
(punbound)	\h'|\niu'45
.sp
(punboundatomp 'yyy)	\h'|\niu'*
(putalist 'alist 'inst) -- macro	\h'|\niu'*
(putalistcp 'alist 'inst) -- macro	\h'|\niu'*
(putbasehooks 'hooklist 'defblock) -- macro	\h'|\niu'*
(putdb1 'db1 'db) -- macro	\h'|\niu'***
(putdb2 'db2 'db) -- macro	\h'|\niu'***
(putdbchildren 'childlist 'db) -- macro	\h'|\niu'***
(putdbname 'name 'db) -- macro	\h'|\niu'*
(putdbparent 'parent 'db) -- macro	\h'|\niu'***
(putdef 'defblock 'valblock) -- macro	\h'|\niu'***
(putdefaultinst 'valblock 'defblock) -- macro	\h'|\niu'***
(putenforce 'slotnum 'defblock) -- macro	\h'|\niu'***
(putexpansionlist 'explist 'defblock) -- macro	\h'|\niu'***
(putformatinfo 'slotnum 'hashnum 'defblock) -- macro	\h'|\niu'***
(puthash* 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthash** 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthash1 'num1 'db1 'item) -- macro	\h'|\niu'*
(puthash2 'num1 'num2 'db2 'item) -- macro	\h'|\niu'*
(puthash3 'num1 'num2 'num3 'db2 'item) -- macro	\h'|\niu'*
(puthash: 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthash:: 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthash< 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthash> 'slotnum 'defblock) -- macro	\h'|\niu'***
(puthashalias 'hashnum 'defblock) -- macro	\h'|\niu'***
(puthashinfo 'slotnum 'hashnum 'defblock) -- macro	\h'|\niu'***
(putisa 'isa 'valblock) -- macro	\h'|\niu'***
.sp
(putpath 'item 'path 'value)	\h'|\niu'10
(putpname 'name 'defblock) -- macro	\h'|\niu'***
(putppset 'slotnum 'setname 'defblock) -- macro	\h'|\niu'*
(putpred 'slotnum 'value 'inst) -- macro	\h'|\niu'*
(putslot 'slotnum 'value 'inst) -- macro	\h'|\niu'***
(putslothooks 'slotnum 'slothooklist 'inst) -- macro	\h'|\niu'*
(putslotname 'slotnum 'slotname 'defblock) -- macro	\h'|\niu'***
(putslottype 'slotnum 'typenum 'defblock) -- macro	\h'|\niu'***
(putstructlength 'size 'defblock) -- macro	\h'|\niu'***
(putsymbolpname 'name 'block) -- macro	\h'|\niu'***
(putuniquenum 'num 'defblock) -- macro	\h'|\niu'***
(putvarval 'slotnum 'value 'inst) -- macro	\h'|\niu'***
(reallitatom 'potatom)	\h'|\niu'*
.sp
(releasedb 'db)	\h'|\niu'38
(removedb 'item ['db])	\h'|\niu'12
(removedb1 'item 'db)	\h'|\niu'12
(removeslot) -- macro	\h'|\niu'**
(revassq 'value 'alist)	\h'|\niu'*
(runbasehooks1 'fcn 'item)	\h'|\niu'33
(runbasehooks2 'fcn 'item1 'item2 'result)	\h'|\niu'33
(runslothooks1 'fcn 'item 'slotname 'value)	\h'|\niu'33
(runslothooks2 'fcn 'item1 'item2 'slotname 'val1 'val2)	\h'|\niu'33
.sp
(savecontinue 'directory 'name)	\h'|\niu'53
(savefresh 'directory 'name)	\h'|\niu'53
(savepearl)	\h'|\niu'*
(scopy 'item) -- macro	\h'|\niu'55
(scopyslot) -- macro	\h'|\niu'**
(setblock blockname) -- fexpr	\h'|\niu'48
.sp
(setdbactive 'db) -- macro	\h'|\niu'***
(setdbsize 'poweroftwo)	\h'|\niu'39
(setv var 'val 'environment) -- fexpr	\h'|\niu'47
(slotequal 'slotnum 'item1 'item2)	\h'|\niu'*
(slotnametonumber 'slotname 'defblock) -- macro	\h'|\niu'**
(smerge 'build 'from)	\h'|\niu'55
.sp
(standardfetch 'item ['db])	\h'|\niu'43
(standardfetch1 'item 'db)	\h'|\niu'43
(standardmatch 'item1 'item2)	\h'|\niu'46
(streamp 'potstream)	\h'|\niu'30
(streamtolist 'stream)	\h'|\niu'14
.sp
(strequal 'item1 'item2)	\h'|\niu'46
(structurenamep 'potname)	\h'|\niu'30
(structurep 'potstruct)	\h'|\niu'30
(symatom 'symbol)	\h'|\niu'4
(symbol name1 name2 ...) -- fexpr	\h'|\niu'4
(symbole 'symname)	\h'|\niu'4
(symbolnamep 'potname)	\h'|\niu'30
.sp
(thawbindings 'struct)	\h'|\niu'49
(thawblock 'blockname)	\h'|\niu'49
(thawstruct 'struct)	\h'|\niu'49
(unbind globalvar) -- fexpr	\h'|\niu'45
(unbindvars 'structure) -- macro	\h'|\niu'46
(unboundatomp 'yyy)	\h'|\niu'*
.sp
(valform 'item)	\h'|\niu'20
(valprint 'item ['lmar ['rmar]])	\h'|\niu'20
(valprint1 'item 'lmar)	\h'|\niu'20
(valslotform) -- macro	\h'|\niu'**
(valueof 'var 'struct)	\h'|\niu'17
.sp
(*var* varname) -- fexpr	\h'|\niu'46
(varset 'var 'val) -- macro	\h'|\niu'*
(varvalue var 'val) -- fexpr	\h'|\niu'17
(visible 'command) -- macro	\h'|\niu'35
(vp 'item ['lmar ['rmar]])	\h'|\niu'56
(while 'val &rest 'body) -- macro	\h'|\niu'54
.fi
.bp
.nr PS 9
.nr VS 11p
.ps 9
.vs 11p
.NH
Concept Index
.LP
.nr PS 8
.nr VS 10p
.ps 8
.vs 10p
.nf
abbreviations						      \ki55-56
accessing slots of structures	\h'|\niu'8-10
accessing structure default instances	\h'|\niu'7
accessing structure definitions	\h'|\niu'7
accessing symbols	\h'|\niu'4
.sp
adding slots to structures	\h'|\niu'40
adding to the data base	\h'|\niu'12
adjunct variables	\h'|\niu'30
affecting forced aliasing (^)	\h'|\niu'27
ako's (expanded structures)	\h'|\niu'40-42
.sp
aliasing of commands	\h'|\niu'51
aliasing in hashing	\h'|\niu'27
ampersand (&) hashing	\h'|\niu'26
and, in predicates	\h'|\niu'28
anti-aliasing in hashing (<)	\h'|\niu'27
*any*	\h'|\niu'15
automatic storing of structures	\h'|\niu'8, 56
.sp
base hooks	\h'|\niu'32-37
bases	\h'|\niu'5
blocks	\h'|\niu'47-48
building structures	\h'|\niu'5
building upon data bases	\h'|\niu'38, 39
.sp
changing slots of structures	\h'|\niu'8
clearing data bases	\h'|\niu'39
colon (:) hashing	\h'|\niu'23
colon-colon (::) hashing	\h'|\niu'24
.sp
command aliasing	\h'|\niu'51
command history	\h'|\niu'51-53
command history, printing	\h'|\niu'53
compatibility functions (UCI, Franz)	\h'|\niu'58-62
.sp
controlling running of hooks	\h'|\niu'33-34
controlling results with hooks	\h'|\niu'35
controlling unbinding of variables	\h'|\niu'48-49
converting from internal form	\h'|\niu'20
copy redefined	\h'|\niu'54
copying structures	\h'|\niu'55
.sp
creating data bases	\h'|\niu'38, 39
creating patterns	\h'|\niu'15-16
creating base structures	\h'|\niu'5
creating individual structures	\h'|\niu'6
creating symbols	\h'|\niu'4
.sp
data bases	\h'|\niu'11
data bases, building upon	\h'|\niu'39
data bases, clearing	\h'|\niu'39
data bases, creating	\h'|\niu'38
data bases, fetching from	\h'|\niu'12, 19, 25, 42, 43, 46
data bases, freeing	\h'|\niu'40
data bases, inserting into	\h'|\niu'12
data bases, printing	\h'|\niu'21
data bases, releasing	\h'|\niu'40
data bases, removing from	\h'|\niu'12
data bases, setting size of	\h'|\niu'39
.sp
debugging	\h'|\niu'21
debugging print	\h'|\niu'21
declaring global variables	\h'|\niu'45
.sp
default fetch function	\h'|\niu'43
default instance for a structure	\h'|\niu'15
default instance, accessing	\h'|\niu'7
default match function	\h'|\niu'46
default printing functions	\h'|\niu'20, 58, 60-61, 62
default values for slots	\h'|\niu'14-15
defaults, inherited	\h'|\niu'41-42
.sp
defining structures	\h'|\niu'5
defining symbols	\h'|\niu'4
definitions of structures, accessing	\h'|\niu'7
deleting from the data base	\h'|\niu'12
demons (hooks)	\h'|\niu'32-37
.sp
disguising in path	\h'|\niu'10-11
disguising in predicates	\h'|\niu'29
don't-care matching variable	\h'|\niu'15
double-colon (::) hashing	\h'|\niu'24
double-star (**) hashing	\h'|\niu'24
dumping PEARL for later	\h'|\niu'53
.sp
efficiency despite variables	\h'|\niu'30
enumerated (ordinal) types	\h'|\niu'30
environment for variable evaluation	\h'|\niu'46-47
environment, top level	\h'|\niu'51-53
environments, in hooks	\h'|\niu'33
.sp
equality of structures	\h'|\niu'46
equivalences of functions (UCI-Franz)	\h'|\niu'58-62
error messages	\h'|\niu'21
evaluating function structures	\h'|\niu'51
evaluating in create	\h'|\niu'22
expanded structures	\h'|\niu'40
expanded structures, fetching	\h'|\niu'42
.sp
feedback, sending	\h'|\niu'21
fetch, standard	\h'|\niu'46
fetching expanded structures	\h'|\niu'42
fetching from all buckets	\h'|\niu'19, 25
fetching from the data base	\h'|\niu'12 , 19, 25, 42, 43, 46
fetching with equality (not matching)	\h'|\niu'46
.sp
filling in special forms (in hooks)	\h'|\niu'33
for loop	\h'|\niu'54
forced aliasing (>)	\h'|\niu'26
forest of data bases	\h'|\niu'39-40
freeing data bases	\h'|\niu'40
freezing variables	\h'|\niu'48-49
.sp
function equivalences (UCI-Franz)	\h'|\niu'58-62
function structures	\h'|\niu'49-51
function structures, evaluating	\h'|\niu'51
getting symbols	\h'|\niu'4
global variables	\h'|\niu'45
greater-than (>) hashing	\h'|\niu'26
.sp
hash aliasing (&)	\h'|\niu'26
hash marking	\h'|\niu'17, 23-27
hashing problems	\h'|\niu'18
hashing with variables	\h'|\niu'30
hiding functions from hooks	\h'|\niu'35
hierarchy of structures	\h'|\niu'40
.sp
history mechanism	\h'|\niu'51-3
history number, printing in prompt	\h'|\niu'53
hooks	\h'|\niu'32-37
hooks, affecting result with	\h'|\niu'35
hooks, controlling running of	\h'|\niu'33-34
hooks, hiding functions from	\h'|\niu'35
hooks, making functions visible to	\h'|\niu'35
hooks, multi-argument	\h'|\niu'28
hooks, running	\h'|\niu'33-34
.sp
if-added functions (hooks)	\h'|\niu'32-37
indirection in path	\h'|\niu'10-11
individuals	\h'|\niu'6
inheritance in structures	\h'|\niu'41-42
(.)init.prl file	\h'|\niu'2-3
.sp
inserting in the data base	\h'|\niu'12
instances	\h'|\niu'6
integer slots	\h'|\niu'30
internal access functions	\h'|\niu'57
internal form printing	\h'|\niu'21
.sp
invisible functions to hooks	\h'|\niu'35
invisible results from functions	\h'|\niu'53
isa's (expanded structures)	\h'|\niu'40-42
less-than (<) hashing	\h'|\niu'27
lexically scoped variables	\h'|\niu'47-48
.sp
looping functions	\h'|\niu'54
low level access functions	\h'|\niu'57
macros, special	\h'|\niu'56
main data base	\h'|\niu'11
marking structures for hashing	\h'|\niu'17, 23-27
.sp
match, standard	\h'|\niu'46
match, without unbinding variables	\h'|\niu'46
match-anything variable	\h'|\niu'15
matching process	\h'|\niu'44
matching two structures	\h'|\niu'43
matching unbound variables	\h'|\niu'44
matching-variables	\h'|\niu'16-17
.sp
merging structures	\h'|\niu'55
modified input line, printing	\h'|\niu'53
multi-argument matching predicates	\h'|\niu'28, 32
next item in a stream	\h'|\niu'13
nilstruct(ure)	\h'|\niu'14
nilsym(bol)	\h'|\niu'14
.sp
or, in predicates	\h'|\niu'28
ordinal types	\h'|\niu'30-31
path functions	\h'|\niu'10
path indirection	\h'|\niu'10-11
pattern-matching variables	\h'|\niu'16-17
.sp
patterns	\h'|\niu'12, 15, 43
patterns in matching	\h'|\niu'43
predicates for object types	\h'|\niu'30
predicates in matching	\h'|\niu'27-29
predicates in matching, when run	\h'|\niu'44
predicates in matching, multi-argument	\h'|\niu'28
.sp
print names	\h'|\niu'4
printing PEARL objects	\h'|\niu'20
printing command history	\h'|\niu'53
printing data bases	\h'|\niu'21
printing functions	\h'|\niu'20
printing functions, standard	\h'|\niu'3-4, 58, 60-61, 62
printing history number in prompt	\h'|\niu'53
printing modified input line	\h'|\niu'53
printing warnings	\h'|\niu'17
.sp
processing a stream	\h'|\niu'13
prompt	\h'|\niu'3-4
prompt-read-eval-print loop	\h'|\niu'2-3, 51
read-eval-print loop	\h'|\niu'2-3, 51
redirecting in create (! and $)	\h'|\niu'22
releasing data bases	\h'|\niu'40
removing from the data base	\h'|\niu'12
.sp
reporting bugs	\h'|\niu'21
retrieving from the data base	\h'|\niu'12
returning invisible results	\h'|\niu'53
running hooks	\h'|\niu'33-34
running under Franz Lisp	\h'|\niu'2
running under UCI Lisp	\h'|\niu'3
.sp
saving PEARL for later	\h'|\niu'53
scalar types	\h'|\niu'30
short-circuiting in create	\h'|\niu'22
side effect setting of adjunct variables	\h'|\niu'30
size of data bases	\h'|\niu'39
.sp
slot hooks	\h'|\niu'32-37
slot names to numbers	\h'|\niu'57
slot types	\h'|\niu'6
slot types, more specific	\h'|\niu'30
slot values	\h'|\niu'8-10
slot values in hooks	\h'|\niu'32
slot values in predicates	\h'|\niu'28
.sp
special forms in hooks	\h'|\niu'32
special forms in predicates	\h'|\niu'28
special forms, filling in	\h'|\niu'33
special macros	\h'|\niu'56
standard fetch function	\h'|\niu'43
standard match function	\h'|\niu'46
.sp
star (*) hashing	\h'|\niu'17, 23
star-star (**) hashing	\h'|\niu'24
(.)start.prl file	\h'|\niu'2-3
startup files	\h'|\niu'2-3
storing structures in the data base	\h'|\niu'12
storing of structures in atoms	\h'|\niu'8, 56
streams	\h'|\niu'13
.sp
structure equality	\h'|\niu'46
structure matching	\h'|\niu'44-45
structure predicates	\h'|\niu'28-29
structure slots, further typing	\h'|\niu'30
structured escapes to Lisp	\h'|\niu'49-51
structures	\h'|\niu'5
structures, copying	\h'|\niu'55
structures, expanded	\h'|\niu'40
structures, function	\h'|\niu'49-51
structures, merging	\h'|\niu'55
.sp
symbols	\h'|\niu'4
testing for nilstruct	\h'|\niu'42
testing for nilsym	\h'|\niu'42
testing for object types	\h'|\niu'30
thawing variables	\h'|\niu'48-49
.sp
top level loop	\h'|\niu'2-3, 51
top level loop functions	\h'|\niu'59, 62
triple (**) hashing	\h'|\niu'24
type tests for objects	\h'|\niu'30
types in structure slots	\h'|\niu'31-2
.sp
unbinding global variables by match (lack of)	\h'|\niu'45
unbinding global variables by user	\h'|\niu'45
unbinding local variables by match	\h'|\niu'45-6
unbinding local variables by user	\h'|\niu'46
unbinding of variables, controlling	\h'|\niu'48-49
up-arrow (^) hashing	\h'|\niu'27
.sp
values of variables	\h'|\niu'17, 46
values of variables, setting	\h'|\niu'47
variables in hooks	\h'|\niu'32
variables in predicates	\h'|\niu'28
variables with hashing	\h'|\niu'30
variable, accessing values	\h'|\niu'17, 46
variables, adjunct	\h'|\niu'30
variables, controlling unbinding	\h'|\niu'48-49
variables, freezing	\h'|\niu'48-49
variables, global	\h'|\niu'45
variables, lexically scoped	\h'|\niu'47-48
variable, setting values	\h'|\niu'47
variables, side effects	\h'|\niu'30
variables, thawing	\h'|\niu'48-49
variables, unbinding	\h'|\niu'46
.sp
visible functions to hooks	\h'|\niu'35
warnings	\h'|\niu'17
while loop	\h'|\niu'54
.fi
.nr PS 10
.nr VS 12p
.ps 10
.vs 12p
.bp 0
.DS C
.LG
\fBTable of Contents\fR
.SM
.DE
.DS L
1.  Introduction                                                                                           \ka  1
2.  Running PEARL               \h'|\nau'  2
      2.1.  Under Franz Lisp    \h'|\nau'  2
      2.2.  Under UCI Lisp      \h'|\nau'  3
3.  Creating Simple Objects     \h'|\nau'  4
      3.1.  Defining Symbols               \h'|\nau'  4
      3.2.  Defining Structures            \h'|\nau'  5
4.  Creating Individual Instances of Defined Structures \h'|\nau'  6
5.  Accessing Slots of Structures \h'|\nau'  8
6.  Storing In and Retrieving From the Data Base -- The Simplest Way \h'|\nau'11
      6.1  Storing In the Data Base: \fIInsertdb\fR and \fIRemovedb\fR\h'|\nau'11
      6.2  Retrieving Hash Buckets From the Data Base: \fIFetch\fR \h'|\nau'12
      6.3  Accessing the Results of \fIFetch\fR: \fINextitem\fR \h'|\nau'13
7.  The Default Values for Unspecified Slots \h'|\nau'14
8.  Using \fIPattern\fRs For More Flexible and Powerful Retrieval \h'|\nau'15
9.  Marking Structures During Creation For More Efficient Retrieval \h'|\nau'17
10. Printing Structures, Symbols and Other PEARL Objects \h'|\nau'20
11. Error Messages, Bugs, and Error Handling Abilities \h'|\nau'21
12. Short-Circuiting and Redirecting \fICreate\fR Using !, $ and Atoms \h'|\nau'22
13. More Flexible Hash Selection \h'|\nau'23
14. Using Predicates to Constrain Fetching \h'|\nau'27
15. More Useful Slot Types \h'|\nau'30
16. Attaching Hooks to Structures (If-Added Demons) \h'|\nau'32
17. Creating and Manipulating Multiple Data Bases \h'|\nau'38
18. Creating a Forest of Data Bases \h'|\nau'39
19. Creating Expanded Subtypes of Previously Defined Objects \h'|\nau'40
20. Fetching Expanded Structures \h'|\nau'42
21. How Two Objects \fIMatch\fR \h'|\nau'43
      21.1  When Is a Pattern not a \fIPattern\fR? \h'|\nau'43
      21.2  The Matching Process \h'|\nau'44
22. Binding Blocks of Structures Together Via Common Variables \h'|\nau'47
23. Controlling the Unbinding of Variables by \fIMatch\fR \h'|\nau'48
24. Function Structures \h'|\nau'49
25. More About the PEARL Top Level Loop and History Mechanism \h'|\nau'51
26. Looping and Copying Functions \h'|\nau'54
27. Miscellaneous Variations and Abbreviations \h'|\nau'55
28. Low Level Access Functions \h'|\nau'57
29. Appendix of UCI Lisp functions added to Franz PEARL \h'|\nau'58
30. Appendix of Franz Lisp functions added to UCI Lisp PEARL \h'|\nau'62
31. Bibliography \h'|\nau'63
32. Index of Global Variables and Functions With Their Arguments \h'|\nau'64
33. Concept Index \h'|\nau'71
.DE
