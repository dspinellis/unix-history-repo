.so /usr/lib/vlpmacs
.ND
.ds CH
.ds CF - % -
.po 1.00i
.nr LL 6.25i
.RP
.TL
.LG
The Implementation of PEARL
.SM
.sp 1
A Package for Efficient Access to
Representations in Lisp*
.FS
* This research was sponsored in part by the Office of Naval Research
under contract N00014-80-C-0732 and the National Science Foundation
under grant MCS79-06543.
.FE
.AU
Joseph Faletti
Robert Wilensky
.AI
Computer Science Division
Department of EECS
University of California, Berkeley
Berkeley, California 94720
.sp 1
March 1982
.AB
PEARL is an AI language developed with space and
time efficiencies in mind.
In addition to providing the usual facilities such as
slot-filler objects, demons and associative data bases,
PEARL introduces stronger typing on slots,
user-assisted hashing mechanisms, and a forest of data bases.
The resulting product is a simple but powerful and efficient
tool for AI research.
.AE
.NH
Introduction
.sp 3
.PP
We have developed an AI language called PEARL (Package for Efficient
Access to Representations in Lisp).
Unlike the recent tendency toward
elaborate representation languages such as KRL [1]
or language generators such as RRL [5], PEARL is
a more modest system that combines a number of useful
AI techniques into a very efficient package.
PEARL provides the user with a set of operators for defining, creating,
and manipulating slot-filler objects, as well as placing them into
associative data bases, upon which further operations may be performed.
Besides the usual goal of providing the user with a more meaningful
interface than is available via Lisp, PEARL has the following salient 
characteristics:
.IP 1)
PEARL combines some features of predicate calculus-like data bases with
those of frame-based systems like FRL [9].
.IP 2)
PEARL introduces typing to user-defined knowledge structures.
.IP 3)
PEARL allows the user to manipulate a forest of associative
data bases implemented as hash tables.
.IP 4)
Fetches from the data base return streams of answers.
Retrieval is based on pattern matching.
.IP 5)
PEARL is very efficient.
PEARL uses its own internal representation
for knowledge structures for both economy of storage and speed.
A great deal of effort has gone into exploiting type information
not available in most AI languages to eliminate searching inefficiencies.
In addition, the user may easily specify, as part of a knowledge
structure definition, a great deal of information about how
objects should be indexed for efficient retrieval.
Thus PEARL provides much of the power
of expression of other AI languages without the usual overhead.
.LP
Perhaps most significantly, PEARL is actually being used in the
construction of several AI systems.
In particular, the latest version of PAM [12],
a story understanding program, has been re-programmed in PEARL.
PANDORA [13], a planning program now under development at Berkeley,
is also written in PEARL.
Our experience has led us to conclude that PEARL
is an effective AI tool for the creation of efficient AI programs.
.sp 3
.PP
The following is a quick overview of the paper:
First we present an overview of PEARL by discussing a sample session
which demonstrates the primary functions provided.
Next we present some measurements as evidence that PEARL is
indeed efficient.
The bulk of the paper then describes the details of each of PEARL's
main functions -- creating structures, the form of the data bases,
indicating indexing methods, fetching from the data bases, predicates
and matching, matching variables, and demons.
This is followed by descriptions of the various implementations of
PEARL and their relative speeds plus evidence that PEARL's hashing
mechanism does in fact speed up fetching.
Finally, we compare PEARL to its nearest cousins, FRL and KRL.
.NH
An Overview and Sample Application Of PEARL
.sp 3
.PP
In the section we give an overview of PEARL by presenting an
extended example of PEARL's use.
The example we will use to demonstrate the various features of
PEARL before going into each in more detail is a \fIvery simple\fR
inference mechanism based on forward and backward inferences rules.
In order to explain and motivate the various pieces of PEARL (and
Lisp) code, we present them in the order that one would most likely
design them, rather than the order that they must be entered
into PEARL.  Afterwards, they would have to be moved around so
that things are defined before being referenced.
.sp 3
.PP
To implement the inference mechanism, we will want to ensure that
we perform forward inferences whenever we insert a concept into
the data base and backward inference whenever we fetch a concept
from the data base.  The easiest way to accomplish this is to
create two demons, one for forward inference and one for backward
inference which will be run when insert and fetching respectively
happens.  These will need to be attached to all concepts which we
want to make inferences from, so we need a \fIstructure\fR (PEARL's
name for a slot/filler object) which we will call \fIConcept\fR.
It will have no slots but will be used as the root of our
conceptual hierarchy so that all concepts can inherit the
inference demons from it.
(We will add these later when we know what their names are).
We do this in PEARL by declaring a \fIbase\fR structure called \fIConcept\fR:
.DS
(create base  Concept)
.DE
.sp 3
.PP
We will then want to describe some of the concepts that we
wish to make inferences about.
For the purposes of this example, we will present only enough
information to use backward inference to determine that Samuel is
probably poor from the fact that he is a professor.
So we will want structures which describe a person, a professor,
a salary level, and being poor.
\fIPerson\fR is an expanded \fIConcept\fR;
that is, it should inherit everything not otherwise specified from
the structure \fIConcept\fR.
It will have one slot (for our current purposes) containing the
person's name which we will represent as a \fIsymbol\fR which is
used in PEARL for creating literals (that it, something with no
conceptual content):
.DS
.Ls
(create \kAexpanded  Concept  Person
        \h'|\nAu'(*  Name  symbol))
.Le
.DE
.LP
Included in our definition is a special hashing mark (*) on the
Name slot which says that the value in this slot should be
helpful in indexing \fIPerson\fR structures.
This is true because we are likely to be asking questions of the
data base like "Is Samuel the name of a person?".
For example, suppose we have declared the symbol Samuel:
.DS
(symbol  Samuel)
.DE
.LP
and asserted into the data base the fact that there is a person named
\fISamuel\fR by creating an individual instance of a \fIPerson\fR
with the \fIName\fR slot filled with the symbol \fISamuel\fR and
inserting it into the data base:
.DS
.Ls
(insertdb  (create \kAindividual  Person  Sam
                   \h'|\nAu'(Name  Samuel)))
.Le
.DE
.LP
(As a side effect of the above, the individual structure instance
\fI(Person (Name Samuel))\fR is stored in the Lisp atom \fISam\fR
for future use.)
The function \fIinsertdb\fR uses the hashing information we gave
for \fIPerson\fR to insert this structure in the data base in
two places: under the fact that it is a \fIPerson\fR which is
automatic for all structures and under the combination of
\fIPerson\fR and \fISamuel\fR because we bothered to provide
the extra information in our definition of \fIPerson\fR.
.sp 3
.PP
Now we can "phrase" the question "Is Samuel the name
of a person?" as:
.DS
.Ls
(setq  Stream  (fetch  (create \kAindividual  Person
                               \h'|\nAu'(Name  Samuel)))
.Le
.DE
.LP
that is, by creating an individual \fIPerson\fR with name \fISamuel\fR,
and fetching it from the data base.
This returns a hash bucket from the data base which is chosen
based on two parts of our pattern: the fact that it is a
\fIPerson\fR structure (because this is always available)
plus the value in the Name slot (because we labelled this slot
in our definition of \fIPerson\fR.
Given this stream (virtual list) of possible matches, we ask
whether there is in fact something there which matches our pattern:
.DS
(setq Result (nextitem Stream))
.DE
.LP
If Result is \fInil\fR then the fact that Sam is a Person is not
in the data base.
If it is, then Result will contain the structure in the data base.
.sp 3
.PP
Similarly, we declare the structure \fIProfessor\fR,
a predicate about a (structure of type) \fIPerson\fR and
assert that Sam is a \fIProfessor\fR using the structure
value we stored in \fISam\fR before:
.DS
.Ls
(create \kAexpanded  Concept  Professor
        \h'|\nAu'(* > Person  Person))
(insertdb  (create \kAindividual  Professor
                   \h'|\nAu'(Person  Sam)))
.Le
.DE
.sp 3
.PP
In choosing a way to index this structure, we consider the fact
that Person slot of \fIProfessor\fR will always contain a
\fIPerson\fR structure and thus the combination of \fIProfessor\fR
and \fIPerson\fR will not help to spread these out in our data base. 
However, the value of the first marked slot \fIName\fR of Person will
contain widely varying information so the combination of
\fIProfessor\fR and this value will work well.
The hashing mark ">" instructs PEARL to do precisely this.
We also define \fISalary\fR, a relation between an \fIEmployee\fR
and a salary level:
.DS
.Ls
(create \kBexpanded  Concept  Salary
        \h'|\nBu'\kA(* > Employee  Person)
        \h'|\nAu'(      Level  symbol))
.Le
.DE
.LP
Here the first slot is starred because we are likely to ask for
the salary of Sam.
If we were also likely to ask for all people with Low salaries,
then we would star the second slot also.
Finally, we define \fIPoor\fR, a predicate about a \fIPerson\fR:
.DS
.Ls
(create \kAexpanded  Concept  Poor
        \h'|\nAu'(* > Person  Person))
.Le
.DE
.LP
Having defined the types of objects we know about and the few
actual facts we know, we are ready to describe the inference
rules.
Forward rules say that if the value in the Fact slot is true then
the value in the Implies slot is true also.
We are likely to fetch them using Fact as a key, so we mark that
slot as useful in hashing:
.DS
.Ls
(create \kBbase  ForwardRule
        \h'|\nBu'\kA(* Fact  Concept)
        \h'|\nAu'(   Implies  Concept))
.Le
.DE
.LP
Backward rules say that if you want to know if the value in the Need
slot is true then check to see if the value in the LookFor slot is true:
.DS
.Ls
(create \kBbase  BackwardRule
        \h'|\nBu'\kA(* Need  Concept)
        \h'|\nAu'(   LookFor  Concept))
.Le
.DE
.LP
Finally, we need to add some rules to our data base.
Since we are likely to know a lot of inference rules and to want
to access them often, it would help to keep them in a different
data base separate from other facts to speed up access.
So we build a special data base just for inference rules:
.DS
(builddb *Rules*)
.DE
.LP
Then we insert some rules into it:
.DS
(symbol  Low)
.DE
.DS
.Ls
\fI; If you want to know if someone is poor, check for a low pay level.\fP
(insertdb  \kA(create \kBindividual  BackwardRule
                   \h'|\nBu'\kC(Need  (Poor  (Person  ?Person)))
                   \h'|\nCu'(LookFor  (Salary  \kB(Employee  ?Person)
                                      \h'|\nBu'(Level  Low))))
           \h'|\nAu'*Rules*)
.Le
.DE
.DS
.Ls
\fI; If you want to know if someone's pay level is low, check to\fP
\fI;    see if they are a professor.\fP
(insertdb  \kA(create \kDindividual  BackwardRule
                   \h'|\nDu'\kB(Need  (Salary  \kC(Employee  ?Person)
                                   \h'|\nCu'(Level  Low)))
                   \h'|\nBu'(LookFor  (Professor  (Person  ?Person))))
           \h'|\nAu'*Rules*)
.Le
.DE
.LP
Note in our rules that we use the pattern matching variable \fI?Person\fR.
In the first rule this ties together the person who is poor with the
person whose is paid at a low level.
In the second rule it ties together the person with low pay
to the professor.
Although these two rules have variables with the same name, no
naming conflict arises because most variables in PEARL are local
to the structure they are used in.
.sp 3
.PP
Next we are in a position to say how to make forward inferences:
.DS
.Ls
\fI; MakeForwardInference is a demon, triggered by insertions into the\fP
\fI;    data base, which fetches forward inference rules predicated upon\fP
\fI;    the Fact being inserted and inserts the implications from the\fP
\fI;    rule into the data base.\fP
(de \kDMakeForwardInference (Fact)
    \h'|\nDu'(prog \kC(Rules Rule)
          \h'|\nCu'\kB(setq \kDRules
                \h'|\nDu'(fetch \kA(create \kCpattern ForwardRule
                               \h'|\nCu'(Fact Fact))
                       \h'|\nAu'*Rules*))
          \h'|\nBu'(while \kA(setq Rule (nextitem Rules))
                 \h'|\nAu'(insertdb (path get Rule 'Implies)))))
.Le
.DE
.LP
This says to fetch all forward inference rules with the new fact
in the Fact slot and assert each of their associated Implies slots.
.sp 3
.PP
Making backward inferences is similar but a bit more complex
because we want it to stop right away if it succeeds:
.DS
.Ls
\fI; MakeBackwardInference is a demon, triggered by fetches from the\fP
\fI;    data base which fetches backward inference rules whose Need\fP
\fI;    slot contains the Fact being inserted and fetches the value\fP
\fI;    of the rule's LookFor slot from the data base until one succeeds.\fP
(de \kBMakeBackwardInference (Fact)
    \h'|\nBu'(prog \kA(Rules Rule Found Try)
          \h'|\nAu'\kC(setq Rules (fetch \kD(create \kBpattern BackwardRule
                                     \h'|\nBu'(Need Fact))
                             \h'|\nDu'*Rules*))
          \h'|\nCu'\kD(setq Found nil)
          \h'|\nDu'\kA(while \kB(and \kC(not Found)
                      \h'|\nCu'(setq Rule (nextitem Rules)))
                 \h'|\nBu'\kC\fI; Get the LookFor slot's value.\fP
                 \h'|\nCu'\kB(setq Try (path get Rule 'LookFor))
                 \h'|\nBu'(cond (\kC(nextitem (fetch Try))
                        \h'|\nCu'\kB(insertdb Fact)
                        \h'|\nBu'(setq Found t))))
          \h'|\nAu'(return Found)))
.Le
.DE
.LP
Finally, we can finish our description of \fIConcept\fR by saying
that all concepts inserted into the data base should run the demon
MakeForwardInference after the insertion has been performed (">insertdb").
All concepts fetched from the data base should run MakeBackwardInference
\fIbefore\fR the fetch has been performed ("<fetch"):
.DS
.Ls
(create \kBbase  Concept
        \h'|\nBu'(if  \kA>insertdb  MakeForwardInference
             \h'|\nAu'<fetch  MakeBackwardInference))
.Le
.DE
.LP
Now we "phrase" our question "Is Sam poor?" as a pattern:
.DS
.Ls
(create \kApattern  Poor  NeededFact
        \h'|\nAu'(Person  Sam))
.Le
.DE
.LP
This fact is not in the data base.
However, upon fetching it:
.DS
(nextitem  (fetch  NeededFact))
.DE
.LP
Our two backward inference rules will be used by our backward
inference demon and the desired fact will have been inserted into
the data base before the fetch does its job.
.sp 3
.PP
This example demonstrates a good deal of what PEARL can do and
gives the flavor of its use.
After presenting some measurements of its speed, we will discuss
each of its features in more detail and describe their
implementations.
.sp 3
.PP
The above example was described as a user would design and enter
code into a file to be read into PEARL.
This is the normal mode for experienced users of PEARL, just as it
is for Lisp programmers.
PEARL can also be used interactively since it is
built on top of Lisp.
However, PEARL constructs objects which Lisp does not know how
to print intelligibly.
To remedy this, the read-eval-print loop of Lisp has been modified
to deal with PEARL objects and uses PEARL's print function to
print a readable representation of them.
To help demonstrate what it is like to use PEARL interactively,
the examples in later sections use italics to indicate those things
typed by PEARL during an interactive session and bold face to
indicate those things typed by the user.
To remind the users that they are using PEARL
rather than Lisp, the prompt has been changed to \fIpearl>\fR.
.NH
How Fast Is PEARL?
.sp 3
.PP
PEARL achieves its space efficiency and some of its time efficiency
by requesting a block of memory from Lisp for each structure
instance or definition.
The contents or defining information are then packed within
this block.
Since much of the defining information is Boolean, 
this provides substantial savings in space for definitions.
Data bases are managed similarly.
.sp 3
.PP
As a rough measure of PEARL's execution speed on the PDP-10,
we created a test data base of 4000 structures, in which the
average unsuccessful query took 0.0042 CPU seconds (237 per
second) and the average successful query took 0.0073 CPU seconds
(136 per second).
Note that PEARL's hashing mechanism results in particularly fast
determination of failure.
As another measure of PEARL's execution speed, we
compared the original implementation of PAM [12] written purely in Lisp
(with no special consideration for efficiency)
with the current implementation which uses PEARL.
The average time required by the original to process a single
sentence in a 5-sentence story was 5.6 CPU seconds.
The new version, which builds a more complete representation of
the overall structure of the story and uses a significantly
larger collection of knowledge, requires an average of only
0.56 CPU seconds per sentence in a 23-sentence story.
.sp 3
.PP
For measurements demonstrating the effectiveness of the hashing,
and comparisons between various implementations, see the section
below on performance.
.NH
Objects and Structures
.sp 3
.PP
PEARL has four basic types: \fIinteger\fR, \fIsymbol\fR, \fIstructure\fR,
and \fIlisp\fR.
Objects of type \fBinteger\fR are the usual numeric type, and objects
of type \fBlisp\fR can be any non-atomic Lisp object.
\fBSymbols\fR (objects of type \fIsymbol\fR) correspond to atoms in Lisp,
and are simply primitive objects with predeclared unique labels.
There is a special built-in symbol \fBnilsym\fR
(corresponding to \fInil\fR when considered as an atom)
which denotes a value of type symbol carrying no special conceptual
information, that is, devoid of meaning.
\fBStructures\fR are collections of slots.
Each slot of a structure
may be filled with an object of one of the four types.
There is also a meta-type for slots, \fBsetof\fR,
which can be applied (recursively) to any basic type to generate a new type,
which will consist of a list of the specified type of objects.
There is a special built-in structure \fBnilstruct\fR respresenting
the standard empty structure (similar to \fInil\fR when considered
as the empty list).
.sp 3
.PP
Types of structures must be predefined, with the number of slots, and
their names and types specified via a user declaration.
When an instance
of a structure is created and its slots filled, only objects with the
same type as the slot may fill it.
In addition, new structures may
build upon old ones in a hierarchical fashion by specifying new slots
to add to the old ones.
This hierarchy may be used in operations upon the data base.
.sp 3
.PP
Since the data bases are hash tables (to be described in
more detail later), each symbol and type of structure is assigned
a unique integer at definition time to be used by the hash function
to compute a location in the hash table.
This contributes significantly to the speed of data base operations
in PEARL since it allows the hash function
to be a simple computation based on these numbers rather than
depending on the spelling of the names.
It also helps to prevent
structures with similar names from being hashed in similar ways.
In particular, the unique numbers 0 and 1 are automatically
assigned to \fInilsym\fR and \fInilstruct\fR.
.sp 3
.PP
For example, symbols are declared as follows:
.DS 
\fIpearl>\fB(symbol  John  Home  Here)
    \fI(John  Home  Here)\fR
.DE
.LP
This call to \fIsymbol\fR sets up three unique objects whose print
names are "John", "Home", and "Here" and associates with
them the next three unique integers (2, 3, and 4).
Note that the value returned is a \fIlist of the symbols created\fR,
not a list of Lisp atoms and PEARL's print function prints this
value out as \fI(John Home Here)\fR.
.sp 3
.PP
The internal structure built by \fIsymbol\fR is a hunk of memory
big enough for two pointers pointing to the name and unique number.
.DS
    Internal representat\kaion of the symbol John:

    s:John --->	      \h'|\nau'Unique Number     \kb---|---> 2
		      \h'|\nau'Print Name        \h'|\nbu'---|---> John

.DE
.LP
Although we generally chose to use hunks of memory where possible
to save space (as demonstrated below), this representation
saves no space since it is equivalent to a cons-cell.
However, we chose to build it as a hunk and not a cons-cell since
in this way, PEARL can more easily distinguish it as a symbol
rather than a list cell.
The atom \fIs:John\fR is created with its value
set to the symbol John so that this unique symbol can be
generated at a later time, leaving the atom \fIJohn\fR available for
use by the user.
.sp 3
.PP
New types of structures and instances of previously defined
types of structures are all created with the function \fBcreate\fR.
The statement
.DS
\fIpearl>\fB(create  base  Act
                     (Actor  symbol) )
    \fI(Act (Actor nilsym))\fR
.DE
.LP
will define the primitive type \fIAct\fR with one slot named
\fIActor\fR and containing any single object of type symbol.
At the same time, \fIcreate\fR produces and returns an individual
instance known as the \fBdefault-instance\fR which contains the
standard default values for each slot.
PEARL also provides a
mechanism for changing these default values at definition time.
In this case, the slot Actor contains the default symbol \fInilsym\fR.
The other defaults are \fInilstruct\fR for structures, zero for integers
and \fInil\fR for slots of type \fIlisp\fR or \fIsetof\fR.
Again, the object returned by \fIcreate\fR is an internally represented
structure, not a list.
The representation of the definition and default-instance structures
internally as hunks of memory is as follows:
.DS
\klStructure definition for\km Act                  Default-instance for Act

                        \h'|\nmu'       <--------\kn|
\h'|\nlu'Unique Number	\h'|\nmu'---|---> 5      \h'|\nnu'|----|---\koDefinition
\h'|\nlu'Length		\h'|\nmu'---|---> 1	\h'|\nou'Var-List                      \kp---|---> nil
\h'|\nlu'Default Instance\h'|\nmu'---|---------->\h'|\nou'Var-List Copy	\h'|\npu'---|---> nil
\h'|\nlu'Isa		\h'|\nmu'---|---> nil
\h'|\nlu'Print Name	\h'|\nmu'---|---> Act	\h'|\nou'Value or Var Name	\h'|\npu'---|---> nilsym
\h'|\nlu'Hash Alias	\h'|\nmu'---|---> 0	\h'|\nou'Var-Value Pair	\h'|\npu'---|---> nil
\h'|\nlu'Expansion List	\h'|\nmu'---|---> nil	\h'|\nou'Predicate List	\h'|\npu'---|---> nil
\h'|\nlu'Base Ifs	\h'|\nmu'---|---> nil	\h'|\nou'Slot If List	\h'|\npu'---|---> nil

\h'|\nlu'Hash Information\h'|\nmu'---|---> 0 		\h'|\nou'  ^
\h'|\nlu'Type Number	\h'|\nmu'---|---> 0 		\h'|\nou'   |
\h'|\nlu'Slot Print Name\h'|\nmu'---|---> Actor		\h'|\nou'i:Act
\h'|\nlu'PP Set Info	\h'|\nmu'---|---> nil
			\h'|\nmu'  ^
			\h'|\nmu'   |
			\h'|\nmu'd:Act
.DE
.LP
There are many values in the above structure which are not yet
important to our discussion and which will be explained later.
The key values so far in the definition information are the unique
number, length of the structure (number of slots), the default
instance, and the print names of the structure itself and of its slot(s).
In the default instance note that a pointer to the definition is
kept in each instance to allow quick access to the unique number,
and other information during hashing and matching.
This means that the only time that a definition must be
accessed through its special atom (\fId:Act\fR in this case)
is when a new instance is created.
.sp 3
.PP
The most important feature of this representation however, is the
speed gained by the use of hunks.
In order to represent this
information as an S-expression, we would need one cons-cell (space
for two pointers) per piece of information with half of this space
wasted on the pointer to the next cons-cell.
Accessing a particular piece of information would require
\fIcdr\fRing down the list an appropriate number of times
which is potentially quite slow with a larger number of slots.
Also, since this definition information is pointed to by all
instances, the uniqueness of at least its header
cell must be maintained requiring some extra effort on the
part of the programmer in Lisp.
However, given the cost of a cons-cell in terms of garbage
collection time, it would be best to maintain the uniqueness
of all of its parts.
.sp 3
.PP
By using a hunk, we can 
.IP 1)
easily guarantee the uniqueness of a definition or instance,
.IP 2)
save the space used by list pointers, thus using half the space,
.IP 3)
use no new cons-cells after a structure is created,
.IP 4)
access any piece of a structure in constant time
(essentially two adds and a multiply at the worst), and
.IP 5)
compile all access operations relatively efficiently.
.LP
Thus, the use of hunks rather than lists contributes significantly
to the speed of PEARL.
.sp 3
.PP
Once we have defined the \fIbase\fR structure Act,
we can define more specific forms of Acts in terms of
it, using the \fBexpanded\fR argument to \fIcreate\fR in
place of \fIbase\fR:
.DS
\fIpearl>\fB(create  expanded  Act  Trans
                    (From  symbol)
                    (To  symbol))
    \fI(Trans  (Actor  nilsym)
	       (From  nilsym)
	       (To  nilsym))\fR
.DE
.LP
Here, we are declaring that Transes (transfers) are Acts with
two additional slots for the initial location From and the final
location To which are both symbols.
In addition to the information
diagrammed above, the structure definitions for Act and Trans are now
connected via their Isa and Expansion List fields (that is, the Isa
field of Trans points to Act and Trans is an element of the
Expansion List field of Act.
In this way, a complete tree of the
concept hierarchy rooted at a base structure is accessible from any
element in that hierarchy.
.sp 3
.PP
This hierarchy can be expanded to any depth.
Thus, we can now further
differentiate between various kinds of transfers, defining mental
transfers (MTrans) and physical transfers (PTrans).
In MTrans, the
mental object MObject slot will contain another concept and is thus
of type structure:
.DS
\fIpearl>\fB(create  expanded  Trans  MTrans
                    (MObject  struct))
    \fI(MTrans  (Actor  nilsym)
		(From  nilsym)
		(To  nilsym)
		(MObject  (nilstruct)))\fR
.DE
.DS
\fIpearl>\fB(create  expanded  Trans  PTrans
                     (From  Here)
                     (Object  symbol))
    \fI(PTrans  (Actor  nilsym)
		(From  Here)
		(To  nilsym)
		(Object  nilsym))\fR
.DE
.LP
Slots which are not filled by the user when creating
an individual are filled in automatically with the default value
from the default instance.
Note in the definition of PTrans that we give the
default value of \fIHere\fR for the previously defined slot
\fIFrom\fR by simply including the slot and its new value.
This means that whenever we create an \fIindividual\fR instance
of PTrans but do not specify a value for the From slot, it will
be filled in with the value Here:
.DS
\fIpearl>\fB(create  individual  PTrans
                    (Actor  John)
                    (Object  John)
                    (To  Home))
\fI(PTrans  (Actor  John)
	    (From  Here)
	    (To  Home)
	    (Object  John))\fR
.DE
.LP
This last structure denotes "John went home (from here)" in Schank's
Conceptual Dependency [11] theory of representation.
These representations are used in the rest of the paper simply
as an example of PEARL's use.
However, PEARL makes no commitment to any
particular set of predicates or primitives and can be used equally
well with any type of slot-filler structure.
.sp 3
.PP
Slots within a structure may also be filled with a
pattern-matching variable, in which case the structure may
be viewed as a pattern.
The simplest form of pattern is one in which any unspecified
slots are filled with the \fImatch-anything\fR variable \fI?*any*\fR.
For example, a pattern matching any PTranses performed by John
could be defined as follows:
.DS
\fIpearl>\fB(create pattern  PTrans
                    (Actor  John))
\fI(PTrans  (Actor  John)
	    (From  ?*any*)
	    (To  ?*any*)
	    (Object  ?*any*))\fR
.DE
.LP
However, \fBany\fR individual PEARL structure, including one with
all of its slots filled with actual values, can be used as a
pattern.
Thus, the first individual PTrans created above is
a pattern which matches only instances of John Ptransing home
from Here.
The sole purpose of using the \fIpattern\fR option to
\fIcreate\fR rather than \fIindividual\fR is
to change the default value for all types of slots to \fI?*any*\fR.
Variables are indicated by preceding them with a question mark
as in ?X for the variable X and, other than \fI?*any*\fR, they are
bound as part of the matching process (usually during a
fetch from the data base) which is discussed further below.
PEARL also provides functions for accessing and changing the
values of slots within individual structures
and for automatically naming the structure created.
.sp 3
.PP
Variables come in two other flavors in PEARL and are discussed in more
detail in the sections on matching and variables.
.NH
Data Base Facilities
.sp 3
.PP
PEARL allows for a forest of associative data bases
into which structures may be placed, and later fetched
via structure patterns.
Since many AI programs spend a significant part of their time
searching for knowledge in growing data bases, this needs to
be as efficient as possible.
.sp 3
.PP
Hashing is the usual programming solution to accessing a particular
element from within a large set.
However, traditionally, hashing has two prerequisites that are seldom
easy for an AI programmer to meet:
.IP 1)
The hash function must be carefully chosen in
advance to do a good job of spreading out the items to be inserted
with a minimum of computation.
In traditional applications of hash tables, this meant finding
a function which converts a (set of) string(s) into an index.
.IP 2)
Only completely specified items can be searched for.
That is, one may
not ask of a hash table "Find the closest one to X".
.LP
Unfortunately, since the knowledge structures used in AI are
much more complex than simple strings, finding a good hash
function is very difficult.
Also, in AI programming, normal hashing would only handle fetching
a particular fact from the data base, which would make the fetching
mean "Is this (completely specified) fact true?"
But it is much more likely that what is wanted is the (set of) thing(s)
which match a much more general pattern.
.sp 3
.PP
For these reasons, hashing in the normal sense is
inappropriate for AI data bases.
As a result, the traditional solution to the need for efficient
indexing into an AI data base is the discrimination net.
Or, in some cases, the data base is reduced to a linear list with
its inefficiency ignored (or tolerated).
With a discrimination net, the user often must carefully determine
the structure of the net and the nature of the tests to be made at
each level.
This is necessary to reduce the breadth of the net at each level,
since a discrimination net usually implies a linear search through
the possible values at each branch point.
As the knowledge changes, the representation hierarchy must change
to avoid this breadth problem, drawing the programmer away from the
problem at hand into worrying about indexing every step of the way
and forcing the representation into unnatural distinctions.
Generalized pattern matching is also difficult, making questions
like "What in the data base is close to X?" hard to ask.
.sp 3
.PP
Thus, a common problem with most AI data base implementations
is the system's lack of knowledge about how best to automatically
organize information for efficient and flexible retrieval.
The user usually has such knowledge, but needs to be able to provide
it in an easy way.
Moreover, if possible, this knowledge should be used to build a
hash table with its attendant speed, rather than a discrimination net.
The system must then provide a hash function which is
flexible enough to handle a large range of objects.
Such a hash table must also be organized in such a way that items may
be found which match a general pattern.
.sp 3
.PP
PEARL provides such a hash table and hash function, designed in such a
way that the user gets a significant speed up with only the effort
required to define objects as already been described above.
In addition, PEARL encourages the user to provide as much extra
knowledge as possible when a structure type is defined.
The choice of a particular structure hierarchy does not affect the
efficiency of the hashing so the representations are not twisted
to achieve efficiency.
Since the purpose of a hash function is to scatter
similar items, the required information consists of
indicating those slots whose values are most likely
to distinguish two similar structures.
.sp 3
.PP
This information is provided in the form of labels on
these slots in the definition of the structure.
Since only symbols and structures are assigned a unique integer at
definition time, slots of type \fIsymbol\fR, \fIstructure\fR
and \fIinteger\fR may contain such hashing information but slots
of type \fIlisp\fR may not.
These labels specify ways in which the unique number of the item being
hashed may be combined with the unique numbers associated with the
values of the labelled slots to provide a set of one, two, or three 
numbers to be combined into an index into the hash table.
The particular ways of specifying these slots and the
ways of grouping them is described below,
but first we describe the form of a single data base and
the organization of a forest of data bases.
.sp 3
.PP
Each data base is implemented as a pair of hash tables in which
each bucket is a list of the objects hashing to that spot.
The possible sizes of the data bases are chosen from the set of primes
which are just barely smaller than the powers of two,
(that is, 1, 3, 7, 13, 29, 61, 127, ...).
The two hash tables are chosen to be off by a factor of four,
(that is, 1+7, 3+13, ... 29+127, ...).
The two data bases are chosen to be of different sizes because 
it was hard to find a hash function to provide a good spread
in a large table for single small integers like the unique numbers
associated with structures.
The currently-used hash functions can be described as follows:
.DS
Let Size1 and Size2 be the sizes of the two hash tables.
Then the hash functions are:

    For indexes based on one number X:
	X mod Size1
    For indexes based on two numbers X and Y:
	(4096 * Y + X ) mod Size2
    For indexes based on three numbers X, Y and Z:
	( (4096 * 4096 * Z) + (4096 * Y) + X ) mod Size2
.DE
.LP
Thus the smaller of the two hash tables is used to enter
items indexed under only one unique number.
The larger is used for items indexed under combinations of
two or three numbers.
The sizes for hash tables can be chosen by the user to match the
number and variety of objects, the number of data bases being used
and the size of their machine's memory.
.sp 3
.PP
In order to allow flexible fetching using a pattern which is only
partly specified, and since the place we look must be determined based
upon the information that \fIis\fR provided in the pattern,
an item must be placed everywhere we are likely to be able to
look for it.
Thus, PEARL will index all individual instances of a structure type
which are inserted into the data base
under as many different hashing strategies as it can,
using the information provided by the user in the
definition of that type of structure.
Then to fetch with a particular pattern, PEARL need only use one of
the hashing strategies which uses slots from the pattern whose
values are considered hashable.
.sp 3
.PP
Whether there is hashing information or not, all individuals are
indexed in the smaller data base under (the unique integer
assigned to) their structure type.
Thus, with no effort, the user automatically gets one level
of distinction which provides a significant improvement
in efficiency over the often-used linked list.
This minimal use of hashing in PEARL is also an
improvement over discrimination nets since nets usually
imply a linear search through the possible values at each
branch point of the net instead of random access.
Of course, if the number of types of structures is
larger than the size of the data base, then after this random access,
there is still potentially a list of items to be searched linearly.
.sp 3
.PP
At this point, the speed with which the matching process
eliminates structures of the wrong type becomes important.
But the easily available unique number in each item provides a quick
test to eliminate items of the wrong type.
(For a complete description of the matching process, see the section on
predicates and matching.)
.sp 3
.PP
However, no amount of speed-up of the matching process can help as
much as a greater degree of discrimination by the hash function.
So to improve upon this automatic type of hashing, PEARL
needs to know which slots or collections of slots of a structure
are likely to help split up objects of the same type.
We will now describe each of the available hashing methods and the
circumstances in which you would want to use them.
.sp 3
.PP
The simplest case of adding hashing information is to label slots
whose values, in combination with the type of structure, would provide
a good distinction.
To indicate that a particular slot is useful in this way,
the user puts an asterisk (*) in that slot in the declaration.
Thus
.DS
\fIpearl>\fB(create base  PlanFor
                    (* Goal  struct)
                    (   Plan  struct))
\fI(PlanFor  (Goal  (nilstruct))
	     (Plan  (nilstruct)))\fR
.DE
.LP
defines a type PlanFor with slots for a goal and a plan, and indicates that
PlanFors should be indexed to be retrieved by the content of
their Goal slot plus the fact that they are PlanFors.
PEARL then uses the unique integers associated with the PlanFor
type and with whatever type of value is in the Goal slot.
.sp 3
.PP
Since the object filling the Goal slot of a PlanFor will always be
a structure of type Goal, using an asterisk in the Goal slot will not
actually distinguish PlanFors from one another.
In this case, we may
also wish to specify that the value that fills the Goal slot is to be
used to in a slightly different way to create the index.
For example, if the Objective of the Goal
is deemed more significant for such purposes than the fact that it
is a Goal, we can indicate this as follows:
.DS
\fIpearl>\fB(create  base  Goal
                    (   Planner  symbol)
                    (& Objective  struct))
\fI(Goal  (Planner  nilsym)
	  (Objective  (nilstruct)))\fR
.DE
.LP
This will inform PEARL that structures that indexing on slots
in other structures which are filled with Goal-type structures
should instead use the Objective slot for further discriminations.
Thus, Goals change the way in which other structures use them to
index but the way in which Goals themselves are indexed
will not be affected.
This hash labelling of Goal is called \fBhash aliasing\fR and
will cause all PlanFors to be indexed
under the number for the PlanFor type plus the number for the type of value in
the Objective slot of the Goal, and thus all PlanFor's for Goals for a
particular type of Objective will be indexed in the same bucket.
As a short hand, the phrase "indexed under the number for the PlanFor
type plus the number for the type of value in the Objective slot of
Goal" is abbreviated as "PlanFor + Objective(Goal)"
.sp 3
.PP
It might be the case that PlanFor was the only structure
indexed based on Goals which would benefit from this and that
some structures would actually be hurt by this because they
expected Goals to be only one of many types of values.
In this case, putting the control of how Goals get used by
other structures into the definition of Goal is a bad idea.
Instead, the control can be moved up into only the
problematic structures.
These structures can simply add the ">" hash label to
a starred slot, causing PEARL to use the first starred slot of
the slot-filling structure instead of its type.
.DS
\fIpearl>\fB(create base  Goal
                    (Planner  symbol)
                    (* Objective  struct))
\fI(Goal  (Planner  nilsym)
	  (Objective  (nilstruct)))\fR
.DE
.DS
\fIpearl>\fB(create base  PlanFor
                    (* > Goal  struct)
                    (Plan  struct))
\fI(PlanFor  (Goal  (nilstruct))
	     (Plan  (nilstruct)))\fR
.DE
.LP
If the user wanted to also star the Planner slot of Goal,
but wanted the Objective slot to be used in cases where the
containing structure had a ">",
then the use of an "^" on the Objective slot will allow that:
.DS
\fIpearl>\fB(create  base  Goal
                     (*    Planner  symbol)
                     (* ^ Objective  struct))
\fI(Goal  (Planner  nilsym)
	  (Objective  (nilstruct)))\fR
.DE
.LP
thus allowing Goals inserted directly into the data base to be
indexed by the combinations (Goal + Planner(Goal)) and
(Goal + Objective(Goal)) while objects containing Goals would
use the Objective slot rather than Goal (Object + Objective(Goal)).
If most structures containing Goals would benefit from the use of
the hash aliasing label & in Goal, but only one or two are hurt by it,
the use of "&" in Goal can be overridden by the structures
which will contain Goals by adding the "<" hash label to the starred
slot, thus giving the controlling structure the last word over how
it is hashed.
.DS
\fIpearl>\fB(create base  Goal
                    (   Planner  symbol)
                    (& Objective  struct))
\fI(Goal  (Planner  nilsym)
	  (Objective  (nilstruct)))\fR
.DE
.DS
\fIpearl>\fB(create base  OffendedStructure
      		    (* < Slot  struct))
\fI(OffendedStructure  (Slot  nilstruct)))\fR
.DE
.sp 3
.PP
The above methods are all designed to allow the indexing of a
structure to be based upon the type of structure and the type of the
value of one slot.
There are sometimes cases where one slot is not
enough to distinguish items sufficiently but two slots would do a much
better job.
For example, a program which dealt with a large number of
Goals of several planners might want to be able to ask whether a
particular planner had a particular objective.
Putting an asterisk in each of the slots of Goal would allow
hashing by one or the other, but it would be even faster to use the
fact it was a Goal, plus the values of both the Planner and Objective
slots.
Labelling this pair of slots with "**" causes their values
plus the structure type to be combined into an index.
.DS
\fIpearl>\fB(create base  Goal
      		    (* ** Planner  symbol)
      		    (* ** Objective struct) )
\fI(Goal  (Planner  nilsym)
	  (Objective  (nilstruct) ) )\fR
.DE
.LP
This is also useful whenever the range of types of values in
each slot is limited but the combinations of the two
have a wider range.
.sp 3
.PP
On the other hand, it may sometimes be useful to know all
structures containing a particular type of value in any prominent
slots.
Thus for example, if a program has many kinds of structures all
containing references to individual planners, it might be useful
to be able to efficiently ask the question "What do I know about
John?".
In this case, the use of a ":" hash label on those slots of relevant
structures which contain Planners causes all those
structure to be indexed by that slot's value only, without
regard to the structure type.
This would result in some bucket in the smaller data base to contain
all structures which refer to John in such a labelled slot,
because they would all be indexed under that single value.
Note that this is similar to the "&" type of hashing,
but affects the structure itself instead of containing structures.
.sp 3
.PP
Finally, there is a hash labelling which is the combination
of these last two ideas.
It may sometimes be useful to know all structures containing a two
particular types of values in prominent slots.
Thus for example, if a program has many kinds of acts and states
all containing references to individual person/object and the time
of occurrence, it might be useful to be able to efficiently ask
the question "What did John do at 8 o'clock?".
Thus, the use of a single pair of slots (in each structure) labelled
with "::" causes the value of those two slots to be combined
into an index.
.DS
\fIpearl>\fB(create base  Act
      		    (:: Actor  struct)
      		    (:: Time  int) )
\fI(Act  (Actor  (nilstruct))
	 (Time  0) )\fR
\fIpearl>\fB(create base  State
      		    (:: Object  struct)
      		    (:: Time  int) )
\fI(State  (Object  (nilstruct))
	   (Time  0) )\fR
.DE
In this case, all states of John or acts by John would be indexed
under John plus the time, thus ending up in the same hash bucket.
.sp 3
.PP
The hashing mechanism was designed to give the user benefit in
proportion to the effort expended in determining hash labels.
With no effort, the structure type provides some help.
With the addition of each label or pair of labels,
an item to be inserted into the data base is indexed into
another location in the hash table.
Thus the cost of extra labels is simply the time to find
another hash bucket (a few adds and multiplies), and add
the item to the front of the list implying the time and
space incurred by one cons-cell.
The benefit at fetching time is the ability to use this
extra information to narrow in on a small subset of
the items in the data base which are most likely to
be what is desired.
.sp 3
.PP
It is often the case that a program needs to build several
data bases where one or more are extensions of another.
For example, consider a planner which is trying to choose
between two alternative plans.
One way to do this is to simulate carrying each one out to
determine its likely effects (good or bad) to help in the
decision.
Thus the program might want to build a data base for each
into which it could assert the various facts determined by
the simulation.
Both of these new data bases would be considered extensions of the
usual data base with the added feature that anything stored in
them was simply expected to be true in the future.
Thus, after the simulation, it might be desireable to delete the
data base of the plan not chosen and the program would certainly
not want to assert the effects of the simulation into its regular
data base since they are not in fact true.
.PP
PEARL provides both these abilities by providing facilities for
building a forest of data bases.
The regular data base which is built automatically is called
\fI*maindb*\fR.
To build two extensions from this, one uses the function
\fIbuilddb\fR: to build a tree of data bases:
.DS
\fIpearl>\fB(builddb Test1 *maindb*)
\fI(database: Test1)\fR
\fIpearl>\fB(builddb Test2 *maindb*)
\fI(database: Test2)\fR
.DE
.LP
We can then assert various facts from the simulation into each of
these new data bases.
If we subsequently fetch from Test1, we will get back all facts
which were asserted into either \fITest1 \fBor \fI*maindb*\fR.
When we have decided which to use, we can then free up the one
that is no longer needed.
.NH
Fetching
.sp 3
.PP
To fetch an object from a data base, the user invokes the
fetcher with a structure to be used as a pattern.
For efficiency,
PEARL tries to narrow down the possible choices without
actually matching this pattern against any knowledge in the 
data base.
Thus, narrowing down the possibilities and avoiding
matching as long has possible are the two driving goals of the
fetching algorithm.
In order to narrow down the choices,
information in the pattern is examined to determine
which of the hashing indices is most likely to narrow
down the choices.
This determination is made based on the ways in which PEARL has been
instructed to hash structures of the same type as the pattern and also
based on which slots of the pattern actually have a useful value for
hashing.
\fINilsym\fR, \fInilstruct\fR, \fInil\fR and \fIunbound\fR values
are not considered useful.
Given the values which are considered useful and the hashing
information for the type of structure, the hierarchy of buckets to be
chosen is as follows:
.DS
** hashing
:: hashing
*  hashing
:  hashing
.DE
.LP
All the other hashing labels are modifiers on these four
methods and affect what values are used to compute the index.
.sp 3
.PP
The resulting hash index is used to choose a bucket from the hash
table which is returned to the user as a result stream.
No matching between the pattern and objects
in the data base occurs at this point and the
stream simply contains pointers to all data base items
in the same hash bucket, regardless of whether they actually 
match the pattern.
PEARL appends the pattern to the front of this
stream for subsequent use.
For example, to fetch all PlanFors involving Goals whose Objective
is a PTrans, we create a pattern for this type of object:
.DS
\fIpearl>\fB(setq  PTransGoals  (create  pattern  PlanFor
                                         (Goal  (Goal  (Objective  (PTrans))))
                                         (Plan  ?Plan)))
\fI(PlanFor (Goal (Goal (Planner ?*any*)
			(Objective (PTrans (Actor ?*any*)
					   (Object ?*any*)
					   (From ?*any*)
					   (To ?*any*)))))
	    (Plan ?Plan))\fR
.DE
.LP
and then call the function \fBfetch\fR with this pattern as an
argument:
.DS
(setq  PTransGoalStream  (fetch  PTransGoals))
.DE
.sp 3
.PP
The user may then extract items from
the stream one at a time by successive requests to \fBnextitem\fR:
.DS
(setq  Result  (nextitem  PTransGoalStream))
.DE
.LP
At each request, the pattern is matched against successive
items from the hash bucket until one matches,
in which case it is returned,
or until the potential items run out,
in which case \fInil\fR is returned.
.NH
Predicates and Matching
.sp 3
.PP
Predicates may also be attached to a slot specifying constraints on
the values of pattern-matching variables.
Each time a match is made between the slots of two structures
(described in detail below), the predicates of each slot are run to
determine whether the match should succeed or fail.
Two types of predicates are provided by PEARL.
The first type are Lisp functions or expressions to be evaluated.
If a predicate is simply the name of a function, that function is
applied to the slot value from the opposing structure.
If it is an S-expression, it is processed for special forms
which indicate where to get the arguments and then evaluated.
For example, the following pattern will require that the variable in
its first slot be bound to a positive integer value with the predicate
\fIplusp\fR.
It also requires that the variable in its third slot be bound
to a value which is a member of its second slot (\fB"*"\fR refers
to the value in the current slot of the opposing structure and
\fI"=Two"\fR refers to the value in the slot named Two of the
opposing structure):
.DS
\fIpearl>\fB(create individual  Example
		    (One  ?One  plusp)
		    (Two  ?*any*)
		    (Three  ?Three  (member  *  =Two) ) )
\fI(Example (One  ?One)
	    (Two  ?*any*)
	    (Three  ?Three) )\fR
.DE
.sp 3
.PP
The second type of predicate is called a \fBstructure predicate\fR and
consists of the name of a structure type.
Its effect is to restrict the value in a structure slot to being a
structure of the specified type.
Thus another way to restrict the value of the Objective of the Goal in
the Goal slot of the PlanFor which was fetched above, is to put a variable
in the slot and add a PTrans predicate:
.DS
\fIpearl>\fB(setq PTransGoals (create pattern PlanFor
                                      (Goal (Goal (Objective ?O PTrans)))
                                      (Plan ?Plan)))
\fI(PlanFor (Goal (Goal (Planner ?*any*)
			(Objective ?O)))
	    (Plan ?Plan))\fR
.DE
.LP
The effect is the same but testing the type of the value is much more
efficient than doing the matching process on the two PTranses slot by
slot.
.sp 3
.PP
For efficiency, the semantics of the matching have been
constrained to avoid the usual variable naming problems.
Two structures match if they can be unified.
However, no attempt is made to detect circularities,
nor are variables ever bound to other variables.
Circularities have never actually occurred in our experience and
most variables are local to the pattern they appear in,
so naming conflicts do not arise.
Of course it would be straightforward to add checks for
these problems if one was willing to incur the expense.
.sp 3
.PP
The variables in a structure are implemented as an assoc-list attached
to the structure so that a list of the variables of a structure can be
located quickly.
However, \fIassoc\fR is only used for external lookup of a variable.
Once the structure has been created, each slot containing a variable
has a pointer to the special cons-cell associated with it in the
assoc-list so that it has immediate access to its value.
In particular, the name of the variable is not even accessed during
the matching process, since its value is all that is needed.
.sp 3
.PP
In general, the matching procedure takes two structures
which each may contain variables.
If the structures are not definitionally the same type
then the match fails automatically.
This quickly eliminates items which happen to hash to the same slot.
Otherwise, each structure is viewed as a sequence of slots
which are successively matched between the two structures.
Two structures of the same type match if and only if each of their
slots matches the corresponding slot of the other structure.
Each slot is filled in one of three ways:
.IP 1)
The slot may contain an actual value of its type (for example,
a slot of type \fIstructure\fR may contain a PTrans).
.IP 2)
The slot may contain a user-defined variable.
.IP 3)
The slot may contain the special match-anything variable \fI?*any*\fR.
.LP
If the slot contains a variable (other than \fI?*any*\fR) which has not
been bound then it may become bound as a side effect of the
matching process.
Once a variable is bound to a real value during the
matching process, for the purposes of matching, it will
be treated as if the slot were filled with that value.
.sp 3
.PP
We now examine each of the pairings of slot values which may
occur and how they are matched.
If either of the two slots being matched contains the special
variable \fI?*any*\fR, then the slots match by definition,
regardless of the contents of the other slot.
If both slots contain variables that are unbound, the slots do not
match.
(This is true even if the two variables are textually
the same name, since they are each considered local to their
particular structures.)
If one slot contains an unbound variable (and the other
a bound variable or a value), then any predicates on the
slot with the unbound variable are tested to see if the
unbound variable should be bound to the bound value.
If so, then the unbound variable is bound to the value
of the other slot, and the two slots match.
If any of the predicates return \fInil\fR, the two slots do not
match, the variable is not bound, and the entire match fails.
.sp 3
.PP
If both slots contain either bound variables or values,
then the values of the two slots are compared.
If the slot is of type \fIstructure\fR, then the entire matching
algorithm is recursively applied.
If the slot is of types \fIinteger\fR or \fIlisp\fR, then \fIequal\fR is used.
If the type is \fIsymbol\fR, than the two values must be the same symbol.
Regardless of the type, any predicates associated with the
slot are run and all must succeed.
.sp 3
.PP
For example, if we create two structures, one representing Sam
and one with a variable in the \fIName\fR slot:
.DS
\fIpearl>\fB(create individual  Person Sam
		    (Name  Samuel))
\fI(Person (Name Samuel))\fR
\fIpearl>\fB(create individual  Person PersonPattern
		    (Name  ?FirstName))
\fI(Person (Name ?FirstName))\fR
.DE
.LP
then match them and look at the result in \fIPersonPattern\fR:
.DS
\fIpearl>\fB(match Sam PersonPattern)
\fIt\fR
\fIpearl>\fBPersonPattern
\fI(Person (Name ?FirstName = Samuel))\fR
.DE
.LP
we find that they do match and the variable \fI?FirstName\fR in
\fIPersonPattern\fR has been bound to the symbol \fISamuel\fR.
.PP
We now take a slightly more complicated example.
In PEARL's matching algorithm there is no sense that one of its
arguments is the pattern and one the thing to be matched to, so
both may have variables.
As long as the variables are in different slots so that \fImatch\fR
will never try to match two unbound variables to each other, the
matching will work fine.
Thus, if we want our backward inference mechanism from the extended
example in section 2 to not only tell us \fIthat\fR Sam has a low
salary but in fact \fIwhat level\fR of salary he had, we could
fetch the following structure:
.DS
.Ls
(create \kBindividual Salary SamsSalary
        \h'|\nBu'\kA(Employee Sam)
        \h'|\nAu'(Level ?Level))
.Le
.DE
.LP
This would result in the backward inference demon using the
following pattern to fetch rules that might tell it about
finding a person's salary:
.DS
\fIpearl> \fB(create pattern BackwardRule Wanted
		     (Need (Salary (Employee Sam)
				   (Level ?Level))))
\fI(BackwardRule (Need (Salary (Employee (Person (Name Sam)))
			       (Level ?Level)))
		 (LookFor ?*any*))\fR
.DE
.LP
In processing the resulting stream, the matcher would be called
upon to match the above pattern \fIWanted\fR to the following
rule (which is in the data base but which we recreate for
this example):
.DS
\fIpearl> \fB(create individual BackwardRule Known
		     (Need  (Salary  (Employee  ?Person)
				     (Level  Low)))
		     (LookFor  (Professor  (Person  ?Person))))
\fI(BackwardRule (Need (Salary (Employee ?Person)
			       (Level Low)))
		 (LookFor (Professor (Person ?Person))))\fR
.DE
Matching these will succeed and in the process the variables 
\fI?Level\fR in \fIWanted\fR and \fI?Person\fR in \fIKnown\fR
will be bound:
.DS
\fIpearl> \fB(match Wanted Known)
   \fIt\fR
\fIpearl> \fBWanted
\fI(BackwardRule (Need (Salary (Employee (Person (Name Sam)))
			       (Level ?Level = Low)))
		 (LookFor ?*any*))\fR
\fIpearl> \fBKnown
\fI(BackwardRule (Need (Salary (Employee ?Person = (Person (Name Sam)))
			       (Level Low)))
		 (LookFor (Professor (Person ?Person
					     = (Person (Name Sam))))))\fR
.DE
.NH
Variables
.sp 3
.PP
There are three types of pattern matching variables in PEARL.
Global variables (which are just Lisp variables) must be declared
and are never unbound by PEARL.
All undeclared variables are local to the individual structure in
which they are mentioned.
Local variables are dummy variables, local to a particular
structure and any of its components which were created
in the same instant.
They are all unbound by PEARL before every match on that structure.
The examples given of variables in the previous section were of local
variables which require no declaration.
The third, intermediate, type of variable provides lexical
scoping within groups of structures.
Lexically scoped variables are like local variables in that they
are unbound by PEARL before a match is made, but have their
scope extended across several structures as indicated by the user.
.sp 3
.PP
Consider the following examples of the three types of variables.
For our first example, suppose that in a data base representing the
planning knowledge of a particular person is an Ego structure which
records the identity of that person.
The program wishes to determine this and to remember it in a variable
called Planner.
Planner is declared to be global and then used to fetch the
appropriate knowledge structure from the data base:
.DS
.Ls
(global  Planner)
(nextitem  (fetch  (create \kAindividual  Ego
                           \h'|\nAu'(Identity ?Planner))))
.Le
.DE
.LP
At this point, the Lisp atom Planner is bound to the
identity of the planner.
We can now ask for all PTranses in the data base involving the planner
as the Actor and Object:
.DS
.Ls
(setq  Pat  (create \kBindividual  Ptrans
                    \h'|\nBu'\kA(Actor  ?Planner)
                    \h'|\nAu'\kB(Object  ?Planner)
                    \h'|\nBu'\kA(From  ?Start)
                    \h'|\nAu'(To  ?Dest)))
(setq  Stream  (fetch  Pat))
.Le
.DE
.LP
At this point the pattern in Pat has two local variables,
\fI?Start\fR and \fI?Dest\fR which will be unbound before each match and
may be bound to a new value during each match.
\fI?Planner\fR on the other hand is global and will continue to have the
value it received during the original fetch it was used in.
.sp 3
.PP
With a global variable, a group of structures are allowed to share a
variable whose value is constant once it is set the first time.
Furthermore, all structures are thereafter required to share this same
variable and are precluded from having their own variable with the
same name.
However, it is sometimes useful to group a set of structures together
via a set of variables which we wish to behave like local variables in
every other way.
Furthermore we might wish to have several such groups which can each
have a variable with this same name.
For example, a body of PEARL structures conceptually composing a
single frame should be made to share the same variables but it should
be possible to have several instances of such a frame with the same
variable names tying each group together without interfering with the
others.
Each instance of this group of variables is then local to that frame.
However, the results of matching any particular component
of the frame will be detectable in the variables associated
with the other components.
This is done in PEARL by dynamically declaring a scope with local
variables which are imposed upon all structures created until that
scope is closed.
For example, consider the following sequence:
.DS
.Ls
(block  Plan1  (Planner  Goal))
(create \kBindividual  PlanFor
        \h'|\nBu'\kA(Goal  ?Goal)
        \h'|\nAu'(Plan  ?Plan))
(create \kBindividual  Goal
        \h'|\nBu'\kA(Planner  ?Planner)
        \h'|\nAu'(Objective  ?Objective))
(create \kAindividual  Plan
        \h'|\nAu'\kB(Planner  ?Planner)
        \h'|\nBu'\kA(Goal  ?Goal)
        \h'|\nAu'(Steps  ?Steps))
(endblock  Plan1)
.Le
.DE
.LP
This sequence creates three structures which are intimately tied
together via the variables \fI?Planner\fR and \fI?Goal\fR which
are declared in the enclosing block.
After this code executes, if any of the structures is fetched from
the data base, any binding of these two variables would have an
immediate effect in all of them.
In addition, the values of these variables are available simply by
knowing the name of the block, so that one can ask for the value of
the Planner variable in Plan1 directly.
However, now that the block has been closed, other structures are
free to have variables with the same names.
.NH
Demons
.sp 3
.PP
A common AI mechanism provided by AI languages is one of
"if-added" functions or demons.
PEARL has a general ability to attach functions called \fIhooks\fR
to base structures (\fIbase hooks\fR) or to slots of individual
structures (\fIslot hooks\fR).
Base hooks are run whenever the particular PEARL function that the
hook is labelled with accesses an individual of that type.
Slot hooks are put into individual and are run whenever the particular
PEARL function that the hook is labelled with accesses that slot
of the individual.
In order to allow these hooks to tailor the operation of the
various PEARL functions on particular structures or types of
structures, these demons may be invoked either before or after
the PEARL function they are labelled with does its work.
If they run before, they are allowed to short-circuit the function's
action or perform it themselves and specify a value to return.
If they run after, they may also modify the value to be returned.
.sp 3
.PP
For example, in the extended example at the beginning of this
paper, we presented a simple inference package which would run
automatically whenever an object was fetched or inserted.
To implement this, we wrote two functions MakeForwardInference
and MakeBackwardInference.
MakeForwardInference was designed to use rules which said if
you learn X then infer Y.
MakeBackwardInference was designed to use rules which said if
you want to know X then check to see if you know Y.
Learning something while using PEARL usually means inserting
something into the data base, so we wish to have
MakeForwardInference run whenever we insert some concept
into the data base, after the insertion.
Wanting to know something while using PEARL means fetching
it from the data base, so we wish to have MakeBackwardInference
run whenever we fetch some concept from the data base, before the
actual fetch takes place.
This was accomplished by attaching these two functions as demons
to the base structure Concept as follows:
.DS
.Ls
(create \kBbase  Concept
        \h'|\nBu'(if  \kA<insertdb  MakeForwardInference
             \h'|\nAu'>fetch  MakeBackwardInference))
.Le
.DE
.LP
A similar mechanism is available for attaching demons to individual
slots of structures.
Other than through matching, the principle way that slots of
already created structures get changed is through the PEARL
function \fIputpath\fR.
For example, if Sam got a raise, making his salary level
\fIMedium\fR, we might want to change the \fILevel\fR slot of
his Salary structure:
.DS
(putpath SamsSalary 'Level (getsymbol 'Medium))
.DE
If there were facts in the data base (like the fact that Sam is
poor) which depended on this fact, we would be interested in
monitoring Sam's salary level so that we could fix this up.
Of course, a general data dependency mechanism would be much
better but if you did not have one, one possible way of
accompishing this would be to attach a demon to the Level slot of
\fISamsSalary\fR at the time of creation:
.DS
.Ls
(create \kBindividual Salary SamsSalary
        \h'|\nBu'\kA(Employee Sam)
        \h'|\nAu'(Level  ^  if  >putpath  (AdjustPoorness  =Employee  *)))
.Le
.DE
This assumes that \fIAdjustPoorness\fR is a function expecting the
name of the person and the new level.
.PP
Like predicates, a PEARL demon may be either the name of a function
to be run with the structure or slot as its argument
or it may be a general S-expression which contains any of the
special forms which refer to the current structure or slot.
Besides the built-in PEARL functions which automatically check for
demons with their names on them attached to slots that they touch,
there is a facility for user-defined functions to explicitly
request that demons on structures or slots that they touch be run.
However, this action is not automatic;
the involved functions must explicitly run the demons.
.NH
Implementations
.sp 3
.PP
The main emphasis of efficiency considerations within PEARL was
to allow the user to avoid inefficient algorithms.
We also tried to make the code itself as efficient as possible.
To make the user interface as friendly as possible,
error checking is done whenever it can be done efficiently.
As a result of these two principles, PEARL is fast and
friendly enough for use as a serious programming language.
.sp 3
.PP
PEARL was also intended to be portable.
It was originally developed on a DEC 20 and moved with no
modification to a DEC PDP-10 under UCI Lisp.
It was then moved to a VAX-11/780 under Franz Lisp [3] [4]
which at that time did not provide a facility for allocating
hunks of memory and thus required the lowest level of the
implementation to be rewritten using arrays.
Since the lowest level of the UCI Lisp version was written in
Lisp assembler (LAP) operating on blocks of memory, this new
Franz Lisp version was somewhat less efficient.
When "hunks" were added to Franz Lisp, we attempted to modify
the VAX version to use them.
However, since Franz Lisp hunks behave significantly differently
in several ways from blocks of memory in UCI Lisp, this was
abandoned temporarily.
Instead, we used what we learned to redesign the lowest level of
the UCI Lisp version of PEARL so that it could be easily moved
between UCI Lisp and Franz Lisp and then moved it back to the VAX.
.PP
We now believe that PEARL could be moved to another Lisp by
rewriting about a dozen functions and adding the macros
needed to convert from UCI Lisp to the target Lisp.
(Only one of these is now machine coded on the PDP-10,
a routine for doing address arithmetic.
The Franz Lisp version is completely in Lisp.)
The primary functions which must be rewritten pertain to creating
and accessing hunks of memory and modifying the top level
read-eval-print loop.
We are currently verifying PEARL's portability by moving it to
both MACLisp and Lisp Machine Lisp.
.NH
Performance
.sp 3
.PP
As mentioned, PEARL gains much of its speed during fetches from the
data base by using a user-assisted hashing mechanism.
Here we present some evidence that this mechanism does in
fact speed up access to the data base.
To test this, we timed the running of a recent version of PAM,
a story understanding program [12], which was written using PEARL.
For these timings, we used the Franz Lisp version of PEARL.
Since the size of PEARL data bases is user-settable, we compared
two runs of PAM on a large (23 sentence) story, one using the
largest available hash table (see below for details of sizes)
and one using the smallest available hash table which is logically
equivalent to a linear list.
.sp 3
.PP
For each run we read in the initial knowledge and program
once and then processed the story three times to test the effects
of the data base getting fuller.
The results are as follows:
.DS
\h'|\nau'
		\kaSmall Table		\kbLarge Table

Load		\h'|\nau'68 + 13	\h'|\nbu'30 + 5

Run 1		\h'|\nau'96 + 10	\h'|\nbu'65 + 10

Run 2		\h'|\nau'113 + 11	\h'|\nbu'66 + 9

Run 3		\h'|\nau'129 + 9	\h'|\nbu'65 + 10
.DE
.LP
Note that while the large hash table was quite stable as the
amount of information in it approximately tripled, the small
hash table causes the execution times to increase substantially 
as the data base fills up.
.sp 3
.PP
In similar comparisons with UCI Lisp on the PDP 10, the results
were even more dramatic.
Times for the large data base were flat but using a small data base,
each run's time was bigger than the previous run by 50% of the first
run's time and each run's garbage collection time was bigger
than the previous by 100% of the first run's garbage collection time.
.DS
		\kaSmall Table		\kbLarge Table

Load		\h'|\nau'17 + 2		\h'|\nbu'16 + 2

Run 1		\h'|\nau'64 + 13	\h'|\nbu'24 + 2

Run 2		\h'|\nau'92 + 22	\h'|\nbu'24 + 1

Run 3		\h'|\nau'125 + 33	\h'|\nbu'26 + 2
.DE
.LP
This indicates that PEARL could make large programs running on
the PDP10 must faster.
It also indicates that although the VAX is a slower machine, 
with its virtual memory it behaves quite well under what a load
that taxes the PDP 10.
.sp 3
.PP
Another piece of timing we performed is also interesting to those
considering moving to VAXes from PDP 10s.
All of the above timings were of compiled versions of PEARL on
both machines.
(The PAM code was not compiled.)
Thus, Franz Lisp on the VAX seems to run the same program
with 2-2.5 times the CPU time of UCI Lisp on the PDP 10.
Since the ratio between the speeds of the processors is estimated
at 2.5, compiled Franz Lisp code competes favorably (modulo the
processor speed) with compiled UCI Lisp code.  
However, we also tried running PAM with uncompiled versions
of PEARL on both machines.
In this case, we found that the Franz Lisp version ran 10 times
slower, while the UCI Lisp version ran only 3 times slower.
This would seem to imply that either the Franz Lisp interpreter
is abnormally slow or that the UCI Lisp interpreter is
unusually fast.
When the MAC Lisp and Lisp Machine Lisp versions are running, we
will explore this further.
.sp 3
.PP
Although we have not done any extensive profiling of PAM to
determine where all the time is spent, we have tried disabling the
printing functions while running PAM.
Doing this, we discovered that PAM spends about 55% of its time
doing input and output.
This breaks down to 5% for input, 10% for conversion to list
structure from internal PEARL structures and 40% for actual
(pretty-)printing by Lisp.
.NH
Comparison to FRL
.sp 3
.PP
Of the existing AI languages, PEARL has the most in common with FRL.
This is true partly because both languages use several good ideas
which have been around in AI for some time.
It is also partly true because some of PEARL's features were added
in imitation of the example representation language XRL presented
in Charniak[2] (which the authors admit is partly in imitation of
FRL and KRL).
In this section, we discuss some of the ways that PEARL differs
from FRL.
.sp 3
.PP
Like PEARL, FRL is designed for representing slot-filler objects.
However, in FRL these objects are modelled more after frames as
described by Minsky [7] whereas PEARL's structures lean more
toward logical predicates.
In particular, frames become \fIactivated\fR or
\fItriggered\fR by being instantiated and the data base
is simply all the activated frames;
there seems to be no distinction between instantiating a
frame and adding it to the data base.
In contrast to PEARL, frames are not encoded internally but
represented as multiple depth association lists and the FRL
data base is not hash coded.
The FRL manual [10] seems to imply that it is in fact a linked list
subject to a sequential search.
The idea of separating frames into groups like PEARL's multiple
data bases has been recently added as "domains" [6].
.sp 3
.PP
Whereas PEARL requires type information on its slots and uses this
information to advantage, FRL requires no information on the type
or even number of values which will be allowed.
This of course allows the user more flexibility but makes it more
difficult for FRL to deal efficiently with each slot.
.sp 3
.PP
In addition to the slot/filler features, FRL uses 6 primary
representation techniques to improve the flexibility of frames.
These are comments on slots, abstraction through slot inheritance,
inherited default values for slots, constraints on slot values,
indirection through values in other frames and attached procedures.
We look briefly now at each of these.
.IP 1)
Comments in FRL are attached to slots and are generally used to
remember where the value in a slot came from although they could
be used for anything.
This is a useful feature which in PEARL must be
implemented as a separate set of predicates inserted into the data
base or as dynamically-added attached procedures.
We have not added them to PEARL because we are unsure whether such
information should rightly be distinguished from predications
about where other pieces of knowledge came from.
.IP 2)
The notion of inheritance of slots from more abstract objects is
quite similar in FRL and PEARL, since this is one of the features
PEARL inherited through XRL.
The principle difference is that while in PEARL all slots must be
predeclared (because of the internal mode of storage), FRL allows
the addition of slots at a later time.
.IP 3)
The notion of default values was similarly inheritted from FRL by PEARL.
However, in designing PEARL we wished to more clearly separate the
idea of a general piece of knowledge represented by the definition
of a type of structure along with its set of defaults from an instance
of such a structure.
As a result PEARL stores the default values for slots in the special
instance of each type of structure called the \fIdefault instance\fR.
In contrast, FRL does not make this distinction clear and provides
for both a default and a value in a slot of a frame.
Apparently a frame may be both an instance and a generalization
at the same time.
.IP 4)
FRL's notion of constraints is significantly stronger and more
complex than PEARL's.
PEARL provides for predicates on slots but these are only enforced
during matching on slots containing variables.
FRL on the other hand provides three flavors of constraint with
different degrees of restriction.
A \fIrequirement\fR is a strong predicate on a slot which must be
true of the value in that slot.
A \fIpreference\fR is a weaker predicate which may be relaxed.
A weaker special case of a preference is a default which simply
suggests a specific value which can be easily overridden.
.IP 5)
A feature of FRL which goes hand-in-hand with the idea of triggered
frames (and is thus lacking in PEARL) is that of indirection.
This allows a frame to specify constraints on slots of other
frames that are currently active when it is triggered.
Thus indirection provides what might be considered a "horizontal"
version of the vertical notion of default inheritance.
.IP 6)
Demons and attached procedures are old ideas in AI but FRL
introduced a new twist on them which PEARL then took one step
further.
FRL provides for if-added, if-needed, and if-removed procedures which
are attached to slots and rather than being triggered by arbitrary
conditions are instead run only in the case of adding, requesting
or removing the value of a slot.
These attached procedures are enforced by the functions that
perform these types of access, thus providing for idiosyncratic
forms of inheritance or finding a slot value.
In PEARL we extend this idea so that there are a large variety of
access functions which may trigger attached procedures (hooks).
In addition, these procedures are allowed to affect the actions
of the access functions, thus allowing a particular class of
objects to tailor the behavior of most of PEARL's functions.
Similarly, procedures to tailor the performance of printing and
other functions on objects (rather than their slots) are provided
by both FRL (via the SELF slot) and by PEARL (via base hooks)
In addition, a form of detached procedures ("sentinels") have
recently been added to FRL [6] in which the triggering condition
is the activation of a group of frames.
.sp 3
.PP
In contrast to more ambitious knowledge representation languages,
FRL and PEARL are similar in their fairly restricted matching
procedures which are essentially slot-by-slot matches with no
provision for matching to a degree or forcing a match via mapping
as in MERLIN [8].
.sp 3
.PP
Finally, there are two features of hierarchical representations
which FRL provides but which are not yet provided by PEARL.
The principal one is the ability to store multiple views of an object
thus allowing a frame to inherit slots from several other frames.
The second one is the ability to move an object down the hierarchy,
thus providing the dynamic ability to further specify a previously
general frame based on new information.
Both of these are in the works since we have encountered a need for them.
.NH
Comparison to KRL
.sp 3
.PP
.bp
.NH
References
.sp 2
.IP [1]
Bobrow, D. G., and Winograd, T. "An Overview of KRL, a Knowledge
Representation Language."
\fICognitive Science\fR 1:1 (1977).
.IP [2]
Charniak, E., Riesbeck, C. K.,  and McDermott, D. V.
\fIArtificial Intelligence Programming\fR. Hilldale, New Jersey:
Lawrence Erlbaum Associates, 1980.
.IP [3]
Fateman, R., "Views on Transportability of Lisp and Lisp-based Systems",
in \fIProc. of the 1981 ACM Symposium on Symbolic and Algebraic
Computation\fR p 137-141, (ACM order no 505810), 1981.
.IP [4]
Foderaro, J. K., and Sklower, K. L.
\fIThe Franz Lisp Manual\fR in \fIBerkeley UNIX Reference Manual\fR,
Vol. 2c., Computer Systems Research Group, Computer Science Div.
EECS Dept., University of California, September, 1981
.IP [5]
Greiner, R., and Lenat, D. "A Representation Language Language."
In \fIProc. First NCAI\fR. Stanford, CA, August, 1980,
165-169.
.IP [6]
?????, ?. "Extended Features of FRL" ?????, reproduced in forthcoming
edition of [4], 1982.
.IP [7]
Minsky, M. "A Framework for Representing Knowledge" in P. H.
WInston (Ed.) \fIThe Psychology of Computer Vision\fR,
New York: McGraw-Hill, 1975.
.IP [8]
Moore, J., and Newell, A. "How Can MERLIN Understand?" in L. Gregg
(Ed.), \fIKnowledge and Cognition\fR, Lawrence Erlbaum Associates, 1973.
.IP [9]
Roberts, R. B., and Goldstein, I. P.
"NUDGE, A Knowledge-Based Scheduling Program."
In \fIProc. IJCAI-77\fR. Cambridge, MA, August, 1977, 257-263.
.IP [10]
Roberts R. B., and Goldstein, I. P.
\fIThe FRL Manual\fR,  MIT AI Memo, September, 1977, reproduced in
forthcoming edition of [4], 1982.
.IP [11]
Schank, R. \fIConceptual Information Processing\fR. Amsterdam: North Holland,
1975.
.IP [12]
Wilensky, R. "Understanding Goal-Based Stories",
Technical Report 140, Computer Science Department,
Yale University, New Haven, CT, September 1978.
.IP [13]
Wilensky, R.
"Meta-Planning: Representing and Using Knowledge about Planning in Problem
Solving and Natural Language Understanding."
\fICognitive Science\fR 5:3 (1981). 
.rm CF
.bp 0
.sp 14
.B
.LG
.ce
Table Of Contents
.SM
.sp 2
.DS
  1.  Introduction                                                        \ka 1
  2.  An Overview and Sample Application Of PEARL	\h'|\nau' 2
  3.  How Fast Is PEARL?		\h'|\nau' 6
  4.  Objects and Structures		\h'|\nau' 7
  5.  Data Base Facilities		\h'|\nau'11
  6.  Fetching				\h'|\nau'17
  7.  Predicates and Matching		\h'|\nau'18
  8.  Variables				\h'|\nau'21
  9.  Demons				\h'|\nau'23
10.  Implementations			\h'|\nau'24
11.  Performance			\h'|\nau'25
12.  Comparison to FRL			\h'|\nau'26
13.  Comparison to KRL			\h'|\nau'29
14.  References				\h'|\nau'31
.DE
.bp 0
.sp 14
.SH
Acknowledgements
.PP
PEARL was originally a joint project of Joe Faletti and Mike
Deering (now at the Fairchild AI Lab in Palo Alto) aimed at
redesigning, extending and completely rewriting an earlier
package designed and written by Mike.
PEARL owes many ideas and much of its success to Mike who
has been involved in all design decisions.
In particular, the hashing scheme which is responsible for
much of PEARL's efficiency was originally his idea.
.PP
The initial move of PEARL to the VAX from which we learned enough
to make the second one easier was accomplished by Mike Deering
and Doug Lanam (now at the Hewlett Packard AI Lab in Palo Alto).
The move was made significantly easier by Doug's UCI Lisp
compatibility package for Franz Lisp.
.PP
The authors wish to thank Mike and Doug for their contributions.
We also wish to thank the members of the Berkeley AI Research
group (BAIR) who have used PEARL during its development and made
many valuable suggestions based on active experience in its use.
