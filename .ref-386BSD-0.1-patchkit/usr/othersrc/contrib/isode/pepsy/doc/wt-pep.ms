\" Walk through of the pepsy library routines
.NH 2
Walk through of pepsy library routines.
.XS
Walk through of pepsy library routines.
.XE
.PP
Here we walk through all the pepsy library routines at least briefly.
If any new routines are added or a routine changed this documentation
is the most likely part that will need changing.
First we give some theory as to how the task have have been brocken
into routines then describe each function in detail.
We assume you are familiar with \fBISODE\fR's \fBPE\fR data
structure manipulation routines.
if not they are documented in the \fBISODE\fR manuals, Volume one, chapter 5,
"Encoding of Data-Structures" (It actually covers decoding as well).
.NH 3
Overview of pepsy library
.XS
Overview of pepsy library
.XE
.PP
Each seperate task is put into a different file.
So all the encoding stuff is in \fIenc.c\fR, all the decoding stuff is
in \fIdec.c\fR, printing stuff in \fIprnt.c\fR and freeing stuff in \fIfre.c\fR.
Actually it breaks down a little in practice, some of the routines for
moving around the tables are used in both \fIenc.c\fR and \fIdec.c\fR
for example.
Probably they should defined in \fIutil.c\fR so that linking one of the files
from the library doesn't force linking any other except \fIutil.o\fR.
.PP
There is a common structure to each of the major files as well.
There is a main routine which the user calls to obtain the services
provided by that file's routines.
As all the files revolve about processing the table entries their
structure is based on running through the table entries.
.PP
We shall call each array of entries a table or an object.
There is a routine, usually with a name ending in _obj, which is designed
to process an object.
For example \fBen_obj\fR is the routine called to generated an encoded
object.
Then there are routines to call on each compound type
such as \fBen_seq\fR for encode a SEQUENCE.
Finally all the primitives are handled by a one function that ends in _type.
This lets each routine concentrate on handling the features particular to
its type and call the appropriate routine to handle each type it finds
with in its compound type.
.PP
Most of these table processing routines have just three arguements:
which are called \fBparm\fR, \fBp\fR, \fBmod\fR.
The \fBparm\fR is char * or char ** in the encoding and decoding routines
respectively.
This points to the user's \fBC\fR structure that data to be encoded
is taken from when encoding.
When decoding it is the address of a pointer which is made to point
the \fBC\fR structure filled with the decode data.
The freeing, which is based on the decoding routines, has a char **
while the printing routines don't look at the user's data and so don't
have such a pointer.
The \fBp\fR points to the current table entry we are up to processing and
the \fBmod\fR arguement points to the \fBmodtyp\fR structure for the current
module we are processing.
.PP
All these processing routines return a \fBPE\fR type,
which is defined in \fBISODE\fR's file \fIh/psap.h\fR, and to return zero
if they have an error, but not always.
In fact the error handling is needs some work and has not
been tested very well.
Generally it tries to print out the table entry where something went wrong and
the name of the function it was in.
It then sometimes does an exit which may not be very pleasent for the
user.
.NH 3
The encoding routines - enc.c
.XS
The encoding routines - enc.c
.XE
.IP enc_f
This is the the routine made available to the user for the encoding routines.
It is fairly simple as it leaves all the hard things up to other routines.
All it does is use the type number and \fBmodtyp\fR pointer to get
a pointer to the table for encoding that type.
Then it calls the table or object encoding routine, \fBen_obj\fR,
on that object.
It first does a consistency check of making sure the first entry in the table 
is a \fBPE_start\fR.
Note that it returns an integer (OK or NOTOK) instead of a \fBPE\fR pointer.
This is to be consitent with \fBISODE\fR functions.
.IP en_obj
We loop through the entries until we come to the end of the table and then we
return the \fBPE\fR we have built up from the user's data which is pointed
to by \fBparm\fR.
In looping through each entry we call the appropriate routine to encode its
data.
The default case is handled by calling \fBen_type\fR which takes care of
all the primitive types.
.PP
The macro \fBNEXT_TPE\fR sets its arguement to point to the next type
in the table, counting compound types as one type.
Thus if \fBNEXT_TPE\fR is called on a \fBSET_START\fR it will skip all the
entries up to and including the matching \fBPE_END\fR.
As many objects consist of one compound type and its components the main
loop will only be run through once.
Even when the object is not based on a compound type it will then consist of
one simple type which is processed by \fBen_type\fR, again probably
going through the loop only once.
In fact the only way it can go through the loop more than once
is to process entries that subsidary to the main type, e.g. \fBETAG\fB entries
and things like that.
To double check this is the case there is some code that looks for
the processing of more than one data generating entry.
.PP
Much of that testing could probably be eliminated with no loss.
Similarly prehaps the \fBIMP_OBJ\fR and \fBETAG\fR could be handled by the
default action of calling \fBen_type\fR.
As these routines have evolved after many changes there are things like
that which really need to be looked at closely before trying.
The comment /*SUPRESS 288*/ means suppress warning 288 to saber C debugging
tool that we use.
.IP en_type
This is one of the longest functions as it has so many cases to handle.
It again is structure as a loop over the types until \fBPE_END\fR but it
actually returns as soon as it has encoded the next type.
We can now look at the encoding of the primative \fBASN.1\fR types in detail.
.IP DFLT_F
Because we have arranged that for encoding tables, that we precede
the entry with a \fBDFLT_F\fR entry we can neatly handle all the default
cases.
All we do is check if the parameter passed in the user data, in \fBparm\fR,
is the same as the default value specified in the \fBDFLT_F\fR entry.
The function \fBsame\fR performs this check.
If it is the same don't encode anything just return, otherwise continue on
and encode it.
.IP ETAG
To handle explicit tags we merely allocate a \fBPE\fR with the right tag
and call \fBen_etype\fR to encode its contents, which are in the following
entries.
The switch on the \fBpe_ucode\fR field use to make a difference
but now it is meaningless and should be cleaned up.
.IP "SEQ_START, SEQOF_START, SET_START, SETOF_START"
We merely call the appropriate function handle them.
Note one \fIimportant\fR difference in the way they are called here from that
in \fBenc_obj\fR, the parm arguement is used as a base to index off and
fetch a new pointer to pass the next function.
This seemly bizarre action is quite straight forward when seen written as
it is normally in \fBC\fR, "\fBparm->offset\fR".
Where the field \fBoffset\fR is a pointer which has an offset from the start
of the structure of \fBp->pe_ucode\fR bytes.
.PP
This is the magic of how we access all the different fields
of the \fBC\fR data structures with the one piece of code.
It is also prehaps the most critical dependency of the whole system
on the implementation of the \fBC\fR language.
As the \BGNU\fR \fBC\fR compiler supports this feature then it is
compilerable on most machines.
But any porters should pay attention to this to ensure that thier compiler
is happy generating these offsets and compiling these casts properly.
.PP
The reason why this is different from the calls in \fBen_obj\fR is that
this is not the first compound type in the table.
The first and only the first does not have an offset and does not need to be
indirected through any pointers.
All the compound types inside this type will have
as their field a pointer which points to a structure.
From here on we shall say \fIindirection\fR  to mean this
adding the \fBpe_ucode\fR field
to the pointer to the structure and using it to reference a pointer.
Whether to use \fIindirection\fR or not is very important matter
that really needs to be understood to understand how the routines are
structured.
.IP IMP_OBJ
Here we have to handle the case where we can encode the object then have to
change its tag and class after encoding.
At the end of this entry this is done very simply by assigning the
right values to the appropriate fields after the object has been built.
This means that if the intermeadiate form is altered this piece of code
may have to be altered as well.
There seems to be no better way of handling this.
.PP
The complication in handling this field is the handling of all the possible
types of object.
If it is an external object we have to perform a call to \fBenc_f\fR with
all the right arguements
where a normal OBJECT, the last else branch, requires a normal call
to \fBen_obj\fR.
Note the case of \fBSOBJECT\fR is the same
as \fBOBJECT\fR \fIexcept there is no indirection\fR.
.IP "SOBJECT and OBJECT"
Here is the code that handles the two cases sperately.
It is exactly as in the \fBIMP_OBJ\fR case except seperated out.
Note the only difference between the two cases is lack of indirection in
the \fBSOBJECT\fR case.
.IP CHOICE_START
This is exactly as all other compound types,
like \fBSEQ_START\fR and \fBOBJECT\fR, we call the appropriate routine with
indirection.
From reading the \fBISODE\fR manuals that the \fBASN.1\fR CHOICE type
is handled by a structure of its own like the other compund types.
.IP "EXTOBJ and SEXTOBJ"

