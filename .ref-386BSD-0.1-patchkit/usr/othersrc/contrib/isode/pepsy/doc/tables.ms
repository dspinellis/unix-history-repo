.NS 2
The structure and meaning of the tables generated from the ASN.1 grammar
.XS
The structure and meaning of the tables generated from the ASN.1 grammar
.XE
.PP
Each collection of \fBASN.1\fR grammar is called a module.
(See 
.[
ASN.1
.]
)
Each \fBASN.1\fR module is completely specified in the program by a
single \fBC\fR structure of type \fBmodtyp\fR and the data which it references.
See the \fIpepdefs.h\fR file in the \fIpepsy\fR directory.
For each \fBASN.1\fR module
there are three tables that are generated from\fBASN.1\fR grammar.
These initialised arrays which we call tables are
called the encoding, decoding and printing tables.
Each of these tables is referenced through a different pointer
of the \fBmodtyp\fR structure.
.PP
Each of these pointers references an array of pointers,
one pointer for each \fBASN.1\fR type defined in the module.
The position of one of these pointers is the unique type number we give to
its corresponding type.
The pointer references an array of type \fBtpe\fR or \fBptpe\fR, depending
whether it is an entry in the decoding/encoding tables or printing tables
respectively.
See \fIpep.h\fR in the \fIpepsy\fR directory.
This array actually contains the necessary information to encode/decode/print
that \fBASN.1\fR type.
So given the \fBmodtyp\fR structure of an \fBASN.1\fR module and its
type number you can call a routine to encode, decode or print that type.
.PP
The rest of this document assumes a good knowledge of \fBASN.1\fR notation
so go read a copy if you haven't already.
From here on I shall mention only \fBtpe\fR and this means \fBtpe\fR in the
case of encoding or decoding and \fBptpe\fR in the case of printing, unless
otherwise stated.
Each type is represented by an array of \fBtpe\fR (or \fBptpe\fR for printing).
The basic element consists of four integer fields,
the printing table is the same with an addition \fBchar\fR pointer field which
contains the name corresponding to that entry in the \fBASN.1\fR grammar.
The first specifies the type of the entry and determines how the
rest are interpreted.
The possible types are listed in \fIpepsy/pep.h\fR.
Each type is an array which starts with an entry of type \fBPE_START\fR
and ends with one of type \fBPE_END\fR.
Each primitive type requires one entry to specify it,
apart from possible \fBPE_START\fR and \fBPE_END\fR used to specify the start
and end of the type.
Constructed types are represented by a list of entries terminated by an
entry of type \fBPE_END\fR.
As \fBASN.1\fR types can be nested inside so will the
representation in \fBtpe\fR entries be nested.
For example the \fBASN.1\fR type definition:
.nf
           Example1 ::=
                   SEQUENCE {
                       seq1 SEQUENCE {
                                an-i INTEGER,
                                an-ostring OCTET STRING
                            },
                       a-bool IMPLICIT [0] BOOLEAN
                   }
.fi
.ce 1
Will generate an encoding array:
.nf
static tpe et_Example1Test[] = {
	{ PE_START, 0, 0, 0 },
	{ SEQ_START, 0, 16, FL_UNIVERSAL },
	{ SEQ_START, OFFSET(struct type_Test_Example1, seq1), 16, FL_UNIVERSAL },
	{ INTEGER, OFFSET(struct element_Test_0, an__i), 2, FL_UNIVERSAL },
	{ OCTETSTRING, OFFSET(struct element_Test_0, an__ostring), 4, FL_UNIVERSAL },
	{ PE_END, 0, 0, 0 },
	{ BOOLEAN, OFFSET(struct type_Test_Example1, a__bool), 0, FL_CONTEXT },
	{ PE_END, 0, 0, 0 },
	{ PE_END, 0, 0, 0 }
	};

.fi
.PP
Here the second last \fBPE_END\fR matches and closes off the
first \fBSEQ_START\fR.
The entries which correspond to the other primative types are pretty
obvious, with the INTEGER entry corresponding to the primative INTEGER.
For fields that generate data the general interpretation of the other three
fields is offset, tag and flags/class fields respectively.
.IP offset
The second field gives the offset in a \fBC\fR data structure
needed to reference the data that corresponds to this table entry.
Each \fBASN.1\fR type has \fBC\fR structure types generated as described in
the 
.[
ISODE
.]
manuals, volume 4 "The applications Cookbook" Section 5.2,
"POSY Environment".
As this offset may have to be determined in a compiler dependent manner
a \fBC\fR preprocessor macro is used hide the actual details.
.IP tag
This is the tag associated with the \fBASN.1\fR type for that entry.
Notice that in the example the [0] IMPLICIT which changes the tag
associated with the BOOLEAN entry actually has the correct tag of 0 in the
table.
Likewise SEQUENCE has the correct tag of 16 in its \fBSEQ_START\fR entry
and so on for the others.
.IP flags/class
This contains the \fBASN.1\fR class associated with the entry's type.
That is UNIVERSAL for all except the BOOLEAN type which is CONTEXT class.
This fourth can also contain flags that specify if the type is OPTIONAL
or DEFAULT.
There is plenty of room here as there is only four possibly classes.
.PP
Now that you have some idea of how these arrays are arranged for a type
definition I will proceed to go through the possible type of
entries and describe what they do and how they work.
These values are defined in \fIpepsy/pep.h\fR.
Those entries with a value below \fBTYPE_DATA\fR are entries that
don't correspond to data to be encoded/decoded and are for other book keeping
type purposes.
.IP "PE_START and PE_END"
As explained above \fBPE_START\fR starts the beginning of a \fBASN.1\fR type's
array.
It probably isn't necessary but the size of the tables is so small it isn't
much of an over head to keep around for cosmetic reasons.
The entry type \fBPE_END\fR is necessary to mark the end of some
compound type as well as the end of \fBASN.1\fR data type.
.IP "XOBJECT and UCODE"
These are obsolete types and probably should be removed.
They were to allow \fBC\fR code written directly by the user to be incorporated
into the encoding/decoding but it was found unnecessary.
Prehaps some brave soul would like to use them in an attempt to implement
a similar system based on \fIpepy\fR which is what we first attempted to
do until we found this to be much easier.
.IP MALLOC
This field only occurs in the decoding tables.
It specifies how much space to malloc out for the current \fBC\fR structure
it is just inside of.
For instance in the example above the decoding table has the following entry:
.DS C
{ MALLOC, 0, sizeof (struct type_Test_Example1), 0 },
.DE
just after the first \fBSEQ_START\fR entry.
It tells it to \fBmalloc\fR out a \fBstruct type_Test_Example1\fR structure
to hold the data from the sequence when it is decoded.
.IP SCTRL
This entry is used in handling the \fBASN.1\fR CHOICE type.
The \fBC\fR type generated for \fBASN.1\fR CHOICE type is a structure with
an offset field in it and a
union of all the \fBC\fR types present in the CHOICE.
Each \fBASN.1\fR type in the CHOICE of types has a \fBC\fR type definition
generated for it.
The union is of all these types, which is quite a logical way to implement
a CHOICE type.
The offset field specifies which possibility of interpreting the union
should be used (which \fImember\fR should selected).
As such it needs to be read by the encoding routines
when encoding the data from the \fBC\fR data structures
and to be set by the decoding routines when it is decoding the data into
the \fBC\fR data structures.
There is one such entry for each \fBCHOICE\fR type to specify where the
offset field is.
.IP CH_ACT
Another redundant entry type.
I think this was also used in code to handle \fBC\fR statements or
actions specified by the user.
It probably should be removed.
.IP OPTL
This is used to handle the optionals field that is generated by
\fBposy\fR when optional types that are \fInot\fR implemented by pointers
are present in the \fBASN.1\fR type.
For example if an \fBASN.1\fR type has an optional integer field how
does the encoding routine determine if the integer is to be
present or not?
If it was implemented as a pointer it could use a \fBNULL\fR (zero) pointer
to mean that the type was not present because
NULL is guaranteed to never occur as a legal pointer to a real object.
But all the possible values for integer could be legally passed so
instead for these types which are not pointers and are optional
a bit map is allocated in the structure.
Each non pointer optional type a bit from the bit map is
allocated.
.PP
If that bit is set the corresponding type is present and it is not
present if the bit is not set.
Each bit has a \fB#define\fR generated for it.
The bit map is merely an integer field called "\fBoptionals\fR" limiting
maximum number of such optionals to 32 on Sun machines, 16 on some others.
(An array of \fBchar\fR as BSD fd_sets would have avoid all such limits,
not that this limit is expected to be exceeded very often !)
Like the \fBSCTRL\fR entry this entry merely serves to specify where
this field is so it can be test and set by the encoding and decoding routines
respectively.
.IP "ANY and CONS_ANY"
The \fBC\fR type corresponding to the entry is a \fBPE\fR pointer.
To conform with \fIpepy\fR the tag and class of this entry are ignored,
which may or may not be the most sensible thing.
The \fBCONS_ANY\fR is a redundant symbol which means the same thing but
is not used.
This should be clean up and removed.
.IP "INTEGER, BOOLEAN, BITSTRING, OCTETSTRING and OBJID"
These are just as described in the first article.
See the ISODE manual to find out what they are allocated as a \fBC\fR data
type to implement them.
The offset fields says where to find this data type with in the current
structure.
.IP "SET_START, SETOF_START, SEQ_START and SEQOF_START"
These compound entries differ from the above in that they group all
the following entries together up to the matching \fBPE_END\fR.
The entries with \fBOF\fR in them correspond to the \fBASN.1\fR types
which have \fBOF\fR in them
e.g. \fBSET OF\fR.
Allowing the \fBOF\fR items to have an arbitrary number of entries is
excessive flexibility, they can only have one type by the \fBASN.1\fR grammar
rules.
The \fBC\fR data type corresponding to them is either a structure if
it is the first such type in the array or a pointer to a structure
is isn't.
This complicates the processing of these structures a little but not greatly.
The \fBOF\fR types differ one other important way,
they may occur zero, one or more times, with no upper bound.
To cope with this the \fBC\fR data type is a linked list structure.
The pointer to the data structure determines whether or not there is another
occurrence of the type, if it is NULL there isn't.
Thus each data structure has this pointer to the next occurrence, the offset of
this pointer is placed in the \fBPE_END\fR field where it can conveniently
be used to determine whether or not to make another pass through the
table entry. 
.IP OBJECT
When one type references another it generates an \fBOBJECT\fR entry.
This specifies the type number of the type
which is present in the 3rd field of the \fBtpe\fR structure,
\fBpe_tag\fR.
The 2nd field still gives the offset in the \fBC\fR data structure
which specifies where the user's data for that type is to be found.
Usually this a pointer to the \fBC\fR data structure for that type.
.IP T_NULL
This entry means the \fBASN.1\fR primative type \fBNULL\fR.
It doesn't have any body and consequently has no offset as it cannot
carry data directly.
Only its absence or presence can mean anything so if it is optional it sets or
clears a bit in the bit map as described earlier for \fBOPTL\fR entry.
.IP T_OID
This use to be used for Object Identifiers and now is unused,
it should be got rid.
.IP OBJID
This corresponds to the Object Identifier \fBASN.1\fR type primitive.
It is implemented the same as other primative types like \fBINTEGER\fR
and \fBOCTET STRING\fR.
.IP ETAG
This entry gives the explicit tag of the following entry.
The usual fields which define class and tag are the only ones which have
meaning in this entry.
By concatenating successive \fBETAG\fR entries it is possibly to build up
an limited number explicit tags, although this hasn't been tested yet.
.IP IMP_OBJ
If a type has an implicit tag usually all we have to do is set its tag
and class appropriately in its entry.
This works for all but one important case, the reference of another type.
This is messy because we can't alter the definition of the type with out
wrecking it for the other uses.
So what we do for encoding is build the type normally
and then afterward it is built
change its tag and class to be the values we want.
Similarly for decoding we match the tag and class up and then decode the body
of the type.
We can't use a \fBOBJECT\fR entry for this because among other
reasons there 3rd field is already to store the type number.
(The forth needs to be free to contain flags such as \fBDEFAULT\fR
and \fBOPTIONAL\fR)
So a new entry type is used, \fBIMP_OBJ\fR, to hold the tag and class.
It must be followed by an \fBOBJECT\fR entry which is used to handle the type
as normal, the \fBIMP_OBJ\fR entry gives the tag and class to be used.
Like the \fBETAG\fR entry the \fBIMP_OBJ\fR affects the entry that follows it.
.IP "EXTOBJ and EXTMOD"
These handle external type references.
This is just like a normal (internal?) type reference except we must now
specify which module as well as the type.
Similarly because there are no more free fields in the \fBOBJECT\fR type
we need two entries to hold all the information we need.
The \fBEXTMOD\fR occurs first and holds the type number and the offset
into the \fBC\fR data structure and the flags, exactly as for an \fBOBJECT\fR
entry.
The next entry, which must be an \fBEXTMOD\fR, contains a pointer to
the \fBmodtyp\fR structure for its module.
Like a normal \fBOBJECT\fR entry to handle the case of an implicit tag
an \fBIMP_OBJ\fR entry would occur before these two entries which gives
the class and tag.
Likewise it could have an explicit tag in which the two entries
would be proceeded by an \fBETAG\fR entry.
.IP "DFLT_F and DFLT_B"
When a type has a default value, to handle decoding and encoding properly you
need to know its value.
As there is no space to store the value in most entries we allocate a whole
entry to specify the value.
When encoding it is convenient to have the default occur before the entry it
refers to.
This allows a single check to handle all the default encoding.
All it has to do is check whether it is the same as the default value and if so
not bother encoding the next type.
On the other hand when decoding it is more convenient to have
the entry after the
one it refers to.
In this case we need to determine that it is missing before we use the default
value to determine the value to pass to the user.
To handle this we have entries of both types.
.B DFLT_F
contains the default value for the following entry (F = Front)
and \fBDFLT_B\fR contains that for the entry before it (B = Back).
Consequently \fBDFLT_F\fR are only used in the decoding tables
and \fBDFLT_B\fR entries are only used in the decoding (and printing tables).
.IP S-Types
These types are entries for the same \fBASN.1\fR type as the entry type
formed by removing the starting `S'.
The above forms would do to handle \fBASN.1\fR but we also have to be
compatible with the \fBC\fR data structures generated by \fIposy\fR.
The implementors decided to optimise the \fBC\fR data structures generated
a little means we have to have all these \fBS\fR type entries.
If a type was a single field in most cases they produced a \fB#define\fR
which eliminates the need to have a whole structure just for that type.
In all the places where this type is used the field of the \fBC\fR structure
is changed from a pointer to field which holds the value directly in the
structure.
See the \fBISODE\fR reference given above for more details.
.PP
We handle this by generating the same tables that would be generated
with out the optimisation, except the optimised types the S-type of entries
instead of the normal ones.
For example an optimised \fBOCTET STRING\fR would have
the type field of its entry as \fBSOCTETSTRING\fR instead of \fBOCTETSTRING\fR.
The only difference in how \fBS\fR type and its corresponding normal are handle
is how they find the \fBC\fR data structure for that entry.
That difference is that there is no indirection through pointers.
.IP "Flags field"
Besides the encoding the class the \fBpe_flags\fR field
also contains a few possible flags.
Mainly \fBFL_OPTIONAL\fR which means the \fBASN.1\fR type
corresponding to this flag is OPTIONAL.
Consequently when encoding it has to determine if the type is present in the
user data possibly using the bit map as described under the \fBOPTL\fR entry.
Likewise when decoding it may have to set a bit in the bit map appropriately.
The other flag at the moment is \fBFL_DEFAULT\fR which means the entry
corresponds to an \fBASN.1\fR DEFAULT type.
This bit is still needed as not all types have \fBDFLT_*\fR entries implmented
for them at the moment.
In particular compound value things like SEQUENCE and SET can't have thier
default value specified.
This is consistent with \fBISODE\fR, if fact implementing that may even
break existing \fBISODE\fR code.
This last flag \fBFL_IMPLICIT\fR is obsolete and not not used any where.
