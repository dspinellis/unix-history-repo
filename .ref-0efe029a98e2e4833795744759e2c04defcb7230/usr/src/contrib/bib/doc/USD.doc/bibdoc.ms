.\"	@(#)bibdoc.ms	6.2 (Berkeley) %G%
.\"
.EH 'USD:31-%''BIB \- A Program for Formatting Bibliographies'
.OH 'BIB \- A Program for Formatting Bibliographies''USD:31-%'
.nr LL 6.5i
.nr LT 6.5i
.TL
\fBBIB \- A Program for Formatting Bibliographies\fP
.AU
Timothy A. Budd
revised by A. Dain Samples, \*(td
.PP
\fIBib\fP is a program for collecting and formatting reference lists in
documents.  It is a preprocessor to the nroff/troff typesetting systems,
(much like the \fItbl\fP [.tbl.] and \fIeqn\fP [.eqn.] programs) and an alternative
to the \fIrefer\fP [.lesk refer.]  and \fItib\fP [.alexander tib.] 
bibliography programs.
\fIBib\fP takes two
inputs: a document to be formatted and a library of references.  Imprecise
citations in the source document are replaced by more conventional
citation strings, the appropriate references are selected from the reference
file, and commands are generated to format both citation and the referenced
item in the bibliography.
.PP
\fIBib\fP has been enhanced to promote sharing of bibliographic
references from the \fIrefer\fP,
\fIbib\fP, and Tib tools.  \fIBib\fP now accepts the Tib style of macro 
invocation, i.e. surrounding names that are to be expanded with vertical bars.
If you or your group are not contemplating
moving to the TeX or LaTeX [.knuth tex, lamport latex.] 
text processors, then you need not worry
about any of the references to Tib in this document.
However, if you are considering such a
move, and are worried about how you will convert your bibliographic
databases to the BibTeX format, then the current version of \fIbib\fP,
in conjunction with the existing Tib software\u1\d 
.FS
1. Tib is available, for a handling charge, from James C. Alexander at the 
Dept. of Mathematics,
University of Maryland, College Park, MD 20742.  It is also available
from \fBeneevax.umd.edu:pub/tib\fP via \fIftp\fP.
.FE
will make life a lot easier for you.
.SH
Description
.PP
An imprecise citation is a list of words surrounded by the characters
[\&. and .].  Words (which are truncated to six letters)
in the imprecise citation are matched against entries in the reference file,
and if an entry is found that matches all words, that reference is used.
For example:
.de 2Q
.sp
.RS
.RS
..
.de 2E
.sp
.RE
.RE
..
.2Q
.PP
In Brooks' interesting book [\&. brooks mythical.] various reasons ...
.2E
.PP
Multiple citations are indicated by simply placing a comma in the imprecise
citation:
.2Q
.PP
In [\&.kernig tools, kernig elements.], Kernighan and Plauger have ...
.2E
.PP
Embedded newlines, tabs and extra blanks within the
imprecise citation are ignored.
.PP
Judicious use of the K (keyword) field in references in the database
can simplify citations
considerably.  Also additional information can be placed into citations by
surrounding text with curly braces.
The additional information is inserted verbatim into the citation,
e.g. [.dragon {,\ Chapter 6}.].
Note that it may be desirable to use non-breakable spaces, in order that the
citation not be split across a line boundary by \fItroff\fP, for example:
.2Q
.PP
For a description of LR parsing, see [\&.dragon {,\e\0Chapter 6}.] by Aho and Ullman.
.2E
.PP
The angle brackets can be used as alternatives to the curly braces.
.2Q
.PP
For a description of LR parsing, see [\&.dragon <,\e\0Chapter 6>.] by Aho and Ullman.
.2E
.PP
An alternative citation style can be used by surrounding the imprecise
citation with {\&. and .\&}.  Most document styles just give the
raw citation, without the braces, in this case.  This is useful, for example,
to refer to citations in running text.
.2Q
.PP
For a discussion of this point, see reference {\&.dragon.\&}.
.2E
.PP
The algorithm used by \fIbib\fP scans the source input in two passes.
In the first pass,
references are collected and the location of citations marked.
In the second pass, these marks are replaced by the appropriate citation,
and the entire list of references is dumped following a call on the macro
\&.[].
This macro is left untouched.
However, this can be altered to achieve other typographic
effects.
.PP
An exception to this process is made in those instances where
references are indicated in footnotes.  In this case the macro that
generates the reference is placed immediately after each line in which
the reference is cited.
.SH
Creating the Database
.PP
Reference files are prepared for \fIbib\fP using \fIinvert\fP.
By default \fIinvert\fP places an inverted index for the
reference list in the file INDEX.  Unless the user specifies an
alternative (see the \-p switch described below), this is the first file
searched by \fIbib\fP in attempting to locate a reference.  
If the entry is not found
in the user's file, a standard system-wide index is searched.  If the
entry is still not found in the system file, a warning message is produced
and a blank citation is generated.
.PP
The format for entries in the reference file is described more fully in
the section `Reference File Formats'.
.PP
Since the user's index is searched before the system index, if the
user wants to alter a specific entry in the system index (say to change
the name W. E. Howden to William E. Howden, for example) it is a simple
matter to copy the system information into a private database and make
the changes locally.
\fIBib\fP issues messages warning the user if there are multiple entries
in a reference file that match an imprecise reference, or if there are
multiple index files that match an imprecise reference.  (Note that
previous versions of \fIbib\fP stopped searching after the first match
was found.  Current users of \fIbib\fP may see warning messages now that
they did not receive before.)
The `duplicate matches' warning message can be controlled by making the 
citations more precise. 
.PP
Citation formats are either determined by explicit switch settings or,
more generally, by using a predefined formatting style.  In the latter form,
usage looks something like:
.sp
.ce
bib \-t\fIstyle\fP [files]
.sp
where \fIstyle\fP is a citation style.
Currently the following citation styles are available:
.IP astro 6m
astrophysical journal style references
.IP compsurv
Computing Surveys style references
.IP foot
footnoted references.
.IP hnf
Hanson Normal Form
.IP jrnl
lists references in alphabetical and date order by journal; see Miscellaneous 
Tools section.
.IP lib
useful for printing the entire bibliography; see Miscellaneous Tools
section
.IP list
a useful format for listrefs, which see.
.IP llist
another useful format for printing everything about your bibliography
database; see Miscellaneous Tools section
.IP opena
same as stda, but using an open format.
.IP openn
same as stdsn, only using an open reference format (each major entry is on
a new line\u1\d).
.FS
1. The open reference format is adapted from \fIA Handbook for Scholars\fP,
by Mary-Claire van Leunen, published by Knopf, 1978.
.FE
.IP spe
format used by the journal \fISoftware\(emPractice and Experience\fP.
.IP spe2
a second format for \fISoftware\(emPractice and Experience\fP.  
.IP stda
standard alphabetic format, where 
citations are three letters followed by the last two
digits of the date; for papers with a single author, the letters are the
first three letters of the authors last name (e.g. Knu);
in papers with two authors the first two letters are from the first author
followed by one letter from the second (e.g. HoU); etc.
(see the section \fBCitation Templates\fP under
\fBReference Format Desiner's Guide\fP for more details).
.IP stdafull
standard alphabetic format, with full references.
.IP stdn
standard numeric citation; reference entries are listed in order of mention.
.IP stdsn
same as stdn, but references are sorted by senior author followed by date.
.IP supn
same as stdn, but using superscripts.
.PP
It is possible to alter slightly the format of standard styles.  For example,
to generate references in standard numeric style with first names
abbreviated:
.sp
.ce
bib \-tstdn \-aa ...
.PP
If two reference items create the same citation string (this can happen
if two works by the same authors published in a single year are referred to
in one paper) a disambiguating final letter is added to the citation
(i.e., Knu79 becomes Knu79a and Knu79b).
This can be altered by using the F field (see the section on Reference File
Formats).
.PP
For the purposes of sorting by author, the last name is taken to be the last
word of the name field.  This means some care must be taken when names contain
embedded blanks, such as in `Hartley Rogers, Jr.'
or `Mary-Claire van Leunen'.
In these cases a concealed space (\e\0) should be used, as in
`Hartley Rogers,\e\0Jr.'.
.PP
\fIbib\fP knows very little about \fItroff\fP usage or syntax.  This
can sometimes be useful.  For example, to cause an entry to appear in a
reference list without having it explicitly cited in the text the citation
can be placed in a \fItroff\fP comment.
.RS
.nr
.sp
 .\e" [\&.imprecise citation.]
.sp
.RE
.PP
It is also possible to embed \fItroff\fP commands within a reference definition.
See `Abbreviations' in the section `Reference Format Designer's
Guide' for an example.  However, be aware that unbridled use of such
embedded processor-specific commands makes it more difficult to convert
to other processors later.  In the section on Miscellaneous Tools, we
discuss ways to avoid such specificity.
.PP
In some styles (superscripts) periods and commas should precede the
citation while spaces follow.
In other styles (brackets) these rules are reversed.  If
a period, comma or space immediately precedes a citation, it will be moved to the
appropriate location for the particular reference style being used.
This movement is not done for citations given in the alternative style
(i.e. {\&.dragon.}).
.PP
The following is a complete list of options for \fIbib\fP:
.IP \-aa 8m
reduce author's first names to abbreviations.
.IP \-ar\fInum\fP
reverse the first \fInum\fP author's names.
If \fInum\fP is omitted all names are reversed.
.IP \-ax
print authors last names in Caps-Small Caps style.  For example Budd becomes
B\s-2UDD\s+2.
.IP \-c\fIstr\fP
build citations according to the template \fIstr\fP;  
see the section \fBCitation Templates\fP under
\fBReference Format Desiner's Guide\fP for the format of the string and
its effect.
.IP "\-d \fIdir\fP"
.ns
.IP "\-d\fIdir\fP"
change the base directory in which files are sought.  It is initially
/usr/new/lib/bmac.
.IP \-ea
abbreviate editors' names
.IP \-ex
places editors' names in Caps-Small Caps style.  (see \-x )
.IP \-er\fInum\fP
reverse the first \fInum\fP editors' names.  
If \fInum\fP is omitted all editors'
names are reversed.
.IP \-f
instead of dumping references following the call on \&.[], dump each
reference immediately following the line on which the citation is placed
(used for footnoted references).
.IP \-h
hyphenate runs of three or more contiguous references in the citation string.
(eg 2,3,4,5 becomes 2-5).  This is most useful for numeric citation styles,
but works generally.
The \-h option implies the \-o option.
.IP "\-i file"
.ns
.IP "\-ifile"
include and process the indicated file.
This is useful for including a private file of string definitions.
.IP \-n\fIstr\fP
turn off the indicated options.  \fIstr\fP must be composed of the
characters \fIafhoRrvx\fP.
.IP \-o
sort contiguous citations according to the order given by the reference
list.  (This option defaults on).
.IP "\-p \fIfile\fP"
.ns
.IP  \-p\fIfile\fP
instead of searching the file INDEX,
search the indicated reference file(s) before searching the system file.
Multiple files are separated by commas.
.IP \-R
print a warning each time there is an attempt to redefine a name.  (No warning
is the default.)
.IP \-r\fInum\fP
synonym for \-ar.
.IP \-s\fIstr\fP
sort references according to the template \fIstr\fP.
.IP "\-t \fItype\fP"
.ns
.IP \-t\fItype\fP
use the standard macros and switch settings to generate citations and references
in the indicated style.
.IP \-Tib
use the Tib macro conventions.  See the discussion in the Reference Format 
Designer's Guide.
.IP \-Tibx
creates the file .bib.m4.in in the current directory.  This file
contains macro definitions that when applied to a database file,
converts calls on macros to the form expected by the Tib
bibliography preprocessor; more information is in the section 
Miscellaneous Tools.
.IP \-v
\fB[[I'm not sure what this does; it is related to the \-f option, but
it apparently suppresses the printing of certain
information.]]\fP
.IP \-x
synonym for \-ax.
.PP
When a file is to be included during normal \fIbib\fP processing
(options \-i and \-t, and the style file command I)
\fIbib\fP searches a specific set of directories and filenames.
For example, if \fB-i myfile\fP is specified on the invocation line, 
\fIbib\fP attempts to open, in order, the following files until one is
found.  
.IP 1.
\fI\&./myfile\fP
.IP 2.
\fBBMACLIB\fP\fI/myfile\fP, where \fBBMACLIB\fP is defined by the \-d
option (default: /usr/new/lib/bmac).
.IP 3.
\fBBMACLIB\fP\fI/tibmacs/myfile\fP if \-Tib was specified, otherwise
\fBBMACLIB\fP\fI/bibmacs/myfile\fP
.IP 4.
\fI\&./bib.myfile\fP
.IP 5.
\fBBMACLIB\fP\fI/bib.myfile.
.PP
If none are found, an error message is issued, and execution halts.
.de Ex
.sp
.RS
.nf
.ta 3m
..
.bp
.ce 
\fBReference File Formats\fP
.sp
.PP
A reference file contains any number of reference
items.  Reference items are separated by one or more blank lines.
There are no restrictions placed on the order of reference items in a file,
although the user will find that imposing some order 
simplifies updates.
.PP
A reference item is a collection of field tags and values.
A field tag is a percent sign followed by a single letter.
Currently, the following field tags are recognized:
.Ex
.ta 0.5i
A	Author's name \(dg
B	Title of book containing item
C	City of publication
D	Date
E	Editor(s) of book containing item \(dg
F	Caption
G	Government (NTIS) ordering number
I	Issuer (publisher)
J	Journal name
K	Keys for searching \(sc \(dg
N	Issue number
O	Other information \(sc \(dg
P	Page(s) of article
R	Technical report number
S	Series title
T	Title
V	Volume number
W	Where the item can be found locally \(sc \(dg
X	Annotations \(sc \(dg
.RE
.PP
Fields marked with \(dg are \fIaccumulated\fP fields
and can be repeated as necessary, but for all others
only the last occurence of the field in any reference will be used.
Those fields marked with \(sc are ignored by most styles designed for 
publication, but can have additional information and are
available to database listing styles and other software tools.
A field can be as long as necessary and can extend onto new lines.  No
continuation characters are necessary:
lines that do not begin with a percent sign or a period
and are not blank
are treated as continuations of the previous line.
The order of fields is irrelevant, except that accumulated fields
are listed in the order of occurrence.
.PP
The format of the reference file for \fIbib\fP
is similar to that used by \fIrefer\fP
except that \fIbib\fP has the following additional capabilities:
.IP 1.
An F field, if present, overrides whatever citation string would otherwise
be constructed.
.IP 2.
Certain defined names can be used, and will be expanded differently by
different document styles.  For example, the string CACM is expanded into
`Communications of the ACM' by some document styles, `Comm.
ACM' by others,
and `Comm. of the Assoc. of Comp. Mach.' by yet others.
Appendix 1 lists some of the currently recognized names.
.IP 3.
The program automatically abbreviates names, reverses names, and
hyphenates strings of contiguous references, if requested.
.IP 4.
A reference can have more than one editor field, and editor's names
can be abbreviated, reversed, and/or printed in cap/small caps style,
independent of any processing done to authors names.
.IP 5.
Comments (lines with '#' in the first column) can be interspersed
throughout the reference file.  They are not used as sources of
keywords (use the %O or %X fields for that).
.PP
The format of the reference file for \fIbib\fP is also similar to that
used by \fItib\fP, which shares a lot of source code with \fIbib\fP,
with the following exceptions:
.IP 1.
\fIBib\fP does not recognize field tags \fIM\fP, \fIZ\fP, \fIa\fP through 
\fIz\fP and \e, while \fItib\fP does.
.IP 2.
\fITib\fP currently does not recognize field tag \fIX\fP, and \fIbib\fP
does.
.IP 3.
\fITib\fP currently does not have any form of comment lines in the
reference file, while \fIbib\fP marks comment lines with a '#' at the
beginning of a line.
.IP 4.
\fITroff\fP commands may be inserted verbatim into \fIbib\fP references.
In \fItib\fP the `%\e' field tag is used to insert TeX commands.
.PP
There may be (and probably are) other differences between \fIbib\fP,
\fItib\fP, and \fIrefer\fP not documented here.
.PP
Generally a reference falls into one of several basic categories: book,
journal article, conference paper, article in a book, compilations,
technical report, PhD thesis, etc.
An example of each and a brief comment is given below.  With less
standard references (Archival Sources, Correspondence, Government
Documents, Newspapers) generally some experimentation is necessary.
.SH
Books
.PP
A book is something with a publisher that isn't a journal article or
a technical report.  Generally, books also have authors and titles
and dates of publication (although some don't).  For books not published
by a major publishing house it is also helpful to give a city for the
publisher.  Some government documents also qualify as books, so a book
may have a government ordering number.
.PP
It is conventional that the authors names appear in the reference in
the same form as on the title page of the book.  Note also that
string definitions are provided for most of the major publishing houses
(PRHALL for Prentice-Hall, for example).
The string definition may include the city as part of the definition,
depending on the database in use.
.Ex
%A	R. E. Griswold
%A	J. F. Poage
%A	I. P. Polonsky
%T	The SNOBOL4 Programming Language
%I	PRHALL
%D	second edition 1971
.RE
.PP
Sometimes a book (particularly old books) will have no listed publisher.
The reference entry must still have an I field.
.Ex
%A	R. Colt Hoare
%T	A Tour through the Island of Elba
%I	(no listed publisher)
%C	London
%D	1814
.RE
.PP
If a reference database contains entries from many people (such
as a departmental-wide database), the W field can be used to indicate
where the referenced item can be found; using the initials of the owner,
for example.
Any entry style can take a W field, since this field is not used in
formatting the reference.
.PP
The K field is used to define general subject categories for an entry.
This is useful in locating all entries pertaining to a specific subject
area.
Note the use of the backslash to indicate the last name is Van Tassel,
and not simply Tassel.
.Ex
%A	Dennie Van\e\0Tassel
%T	Program Style, Design, Efficiency,
Debugging and Testing
%I	PRHALL
%D	1978
%W	tab
%K	testing debugging
.RE
.SH
Journal article
.PP
The only requirement for a journal article is that it have a
journal name and a volume number.
Usually journal articles also have authors, titles, page
numbers, and a date of publication.  They may also have numbers, and,
less frequently, a publisher.  (Generally, publishers are only listed for
obscure journals).
.PP
Note that string names (such as CACM for \fICommunications of the ACM\fP)
are defined for most major journals.
There are also string names for the months of the year, so that months
can be abbreviated to the first three (capital) letters.
Note also in this example the use of the %K field to define a short
name (hru) that can be used as a shorthand in an imprecise citation.
(This is to be contrasted with BibTeX which not only \fIrequires\fP
user selected abbreviations, 
but also requires that they all be distinct from one
another.)
.Ex
%A	M. A. Harrison
%A	W. L. Ruzzo
%A	J. D. Ullman
%T	Protection in Operating Systems
%J	CACM
%V	19
%N	8
%P	461-471
%D	AUG 1976
%K	hru
.RE
.SH
Article in conference proceedings
.PP
An article from a conference is printed as though it were a journal
article and the journal name was the name of the conference.
Note that string names (SOSP, The Symposium on Operating System Principles)
are also defined for the major conferences.
.Ex
%A	M. Bishop
%A	L. Snyder
%T	The Transfer of Information and Authority
in a Protection System
%J	Proceedings of the 7th SOSP
%P	45-54
%D	1979
.RE
.SH
Article in book
.PP
An article in a book has two titles, the title of the article and the title
of the book.  The first goes into the T field and the second into the B
field.  Similarly the author of the article goes into the A field and the
editor of the book goes into the E field.
.Ex
%A	John B. Goodenough
%T	A Survey of Program Testing Issues
%B	Research Directions in Software Technology
%E	Peter Wegner
%I	MIT Press
%P	316-340
%D	1979
.RE
.PP
If a work as more than one editor, they each get their own %E field.
.Ex
%A	R. J. Lipton
%A	L. Snyder
%T	On Synchronization and Security
%E	Richard A. DeMillo
%E	David P. Dobkin
%E	Anita K. Jones
%E	Richard J. Lipton
%B	Foundations of Secure Computation
%P	367-388
%I	ACPRESS
%D	1978
.RE
.PP
Sometimes the book is part of a multi-volume series, and hence may
contain a volume field and/or a series name.
.Ex
%A	C.A.R. Hoare
%T	Procedures and parameters: An axiomatic approach
%B	Symposium on semantics of algorithmic languages
%E	E. Engeler
%P	102-116
%S	Lecture Notes in Mathematics
%V	188
%I	Springer-Verlag
%C	Berlin-Heidelberg-New York
%D	1971
.RE
.PP
In any reference format, the O field can be used to give additional information.
This is frequently used, for example, for secondary references.
.Ex
%A	A. Girard
%A	J-C Rault
%T	A Programming Technique for Software Reliability
%B	Symposium on Software Reliability
%I	IEEE
%C	Montvale, New Jersey
%D	1977
%O	(Discussed in Glib [32])
.RE
.SH
Compilations
.PP
A compilation is the work of several authors gathered together by an editor
into a book.  The reference format is the same as for a book, with
the editor(s) taking the place of the author.
.Ex
%E	R. A. DeMillo
%E	D. P. Dobkin
%E	A. K. Jones
%E	R. J. Lipton
%T	Foundations of Secure Computation
%I	ACPRESS
%D	1978
.RE
.SH
Technical Reports
.PP
A technical report must have a report number.  They usually have authors,
titles, dates and an issuing institution (the I field is used for this).
They may also have a city and a government issue number.  Again string
values (UATR for `University of Arizona Technical Report') will
frequently simplify typing references.
.Ex
%A	T. A. Budd
%T	An APL Complier
%R	UATR 81-17
%C	Tucson, Arizona
%D	1981
.RE
.PP
If the institution name is not part of the technical report number, then
the institution should be given separately.
.Ex
%A	Douglas Baldwin
%A	Frederick Sayward
%T	Heuristics for Determining Equivalence of Program Mutations
%R	Technical Report Number 161
%I	Yale University
%D	1979
.RE
.SH
PhD Thesis
.PP
A PhD thesis is listed as if it were a book, and the institution granting
the degree the publisher.
.Ex
%A	Martin Brooks
%T	Automatic Generation of Test Data for
Recursive Programs Having Simple Errors
%I	PhD Thesis, Stanford University
%D	1980
.RE
.PP
Some authors prefer to treat Master's and Bachelor theses similarly, although
most references on style instruct say to treat a Master's degree as an
article or as a report.
.Ex
%A	A. Snyder
%T	A Portable Compiler for the Language C
%R	Master's Thesis
%I	M.I.T.
%D	1974
.RE
.SH
Miscellaneous
.PP
A miscellaneous object is something that does not fit into any other form.
It can have any of the the following fields; an author, a title, a date,
page numbers, and, most generally, other information (the O field).
.PP
Any reference item can contain an F field, and the corresponding text
will override whatever citation would otherwise be constructed.
.Ex
%F	BHS--
%A	Timothy A. Budd
%A	Robert Hess
%A	Frederick G. Sayward
%T	User's Guide for the EXPER Mutation Analysis system
%O	(Yale university, memo)
.RE
.bp
.ce
\fBReference Format Designer's Guide\fP
.sp
.PP
This section need only be read by those users
who wish to write their own formatting macro packages.
.PP
The information necessary for generating citations and references of a
particular style is contained in a \fIformat file\fP.  A format file
consists of two parts; a sequence of format commands, which are read and
interpreted by \fIbib\fP, and a sequence of text lines (usually \fItroff\fP macro
definitions) which are merely copied to output.
The format file name is always prefixed by `bib.'.
Thus the format file for a standard document type, such as stdn, is found
in a file called bib.stdn.
.PP
Each formatting command is distinguished by a single
letter, which must be the first character on a line.
The formatting commands in a database file are similar to the command line options
for \fIbib\fP.  The legal commands,
and their arguments, are as follows:
.sp
.IP "#\fItext\fP" 8m
A line beginning with a sharp sign is a comment, and all remaining text on the
line is ignored.
.sp
.IP A
The A command controls how authors' names are to be formatted.  It can be
followed by the following character sequences:
.RS
.IP A 6m
Authors' names are to be abbreviated
(see the section \fBAbbreviations\fP, below).
.IP R\fInum\fP
The first \fInum\fP authors' names are to be reversed.  If \fInum\fP is
omitted, all authors' names are reversed.
.IP X
Authors' names are to be printed in Caps-Small Caps style.
.RE
.IP E
The E command is equivalent to the A command, except that it controls the
formatting of editors' names.
.IP F
The F command indicates that references are to be dumped immediately after
a line containing a citation, such as when the references are to be placed
in footnotes.
.IP "S \fItemplate\fP"
The S command indicates references are to be sorted before being dumped.
The comparison used in sorting is based on the \fItemplate\fP.  See
the discussion in the section \fBSort Template\fP for an explanation of 
templates.
.IP "C \fItemplate\fP"
The \fItemplate\fP is used as a model in constructing citations.
See the discussion below in the section \fBCitation Templates\fP.
.IP "D \fI\0word \0definition\fP"
The word-definition pair is placed into a table.
Before each reference is dumped it is examined for the
occurrence of these words.  Any occurrence of a word from this table is replaced
by the definition, which is then rescanned for other words.
Words are limited to alphanumeric characters, ampersand and underscore.
Definitions can extend over multiple lines by ending lines with a backslash
(\e).  The backslash will be removed, and the definition, including the newline
and the next line,
will be entered into the table.  This is useful for including several
fields as part of a single definition (city names can be included as part
of a definition for a publishing house, for example).
.IP
\fIBib\fP has been enhanced to recognize macro calls where the macro name is 
surrounded by vertical \(brbars\(br.  This enhancement was implemented to
to provide a little more compatibility between \fIbib\fP and Tib,
a preprocessor that uses a database format very similar to \fIbib\fP's.
To have \fIbib\fP recognize only macro names surrounded by vertical
bars, invoke the \-Tib option.
.IP "I \fIfilename\fP"
The indicated file is included at the current point.  The included file may
contain other formatting commands.
.IP H
Three or more contiguous citations that refer to adjacent items in the
reference list are replaced by a hyphenated string.  For example, the
citation 2,3,4,5 would be replaced by 2-5.  This is most useful with
numeric citations.  The H option implies the O option.
.IP O
Contiguous citations are sorted according to the order given by the reference
list.
.IP "R \fInumber\fP"
The first \fInumber\fP author's names are reversed on output (i.e. T. A. Budd
becomes Budd, T. A.).
If number is omitted all names are reversed.
.IP "T \fIstr\fP"
The \fIstr\fP is a list of field names.  Each time a definition string for
a named field is produced, a second string containing just the last character
will also be generated.  See the section \fBTrailing Characters\fP below.
.IP X
Authors' last names are to be printed in Caps/Small Caps
format (i.e., Budd becomes B\s-2UDD\s+2).
.PP
\fB ** NOTE ** 
The first line encountered in the format input that does not match 
a format command
causes that line, and all subsequent lines, to be immediately copied to
the output without further processing.\fP
.SH
File Naming Conventions
.PP
Standard database format files are kept in a standard library area.
The string BMACLIB in bib.h points to this directory (/usr/new/lib/bmac in
the distribution).  In addition, this name is always defined when
reading format files, and is defined with the l] macro when processing
with \fItroff\fP.
The first command output by \fIbib\fP defines the string l] to be this
standard macro database directory.  This allows macro files to be
independent of where they are actually stored.
There are three types of files:
.IP bib.xxx 10m
These files contain bib commands to format documents in the xxx style.
.IP bmac.xxx
These files are the \fItroff\fP macros to actually implement a style.
They are generally not examined by \fIbib\fP at all, but are processed
by troff in response to a .so command.
.IP bibinc.xxx
These files contain information (such as definitions) used by more than one
style database.  The two usual files are bibinc.fullnames and 
bibinc.shortnames.  Both of these include files bibinc.Xlocal (where X
is full or short, respectively).  Due to the way \fIbib\fP searches for
files, if the user has these in his local directory, they will be read
just before the contents of bibinc.Xnames are processed.
.SH
Troff Naming Conventions
.PP
There is a simple naming convention for strings, registers and macros used
by \fIbib\fP during processing by \fItrof\fP.  
All strings, registers and macros are denoted by two character
names containing either a left or right brace.  The following are general rules:
.IP [\fIx\fP
If \fIx\fP is alphanumeric, the string contains the value of a reference field.
If \fIx\fP is nonalphanumeric, this is a formatting string preceding a citation.
.IP ]\fIx\fP
If \fIx\fP is alphanumeric, this is the final character from a reference field.
If \fIx\fP is nonalphanumeric, the string is formatting information within a citation.
.IP \fIx\fP[
Strings in this format, where \fIx\fP is can be any character, are defined by the
specific macro package in use and are not specified by \fIbib\fP.
.IP \fIx\fP]
If \fIx\fP is nonalphanumeric these strings represent formatting commands following
citations (the inverse of [\fIx\fP commands).  Other strings represent
miscellaneous formatting commands,
such as the space between leading letters in abbreviated names.
.PP
There are two such macro names to be particularly aware of: .s[ and .e[\&.
The first is called at the beginning of formatting a reference and the
latter is called at the end of the formatting.  
The user can have some
control over the formatting of
references by redefining these macros.
.PP
It might be noted here that the best way to understand this stuff is to
look at some existing definition files, and start making small changes
here and there.
.SH
Sort Templates
.PP
The sort template is used in comparing two references to generate
the sorted reference list.  The sort template is a sequence of
sort objects.  Each sort object consists of an optional negative sign, followed
by a field character, followed by an optional signed size.  The leading negative
sign, if present, specifies the sort is to be in decreasing order, rather than
increasing.  The field character indicates which field in the reference
is to be compared.  The entire field is used, except in the case of the `A'
field, in which case only the senior author's last name is used.
A positive number following the field character indicates that only the first
n characters are to be examined in the comparison.  The negative value indicates
only the last \fIn\fP characters.  
Thus, for example, the template AD\-2 indicates
that sorting is to be done by the senior author followed by the last two
characters of the date.
.PP
The sort algorithm is stable, so that two documents which compare equally
will be listed in citation order.
.PP
Note that in sorting, citation construction, and elsewhere, if an author
field is not present the senior editor will be used.  If neither author nor
editor fields are present the institution name will be used.
.SH
Citation Templates
.PP
A citation template is similar to a sort template, with the following
additions:  
.IP 0
suppresses all printing. \fB[[True?  can anyone tell me what this does?]]\fP
.IP 1
refers to the number which
represents the position of the reference in the reference list (after sorting).
.IP 2
generates a three character sequence: if the
paper being referenced has only one author, this is the first three characters
of the author's last name; for two author papers, this is the first two
characters of the senior author, followed by the first character of the second
author; for papers with three or more authors the first letter of the first
three authors is used.
.IP 3
used to specify a format consisting of the authors'
last names, or the senior author followed by the text `et al' if more than
four authors are listed.  This is the Astrophysical Journal style of
citation.
.IP 4
the Computing Surveys style of citation.
.IP 8
full alphabetic.
.IP 9
last name of senior author.
.PP
.PP
Each object can be followed by either of the letters `u' or
`l' and the field will be printed in all upper or all lower case,
respectively.
.PP
If necessary for disambiguating, the character `@' can be used as
a separator between objects in the citation template.  Any text which should
be inserted into the citation uninterpreted should be surrounded by either
\&'{\&..}' or '<\&..>' pairs.
.SH
Citation Formatting in Troff
.PP
In the output, each citation is surrounded by the strings \e*([[ and \e*(]]
(or in the alternative style \e*([{ and \e*(}]).
Multiple citations are separated by the string \e*(],.
The text portion of a format file should contain \fItroff\fP definitions for
these strings to achieve the appropriate typographic effect.
.PP
Citations that are preceded by a period, comma, space or other punctuation
are surrounded by string values for formatting the punctuation in the
appropriate location.
Again, \fItroff\fP commands should be given to insure the appropriate values 
are produced.
.KS
.PP
The following table summarizes the string values that must be defined
to handle citations.
.TS
center;
l l l.
[[	]]	Standard citation beginning and ending
{[	}]	Alternate citation beginning and ending
[\&.	.]	Period before and after citation
[,	,]	Comma before and after citation
[?	?]	Question mark before and after citation
[!	!]	Exclamation Point before and after citation
[:	:]	Colon before and after citation
[;	;]	Semi-Colon before and after citation
["	"]	Double Quote before and after citation
['	']	Single Quote before and after citation
[<	>]	Space before and after citation
],		Multiple citation separator
]-		Separator for a range of citations
.TE
.KE
.SH
Name Formatting in Troff
.PP
Authors' (and editors') names can be abbreviated, reversed, and/or printed
in Caps-small Caps format.  In producing the string values for an author,
formatting strings are inserted to give the macro writer greater flexibility
in producing the final output.  Currently the following strings are used:
.TS
center;
l l.
a]	gap between successive initials
b]	comma between last name and initial in reversed text
c]	comma between authors
n]	\fIand\fP between two authors
m]	\fIand\fP between last two authors
p]	period following initial
.TE
.PP
For example, suppose the name `William E. Howden' is abbreviated
and reversed.  It will come out looking like
.ce
Howden\\*(b]W\\*(p]\\*(a]E\e*(p]
.SH
Reference Formatting in Troff
.PP
The particular style used in printing references is decided by macros
passed to \fItroff\fP.  Basically, for each reference,
\fIbib\fP generates a sequence of string definitions, one for each field in the
reference, followed by a call on the formatting macro.  For example an
entry which in the reference file looks like:
.KS
.nf
.ta 3m
.sp
%A	M. A. Harrison
%A	W. L. Ruzzo
%A	J. D. Ullman
%T	Protection in Operating Systems
%J	CACM
%V	19
%N	8
%P	461-471
%D	1976
%K	hru
.sp
.KE
.LP
is converted into the following sequence of \fItroff\fP commands
.KS
.nf
.sp
 .[\-
 .ds [F 1
 .ds [A M\e*(p]\e*(a]A\e*(p] Harrison
 .as [A \e*(c]W\e*(p]\e*(a]L\e*(p] Ruzzo
 .as [A \e*(m]J\e*(p]\e*(a]D\e*(p] Ullman
 .ds [T Protection in Operating Systems
 .ds [J Communications of the ACM
 .ds [V 19
 .ds [N 8
 .nr [P 1
 .ds [P 461-471
 .ds [D 1976
 .ds [K hru
 .][
.sp
.KE
.PP
Note that the commands are preceded by a call on the macro `.[\-'.
This can be used by the macro routines for initialization, for example to
delete old string values.  (For some reason, the ending macro .e[ also calls
.[\- which seems redundant.)
.PP
The string [F is the citation string used in the document.
Note that the string CACM has been expanded.
.PP
The strings c], n] and m] are used to separate authors.  c] separates
the initial authors in multi-author documents (it is usually a comma
with some space before and after), n] separates authors in two author
documents (usually `\ and\ '), and m] separates the last two authors
in multi-author documents (either `\ and\ ' or `,\ and\ ').
.PP
If abbreviation is specified, the string a] is used to separate initials
in the author's first name, and the string p] determines the punctuation
to use (usually a period).
.PP
The \fIbib\fP system provides minimal assistance in
deciding format types.  For example note that the number register [P has
been set of 1, to indicate that the article is on more than one page.
Similarly, in documents with editors, the register [E is set to the number
of editors.
.SH
Trailing Characters
.PP
There is a problem with fields that end with punctuation characters causing
multiple occurrences of those characters to be printed.  For example, suppose
author fields are terminated with a period, as in T. A. Budd.  If names
are reversed, this could be printed as Budd, T. A..  Even if names are not
reversed, abbreviations, such as in Jr. can cause problems.
.PP
To avoid this problem \fIbib\fP, if instructed, generates the last
character from a particular field as a separate string.  The string name
is a right brace  followed by the field character (e.g. ]A for the
author field).  Macro packages should
test this value before generating punctuation.
.SH
Abbreviations
.PP
The algorithm used to generate abbreviations from first names is fairly
simple: Each word in the first name field that begins with a capital
is reduced to that capital letter followed by a period.
In some cases, this may not be sufficient.  For example, suppose
Ole-Johan Dahl should be abbreviated `O\-J. Dahl'.  The only
way to achieve this (short of editing the output) is to include \fItroff\fP commands
in the reference file that alter the strings produced by \fIbib\fP, as in the following
.RS
.sp
 ...
 %A Ole-Johan Dahl
 .ds [A O\-J. Dahl
 ...
.sp
.RE
.PP
In fact, if absolutely necessary, any \fItroff\fP 
commands can be entered in the middle of a reference
entry, and the commands are copied uninterpreted to the output.
For example, the user may wish to have a switch indicating whether the name
is to be abbreviated or not:
.RS
.sp
 ...
 %A Ole-Johan Dahl
 .if \en(i[ .ds [A O\-J. Dahl
 ...
.sp
.RE
.PP
However, keep in mind the restrictions this imposes on the portability
and convertibility of the database.
.bp
.SH
An Example
.PP
Figure 1 shows the format file for the standard alphabetic format.
The I command includes a file of definitions
for common strings, such as dates and journal names.  A portion of this
file is shown in figure 2.
The sort command indicates that sorting is to be done by senior author,
followed by the last two digits of the date.  The citation template
indicates that citations will be a three character sequence 
followed by the last two characters of the date (e.g. AHU79)
(described in the section on citations above).
Author and editor names will be abbreviated.
The \.so command causes \fItroff\fP to read a basic set of macro
definitions for formatting the references; the beginning of this file 
is shown in figure 3;
note that a no-op has been inserted into the definition string for
BIT in order to avoid further expansion when the
definition is rescanned.
Finally, the \fItroff\fP macro s[ is redefined to turn off
hyphenation across line-boundaries, and to make sure the citation is put
in the bibliography list indented 10 ens and surrounded by square
brackets.
.KS
.nf
.sp
#  standard alphabetic format
I bibinc.shortnames
SAD\-2
C2D\-2
AA
EA
 .so \e*(l]/bmac.std
 .de s[	\e" start reference by turning off hyphenation
 .nh
 .IP [\e\e*([F] 10n	\e" indent the citation
 .\.
.sp
.ce
\fBFigure 1\fP
.sp
.KE
.KS
.nf
.sp
D ACTA Acta Informatica
D BIT B\e&IT
D CACM Communications of the ACM
 ...
D JAN Jan.
 ...
D DEC Dec.
.sp
.ce
\fBFigure 2\fP
.sp
.KE
.KS
.nf
.sp
 .\e" standard format troff commands
 .\e" citation formatting strings
 .ds [[ [
 .ds ]] ]
 .ds ], ,\e\(br
 .ds ]- -
 .ds [\&. " \e&
 .ds .] .
 .ds [, " \e&
 .ds ,] ,
   ...
 .\e" reference formating strings
 .ds a] " \e&
 .ds b] , \e&
 .ds c] , \e&
 .ds n] "\e& and \e&
 .ds m] "\e& and \e&
 .ds e] \e\efIet al.\e\efP
 .ds p] .
   ...
 .\e" reference formating macros
 .de s[   \e" start reference
 .nh
 .IP "[\e*([F]" 5m
 ..
 .de e[   \e" end reference
 .[\-
 ..
 .de []   \e" start to display collected references
 .LP
 ..
.sp
.ce
\fBFigure 3\fP
.sp
.KE
.KS
.nf
.sp
 .de ][   \e" choose format
 .ie !"\e\e*([J"" \e{\e
 .    ie !"\e\e*([V"" .nr t[ 1    \e" journal
 .    el            .nr t[ 5    \e" conference paper
 .\e}
 .el .ie !"\e\e*([B"" .nr t[ 3    \e" article in book
 .el .ie !"\e\e*([R"" .nr t[ 4    \e" technical report
 .el .ie !"\e\e*([I"" .nr t[ 2    \e" book
 .el                .nr t[ 0    \e" other
 .\e\en(t[[
 ..
.sp
.ce
\fBFigure 4\fP
.sp
.KE
.PP
On the basis of some simple rules (the presence or absence of certain fields)
the document is identified as one of five different types, and a call made
on a different macro for each type.  This is shown in figure 4.  Note
that how the reference is printed (as a book, a journal article
reference, etc.) is not determined by \fIbib\fP, but by the .][ macro
invoked by \fItroff\fP.
.PP
Finally figure 5 shows the macro for one of those different types, in this
case the formatting macro for references to books.
.KS
.nf
.sp
 .de 2[ \e" book
 .s[
 .ie !"\e\e*([A"" \e\e*([A,
 .el .if !"\e\e*([E"" \e{\e
 .       ie \e\en([E-1 \e\e*([E, eds.,
 .       el \e\e*([E, ed.,\e}
 .if !"\e\e*([T"" \e\efI\e\e*([T\e\efP,
 .rm a[
 .if !"\e\e*([I"" .ds a[ \e\e*([I
 .if !"\e\e*([S"" , \e\e*([S\ec       \e" book in a series
 .if !"\e\e*([C"" \e{\e
 .       if !"\e\e*(a["" .as a[ , \e\e&
 .       as a[ \e\e*([C\e}
 .if !"\e\e*([D"" \e{\e
 .       if !"\e\e*(a["" .as a[ , \e\e&
 .       as a[ \e\e*([D\e}
 \e\e*(a[\&.
 .if !"\e\e*([G"" Gov. ordering no. \e\e*([G.
 .if !"\e\e*([O"" \e\e*([O.
 .e[
 ..
.sp
.ce
\fBFigure 5\fP
.sp
.KE
.bp
.ce
Miscellaneous Tools
.SH
Bibinc
.PP
Some editors require journal names in a reference to be spelled out
completely, while other journals expect a standardized shortened
version.  \fIBib\fP is able to accomodate these requirements by reading
different different definition files.  For example, standard open
alphabetic format (see \fBBMACLIB\fP/\fIbib.opena\fP) expects a definition
file called \fIbibinc.fullnames\fP, while \fBBMACLIB\fP/\fIbib.spe\fP reads
\fIbibinc.shortnames\fP.  Maintaining these two files and making sure
they are consistent with one another can be tedious.  A program is
included with \fIbib\fP to ease this problem.  It allows the user
to keep a single file with all definitions, and to
have those definitions sent
to the appropriate file(s).  In what follows we assume that the name of
this common file, the input file to \fIbibinc\fP, is called \fIbibinc.names\fP.
.PP
The format of the lines in \fIbibinc.names\fP is formally:
.LD
<inputline> ::= '+'<char1> <filename>
              | '+'<char1> '+'<char2> '+'<char3> ... 
              | '+'<char> <name> <stuff>
              | '+'<char><stuff>
              | '?'<name>
.DE
In all cases the '+' or '?' must be the first character of a line to be
recognized, and there can be no spaces between it and the following
character.  Continuation lines are all lines from the preceding '+' to
the following '+'.  Any and all whitespace at the beginning of a
continuation line is discarded.
.PP
The first form defines <char1> to be the character that denotes an outputfile
named <filename>.  It must be the first occurence of <char1> following a '+'
in the file.  
.PP
The second form defines <char1> to be the character that denotes several
previously defined outputfiles.  It must be the first occurence of '+'<char1>
in the file.
.PP
The third form says that the following line is to be written to the file or
files denoted by <char> as:
.LD
D <name> <stuff>
.DE
.PP
The fourth form allows the inclusion of arbitrary stuff into the file.  Note  
that there is no space between the <stuff> and <char> in the fourth form.
.PP
The fifth form is used to provide some mechanism to select lines to be 
processed according to whether one is intending to use \fItib\fP or
\fIbib\fP, and whether one is intending to use the TeX or the
\fItroff\fP processors.
The problem to be solved is 
that we want to be able to create files for three possibilities:
.IP (1) 
the user is using bib-style macros with troff,
.IP (2) 
the user is using tib-style macros with troff,
.IP (3) 
the user is using tib-style macros with TeX or LaTeX.
.PP
Therefore, the user can type the invocation line as:
.LD
   bibinc bib troff  <bibinc.names
   bibinc bib        <bibinc.names     (troff implied)
   bibinc tib troff  <bibinc.names
   bibinc tib tex    <bibinc.names
   bibinc tex        <bibinc.names     (tib implied)
.DE
(Note that `bibinc bib tex' is illegal: not a supported combination.)
If a line of the form "?tib" is encountered in \fIbibinc.names\fP, 
then the lines following that
line are processed only if "tib" was specified or implied on the
invocation line.  This restriction remains true until a line
beginning "?bib" or "?" is
encountered.  Likewise, "?troff" will permit the processing of the 
following lines only
if troff was specified or implied on the invocation line, and this remains
true until "?tex" or "?" is encountered.
.SH
More on Tib style macro invocation
.PP
\fITib\fP macro mechanism is quite different
from the \fIbib\fP macro style.  In \fIbib\fP, you define and use a macro
as follows:
.LD
D macro expansion text
	:
%A A. Nonymous
%T macro on macro
.DE
The title will be expanded to `expansion text on expansion text'.
In \fItib\fP, macro calls are always enclosed in vertical bars:
.LD
D macro expansion text
    :
%A A. Nonymous
%T The macro as \(brmacro\(br
.DE
The title will be expanded to `The macro as expansion text', therewith 
demonstrating a major benefit of using the vertical \(brbars\(br.  Here is another
benefit of the bars:
.LD
D u_um \e"{u}
D Karlsruhe Karlsr\(bru_um\(brhe
.DE
It's easy now to get the diacritical marks right, and still have a 
recognizable word to act as a key in the INDEX.  It is possible to acheive
this same effect with \fIbib\fP-style macros by using 
the non-printing space `\e&'.
.LD
D u_um \e*(:u
D Karlsruhe Karlsr\e&u_um\e&he
.DE
.SH
A Bibinc Example
.PP
The \(brKarlsruhe\(br example has a problem if your database is being used
for both TeX and ditroff: bib/tib macros have no conditionals, and you
have to choose either the TeX code or the troff code for special
characters.  Bibinc has a mechanism to solve this.  Let's assume you
have the following in a file called bibinc.names:
.LD
================================== bibinc.names
# first line of bibinc.names
#
# first use of a letter after `+' constitutes its definition
#
#  define the letter F to mean 'write this line into the fullnames file'
?bib
?troff
+F bibinc.fullnames
+S bibinc.shortnames
+B +F +S        # both
?tib
?troff
+F bibinc-t.fullnames
+S bibinc-t.shortnames
?tex
+F tibinc.fullnames
+S tibinc.shortnames
?               # turns off specialization
+B +F +S        # both
#
?tex
+B a_um \e"{a}
+B o_sl \eo
+B o_um \e"{o}
+B u_um \e"{u}
+B TCOLADA $\embox{TCOL}_\embox{Ada}$
+B dash --
+B Rn  $\embox{R}^\embox{n}$
+B AMP \e\e&
?troff          # either bib or tib
+B a_um \e*(:a
+B o_sl \e*(/o
+B o_um \e*(:o
+B u_um \e*(:u
+B TCOLADA TCOL\edAda\eu
+B dash \e(hy
+B Rn  \e*(Rn
+B AMP &
?
+B fuer f\(bru_um\(brr
#
+B IFI   Institut \(brfuer\(br Informatik
+B Universitat Universit\(bra_um\(brt
+B KARLSRUHE Karlsr\(bru_um\(brhe
+F Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br
    %C \(brKARLSRUHE\(br, West Germany        # note that leading blanks are removed
+S Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br
+F SIGPLAN SIG\e&PLAN Notices
+S SIGPLAN SIG\e&PLAN
+B GUNS Smith \(brAMP\(br Wesson
# last line of bibinc.names
================================== bibinc.names
.DE
This input to bibinc will select the correct expansion for u_um depending
on whether -Tib was specified on its invocation line or not.  That is,
with the following invocation:
.LD
% bibinc troff tib <bibinc.names
.DE
two files will be written, bibinc-t.fullnames and bibinc-t.shortnames:
.LD
================================== bibinc-t.fullnames
D a_um \e*(:a
D o_sl \e*(/o
D o_um \e*(:o
D u_um \e*(:u
D TCOLADA TCOL\edAda\eu
D dash \e(hy
D Rn  \e*(Rn
D AMP &
D fuer f\(bru_um\(brr
D IFI   Institut \(brfuer\(br Informatik
D Universitat Universit\(bra_um\(brt
D KARLSRUHE Karlsr\(bru_um\(brhe
D Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br\e
%C \(brKARLSRUHE\(br, West Germany
D SIGPLAN SIGPLAN Notices
================================== bibinc-t.fullnames
.DE
and
.LD
================================== bibinc-t.shortnames
D a_um \e*(:a
D o_sl \e*(/o
D o_um \e*(:o
D u_um \e*(:u
D TCOLADA TCOL\edAda\eu
D dash \e(hy
D Rn  \e*(Rn
D AMP &
D fuer f\(bru_um\(brr
D IFI   Institut \(brfuer\(br Informatik
D Universitat Universit\(bra_um\(brt
D KARLSRUHE Karlsr\(bru_um\(brhe
D Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br
D SIGPLAN SIGPLAN
================================== bibinc-t.shortnames
.DE
Users of bib/ditroff will have to specify -Tib on their
invocations of bib to use these files.
.PP
The command:
.LD
%bibinc bib troff <bibinc.names
.DE
produces
.LD
================================== bibinc.fullnames
D a_um \e*(:a
D o_sl \e*(/o
D o_um \e*(:o
D u_um \e*(:u
D TCOLADA TCOL\edAda\eu
D dash \e(hy
D Rn  \e*(Rn
D AMP &
D fuer f\e&u_um\e&r
D IFI   Institut fuer Informatik
D Universitat Universit\e&a_um\e&t
D KARLSRUHE Karlsr\e&u_um\e&he
D Karlsruhe  IFI, Universitat KARLSRUHE\e
%C KARLSRUHE, West Germany
D SIGPLAN SIG\e&PLAN Notices
================================== bibinc.fullnames
.DE
and
.LD
================================== bibinc.shortnames
D a_um \e*(:a
D o_sl \e*(/o
D o_um \e*(:o
D u_um \e*(:u
D TCOLADA TCOL\edAda\eu
D dash \e(hy
D Rn  \e*(Rn
D AMP &
D fuer f\e&u_um\e&r
D IFI   Institut fuer Informatik
D Universitat Universit\e&a_um\e&t
D KARLSRUHE Karlsr\e&u_um\e&he
D Karlsruhe  IFI, Universitat KARLSRUHE
D SIGPLAN SIG\e&PLAN
================================== bibinc.shortnames
.DE
.PP
In this mode, users of bib/ditroff will have to be very
careful that their macro names do not conflict with anything in normal
text.  
.PP
The command:
.LD
%bibinc tex tib <bibinc.names
.DE
produces:
.LD
================================== tibinc.fullnames
D a_um \e"{a}
D o_sl \eo
D o_um \e"{o}
D u_um \e"{u}
D TCOLADA $\embox{TCOL}_\embox{Ada}$
D dash --
D Rn  $\embox{R}^\embox{n}$
D AMP \e&
D fuer f\(bru_um\(brr
D IFI   Institut \(brfuer\(br Informatik
D Universitat Universit\(bra_um\(brt
D KARLSRUHE Karlsr\(bru_um\(brhe
D Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br\e
%C \(brKARLSRUHE\(br, West Germany
D SIGPLAN SIGPLAN Notices
================================== tibinc.fullnames
.DE
and
.LD
================================== tibinc.shortnames
D a_um \e"{a}
D o_sl \eo
D o_um \e"{o}
D u_um \e"{u}
D TCOLADA $\embox{TCOL}_\embox{Ada}$
D dash --
D Rn  $\embox{R}^\embox{n}$
D AMP \e&
D fuer f\(bru_um\(brr
D IFI   Institut \(brfuer\(br Informatik
D Universitat Universit\(bra_um\(brt
D KARLSRUHE Karlsr\(bru_um\(brhe
D Karlsruhe  \(brIFI\(br, \(brUniversitat\(br \(brKARLSRUHE\(br
D SIGPLAN SIGPLAN
================================== tibinc.shortnames
.DE
.PP
Notice how bibinc handles the bib/troff use of the special character
sequence \e& in the above definitions of the SIGPLAN macro.
I.e., bibinc removes all user inserted \e&'s when tib/tex is specified.
NOTICE that when tib/tex is specified, any other \e<char> is passed 
through exactly as is, unless <char> is \e, in which case bibinc reduces 
it to a single \e.
Otherwise, all other \e<char> are passed through as is.
.SH
An Example Makefile
.PP
Included with the distribution of \fIbib\fP is an example makefile that
provides some hints on how to print out one's bibliographic database.
The file is \fBBMACLIB\fP/Makefile.e.g., with a final period.
For example, by typing
.LD
make -f Makefile.e.g. KEYWORD=cache print
.DE
you will get a listing of all entries in your database that have the
keyword \fIcache\fP in them.  Typing
.LD
make -f Makefile.e.g. bib
.DE
a complete listing of the database is produced.  And finally,
.LD
make -f Makefile.e.g. KEYWORD=Ineed2read journals
.DE
will list all entries which have the keyword \fIIneed2read\fP,
presumably put in the %K field of references the user wants to read.
Moreover, the listing is then sorted by the %J (journal) field, which is a
convenient ordering for organizing your trips to the library.
.SH
Converting a database to a \(brdatabase\(br
.PP
If at any point in the future you intend to begin using the TeX or LaTeX 
text processors, you will face the problem of converting your existing
database files either to BibTeX format or to Tib format.  Use of the
\&-Tibx option to \fIbib\fP can simplify the conversion to \fItib\fP.
There is a public domain program called \fItr2tex\fP which may also help
convert existing documents; however, it
is not included with the distribution of \fIbib\fP.  Try contacting
Kamal Al-Yahya at kamal@hanauma.stanford.edu.
If that doesn't work try kamal%hanauma@score.stanford.edu.
.PP
To convert a database, run your usual sequence of commands to create
your document using \fIbib\fP, except add the option -Tibx to \fIbib\fP's
command line.  After the creation of your document, will also find the
file \fIbib.m4.in\fI which contains a set of m4 macro definitions.  
For
each database file INDEX you wish to convert, type
.LD
% bib2tib INDEX
.DE
and that's it!  Well, almost.  At this point your database files have
had almost all macro calls converted to the vertical bar style.  The
ones that have not been caught are those that have ampersands in them:
\fIbib\fP allows ampersands in macro names, but the m4 macro processor
does not.  Therefore, you will need to chase those down by hand.
.PP
Also, this has done nothing to remove any \fItroff\fP specific commands
that may be embedded in you source files: they, too, will need to be
changed \fIbut only if you are converting to TeX\fP.  If you are staying
with \fItroff\fP for a while, or even for a long time, there is nothing
more you need do.
.bp
.SH
Acknowledgements
.PP
\fIbib\fP was inspired by \fIrefer\fP, written by M. Lesk.
Enhancements to support \fItib\fP style macros were discussed in depth with
Michael van De Vanter and Ethan Munson.  However, they did not do the 
implementation: blame that on Dain Samples.
.[]
.rs
.bp
.SH
APPENDIX 1
.nr PS 9
.nr VS 11
.sp
Standard Names
.PP
The following list gives some of the standard names recognized in most
citation styles.  Various different forms for the output are used
by the different styles.
In the longer reference style,
the conference proceedings will also refer
to the date (%D), city (%C),
and when the proceedings are published as a journal,
the journal name (%J), volume (%V) and number (%N).
Not all names are listed here.  See BMACLIB/bibmacs/bibinc.fullnames for a 
complete list.
.sp
.TS
expand;
l s
l l.
\fBMiscellaneous Names\fP
a_um	\*(:a
o_um	\*(:o
u_um	\*(:u
A_um	\*(:A
O_um	\*(:O
U_um	\*(:U
o_sl	\*(/o
AMP	&
dash	\(hy
ARCH	Architecture
COMP	Computer
COMPs	Computers
COMPg	Computing
CONF	Conference
CORP	Corporation
CS	Computer Science
CSD	Computer Science Department
DCS	Department of Computer Science
DEPT	Department
DISS	Dissertation
ENG	Engineering
EE	Electrical Engineering
fuer	f\*(:ur
INTL	International
JOUR	Journal
LANG	Language
LANGs	Languages
NATL	National
PHD	PhD Dissertation
PROC	Proceedings
PROG	Program
PRing	Programming
PROCSIGPLAN	Proceedings of the ACM-SIG\&PLAN
SCI	Science
SCIs	Sciences
SOFT	Software
SYMP	Symposium
SYS	System
SYSs	Systems
TRANS	Transactions
TR	Technical Report
UNIV	University
UNIVCA	University of California
.TE	
.sp
.TS
expand;
l s
l l.
\fBJournal Names\fP
ACMCS	ACM Computing Surveys
ACTA	Acta Informatica
AT&TTJ	AT&T Technical Journal
BIT	B\&IT
BSTJ	Bell System Technical Journal
CACM	Communications of the ACM
CAN	Computer Architecture News
COMPJOUR	The Computer Journal
COMPLANG	Journal of Computer Languages
COMPSUR	ACM Computing Surveys
HCI	Human-Computer Interaction
HUMFACT	Human Factors
I&C	Information and Control
IBMJRD	IBM Journal of Research and Development
IBMSJ	IBM Systems Journal
IEEEC	IEEE Computer
IEEEIT	IEEE Transactions on Information Theory
IEEEM	IEEE Micro
IEEESW	IEEE Softare
IEEETC	IEEE Transactions on Computers
IEEETIT	IEEE Transactions on Information Theory
IEEETSE	IEEE Transactions on Softare Engineering
IEEETSMC	IEEE Transactions on Systems, Man, and Information Sciences
IJCAI	International Joint Conference on Artificial Intelligence
IJCIS	International Journal of Computer and Information Sciences
IJCM	International Journal of Computer Mathematics
IJMMS	International Journal of Man-Machine Studies
IPL	Information Processing Letters
JACM	Journal of the ACM
JALG	Journal of Algorithms
JCSS	Journal of Computer and System Sciences
JPhil	Journal of Philosophy
JS&S	Journal of Systems and Softare
MATHST	Mathematics Systems Theory
NMATH	Numerical Mathematics
PerfEv	Performance Evaluation
SIAMJAM	SIAM Journal on Applied Mathematics
SIAMJC	SIAM Journal on Computing
SIAMJNA	SIAM Journal on Numerical Analysis
SIGACT	SIG\&ACT News
SIGCHI	ACM S\&IGCHI Bulletin
SIGPLAN	SIG\&PLAN Notices
SIGSOFT	Softare Engineering Notes
SP&E	Softare\-Practice & Experience
TIPSJ	Transactions of the Information Processing Society of Japan (Japanese)
TOCS	ACM Transactions on Computer Systems
TODS	ACM Transactions on Database Systems
TOMS	ACM Transactions on Mathematical Softare
TOOIS	ACM Transactions on Office Information Systems
TOPLAS	ACM Transactions on Programming Languages and Systems
TSE	IEEE Transactions on Softare Engineering
.TE
.sp
.TS
expand;
l s
l l.
\fBSelected Conferences\fP
ADA80	PROC of the ACM-SI\&GPLAN Symposium on the Ada Programming Language, SIGPLAN
ASPLOS82	PROC of the SYMP on Architectural Support for Programming Languages and Operating Systems, SIGPLAN
CCC79	PROC of the SI\&GPLAN 1979 SYMP on Compiler Construction, SIGPLAN
CCC82	PROC of the SI\&GPLAN 1982 SYMP on Compiler Construction, SIGPLAN
CCC84	PROC of the SI\&GPLAN 1984 SYMP on Compiler Construction, SIGPLAN
CONF	Conference
FJCC	Fall Joint Computer Conference
FOCS	Annual SYMP on Foundations of Computer Science
HICSS	Hawaii International CONF on System Science
ICSE	International CONF on Software Engineering
JER3	PROC Third Jerusalem CONF on Information Technology
JICAI	Joint International CONF on Artificial Intelligence
PLISS83	PROC SI\&GPLAN 1983 SYMP on Programming Language Issues in Software Systems, SIGPLAN
POPL	ACM SYMP on Principles of Programming Languages
POPL5	Conference Record of the Fifth POPL
POPL6	Conference Record of the Sixth POPL
POPL7	Conference Record of the Seventh POPL
POPL8	Conference Record of the Eighth POPL
POPL9	Conference Record of the Ninth POPL
POPL10	Conference Record of the Tenth POPL
POPL11	Conference Record of the Eleventh POPL
PROC	Proceedings
SOSP	SYMP on Operating System Principles
STOC	Annual ACM SYMP on Theory of Computing
SYMP	Symposium
WJCC	PROC Western Joint Computer CONF
.TE
.sp
.TS
expand;
l s
l l.
\fBPlace Names\fP
BTLHO	Bell Laboratories
BTLMH	Bell Laboratories
CMU	Carnegie-Mellon University
CMUCS	Computer Science Department, Carnegie-Mellon University
DG	Data General
MITAI	MIT Artificial Intelligence Laboratory
MITLCS	MIT Laboratory for Computer Science
SUCS	Computer Science Department, Stanford University
SUCSL	Computer Systems Lab., Stanford Electronics Lab., Dept. of Electrical Engineering and Computer Science
SUEE	Department of Electrical Engineering, Stanford University
TUM	Technische Universit\*:at M\*:unchen
UCB	University of California, Berkeley
UCBCS	Computer Science Division, EECS, UCB
UCBERL	ERL, EECS, UCB
.TE
.sp
.TS
l s
l l.
\fBMonths of the year\fP
JAN	January
FEB	February
MAR	March
APR	April
MAY	May
JUN	June
JUL	July
AUG	August
SEP	September
OCT	October
NOV	November
DEC	December
.TE
.sp
.TS
l s
l l.
\fBPublishers\fP
ACADEMIC	Academic Press
ACPRESS	Academic Press
ADDISON	Addison Wesley
ANSI	American National Standards Institute
CSPRESS	Computer Science Press
DIGITAL	Digital Press
ELSEVIER	American Elsevier
FREEMAN	W. H. Freeman and Company
GPO	U. S. Government Printing Office
HOLT	Holt, Rinehart, and Winston
IEEEP	IEEE Press
MCGRAW	McGraw-Hill
MGHILL	McGraw-Hill
MITP	MIT Press
NHOLL	North-Holland
NYC	New York, N\&Y
PRENTICE	Prentice Hall
PRHALL	Prentice Hall
SPRINGER	Springer Verlag
SRA	Science Research Associates
WILEY	John Wiley & Sons
WINTH	Winthrop Publishers
.TE

