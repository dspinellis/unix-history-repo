.\"	@(#)bibdoc.ms	6.1 (Berkeley) 6/2/86
.\"
.EH 'USD:31-%''BIB \- A Program for Formatting Bibliographies'
.OH 'BIB \- A Program for Formatting Bibliographies''USD:31-%'
.nr LL 6.5i
.nr LT 6.5i
.TL
\fBBIB \- A Program for Formatting Bibliographies\fP
.AU
Timothy A. Budd
.AI
The University of Arizona
Department of Computer Science
Tucson, Arizona 85721
.PP
\fIBib\fP is a program for collecting and formatting reference lists in
documents.  It is a preprocessor to the nroff/troff typesetting systems,
(much like the \fItbl\fP [.tbl.] and \fIeqn\fP [.eqn.] programs) and an alternative
to the \fIrefer\fP [.lesk refer.] bibliography program.  \fIBib\fP takes two
inputs: a document to be formatted and a library of references.  Imprecise
citations in the source document are replaced by more conventional
citation strings, the appropriate references are selected from the reference
file, and commands are generated to format both citation and the referenced
item in the bibliography.
.PP
An imprecise citation is a list of words surrounded by the characters
[\&. .].  Words (which are truncated to six letters)
in the imprecise citation are matched against entries in the reference file,
and if an entry is found that matches all words, that reference is used.
For example:
.de 2Q
.sp
.QS
.QS
..
.de 2E
.sp
.QE
.QE
..
.2Q
.PP
In Brooks's interesting book [\&. brooks mythical.] various reasons ...
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
.PP
Reference files are prepared for \fIbib\fP using \fIinvert\fP.
By default \fIinvert\fP places an inverted index for the
reference list in the file INDEX.  Unless the user specifies an
alternative (see the \-p switch described below), this is the first file
searched by \fIbib\fP in attempting to locate a reference.  If the entry is not found
in the user's file, a standard system-wide index is searched.  If the
entry is still not found in the system file, a warning message is produced
and a blank citation is generated.
.PP
The format for entries in the reference file is described more fully in
the section `Reference File Formats'.
This format is similar to that used by \fIrefer\fP
with the following exceptions:
.IP 1.
An F field, if present, overrides whatever citation string would otherwise
be constructed.
.IP 2.
Certain defined names can be used, and will be expanded differently by
different document styles.  For example, the string CACM is expanded into
`Communications of the ACM' by some document styles, `Comm.
ACM' by others,
and `Comm. of the Assoc. of Comp. Mach.' by yet others.
Appendix 1 lists the currently recognized names.
.IP 3.
The program automatically abbreviates names, reverses names, and
hyphenates strings of contiguous references, if requested.
.IP 4.
A reference can have more than one editor field, and editor's names
can be abbreviated, reversed, and/or printed in cap/small caps style,
independent of any processing done to authors names.
.PP
Since the user's index is searched before the system index, if the
user wants to alter a specific entry in the system index (say to change
the name W. E. Howden to William E. Howden, for example) it is a simple
matter to copy the system information into a private database and make
the changes locally.
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
.IP stdn\0\0 6m
(standard numeric) numeric citation.  Reference entries are listed in
citation order.
.IP stdsn
same as stdn, but references are sorted by senior author followed by date.
.IP stda
(standard alphabetic) citations are three letters followed by the last two
digits of the date.  For papers with a single author, the letters are the
first three letters of the authors last name (e.g. Knu).
In papers with two authors the first two letters are from the first author
followed by one letter from the second (e.g. HoU). If three or more
authors are given the first letters from the first three authors are used
(e.g. AHU).
.IP openn
same as stdsn, only using an open reference format (each major entry is on
a new line\u1\d).
.FS
1. The open reference format is adapted from \fIA Handbook for Scholars\fP,
by Mary-Claire van Leunen, published by Knopf, 1978.
.FE
.IP opena
same as stda, but using an open format.
.IP foot
footnoted references.
.IP supn
same as stdn, but using superscripts.
.IP spe
format used by the journal \fISoftware\(emPractice and Experience\fP.
Eventually there will be macro packages available for several journal styles.
.PP
It is possible to alter slightly the format of standard styles.  For example,
to generate references in standard numeric style, but abbreviate first names,
the following can be used:
.sp
.ce
bib \-tstdn \-aa ...
.PP
If two reference items create the same citation string (this can happen
if two papers authored by the same person in a single year are referred to
in one paper) a disambiguating final letter is added to the citation
(i.e., Knu79 becomes Knu79a and Knu79b).
As noted previously, this can be altered by using the F field.
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
.QS
.nr
.sp
 .\e" [\&.imprecise citation.]
.sp
.QE
.PP
It is also possible to embed \fItroff\fP commands within a reference definition.
See `abbreviations' in the section `Reference Format Designer's
Guide' for an example.
.PP
In some styles (superscripts) periods and commas should precede the
citation while spaces follow.
In other styles (brackets) these rules are reversed.  If
a period, comma or space immediately precedes a citation, it will be moved to the
appropriate location for the particular reference style being used.
This movement is not done for citations given in the alternative style.
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
build citations according to the template \fIstr\fP.  See the reference
format designer's guide for more information on templates.
.IP \-ea
abbreviate editors' names
.IP \-ex
places editors' names in Caps-Small Caps style.  (see \-x )
.IP \-er\fInum\fP
reverse the first \fInum\fP editors' names.  If \fInum\fP is omitted all editors'
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
characters \fIafhorx\fP.
.IP \-o
sort contiguous citations according to the order given by the reference
list.  (This option defaults on).
.IP "\-p \fIfile\fP"
.ns
.IP  \-p\fIfile\fP
instead of searching the file INDEX,
search the indicated reference file(s) before searching the system file.
Multiple files are separated by commas.
.IP \-s\fIstr\fP
sort references according to the template \fIstr\fP.
.IP "\-t \fItype\fP"
.ns
.IP \-t\fItype\fP
use the standard macros and switch settings to generate citations and references
in the indicated style.
.de Ex
.sp
.QS
.nf
.ta 3m
..
.sp 1.0i
.ce 
\fBReference File Formats\fP
.sp
.PP
A reference file is a file containing any number of reference
items.  Reference items are separated by one or more blank lines.
There are no restrictions placed on the order of items in a file,
although imposing some order (such as sorting
items alphabetically) simplifies updates.
.PP
A reference item is a collection of field tags and values.
A field tag is a percent sign followed by a single letter.
Currently, the following field tags are recognized:
.Ex
.ta 0.5i
A	Author's name
B	Title of book containing item
C	City of publication
D	Date
E	Editor(s) of book containing item
F	Caption
G	Government (NTIS) ordering number
I	Issuer (publisher)
J	Journal name
K	Keys for searching
N	Issue number
O	Other information
P	Page(s) of article
R	Technical report number
S	Series title
T	Title
V	Volume number
W	Where the item can be found locally
X	Annotations (not in all macro styles)
.QE
.PP
Author and editor fields can be repeated, as necessary, but all other fields
can occur at most once
in any reference.  The field information is as long as necessary,
and can extend onto new lines.
Lines that do not begin with a percent sign or a period
are treated as continuations of the previous line.
The order of fields is irrelevant, except that authors and editors
are listed in the order of occurrence.
.PP
Generally a reference falls into one of several basic categories.
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
.QE
.PP
Sometimes a book (particularly old books) will have no listed publisher.
The reference entry must still have an I field.
.Ex
%A	R. Colt Hoare
%T	A Tour through the Island of Elba
%I	(no listed publisher)
%C	London
%D	1814
.QE
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
Note the use of the backslash, to indicate the last name is Van Tassel,
and not simply Tassel.
.Ex
%A	Dennie Van\e\0Tassel
%T	Program Style, Design, Efficiency,
Debugging and Testing
%I	PRHALL
%D	1978
%W	tab
%K	testing debugging
.QE
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
can be abbreviated to the first three letters.
Note also in this example the use of the K field to define a short
name (hru) that can be used in searching for the reference.
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
.QE
.SH
Article in conference proceedings
.PP
An article from a conference is printed as though it were a journal
article and the journal name was the name of the conference.
Note that string names (SOSP) are also defined for the major
conferences (Symposium on Operating System Principles).
.Ex
%A	M. Bishop
%A	L. Snyder
%T	The Transfer of Information and Authority
in a Protection System
%J	Proceedings of the 7th SOSP
%P	45-54
%D	1979
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
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
.QE
.sp 1.0i
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
The format file name is always prefixed with the string bib.
Thus the format file for a standard document type, such as stdn, is found
in a file called bib.stdn in the standard library area.
.PP
When \fIbib\fP encounters a \-t switch, the user's directory is first searched for
a format file matching the given name, before the system area is examined.
Thus the user can create individual style database files.
.PP
Each formatting command is distinguished by a single
letter, which must be the first character on a line.
The formatting commands in a database file are similar to the command line options
for \fIbib\fP.  The legal commands,
and their arguments, are as follows:
.sp
# text
.PP
A line beginning with a sharp sign is a comment, and all remaining text on the
line is ignored.
.sp
A
.PP
The A command controls how authors' names are to be formatted.  It can be
followed by the following character sequences:
.QS
.IP A 8m
Authors' names are to be abbreviated.  (see \*(oqabbreviations\*(cq, below).
.IP R\fInum\fP
The first \fInum\fP authors' names are to be reversed.  If \fInum\fP is
omitted, all authors' names are reversed.
.IP X
Authors' names are to be printed in Caps-Small Caps style.
.QE
.sp
E
.PP
The E command is equivalent to the A command, except that it controls the
formatting of editors' names.
.sp
F
.PP
The F command indicates that references are to be dumped immediately after
a line containing a citation, such as when the references are to be placed
in footnotes.
.sp
S \fItemplate\fP
.PP
The S command indicates references are to be sorted before being dumped.
The comparison used in sorting is based on the \fItemplate\fP.  See
the discussion on sorting (below) for an explanation of templates.
.sp
C \fItemplate\fP
.PP
The \fItemplate\fP is used as a model in constructing citations.
See the discussion below.
.sp
D \fI\0word \0definition\fP
.PP
The word-definition pair is placed into a table.
Before each reference is dumped it is examined for the
occurrence of these words.  Any occurrence of a word from this table is replaced
by the definition, which is then rescanned for other words.
Words are limited to alphanumeric characters, ampersand and underscore.
.PP
Definitions can extend over multiple lines by ending lines with a backslash
(\e).  The backslash will be removed, and the definition, including the newline
and the next line,
will be entered into the table.  This is useful for including several
fields as part of a single definition (city names can be included as part
of a definition for a publishing house, for example).
.sp
I \fIfilename\fP
.PP
The indicated file is included at the current point.  The included file may
contain other formatting commands.
.sp
H
.PP
Three or more contiguous citations that refer to adjacent items in the
reference list are replaced by a hyphenated string.  For example, the
citation 2,3,4,5 would be replaced by 2-5.  This is most useful with
numeric citations.  The H option implies the O option.
.sp
O
.PP
Contiguous citations are sorted according to the order given by the reference
list.
.sp
R \fInumber\fP
.PP
The first \fInumber\fP author's names are reversed on output (i.e. T. A. Budd
becomes Budd, T. A.).
If number is omitted all names are reversed.
.sp
T \fIstr\fP
.PP
The \fIstr\fP is a list of field names.  Each time a definition string for
a named field is produced, a second string containing just the last character
will also be generated.  See `Trailing characters', below.
.sp
X
.PP
Authors' last names are to be printed in Caps/Small Caps
format (i.e., Budd becomes B\s-2UDD\s+2).
.sp 2
.PP
The first line in the format file that does not match a format command
causes that line, and all subsequent lines, to be immediately copied to
the output.
.SH
File Naming Conventions
.PP
Standard database format files are kept in a standard library area.
The string BMACLIB in bib.h points to this directory (/usr/new/lib/bmac in
the distribution).  In addition, this name is always defined when
reading format files.
There are three types of files:
.IP bib.xxx 10m
These files contain bib commands to format documents in the xxx style.
.IP bibinc.xxx
These files contain information (such as definitions) used by more than one
style database.
.IP bmac.xxx
These files are the \fItroff\fP macros to actually implement a style.
They are generally not examined by \fIbib\fP at all, but are processed
by troff in response to a .so command.
.PP
The first command output by \fIbib\fP defines the string l] to be the
standard macro database directory.  This allows macro files to be
independent of where they are actually stored.
.SH
Naming Conventions
.PP
There is a simple naming convention for strings, registers and macros used
by \fIbib\fP.  All strings, registers and macros are denoted by two character
names containing either a left or right brace.  The following are general rules:
.IP [x
If x is alphanumeric, the string contains the value of a reference field.
If x is nonalphanumeric, this is a formatting string preceding a citation.
.IP ]x
If x is alphanumeric, this is the final character from a reference field.
If x is nonalphanumeric, the string is formatting information within a citation.
.IP x[
Strings in this format, where x is can be any character, are defined by the
specific macro package in use and are not specified by \fIbib\fP.
.IP x]
If x is nonalphanumeric these strings represent formatting commands following
citations (the inverse of [x commands).  Other strings represent
miscellaneous formatting commands,
such as the space between leading letters in abbreviated names.
.SH
Sorting
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
only the last \fIn\fP characters.  Thus, for example, the template AD\-2 indicates
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
Citations
.PP
A citation template is similar to a sort template, with the following
exceptions:  The field name `1' refers to the number which
represents the position of the reference in the reference list (after sorting).
The field name `2' generates a three character sequence; If the
paper being referenced has only one author, this is the first three characters
of the author's last name.  For two author papers, this is the first two
characters of the senior author, followed by the first character of the second
author.  For papers with three or more authors the first letter of the first
three authors is used.
The field name `3' is used to specify a format consisting of the authors'
last names, or the senior author followed by the text `et al' if more than
four authors are listed.  The fields `4' through `9' are reserved to be
used to specify formats that cannot be produced using templates.  These
will be implemented either as local modifications to \fIbib\fP or in future
releases.
.PP
In order to postpone the inevitable clash of local changes versus new releases, it is
suggested that local formatting styles use numbers starting at 9 and working
downward.
.PP
Each object can be followed by either of the letters `u' or
`l' and the field will be printed in all upper or all lower case,
respectively.
.PP
If necessary for disambiguating, the character `@' can be used as
a separator between objects in the citation template.  Any text which should
be inserted into the citation uninterpreted should be surrounded by either
{} or <> pairs.
.SH
Citation Formatting
.PP
In the output, each citation is surrounded by the strings \e*([[ and \e*(]]
(\e*([{ and \e*(}] in the alternative style).
Multiple citations are separated by the string \e*(],.
The text portion of a format file should contain \fItroff\fP definitions for
these strings to achieve the appropriate typographic effect.
.PP
Citations that are preceded by a period, comma, space or other puncuation
are surrounded by string values for formatting the puncuation in the
approprate location.
Again, \fItroff\fP commands should be given to insure the appropriate values are
produced.
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
[!	!]	Exclaimation Point before and after citation
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
Name Formatting
.PP
Authors' (and editors') names can be abbreviated, reversed, and/or printed
in Caps-small Caps format.  In producing the string values for an author,
formatting strings are inserted to give the macro writer greater flexibility
in producing the final output.  Currently the following strings are used:
.TS
center;
l l.
a]	gap between sucessive initials
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
Howden\\*(b]W\\*(p]\\*(a]E\\*(p]
.SH
Reference Formatting
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
is converted into the following sequence of commands
.KS
.nf
.sp
 .[\-
 .ds [F 1
 .ds [A M. A. Harrison
 .as [A \e*(c]W. L. Ruzzo
 .as [A \e*(m]J. D. Ullman
 .ds [T Protection in Operating Systems
 .ds [J Communications of the ACM
 .ds [V 19
 .ds [N 8
 .nr [P 1
 .ds [P 461-471
 .ds [D 1976
 .][
.sp
.KE
.PP
Note that the commands are preceded by a call on the macro `.[\-'.
This can be used by the macro routines for initialization, for example to
delete old string values.  The string [F is the citation value used
in the document.
Note that the string CACM has been expanded.
.PP
The strings c], n] and m] are used to separate authors.  c] separates
the initial authors in multi-author documents (it is usually a comma
with some space before and after), n] separates authors in two author
documents (usually `\ and\ '), and m] separates the last two authors
in multi-author documents (either `\ and\ ' or `,\ and\ ').
.PP
If abbreviation is specified, the string a] is used to separate initials
in the author's first name.
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
is a right brace  followed by the field character.  Macro packages should
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
.QS
.sp
 ...
 %A Ole-Johan Dahl
 .ds [A O\-J. Dahl
 ...
.sp
.QE
.PP
In fact, any \fItroff\fP commands can be entered in the middle of a reference
entry, and the commands are copied uninterpreted to the output.
For example, the user may wish to have a switch indicating whether the name
is to be abbreviated or not:
.QS
.sp
 ...
 %A Ole-Johan Dahl
 .if \en(i[ .ds [A O\-J. Dahl
 ...
.sp
.QE
.SH
An Example
.PP
Figure 1 shows the format file for the standard alphabetic format.
The sort command indicates that sorting is to be done by senior author,
followed by the last two digits of the date.  The citation template
indicates that citations will be the three character sequence described
in the section of citations
followed by the last two characters of the date (i.e. AHU79, for
example).
.KS
.nf
.sp
#  standard alphabetic format
SAD\-2
C2D\-2
I BMACLIB/bibinc.fullnames
I BMACLIB/bibinc.std
.sp
.ce
\fBFigure 1\fP
.sp
.KE
.PP
The two I commands include two files.  The first is a file of definitions
for common strings, such as dates and journal names.  A portion of this
file is shown in figure 2.
Note that a no-op has been inserted into the definition string for
BIT in order to avoid further expansion when the
definition is rescanned.
.PP
The second file is a sequence of \fItroff\fP macros
for formatting the references.  The beginning of this file is shown in figure 3.
.PP
On the basis of some simple rules (the presence or absence of certain fields)
the document is identified as one of five different types, and a call made
on a different macro for each type.  This is shown in figure 4.
.PP
Finally figure 5 shows the macro for one of those different types, in this
case the book formatting macro.
.KS
.nf
.sp
# full journal names, and other names
#
# journals
D ACTA Acta Informatica
D BIT B\e&IT
D CACM Communications of the ACM
 ...
# months
D JAN January
 ...
D DEC December
.sp
.ce
\fBFigure 2\fP
.sp
.KE
.KS
.nf
.sp
 #  standard end macros
 #
 .ds [ [
 .ds ] ]
 .ds , ,
 .ds >. .
 .ds >, ,
 .ds c] , \e&
 .ds n] "" and \&
 .ds m] , and \&
   ...
 .de p[   \e" produce reference beginning
 .IP [\e\e$1]\0\0
 ..
 .de []   \e" start displaying collected references
 .SH
 References
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
.KS
.nf
.sp
   ...
 .de 2[ \e" book
 .if !"\e\e*([F"" .p[ \e\e*([F
 .if !"\e\e*([A"" \e\e*([A,
 .if !"\e\e*([T"" \e\ef2\e\e*([T,\e\ef1
 \e\e*([I\ec
 .if !"\e\e*([C"" , \e\e*([C\ec
 .if !"\e\e*([D"" \e& (\e\e*([D)\ec
 \e&.
 .if !"\e\e*([G"" Gov't. ordering no. \e\e*([G.
 .if !"\e\e*([O"" \e\e*([O
 .]\-
 ..
.sp
.ce
\fBFigure 5\fP
.sp
.KE
.SH
Acknowledgements
.PP
\fIbib\fP was inspired by \fIrefer\fP, written by M. Lesk.
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
The following list gives the standard names recognized in most
citation styles.  Various different forms for the output are used
by the different styles.
In the longer reference style,
the conference proceedings will also refer
to the date (%D), city(%C),
and when the proceedings are published as a journal,
the journal name (%J), volumn (%V) and number (%N).
.sp
.TS
expand;
l s
l l.
\fBJournal Names\fP
ACTA	Acta Informatica
BIT	B\&IT
BSTJ	Bell System Technical Journal
CACM	Communications of the ACM
COMP	IEEE Computer
COMPJOUR	The Computer Journal
COMPLANG	Journal of Computer Languages
COMPSUR	ACM Computing Surveys
I&C	Information and Control
IBMJRD	IBM Journal of Research and Development
IBMSJ	IBM Systems Journal
IEEETC	IEEE Transactions on Computers
IEEETSE	IEEE Transactions on Software Engineering
IJCIS	International Journal of Computer and Information Sciences
IPL	Information Processing Letters
JACM	Journal of the ACM
JCSS	Journal of Computer and System Sciences
MATHST	Mathematics Systems Theory
NMATH	Numerical Mathematics
SIAMJC	Siam Journal on Computing
SIAMJNA	Siam Journal on Numerical Analysis
SIGACT	S\&IGACT News
SIGPLAN	SI\&GPLAN Notices
SIGSOFT	Software Engineering Notes
SP&E	Software\(emPractice & Experience
SPE	Software\(emPractice & Experience
TOCS	ACM Transactions on Computer Systems
TODS	ACM Transactions on Database Systems
TOMS	ACM Transactions on Mathematical Software
TOPLAS	ACM Transactions on Programming Languages and Systems
.TE
.sp
.TS
expand;
l s
l l.
\fBConferences\fP
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
\fBLonger place names\fP
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
.2C
.TS
l s
l l.
\fBShort place names\fP
CORP	Corporation
CSD	Computer Science Department
DCS	Department of Computer Science
DEPT	Department
DISS	Dissertation
TR	Technical Report
UATR	University of Arizona Technical Report
UNIV	University
ERL	Electronics Research Laboratory
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

