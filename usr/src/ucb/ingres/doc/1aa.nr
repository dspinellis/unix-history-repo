.th INTRODUCTION INGRES 3/1/81
.s3
This manual is a reference manual for the \*(II data base
system.
It documents the use of \*(II in a very terse manner.
To learn how to use \*(II, refer to the
document called ``A Tutorial on \*(II''.
.s3
The \*(II reference manual is subdivided into four parts:
.lp +10 8
Quel	describes the commands and features which
are used inside of \*(II.
.lp +10 8
Unix	describes the \*(II programs which are executable
as \*(UU commands.
.lp +10 8
Files	describes some of the important files used
by \*(II.
.lp +10 8
Error	lists all the user generatable error messages
along with some elaboration as to what they mean
or what we think they mean.
.i0
.s3
Each entry in this manual has one or more of
the following sections:
.s3
.in +5
.ti -5
NAME section
.br
This section repeats the name of the entry and gives an indication of
its purpose.
.s3
.ti -5
SYNOPSIS section
.br
This section indicates the form of the command (statement).
The conventions which are used are as follows:
.s3
.in +10
.ti -7
Bold face names are
used to indicate reserved keywords.
.ti -7
Lower case words indicate generic types of information which must
be supplied by the user;
legal values for these names are described in the DESCRIPTION section.
.ti -7
Square brakets ( [] ) indicate that the enclosed item is optional.
.ti -7
Braces ( {} ) indicate an optional item which may be repeated.
In some cases they indicate simple (non-repeated) grouping;
the usage should be clear from context.
.in -10
.s3
When these conventions are insufficient to fully specify
the legal format of a command a more general form is given
and the allowable subsets are specified in the DESCRIPTION section.
.s3
.ti -5
DESCRIPTION section
.br
This section gives a detailed description of the entry with references
to the generic names used in the SYNOPSIS section.
.s3
.ti -5
EXAMPLE section
.br
This section gives one or more examples of the use of the entry.
Most of these examples are based on the following relations:
.ce 5
emp(name,sal,mgr,bdate)
and
newemp(name,sal,age)
and 
parts(pnum, pname, color, weight, qoh)
.s3
.ti -5
SEE ALSO section
.br
This section gives the names of entries in the manual which
are closely related to the current entry or which are referenced 
in the description of the current entry. 
.s3
.ti -5
BUGS section
.br
This section indicates known bugs or deficiencies in the command.
.s3
.in -5
To start using \*(II you must be entered as an
\*(II user; this is done by the
\*(II administrator who will enter
you in the ``users'' file (see users(files)).
To start using ingres
see the section on ingres(unix),
quel(quel), and monitor(quel).
.s3
.in +5
.sh ACKNOWLEDGEMENTS
We would like to acknowledge the
people who have worked on
\*(II in the past:
.in +5
.nf
.s3
Rick Birman
Bob Epstein
James Ford
Paula Hawthorn
Gerald Held
Peter Kreps
Dan Ries
Peter Rubinstein
Mike Ubell
Nick Whyte
Karel Youssefi
William Zook
.i0
.fi
.sh FOOTNOTE
\s-2UNIX\s0 is a trademark
of Bell Laboratories.
