/*
read bibinc.names, create bibinc.shortnames and bibinc.fullnames

The format of the lines in bibinc.names is formally:

<inputline> ::= '+'<char1> <filename>
              | '+'<char1> '+'<char3> '+'<char3> ... 
	      | '+'<char1> <name> <stuff>
	      | '+'<char1><stuff>
	      | '?'<name>

In all cases the '+' or '?' must be the first character of a line to be
recognized, and there can be no spaces between it and the following
character.  Continuation lines are all lines from the preceding '+' to
the following '+'.  Any and all whitespace at the beginning of a
continuation line is discarded.

The first form defines <char1> to be the character that denotes an outputfile
named <filename>.  It must be the first occurence of <char1> following a '+'
in the file.  

The second form defines <char1> to be the character that denotes several
previously defined outputfiles.  It must be the first occurence of '+'<char1>
in the file.

The third form says that the following line is to be written to the file or
files denoted by <char1>:

D <name> <stuff>

The fourth form allows the inclusion of arbitrary stuff into the file.  Note
that there is no space between the <stuff> and <char1> in the fourth form.

The fifth form is used to select lines to be processed, and is a very
brain damaged hack to accomplish the goal.  The problem to be solved is 
that we want to be able to create files for three possibilities:
   (1) the user is using bib-style macros with troff,
   (2) the user is using tib-style macros with troff,
   (3) the suer is using tib-style macros with TeX or LaTeX.

Therefore, the user can specify on the command line:

   bib troff	or just	bib (troff implied)
   tib troff
   tib tex	or just	tex (tib implied)
   [bib tex is illegal: not a supported combination]

then if a line of the form "?tib" is encountered, then the following
lines are processed only if "tib" was specified or implied on the
invocation line.  This will remain true until "?bib" or "?" is
encountered.  Likewise, "?troff" will process the following lines only
if troff was specified or implied on the invocation line, and remains
true until "?tex" or "?" is encountered.

Some Commentary:

The problem is that the tib macro mechanism is quite different
from the bib macro style.  In bib, you define and use a macro:

D macro expansion text
	:
%A A. Nonymous
%T More on macro

The title will be expanded to `More on expansion text'.

In tib, macro calls are always enclosed in vertical bars:

D macro expansion text
    :
%A A. Nonymous
%T The macro as |macro|

The title will be expanded to `The macro as expansion text', therewith 
demonstrating a major benefit of using the |vertical bars|.  Here is another
benefit of the bars:

D u_um \"{u}
D Karlsruhe Karlsr|u_um|he

It's easy now to get the diacritical marks right, and still have a 
recognizable word to act as a key in the INDEX.

Currently, once the vertical bars are introduced into your data base,
there is no going back to bib: it does not recognize them.
However, it does not prevent bibinc from creating bibinc files for either
bib or tib, as we shall see below.

The |Karlsruhe| example has a problem if your database is being used
for both TeX and ditroff: bib/tib macros have no conditionals, and you
have to choose either the TeX code or the troff code for special
characters.  Bibinc has a mechanism to solve this.  Let's assume you
have the following in a file called bibinc.names:

#================================== bibinc.names
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
+B a_um \"{a}
+B o_sl \o
+B o_um \"{o}
+B u_um \"{u}
+B TCOLADA $\mbox{TCOL}_\mbox{Ada}$
+B dash --
+B Rn  $\mbox{R}^\mbox{n}$
+B AMP \\&
?troff          # either bib or tib
+B a_um \*(:a
+B o_sl \*(/o
+B o_um \*(:o
+B u_um \*(:u
+B TCOLADA TCOL\dAda\u
+B dash \(hy
+B Rn  \*(Rn
+B AMP &
?
+B fuer f|u_um|r
#
+B IFI   Institut |fuer| Informatik
+B Universitat Universit|a_um|t
+B KARLSRUHE Karlsr|u_um|he
+F Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|
    %C |KARLSRUHE|, West Germany        # note that leading blanks are removed
+S Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|
+F SIGPLAN SIG\&PLAN Notices
+S SIGPLAN SIG\&PLAN
+B GUNS Smith |AMP| Wesson
# last line of bibinc.names
#================================== bibinc.names

This input will cause bibinc to select the correct expansion of u_um depending
on whether -Tib was specified on its invocation line or not.  That is,
assuming the following invocation:

% bibinc troff tib <bibinc.names

two files will be written, bibinc-t.fullnames and bibinc-t.shortnames:

================================== bibinc-t.fullnames
D a_um \*(:a
D o_sl \*(/o
D o_um \*(:o
D u_um \*(:u
D TCOLADA TCOL\dAda\u
D dash \(hy
D Rn  \*(Rn
D AMP &
D fuer f|u_um|r
D IFI   Institut |fuer| Informatik
D Universitat Universit|a_um|t
D KARLSRUHE Karlsr|u_um|he
D Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|\
%C |KARLSRUHE|, West Germany
D SIGPLAN SIGPLAN Notices
================================== bibinc-t.fullnames
and
================================== bibinc-t.shortnames
D a_um \*(:a
D o_sl \*(/o
D o_um \*(:o
D u_um \*(:u
D TCOLADA TCOL\dAda\u
D dash \(hy
D Rn  \*(Rn
D AMP &
D fuer f|u_um|r
D IFI   Institut |fuer| Informatik
D Universitat Universit|a_um|t
D KARLSRUHE Karlsr|u_um|he
D Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|
D SIGPLAN SIGPLAN
================================== bibinc-t.shortnames

Users of bib/ditroff will have to specify -Tib on their
invocations of bib to use these files.

The command:

%bibinc bib troff <bibinc.names

produces

================================== bibinc.fullnames
D a_um \*(:a
D o_sl \*(/o
D o_um \*(:o
D u_um \*(:u
D TCOLADA TCOL\dAda\u
D dash \(hy
D Rn  \*(Rn
D AMP &
D fuer f\&u_um\&r
D IFI   Institut fuer Informatik
D Universitat Universit\&a_um\&t
D KARLSRUHE Karlsr\&u_um\&he
D Karlsruhe  IFI, Universitat KARLSRUHE\
%C KARLSRUHE, West Germany
D SIGPLAN SIG\&PLAN Notices
================================== bibinc.fullnames
and
================================== bibinc.shortnames
D a_um \*(:a
D o_sl \*(/o
D o_um \*(:o
D u_um \*(:u
D TCOLADA TCOL\dAda\u
D dash \(hy
D Rn  \*(Rn
D AMP &
D fuer f\&u_um\&r
D IFI   Institut fuer Informatik
D Universitat Universit\&a_um\&t
D KARLSRUHE Karlsr\&u_um\&he
D Karlsruhe  IFI, Universitat KARLSRUHE
D SIGPLAN SIG\&PLAN
================================== bibinc.shortnames


In this mode, users of bib/ditroff will have to be very
careful that their macro names do not conflict with anything in normal
text.  

The command:

%bibinc tex tib <bibinc.names

produces:

================================== tibinc.fullnames
D a_um \"{a}
D o_sl \o
D o_um \"{o}
D u_um \"{u}
D TCOLADA $\mbox{TCOL}_\mbox{Ada}$
D dash --
D Rn  $\mbox{R}^\mbox{n}$
D AMP \&
D fuer f|u_um|r
D IFI   Institut |fuer| Informatik
D Universitat Universit|a_um|t
D KARLSRUHE Karlsr|u_um|he
D Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|\
%C |KARLSRUHE|, West Germany
D SIGPLAN SIGPLAN Notices
================================== tibinc.fullnames
and
================================== tibinc.shortnames
D a_um \"{a}
D o_sl \o
D o_um \"{o}
D u_um \"{u}
D TCOLADA $\mbox{TCOL}_\mbox{Ada}$
D dash --
D Rn  $\mbox{R}^\mbox{n}$
D AMP \&
D fuer f|u_um|r
D IFI   Institut |fuer| Informatik
D Universitat Universit|a_um|t
D KARLSRUHE Karlsr|u_um|he
D Karlsruhe  |IFI|, |Universitat| |KARLSRUHE|
D SIGPLAN SIGPLAN
================================== tibinc.shortnames

Notice how bibinc handles the bib/troff use of the special character
sequence \& in the above definitions of the SIGPLAN macro.
I.e., bibinc removes all user inserted \&'s when tib/tex is specified.
NOTICE that when tib/tex is specified, any other \<char> is passed 
through exactly as is, unless <char> is \, in which case bibinc reduces 
it to a single \.
Otherwise, all other \<char> are passed through as is.

There is a problem here: \& to troff means `hard blank' while to TeX it
means `put an ampersand here'.  Somehow the designer of the input to
bibinc needs to be able to distinguish these two usages.  We will put the onus
on the TeX user.  If you must pass \& to TeX somewhere in the macros being
defined here, then you must define a macro to do that!  E.g.

?tex
+B AMP \\&
?troff
+B AMP &  # or whatever the troff characters are that will produce &

and now you can do things like

+B GUNS Smith |AMP| Wesson

/* */

