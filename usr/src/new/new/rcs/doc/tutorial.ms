.SH
How to get started with RCS
.PP
Suppose you have a file f.c that you wish to put under control of RCS. 
Invoke the checkin command:
.DS
ci  f.c 
.DE
This command creates f.c,v, stores f.c into it as revision 1.1, and
deletes f.c.
It also asks you for a description. The description should be
a synopsis of the contents of the file.
All later checkin commands will ask you for a log entry,
which should summarize the changes that you made.
.PP
Files ending in ,v are called RCS files ("v" stands for "versions"),
the others are called working files.
To get back the working file f.c in the previous example, use the checkout
command:
.DS
co  f.c
.DE
This command extracts the latest revision from f.c,v and writes
it into f.c.
You can now edit f.c and check it in back in by invoking:
.DS
ci  f.c
.DE
\fICi\fR increments the revision number properly. 
If \fIci\fR complains with the message
.DS
        ci error: no lock set by <your login>
.DE
then your system administrator has decided to create all RCS files
with the locking attribute set to `strict'. In this case, you should
have locked the revision during the previous checkout. 
Thus, your last checkout should have been
.DS
co  -l  f.c
.DE
Locking assures that you, and only you, can check in the next update, and
avoids nasty problems if several people work on the same file.
Of course, it is too late now to do the checkout with locking, because you
probably modified f.c already, and a second checkout would
overwrite your modifications. Instead, invoke
.DS
rcs  -l  f.c
.DE
This command will lock the latest revision for you, unless somebody
else got ahead of you already. In this case, you'll have to negotiate with 
that person.
.PP
If your RCS file is private, i.e., if you are the only person who is going
to deposit revisions into it, strict locking is not needed and you
can turn it off.
If strict locking is turned off,
the owner off the RCS file need not have a lock for checkin; all others
still do. Turning strict locking off and on is done with the commands:
.DS
rcs  -U  f.c       and         rcs  -L  f.c
.DE
You can set the locking to strict or non-strict on every RCS file.
.PP
If you don't want to clutter your working directory with RCS files, create 
a subdirectory called RCS in your working directory, and move all your RCS 
files there. RCS commands will look first into that directory to find 
needed files. All the commands discussed above will still work, without any 
modification\u*\d. 
.FS
* Pairs of RCS and working files can actually be specified in 3 ways: 
a) both are given, b) only the working file is given, c) only the
RCS file is given. Both files may have arbitrary path prefixes;
RCS commands pair them up intelligently.
.FE
.PP
To avoid the deletion of the working file during checkin (in case you want to
continue editing), invoke
.DS
ci  -l  f.c
.DE
This command checks in f.c as usual, but performs an additional
checkout with locking.
Thus, it saves you one checkout operation.
There is also an option
\fB-u\fR for \fIci\fR which does a checkin followed by a checkout without
locking. This is useful if you want to compile the file after the checkin.
Both options also update the identification markers in your file (see below).
.SH
Automatic Identification
.PP
RCS can put special strings for identification into your source and object
code. To obtain such identification, place the marker
.DS
$Header$
.DE
into your text, for instance inside a comment.
RCS will replace this marker with a string of the form
.DS
$Header:  filename  revisionnumber  date  time  author  state $
.DE
You never need to touch this string, because RCS keeps it
up to date automatically.
To propagate the marker into your object code, simply put
it into a literal character string. In C, this is done as follows:
.DS
static char rcsid[] = "$Header$";
.DE
The command \fIident\fR extracts such markers from any file, even object code.
Thus, \fIident\fR helps you to find out 
which revisions of which modules were used in a given program. 
.PP
You may also find it useful to put the marker
.DS
$Log$
.DE
into your text, inside a comment. This marker accumulates
the log messages that are requested during checkin.
Thus, you can maintain the complete history of your file directly inside it.
There are several additional identification markers; see \fIco\fR (1) for
details.
.SH
How to combine MAKE and RCS
.PP
If your RCS files are in the same directory as your working files,
you can put a default rule into your makefile. Do not use a rule
of the form .c,v.c, because such a rule keeps a copy of every
working file checked out, even those you are not working on. Instead, use this:
.DS

 .SUFFIXES: .c,v

 .c,v.o:
           co -q $*.c
           cc $(CFLAGS) -c $*.c
           rm -f $*.c

 prog:   f1.o f2.o .....
           cc f1.o f2.o ..... -o prog
.DE
This rule has the following effect. If a file f.c does not exist, and f.o 
is older than f.c,v, MAKE checks out f.c, compiles f.c into f.o, and then 
deletes f.c. 
From then on, MAKE will use f.o until you change f.c,v.
.PP
If f.c exists (presumably because you are working on it), the default
rule .c.o takes precedence, and f.c is compiled into f.o, but not deleted.
.PP
If you keep your RCS file in the directory ./RCS, all this won't work
and you have to write explicit checkout rules for every file, like
.DS
f1.c:   RCS/f1.c,v; co -q f1.c
.DE
Unfortunately, these rules do not
have the property of removing unneeded .c-files.
A modification of MAKE which understands RCS directories
will be available soon.
.SH
Additional Information on RCS
.PP
If you want to know more about RCS, for example how to work
with a tree of revisions and how to use symbolic revision numbers, read
the following paper:
.sp 1
Walter F. Tichy, "Design, Implementation, and Evaluation of a Revision Control
System," in \fIProceedings of the 6th International Conference on Software
Engineering\fR, IEEE, Tokyo, Sept. 1982.
.PP
Taking a look at the manual page RCSFILE (5)
should also help to understand the revision tree permitted
by RCS. A full user manual for RCS is in preparation.
