.de HE 'Version six changes'INGRES'%'
.so nmacs
Users of INGRES version five should be aware of certain differences
between version five and version six.
Version six fixes all the known bugs that were in version five.
Some of these fixes and improvements might cause
some version five queries to not run correctly on version six.
For this reason, we have itemized the changes which
you should be aware of.
Refer to the new version six reference manual for additional details.

.sh 1
Storage Structures.
.dn
Incompatibilities.
.PS
Version six of INGRES will NOT run on a
version five data base.
All data bases must be recreated.
There is a utility program called
"copydb5" which aids in the 
conversion process.
Refer to the INGRES reference manual
under copydb(UNIX) for documentation
on how to use copydb5.
.sh 1
Monitor
.dn
Different working directory
.PS
The monitor now runs in the working
directory where the user was at the
time INGRES was invoked.
This means that when you escape to the
shell or the editor, you will
be in the same working directory as
when you invoked INGRES.
.sh
Auto-clear mode
.PS
The version five monitor required you to
reset the workspace after a query ("\\r").
The version six monitor does approximately
the opposite.

If the first line after a query does NOT
begin with the command "\\p", "\\g", "\\e", or "\\a", then
the monitor automatically resets the workspace
for you.
Otherwise, the command is interpreted and new lines are
appended to the workspace.
The autoclear feature can be suppressed by using
the "-a" flag when invoking INGRES.
Once you use the autoclear feature for a while,
however, you will really appreciate it.
.sh
Rubout works! (nearly always)
.PS
Typing a rubout or break causes INGRES to
stop what it is doing and return cleanly
to the monitor.
If a rubout is typed during a modify or defered
update, a message is issued and further rubouts
are ignored until the processing is complete.

Many critical sections in the INGRES
code still exist
where typing a rubout can cause a
SYSERR message.
Those are being cleaned out on a gradual basis.
In general rubout processing works correctly and in
any case will never hurt the data base if all
you are doing is a retrieve
or a print.
.sh 1
Parser
.dn
New keywords and features.
.PS
We have implemented two new commands:
CONCAT and ASCII.
Those words are now reserved words.
ASCII converts a numeric of any type to
a character string.
CONCAT takes two character strings and forms
a new one.
Trailing blanks of the first character string are
removed before concatenating the second string.
.sh
The shorthand notation x.all.
.PS
ALL can be used in retrieves and appends.
Its meaning expands to be all domains of
a relation.
The order of the domains is nearly always the
same as the order from the create statement;
however, this is not presently guaranteed.
.sh
Pattern matching
.PS
We have implemented the "shell syntax" pattern
matching characters "*", "?", "[]".
They can be used in a qualification but not
in a target list.
Their special meaning
can be turned off by preceding them with a
"\\".
.sh
Correct precedence.
.PS
The parser now gives the correct precedence
to numeric operators.
It previously gave equal precedence
to all numeric operators (+,*,**, etc.).
.sh 1
Copy
.dn
File modes.
.PS
The previous version of copy created files
owned as INGRES in mode 666, and demanded
that any file to be read must be
readable by INGRES.

The version six copy
runs as the user.
Thus on coping into a relation, the
UNIX file need be readable only by the
real users.
On a copy out of a relation, the file
is created in mode 600 and is owned by the
real user.
.sh
Copy "into" works correctly.
.PS
The version five "copy into" ignored the
order of the domains and demanded that
all domains be present and in the
identical format as the relation.

The version six copy eliminates all those
restriction, both on copy into and copy from.
.sh 1
Help
.dn
Help on a relation/manual is changed.
.PS
We have reversed the use of the help command.
Relations names should be given without
quotes ("") and manual sections should
include them.
This is consistent with the print command.
.sh
Documentation reorganized.
.PS
The INGRES documentation has been subdivided
into quel, unix, files and error sections.
The help command no longer looks in /usr/man/...
for documentation.
.sh 2
Help "" changed.
.PS
"Help" alone
now lists relations
in the database
instead of the help page
of the reference manual.
Help ""
no longer exists.
.sh 1
Purge
.dn
An easy way to clean up a data base
.PS
The purge command (formerly resetdb and relkiller) provides
an easy way to see what relations have expired
and also to remove any temporary files and
relations which were left over after a system error.
.sh 1
The Users File.
.dn
The users file replaces the authority file
.PS
The users file specifies who can use INGRES,
what data bases the user can use,
special permissions the user has,
any default flags the user wants.

The users file can provide some protection to
a data base but is obviously insufficient.
What protection it provides was implemented
only because it was easy to provide and
somewhat useful.
.sh 1
EQUEL
.dn
Recompilation
.PS
A version five equel program must
be preprocessed again and then
recompiled in order to run on
a version six data base.

If you run an equel program without
recompiling you will get
an error message from INGRES informing
you to redo your equel program.

If you only recompile and don't first
run the equel preprocessor,
you will get an undefined reference
to "skiptup" at load time.

Again, simply run equel on all ".q"
files and then recompile your programs.
.sh
Retrieving domains
.PS
The version five EQUEL allowed you to retrieve a numeric
domains into a character variable.
Version six does not allow this directly.
The ASCII command can be used to perform this
function.
.sh
The debug flag
.PS
Version five equel printed the file name and line
number on an error.
This required a lot of extra storage at run time.
Version six equel has a "-d" flag which
passes file and line information
on an error.
Otherwise, the information is not kept.
.sh 1
Miscellaneous.
.dn
Relation Names.
.PS
Version five relation names appended the users
UNIX group and user id's to a relation name.
INGRES user id's are now kept
in the "users" file and are printing
characters starting with "aa", "ab", and up.
.sh
Running as someone else.
.PS
For data base administrators with alter-ego
problems, INGRES now lets you run as
someone other than yourself.
The "-u" flag can be used at the time
INGRES is invoked to let you run as
a different user.
The "-u" flag may also be used
by the "ingres" user.

The "-u" flag is also available
in creatdb.
It may only be used by the "ingres" user
in this context.
.sh
The Blip is Gone.
.PS
Version five printed a space followed by a
backspace, at the start of every query and
dbu request.
The "blip" was there for historical reasons
only and has been removed.
