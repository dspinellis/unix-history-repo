.th MACROS QUEL 2/19/79
.sh NAME
macros \- terminal monitor macro facility
.sh DESCRIPTION
The terminal monitor macro facility
provides the ability
to tailor the QUEL language
to the user's tastes.
The macro facility
allows strings of text
to be removed from the query stream
and replaced with other text.
Also,
some built in macros
change the environment
upon execution.
.s1
.bd "Basic Concepts"
.s2
All macros are composed of two parts,
the
.it template
part and the
.it replacement
part.
The template part defines when the macro
should be invoked.
For example,
the template
``ret''
causes the corresponding macro
to be invoked upon encountering
the word
``ret''
in the input stream.
When a macro is encountered,
the template part is removed
and replaced with the replacement part.
For example,
if the replacement part of the
``ret'' macro
was ``retrieve'',
then all instances of the word
``ret'' in the input text
would be replaced with the word
``retrieve'',
as in the statement
.s3
	ret (p.all)
.s3
Macros may have parameters,
indicated by a dollar sign.
For example,
the template
``get $1''
causes the macro to be triggered by the word
``get''
followed by any other word.
The word following ``get''
is remembered
for later use.
For example,
if the replacement part
of the ``get'' macro
where
.s3
	retrieve (p.all) where p.pnum = $1
.s3
then typing ``get 35''
would retrieve all information about part number 35.
.s1
.bd "Defining Macros"
.s2
Macros can be defined
using the special macro called
``define''.
The template for the define macro
is (roughly)
.s3
	{define; $t; $r}
.s3
where $t and $r are the template and replacement
parts of the macro, respectively.
.s3
Let's look at a few examples.
To define the ``ret'' macro
discussed above,
we would type:
.s3
	{define; ret; retrieve}
.s3
When this is read,
the macro processor
removes everything between the curly braces
and updates some tables
so that ``ret'' will be recognized
and replaced with the word ``retrieve''.
The define macro has the null string
as replacement text,
so that this macro seems to disappear.
.s3
A useful macro
is one which shortens range statements.
It can be defined
with
.s3
	{define; rg $v $r; range of $v is $r}
.s3
This macro causes the word ``rg'' followed by the
next two words
to be removed and replaced
by the words
``range of'',
followed by the first word which followed ``rg'',
followed by the word ``is'',
followed by the second word
which followed ``rg''.
For example,
the input
.s3
	rg p parts
.s3
becomes the same as
.s3
	range of p is parts
.s3
.s1
.bd "Evaluation Times"
.s2
When you type in a define statement,
it is not processed immediately,
just as queries are saved
rather than executed.
No macro processing is done
until the query buffer
is evaluated.
The commands
\ego,
\elist,
and \eeval
evaluate the query buffer.
\ego sends the results
to \*(II,
\elist prints them on your terminal,
and \eeval
puts the result back into the query buffer.
.s3
It is important to evaluate any define statements,
or it will be exactly like you did not type them in
at all.
A common way to define macros is to type
.s3
	{define . . . }
.br
	\eeval
.br
	\ereset
.s3
If the \eeval was left out,
there is no effect at all.
.s1
.bd "Quoting"
.s2
Sometimes strings must be passed
through the macro processor
without being processed.
In such cases the grave and acute accent marks
(\*g and \*a)
can be used to surround the literal text.
For example,
to pass the word ``ret''
through without converting it to ``retrieve''
we could type
.s3
	\*gret\*a
.s3
Another use for quoting is during parameter collection.
If we want to enter more than one word
where only one was expected,
we can surround the parameter with accents.
.s3
The backslash character
quotes only the next character
(like surrounding the character with accents).
In particular,
a grave accent can be used literally
by preceeding it
with a backslash.
.s3
Since macros can normally only be on one line,
it is frequently useful to use a backslash
at the end of the line
to hide the newline.
For example,
to enter the long ``get'' macro,
you might type:
.nf
	{define; get $n; retrieve (e.all) \e
	where e.name = "$n"}
.fi
.s3
The backslash always quotes
the next character
even when it is a backslash.
So, to get a real backslash,
use two backslashes.
.s1
.bd "More Parameters"
.s2
Parameters need not be limited
to the word following.
For example,
in the template descriptor
for define:
.s3
	{define; $t; $r}
.s3
the $t parameter ends at the first semicolon
and the $r parameters ends at the first
right curly brace.
The rule is that the character
which follows the parameter
specifier
terminates the parameter;
if this character is a space,
tab,
newline,
or the end of the template
then one word is collected.
.s3
As with all good rules,
this one has an exception.
Since system macros are always surrounded by curly braces,
the macro processor knows that they must be properly nested.
Thus, in the statement
.s3
	{define; x; {sysfn}}
.s3
The first right curly brace will close the ``sysfn''
rather than the ``define''.
Otherwise this would have to be typed
.s3
	{define; x; \*g{sysfn}\*a}
.s3
.s3
Words are defined in the usual way,
as strings of letters and digits
plus the underscore character.
.s1
.bd "Other Builtin Macros"
.s2
There are several other macros
built in to the macro processor.
In the following description,
some of the parameter specifiers are marked
with two dollar signs rather than one;
this will be discussed in the section on prescanning below.
.s3
{define; $$t; $$r}
defines a macro as discussed above.
Special processing occurs on the template part
which will be discussed in a later section.
.s3
{rawdefine; $$t; $$r}
is another form of define,
where the special processing does not take place.
.s3
{remove; $$n}
removes the macro
with name $n.
It can remove more than one macro,
since it actually removes all macros
which might conflict
with $n
under some circumstance.
For example,
typing
.s3
	{define; get part $n; . . . }
.br
	{define; get emp $x; . . . }
.br
	{remove; get}
.s3
would cause both the get macros to be removed.
A call to
.s3
	{remove; get part}
.s3
would have only removed
the first macro.
.s3
{type $$s}
types $s onto the terminal.
.s3
{read $$s}
types $s and then reads a line
from the terminal.
The line which was typed
replaces the macro.
A macro called ``{readcount}''
is defined
containing the number of characters read.
A control-D
(end of file)
becomes \*-1,
a single newline becomes zero,
and so forth.
.s3
{readdefine; $$n; $$s}
also types $s and reads a line,
but puts the line
into a macro named $n.
The replacement text
is the count of the number
of characters
in the line.
{readcount} is still defined.
.s3
{ifsame; $$a; $$b; $t; $f}
compares the strings $a and $b.
If they match exactly
then the replacement text
becomes $t,
otherwise it becomes $f.
.s3
{ifeq; $$a; $$b; $t; $f}
is similar,
but the comparison is numeric.
.s3
{ifgt; $$a; $$b; $t; $f}
is like ifeq,
but the test is for $a
strictly greater than $b.
.s3
{substr; $$f; $$t; $$s}
returns the part of $s
between character positions $f and $t,
numbered from one.
If $f or $t are out of range,
they are moved in range as much as possible.
.s3
{dump; $$n}
returns the value of the macro
(or macros)
which match $n
(using the same algorithm as remove).
The output is a rawdefine
statement
so that it can be read back in.
{dump} without arguments
dumps all macros.
.s1
.bd "Metacharacters"
.s2
Certain characters
are used internally.
Normally you will not even see them,
but they can appear in the output of a dump command,
and can sometimes be used
to create very fancy macros.
.s3
\e\*|\*v matches any number of spaces,
tabs,
or newlines.
It will even match zero,
but only between words,
as can occur with punctuation.
For example,
\e\*|\*v will match the spot
between the last character of a word
and a comma following it.
.s3
\e^ matches exactly one space, tab, or newline.
.s3
\e& matches exactly zero spaces, tabs, or newlines,
but only between words.
.s1
.bd "The Define Process"
.s2
When you define a macro
using 
define,
a lot of special processing happens.
This processing is such that 
define
is not functionally complete,
but still adequate for most requirements.
If more power is needed,
rawdefine can be used;
however,
rawdefine is particularly difficult to use correctly,
and should only be used by gurus.
.s3
In define,
all sequences of spaces, tabs, and newlines
in the template,
as well as all ``non-spaces''
between words,
are turned into a single
\e\*|\*v character.
If the template ends with a parameter,
the \e& character is added at the end.
.s3
If you want to match a real tab or newline,
you can use \et or \en respectively.
For example,
a macro which reads an entire line
and uses it as the name of an employee
would be defined with
.s3
	{define; get $n\en; \e
.br
	    ret (e.all) where e.name = "$n"}
.s3
This macro might be used by typing
.s3
	get *Stan*
.s3
to get all information about everyone
with a name which included ``Stan''.
By the way,
notice that it is ok to nest the ``ret''
macro inside the ``get'' macro.
.s1
.bd "Parameter Prescan"
.s2
Sometimes it is useful to macro process a parameter
before using it in the replacement part.
This is particularly important
when using certain builtin macros.
.s3
For prescan to occur,
two things must be true:
first,
the parameter must be specified in the template
with two dollar signs instead of one,
and second,
the actual parameter must begin with an ``at'' sign
(``@'')
(which is stripped off).
.s3
For an example of the use of prescan,
see ``Special Macros'' below.
.s1
.bd "Special Macros"
.s2
Some special macros are used by the terminal monitor
to control the environment and return results
to the user.
.s3
{begintrap}
is executed at the beginning of a query.
.s3
{endtrap}
is executed after the body of a query
is passed to \*(II.
.s3
{continuetrap}
is executed after the query completes.
The difference between this and endtrap
is that endtrap occurs after the query is submitted,
but before the query executes,
whereas continuetrap is executed after the query executes.
.s3
{editor}
can be defined
to be the pathname of an editor to use
in the \eedit command.
.s3
{shell}
can be defined to be the pathname
of a shell to use in the \eshell command.
.s3
{tuplecount}
is set after every query
(but before continuetrap is sprung)
to be the count of the number of tuples which satisfied
the qualification of the query
in a retrieve,
or the number of tuples changed in an update.
It is not set for DBU functions.
If multiple queries are run at once,
it is set to the number of tuples which satisfied the
last query run.
.s3
For example,
to print out the number of tuples touched
automatically after each query,
you could enter:
.nf
	{define; {begintrap}; {remove; {tuplecount}}}
	{define; {continuetrap}; \e
	   {ifsame; @{tuplecount}; {tuplecount};; \e
	     {type @{tuplecount} tuples touched}}}
.fi
.sh "SEE ALSO"
monitor(quel)
