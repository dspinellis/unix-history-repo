# include	"/usr/sys/param.h"

/*
**  DEMO PROGRAM
**
**	This hunk of code does virtually nothing of use.  Its main
**	purpose is to demonstrate the "official" ingres coding style.
**
**	This demonstrates comments.  There should be a block comment
**	at the beginning of every file and/or procedure to explain
**	the function of the code.  Important information to put here
**	includes the parameters of the routines, any options that the
**	user may specify, etc.
**
**	The first line of the comment should be a one-line description
**	of what's going on.  The remainder should be more detailed.
**	Blank lines should seperate major points in the comments.  In
**	general, ask yourself the question, "If I didn't know what this
**	code was, what it was for, how it fit in, etc., and if I didn't
**	even have the documentation for it, would these comments be
**	enough for me?"
**
**	Some general guidelines for the code:
**
**	- Commas and semicolons should always be followed by a space.
**		Binary operators should be surrounded on both sides by
**		spaces.  Unary operators should be in direct contact
**		with the object that they act on, except for "sizeof",
**		which should be seperated by one space.
**
**	- Two statements should never go on the same line.  This includes
**		such things as an if and the associated conditionally
**		executed statement.
**		In cases such as this, the second half of the if
**		should be indented one tab stop more than the if.  For
**		example, use:
**			if (cond)
**				statement;
**		never:
**			if (cond) statement;
**		or:
**			if (cond)
**			statement;
**
**	- Braces ({}) should (almost) always be on a line by them-
**		selves.  Exceptions are closing a do, and terminating
**		a struct definition or variable initialization.  Braces
**		should start at the same indent as the statement with
**		which they bind, and code inside the braces should be
**		indented one stop further.  For example, use:
**			while (cond)
**			{
**				code
**			}
**		and never:
**			while (cond)
**				{
**				code
**				}
**		or:
**			while (cond) {
**				code
**			}
**		or:
**			while (cond)
**			{
**			code
**			}
**		or anything else in that line.  Braces which match
**		should always be at the same tab stop.
**
**	- Declarations should always have at least one tab between the
**		declaration part and the variable list part, but never
**		any tabs within the declaration part or the variable
**		list part.  For example, in the line:
**			register int	i, j;
**		There is a tab between the "int" and the "i", but a
**		space between "register" and "int", since together
**		these make up the declaration part.
**
**	- There should always be a space following a keyword (i.e.,
**		for, if, do, while, switch, and return), but never 
**		between a function and the paren preceeding its
**		arguments.  For example, use:
**			if (i == 0)
**				exit();
**		never:
**			if(i == 0)
**				exit ();
**
**	- Every case in a switch statement (including default) should
**		be preceeded by a blank line.  The actual word "case" or
**		"default" should have the indent of the switch statement plus
**		two spaces.  It should be followed by a space (not a
**		tab) and the case constant.  Multiple case labels on
**		a single block of code should be on seperate lines, but
**		they should not be seperated by blank lines.  The
**		switch statement should in general be used in place of
**		such constructs as:
**			if (i == 1)
**				code1;
**			else
**				if (i == 34)
**					code2;
**				else
**					if (i == -1643)
**						code3;
**		which can be more succinctly stated as:
**			switch (i)
**			{
**
**			  case 1:
**				code1;
**				break;
**
**			  case 34:
**				code2;
**				break;
**
**			  case -1643:
**				code3;
**				break;
**
**			}
**		In point of fact the equivalent switch will compile
**		extremely efficiently.  (Note that if you had some
**		instance where you could not use a case, e.g., checking
**		for i < 5, else check for j > 3, else whatever, then
**		the above ("if") code is in the correct style.  However,
**		an acceptable alternate structure is to consider "else if"
**		as a primitive.  Hence:
**			if (i < 5)
**				code1;
**			else if (j > 3)
**				code2;
**			else
**				code3;
**		is acceptable.
**
**	- Do statements must always be of the form:
**			do
**			{
**				code;
**			} while (cond);
**		even if "code" is only one line.  This is done so that
**		it is clear that the "while" is with a do, rather than
**		a standalone "while" which is used for the side effects of
**		evaluation of the condition.
**
**	- Defined constants (defined with the # define feature) must
**		be entirely upper case.  The exceptions to this are
**		compilation flags, which begin with a lower case "x",
**		and some sub-types for parser symbols.  In any case,
**		the majority of the symbol is upper case.
**
**	- Global variables should begin with an upper case letter and
**		be otherwise all lower case.  Local symbols should be
**		entirely lower case.  Procedure names are all lower
**		case.  The only exception to this is the trace routine
**		"tTf".  You should avoid user non-local symbols (globals
**		or # define'd symbols) which are one character only;
**		it is impossible to distinguish them.
**
**	- # defines and # includes should have a space after the sharp
**		sign and be followed by a tab.  In general, try to make
**		things line up.  Use:
**			# define	ARPA		25
**			# define	MAXFIELDS	18
**		and not:
**			#define ARPA 25
**			#define MAXFIELDS 18
**		Conditional compilation statements should have as many
**		tabs as are necessary to bring the "ifdef",
**		"ifndef", or "endif" to the tab stop of the surrounding
**		code.  The keyword ("ifdef" or "ifndef") should be
**		followed by a space and the conditional compilation
**		variable.  Conditional compilation should be used
**		around all trace information, timing code, and code
**		which may vary from version to version of UNIX.  See
**		the code below for an example of conditional compila-
**		tion use.
**
**	- A blank line should seperate the declarations and the code
**		in a procedure.  Blank lines should also be used freely
**		between major subsections of your code.  The major
**		subsections should also have a comment giving some idea
**		of what is about to occur.
**
**	- Use descriptive variable names, particularly for global var-
**		iables.  "IEH3462" tells me nothing; nor does "R".  On
**		the other hand, "Resultid" tells me quite a lot,
**		including what it might be, where I might go to see
**		how it is initialized, etc.  Try not to use variables
**		for multiple purposes.
**
**	- It is quite possible to name a file "printr.c" and then
**		put the code for "destroydb" in it.  Try to arrange
**		the names of your files so that given the name of a
**		routine, it is fairly easy to figure out which file
**		it is in.
**
**	- Sometimes it is really pretty much impossible to avoid doing
**		something which is not immediately obvious.  In these
**		cases, put in a comment telling what you are doing and
**		why you are doing it.
**
**	- Try to write things that are clear, rather than things which
**		you think are easier to compile.  I mean, who really
**		cares?  For example, always declare temporary buffers
**		as local, rather than as global.  This way you can
**		guarantee that you will never clobber the buffer in
**		another routine accidently when it still had useful
**		info in it.
**
**	Remember, it is easy to write incomprehensible code in
**	C.  If you really get off on doing this, however, go get
**	a job programming in APL.
**
**	For efficiency reasons, you should always use register variables
**	when possible.  A simple and extremely effective tip is to define
**	a register variable, and assign an oft-used parameter to it,
**	since it is particularly inefficient to reference a parameter.
**	Another particularly inefficient operation is referencing arrays
**	of structures.  When possible, define a register pointer to the
**	structure, and then say:
**		struct xyz		structure[MAX];
**		register struct xyz	*p;
**		...
**		for (i = 0; i < MAX; i++)
**		{
**			p = &structure[i];
**			p->x = p->y + p->z;
**			(diddle with p->???)
**		}
**	and not:
**		struct xyz		structure[MAX];
**		...
**		for (i = 0; i < MAX; i++)
**		{
**			Structure[i].x = Structure[i].y + Structure[i].z;
**			(diddle with Structure[i].???)
**		}
**	Remember, the nice things about register variables is that they
**	make your code smaller and they run faster.  It is hard to
**	lose with registers.  There are three restrictions which you
**	should be aware of on register variables, however.  First,
**	The only types which may be registers are int's, char's,
**	and pointers.  Second, there may only be three register
**	variables per subroutine.  Third, you may not take the address
**	of a register variable (i.e., you may not say "&i" if i is
**	typed as a register variable).
*/


# define	XEQ1		5

struct magic
{
	char	*name;		/* name of symbol */
	int	type;		/* type of symbol, defined in symbol.h */
	int	value;		/* optional value.  This is actually
				 * the value if it is type "integer",
				 * a pointer to the value if it is a
				 * string. */
};

struct magic	Stuff;

main(argc, argv)
int	argc;
char	*argv[];
{
	register struct magic	*r;
	register int		i;
	register int		j;
	int			timebuf[2];
	int			status;

	/* Note that in the declarations of argc and argv above, all
	 * parameters to any function should be declared, even if they
	 * are of type int (which is the default). */

	r = &Stuff;
	/* initialize random # generator */
	time(timebuf);
	srand(timebuf[1]);

	/* scan Stuff structure */
	for (i = 0; i < XEQ1; i++)
	{
#		ifdef xTRACE
		if (tTf(5, 13))
			printf("switch on type %d\n", r->reltype);
#		endif
		switch (r->type)
		{

		  case 0:
		  case 1:
		  case 3:
			/* initialize */
			printf("hi\n");
			break;

		  case 2:
			/* end of query */
			printf("bye\n");
			break;

		  default:
			/* be sure to print plenty of info on an error;
			 * "syserr("bad reltype");" would not have been
			 * sufficient */
			syserr("bad type %d", r->type);

		}
	}

	/* resist the temptation to say "} else {" */
	if (i == 5)
	{
		i++;
		j = 4;
	}
	else
		i--;

	/* plot the results */
	do
	{
		i = rand() & 017;
		while (i--)
		{
			printf("*");
		}
		printf("\n");
	} while (j--);

	/* wait for child processes to complete */
	wait(&status);
	/* end of run, print termination message and exit */
	for (i = 0; i < 2; i++)
		printf("bye ");
	printf("\n");
}
