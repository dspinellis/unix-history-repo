# include	"stdio.h"
# include	"seven/types.h"
# include	"seven/macros.h"
# include	"fatal.h"

SCCSID(@(#)vc.c	4.2);
USXALLOC();

/*
 * The symbol table size is set to a limit of forty keywords per input
 * file.  Should this limit be changed it should also be changed in the
 * Help file.
 */

# define SYMSIZE 40
# define PARMSIZE 10
# define NSLOTS 32

# define USD  1
# define DCL 2
# define ASG 4

# define EQ '='
# define NEQ '!'
# define GT '>'
# define LT '<'
# define DELIM " \t"
# define TRUE 1
# define FALSE 0

char	Ctlchar	':';

struct	symtab	{
	int	usage;
	char	name[PARMSIZE];
	char	*value;
	int	lenval;
};
struct	symtab	Sym[SYMSIZE];

struct {
	char	chr;
};

int	Skiptabs;
int	Repall;

/*
 * Delflag is used to indicate when text is to be skipped.  It is decre-
 * mented whenever an if condition is false, or when an if occurs
 * within a false if/end statement.  It is decremented whenever an end is
 * encountered and the Delflag is greater than zero.  Whenever Delflag
 * is greater than zero text is skipped.
 */

int	Delflag;

/*
 * Ifcount keeps track of the number of ifs and ends.  Each time
 * an if is encountered Ifcount is incremented and each time an end is
 * encountered it is decremented.
 */

int	Ifcount;
int	Lineno;

char	*Repflag;
char	*Linend;
int	Silent;


/*
 * The main program reads a line of text and sends it to be processed
 * if it is a version control statement. If it is a line of text and
 * the Delflag is equal to zero, it is written to the standard output.
 */

main(argc, argv)
int argc;
char *argv[];
{
	register  char *lineptr, *p;
	register int i;
	char line[512];
	extern int Fflags;

	Fflags = FTLCLN | FTLMSG | FTLEXIT;
	setsig();
	for(i = 1; i< argc; i++) {
		p = argv[i];
		if (p[0] == '-')
			switch (p[1]) {
			case 's':
				Silent = 1;
				break;
			case 't':
				Skiptabs = 1;
				break;
			case 'a':
				Repall = 1;
				break;
			case 'c':
				Ctlchar = p[2];
				break;
			}
		else {
			p[size(p) - 1] = '\n';
			asgfunc(p);
		}
	}
	while (fgets(line,sizeof(line),stdin) != NULL) {
		lineptr = line;
		Lineno++;

		if (Repflag != 0) {
			free(Repflag);
			Repflag = 0;
		}

		if (Skiptabs) {
			for (p = lineptr; *p; p++)
				if (*p == '\t')
					break;
			if (*p++ == '\t')
				lineptr = p;
		}

		if (lineptr[0] != Ctlchar) {
			if (lineptr[0] == '\\' && lineptr[1] == Ctlchar)
				for (p = &lineptr[1]; *lineptr++ = *p++; )
					;
			if(Delflag == 0) {
				if (Repall)
					repfunc(line);
				else
					fputs(line,stdout);
			}
			continue;
		}

		lineptr++;

		if (imatch("if ", lineptr))
			iffunc(&lineptr[3]);
		else if (imatch("end", lineptr))
			endfunc();
		else if (Delflag == 0) {
			if (imatch("asg ", lineptr))
				asgfunc(&lineptr[4]);
			else if (imatch("dcl ", lineptr))
				dclfunc(&lineptr[4]);
			else if (imatch("err", lineptr))
				errfunc(&lineptr[3]);
			else if (imatch("msg", lineptr))
				msgfunc(&lineptr[3]);
			else if (lineptr[0] == Ctlchar)
				repfunc(&lineptr[1]);
			else if (imatch("on", lineptr))
				Repall = 1;
			else if (imatch("off", lineptr))
				Repall = 0;
			else if (imatch("ctl ", lineptr))
				Ctlchar = lineptr[4];
			else error("unknown command on line %d (901)",Lineno);
		}
	}
	for(i = 0; Sym[i].usage != 0 && i<SYMSIZE; i++) {
		if ((Sym[i].usage&USD) == 0)
			warn("`%s' never used (902)\n",Sym[i].name);
		if ((Sym[i].usage&DCL) == 0)
			warn("`%s' never declared (903)\n", Sym[i].name);
		if ((Sym[i].usage&ASG) == 0)
			warn("`%s' never assigned a value (920)\n", Sym[i].name);
	}
	if (Ifcount > 0)
		error("`if' with no matching `end' (904)");
	exit(0);
}


/*
 * Asgfunc accepts a pointer to a line picks up a keyword name, an
 * equal sign and a value and calls putin to place it in the symbol table.
 */

asgfunc(aptr)
register char *aptr;
{
	register char *end, *aname;
	char *avalue;

	aptr = replace(aptr);
	NONBLANK(aptr);
	aname = aptr;
	end = Linend;
	aptr = findstr(aptr,"= \t");
	if (*aptr == ' ' || *aptr == '\t') {
		*aptr++ = '\0';
		aptr = findch(aptr,'=');
	}
	if (aptr == end)
		error("syntax on line %d (917)",Lineno);
	*aptr++ = '\0';
	avalue = getid(aptr);
	chksize(aname);
	putin(aname, avalue);
}


/*
 * Dclfunc accepts a pointer to a line and picks up keywords
 * separated by commas.  It calls putin to put each keyword in the
 * symbol table.  It returns when it sees a newline.
 */

dclfunc(dptr)
register char *dptr;
{
	register char *end, *dname;
	int i;

	dptr = replace(dptr);
	end = Linend;
	NONBLANK(dptr);
	while (dptr < end) {
		dname = dptr;
		dptr = findch(dptr,',');
		*dptr++ = '\0';
		chksize(dname);
		if (Sym[i = lookup(dname)].usage&DCL)
			error("`%s' declared twice on line %d (905)", 
				dname, Lineno);
		else
			Sym[i].usage |= DCL;
		NONBLANK(dptr);
	}
}


/*
 * Errfunc calls fatal which stops the process.
 */

errfunc(eptr)
char *eptr;
{
	warn("ERROR:%s\n",replace(eptr));
	error("err statement on line %d (915)", Lineno);
}


/*
 * Endfunc indicates an end has been found by decrementing the if count
 * flag.  If because of a previous if statement, text was being skipped,
 * Delflag is also decremented.
 */

endfunc()
{
	if (--Ifcount < 0)
		error("`end' without matching `if' on line %d (910)",Lineno);
	if (Delflag > 0)
		Delflag--;
	return;
}


/*
 * Msgfunc accepts a pointer to a line and prints that line on the 
 * diagnostic output.
 */

msgfunc(mptr)
char *mptr;
{
	warn("Message(%d):%s\n", Lineno, replace(mptr));
}


repfunc(s)
char *s;
{
	fprintf(stdout,"%s\n",replace(s));
}


/*
 * Iffunc and the three functions following it, door, doand, and exp
 * are responsible for parsing and interperting the condition in the
 * if statement.  The BNF used is as follows:
 *	<iffunc> ::=   [ "not" ] <door> EOL
 *	<door> ::=     <doand> | <doand> "|" <door>
 *	<doand>::=     <exp> | <exp> "&" <doand>
 *	<exp>::=       "(" <door> ")" | <value> <operator> <value>
 *	<operator>::=  "=" | "!=" | "<" | ">"
 * And has precedence over or.  If the condition is false the Delflag
 * is bumped to indicate that lines are to be skipped.
 * An external variable, sptr is used for processing the line in
 * iffunc, door, doand, exp, getid.
 * Iffunc accepts a pointer to a line and sets sptr to that line.  The
 * rest of iffunc, door, and doand follow the BNF exactly.
 */

char *sptr;

iffunc(iptr)
char *iptr;
{
	register int value, not;

	Ifcount++;
	if (Delflag > 0)
		Delflag++;

	else {
		sptr = replace(iptr);
		NONBLANK(sptr);
		if (imatch("not ", sptr)) {
			not = FALSE;
			sptr += 4;
		}
		else not = TRUE;

		value = door();
		if( *sptr != 0)
			error("syntax on line %d (918)",Lineno);

		if (value != not)
			Delflag++;
	}

	return;
}


door()
{
	int value;
	value = doand();
	NONBLANK(sptr);
	while (*sptr=='|') {
		sptr++;
		value |= doand();
		NONBLANK(sptr);
	}
	return(value);
}


doand()
{
	int value;
	value = exp();
	NONBLANK(sptr);
	while (*sptr=='&') {
		sptr++;
		value &= exp();
		NONBLANK(sptr);
	}
	return(value);
}


/*
 * After exp checks for parentheses, it picks up a value by calling getid,
 * picks up an operator and calls getid to pick up the second value.
 * Then based on the operator it calls either numcomp or equal to see
 * if the exp is true or false and returns the correct value.
 */

exp()
{
	register char op, save;
	register int value;
	char *id1, *id2, next;

	NONBLANK(sptr);
	if(*sptr == '(') {
		sptr++;
		value = door();
		NONBLANK(sptr);
		if (*sptr == ')') {
			sptr++;
			return(value);
		}
		else error("parenthesis error on line %d (911)",Lineno);
	}

	id1 = getid(sptr);
	if (op = *sptr)
		*sptr++ = '\0';
	if (op == NEQ && (next = *sptr++) == '\0')
		--sptr;
	id2 = getid(sptr);
	save = *sptr;
	*sptr = '\0';

	if(op ==LT || op == GT) {
		value = numcomp(id1, id2);
		if ((op == GT && value == 1) || (op == LT && value == -1))
			value = TRUE;
		else value = FALSE;
	}

	else if (op==EQ || (op==NEQ && next==EQ)) {
		value = equal(id1, id2);
		if ( op == NEQ)
			value = !value;
	}

	else error("invalid operator on line %d (912)", Lineno);
	*sptr = save;
	return(value);
}


/*
 * Getid picks up a value off a line and returns a pointer to the value.
 */

getid(gptr)
register char *gptr;
{
	register char c, *id;

	NONBLANK(gptr);
	id = gptr;
	gptr = findstr(gptr,DELIM);
	if (*gptr)
		*gptr++ = '\0';
	NONBLANK(gptr);
	sptr = gptr;
	return(id);
}


/*
 * Numcomp accepts two pointers to strings of digits and calls numck
 * to see if the strings contain only digits.  It returns -1 if
 * the first is less than the second, 1 if the first is greater than the
 * second and 0 if the two are equal.
 */

numcomp(id1, id2)
register char *id1, *id2;
{
	int k1, k2;

	numck(id1);
	numck(id2);
	while (*id1 == '0')
		id1++;
	while (*id2 == '0')
		id2++;
	if ((k1 = size(id1)) > (k2 = size(id2)))
		return(1);
	else if (k1 < k2)
		return(-1);
	else while(*id1 != '\0') {
		if(*id1 > *id2)
			return(1);
		else if(*id1 < *id2)
			return(-1);
		id1++;
		id2++;
	}
	return(0);
}


/*
 * Numck accepts a pointer to a string and checks to see if they are
 * all digits.  If they're not it calls fatal, otherwise it returns.
 */

numck(nptr)
register char *nptr;
{
	for (; *nptr != '\0'; nptr++)
		if (!numeric(*nptr))
			error("non-numerical value on line %d (914)",Lineno);
	return;
}


/*
 * Replace accepts a pointer to a line and scans the line for a keyword
 * enclosed in control characters.  If it doesn't find one it returns
 * a pointer to the begining of the line.  Otherwise, it calls
 * lookup to find the keyword.
 * It rewrites the line substituting the value for the
 * keyword enclosed in control characters.  It then continues scanning
 * the line until no control characters are found and returns a pointer to
 * the begining of the new line.
 */

# define INCR(int) if (++int==NSLOTS) error(subrng,Lineno)
char *subrng "out of space [line %d] (916)";

replace(ptr)
char *ptr;
{
	char *slots[NSLOTS];
	int i,j,newlen;
	register char *s, *t, *p;

	for (s=ptr; *s++!='\n';);
	*(--s) = '\0';
	Linend = s;
	i = -1;
	for (p=ptr; *(s=findch(p,Ctlchar)); p=t) {
		*s++ = '\0';
		INCR(i);
		slots[i] = p;
		if (*(t=findch(s,Ctlchar))==0)
			error("unmatched `%c' on line %d (907)",Ctlchar,Lineno);
		*t++ = '\0';
		INCR(i);
		slots[i] = Sym[j = lookup(s)].value;
		Sym[j].usage |= USD;
	}
	INCR(i);
	slots[i] = p;
	if (i==0) return(ptr);
	newlen = 0;
	for (j=0; j<=i; j++)
		newlen += (size(slots[j])-1);
	t = Repflag = alloc(++newlen);
	for (j=0; j<=i; j++)
		t = ecopy(slots[j],t);
	Linend = t;
	return(Repflag);
}


/*
 * Lookup accepts a pointer to a keyword name and searches the symbol
 * table for the keyword.  It returns its index in the table if its there,
 * otherwise it puts the keyword in the table.
 */

lookup(lname)
char *lname;
{
	register int i;
	register char *t;
	register struct symtab *s;

	t = lname;
	while ((i.chr = *t++) &&
		((i.chr>='A' && i.chr<='Z') || (i.chr>='a' && i.chr<='z') ||
			(i.chr!= *lname && i.chr>='0' && i.chr<='9')));
	if (i.chr)
		error("invalid keyword name on line %d (909)",Lineno);

	for(i =0; Sym[i].usage != 0 && i<SYMSIZE; i++)
		if (equal(lname, Sym[i].name)) return(i);
	s = &Sym[i];
	if (s->usage == 0) {
		copy(lname,s->name);
		copy("",(s->value = alloc(s->lenval = 1)));
		return(i);
	}
	error("out of space (906)");
}


/*
 * Putin accepts a pointer to a keyword name, and a pointer to a value.
 * It puts this information in the symbol table by calling lookup.
 * It returns the index of the name in the table.
 */

putin(pname, pvalue)
char *pname;
char *pvalue;
{
	register int i;
	register struct symtab *s;

	s = &Sym[i = lookup(pname)];
	free(s->value);
	s->lenval = size(pvalue);
	copy(pvalue, (s->value = alloc(s->lenval)));
	s->usage |= ASG;
	return(i);
}


chksize(s)
char *s;
{
	if (size(s) > PARMSIZE)
		error("keyword name too long on line %d (908)",Lineno);
}


findch(astr,match)
char *astr, match;
{
	register char *s, *t, c;
	char *temp;

	for (s=astr; (c = *s) && c!=match; s++)
		if (c=='\\') {
			if (s[1]==0)
				error("syntax on line %d (919)",Lineno);
			else {
				for (t = (temp=s) + 1; *s++ = *t++;);
				s = temp;
			}
		}
	return(s);
}


ecopy(s1,s2)
char *s1, *s2;
{
	register char *r1, *r2;

	r1 = s1;
	r2 = s2;
	while (*r2++ = *r1++);
	return(--r2);
}


error(arg)
{
	fatal(sprintf(Error,"%r",&arg));
}


findstr(astr,pat)
char *astr, *pat;
{
	register char *s, *t, c;
	char *temp;

	for (s=astr; (c = *s) && any(c,pat)==0; s++)
		if (c=='\\') {
			if (s[1]==0)
				error("syntax on line %d (919)",Lineno);
			else {
				for (t = (temp=s) + 1; *s++ = *t++;);
				s = temp;
			}
		}
	return(s);
}


warn(arg)
{
	if (!Silent)
		fprintf(stderr,"%r",&arg);
}
