/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)arith.c	1.1 */

/*
 *   ARITH.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   AEVAL (STRING)
 *
 *        Evaluate STRING as an arithmetic expression (possibly
 *        containing variables from trees in NAMEP) and return its value.
 *
 *   LOOKUP (NAME)
 *
 *        Return a pointer to the shell-owned Namnod in TREE
 *        whose namid is NAME.  If TYPE is non-zero, a new Namnod
 *        with the given namid will be inserted, when none is found.
 *
 *   These functions are indirectly mutually recursive.
 *
 *
 *
 *   See Also:  LET(I), findnod(III)
 */

#ifdef KSHELL
#include	"shtype.h"
#else
#include	<ctype.h>
#endif	/* KSHELL */
#include	<stdio.h>
#include	"name.h"
#include        "flags.h"

#define getchr()	(*(unsigned char*)strg++)
#define seekto(loc)	(strg=(loc))
#define ungetc()	(--strg)
#define peekc()		(*strg)
#define MAXLOOP	10

extern union Namval *aget_up();
extern char	*bracket_match();
extern char	*valup();
extern void	failed();
extern struct Namnod *findnod();
#ifdef  NAME_SCOPE
extern struct Namnod *copy_nod();
#endif
struct Namnod *lookup();

static long arith();
static void aerror();

static char *strg	=	0;
static int level	= 0;

/*
 *   AEVAL (STRING)
 *
 *        char *STRING;
 *
 *
 *   Evaluate string as an arithmetic expression (possibly
 *   containing variables from trees in NAMEP) and return its
 *   value as a long int.  STRING can be anything acceptable to
 *   the LET(I) builtin of the shell.
 *
 */

long aeval(string)
char *string;
{
	long r;
	long arith();
	char *ostr;
	ostr = strg;
	if(level++ > MAXLOOP)
		aerror(string,badnum);
	strg = string;
	r = arith(0);
	strg = ostr;
	level--;
	return(r);
}

/*   
 *   ARITH (PREC)
 *
 *        int PREC;
 *
 *   Evaluate the expression given in the global pointer strg
 *   as an arithmetic expression, to PREC digits.  The form
 *   of strg is as is given for the LET builtin.
 */

static long int arith(prec)
{
	register int c;
#ifdef pdp11
	long r;
	long rr;
#else
	register long r;
	register long rr;
#endif
	int base;
	char *ostr;
	char dot = 0;

 /* ignore whitespace */
	while((c=getchr()),isspace(c));
	if(c==0)
		goto done;
	ostr = (strg-1);
	r = 0;
	if(c == '-')
		r = -arith(8);
	else if(c == '!')
		r = !arith(7);
	else if(isalpha(c))
	{
		int oldc;
		char *varname,*sp;
		varname = ostr;
		for(;isalnum(c);c=getchr());
		if(c == '[')
		{
			seekto(bracket_match(ungetc()));
			c = getchr();
			if(c == 0)
				aerror(varname,subscript);
			c = getchr();
		}
		/* null terminate variable name */
		sp = (strg-1);
		*sp = 0;
		/* skip over whitespace */
		for(oldc = c;isspace(c);c = getchr());
		if(c == '='&& peekc() != '=')
			asslong(lookup(varname),r=arith(2));
		else
		{
			char *str;
			register struct Namnod *np = lookup(varname);
			register union Namval *up;
			ungetc();
                	if (attest (np, INT_GER))
			{
#ifdef NAME_SCOPE
				if (attest (np,C_WRITE))
					np = copy_nod(np,1);
#endif
				up= &np->value.namval;
				if(attest(np,IN_DIR))
					up = up->up;
				if(attest (np, (BLT_NOD)))
					r = (long)((*up->fp->f_vp)());
				else if(up->lp==NULL)
					r = 0;
				else
	                        	r = *up->lp;
			}
		 else
			{
				if((str=valup(np))==0 || *str==0)
					aerror(varname,badnum);
				r = aeval(str);
			}
		}
		*sp = oldc;
	}
	else
	{
		base = 10;
		ungetc();
		lastbase = base;
	}

	while((c=getchr()) && c != ']')
	{
		switch(c)
		{
			case ')':
				if(prec)
					goto done;
				else
					aerror(ostr,synmsg);

			case '(':
				r = arith(1);
				if((c=getchr()) != ')')
					aerror(ostr,synmsg);
				break;

			case '=':	case '!':
				if(prec > 3)
					goto done;
				if(getchr() != '=')
					aerror(ostr,synmsg);
				rr = arith(4);
				if(c == '=')
					r = r == rr;
				else
					r = r != rr;
				break;

			case	'<':	case	'>':
				if(prec > 4)
					goto done;
				if(peekc() == '=')
				{
					getchr();
					rr = arith(5);
					if(c == '<')
						r = r <= rr;
					else
						r = r >= rr;
					break;
				}
				rr = arith(5);
				if(c == '<')
					r = r < rr;
				else
					r = r > rr;
				break;

			case '+':	case '-':
				if(prec > 5)
					goto done;
				rr = arith(6);
				if(c == '+')
					r +=  rr;
				else
					r -= rr;
				break;

			case '*':	case '/':	case '%':
				if(prec > 6)
					goto done;
				rr = arith(7);
				if (c == '*')
					r *= rr;
				else if(rr == 0)
					aerror(ostr,divzero);
				else if (c == '/')
					r /= rr;
				else
					r %= rr;
				break;

			case ' ':	case '\n':	case '\t':
				break;

			case '#':
				lastbase = base = r;
				r = 0;
				break;

			case '.':
				if(dot++==0)
					continue;

			default:
			{
				register int d;
				for(d=0; hdigits[d] && c != hdigits[d];d++);
				d >>= 1;
				if( d < base )
				{
					if(dot==0)
						r = base*r + d;
				}
				else
					aerror(ostr,badnum);
			}
		}
	}
done:
	ungetc();
	return(r);
}

static void aerror(name,msg)
char *name,*msg;
{
	level = 0;
	failed(name,msg);
}

/*
 * lookup name and return Namnod pointer with this name.
 * If none exists, it will be created.
 */

struct Namnod *lookup(name)
char *name;
{
	register struct Namnod *np = NULL;
	register struct Amemory *ap;
	register struct Amemory *app = namep;
	int type = 0;
	while((ap=app) && np==NULL)
	{
		app = app->nexttree;
 		np =findnod(name,ap,type|(app==NULL));
		type = RE_USE;
	}
	return(np);
}

