/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lex.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * lex.c
 *
 * Lexical scanner routines for the f77 compiler, pass 1, 4.2 BSD.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	lex.c,v $
 * Revision 1.2  84/10/27  02:20:09  donn
 * Fixed bug where the input file and the name field of the include file
 * structure shared -- when the input file name was freed, the include file
 * name got stomped on, leading to peculiar error messages.
 * 
 */

#include "defs.h"
#include "tokdefs.h"
#include "pathnames.h"

# define BLANK	' '
# define MYQUOTE (2)
# define SEOF 0

/* card types */

# define STEOF 1
# define STINITIAL 2
# define STCONTINUE 3

/* lex states */

#define NEWSTMT	1
#define FIRSTTOKEN	2
#define OTHERTOKEN	3
#define RETEOS	4


LOCAL int stkey;
LOCAL int lastend = 1;
ftnint yystno;
flag intonly;
LOCAL long int stno;
LOCAL long int nxtstno;
LOCAL int parlev;
LOCAL int expcom;
LOCAL int expeql;
LOCAL char *nextch;
LOCAL char *lastch;
LOCAL char *nextcd 	= NULL;
LOCAL char *endcd;
LOCAL int prevlin;
LOCAL int thislin;
LOCAL int code;
LOCAL int lexstate	= NEWSTMT;
LOCAL char s[1390];
LOCAL char *send	= s+20*66;
LOCAL int nincl	= 0;
LOCAL char *newname = NULL;

struct Inclfile
	{
	struct Inclfile *inclnext;
	FILEP inclfp;
	char *inclname;
	int incllno;
	char *incllinp;
	int incllen;
	int inclcode;
	ftnint inclstno;
	} ;

LOCAL struct Inclfile *inclp	=  NULL;
LOCAL struct Keylist { char *keyname; int keyval; char notinf66; } ;
LOCAL struct Punctlist { char punchar; int punval; };
LOCAL struct Fmtlist { char fmtchar; int fmtval; };
LOCAL struct Dotlist { char *dotname; int dotval; };
LOCAL struct Keylist *keystart[26], *keyend[26];




inilex(name)
char *name;
{
nincl = 0;
inclp = NULL;
doinclude(name);
lexstate = NEWSTMT;
return(NO);
}



/* throw away the rest of the current line */
flline()
{
lexstate = RETEOS;
}



char *lexline(n)
int *n;
{
*n = (lastch - nextch) + 1;
return(nextch);
}





doinclude(name)
char *name;
{
FILEP fp;
struct Inclfile *t;
char temp[100];
register char *lastslash, *s;

if(inclp)
	{
	inclp->incllno = thislin;
	inclp->inclcode = code;
	inclp->inclstno = nxtstno;
	if(nextcd)
		inclp->incllinp = copyn(inclp->incllen = endcd-nextcd , nextcd);
	else
		inclp->incllinp = 0;
	}
nextcd = NULL;

if(++nincl >= MAXINCLUDES)
	fatal("includes nested too deep");
if(name[0] == '\0')
	fp = stdin;
else if(name[0]=='/' || inclp==NULL)
	fp = fopen(name, "r");
else	{
	lastslash = NULL;
	for(s = inclp->inclname ; *s ; ++s)
		if(*s == '/')
			lastslash = s;
	if(lastslash)
		{
		*lastslash = '\0';
		sprintf(temp, "%s/%s", inclp->inclname, name);
		*lastslash = '/';
		}
	else
		strcpy(temp, name);

	if( (fp = fopen(temp, "r")) == NULL )
		{
		sprintf(temp, "%s/%s", _PATH_INCLUDES, name);
		fp = fopen(temp, "r");
		}
	if(fp)
		name = copys(temp);
	}

if( fp )
	{
	t = inclp;
	inclp = ALLOC(Inclfile);
	inclp->inclnext = t;
	prevlin = thislin = 0;
	inclp->inclname = name;
	infname = copys(name);
	infile = inclp->inclfp = fp;
	}
else
	{
	fprintf(diagfile, "Cannot open file %s", name);
	done(1);
	}
}




LOCAL popinclude()
{
struct Inclfile *t;
register char *p;
register int k;

if(infile != stdin)
	clf(&infile);
free(infname);

--nincl;
t = inclp->inclnext;
free(inclp->inclname);
free( (charptr) inclp);
inclp = t;
if(inclp == NULL)
	return(NO);

infile = inclp->inclfp;
infname = copys(inclp->inclname);
prevlin = thislin = inclp->incllno;
code = inclp->inclcode;
stno = nxtstno = inclp->inclstno;
if(inclp->incllinp)
	{
	endcd = nextcd = s;
	k = inclp->incllen;
	p = inclp->incllinp;
	while(--k >= 0)
		*endcd++ = *p++;
	free( (charptr) (inclp->incllinp) );
	}
else
	nextcd = NULL;
return(YES);
}




yylex()
{
static int  tokno;

	switch(lexstate)
	{
case NEWSTMT :	/* need a new statement */
	if(getcds() == STEOF)
		return(SEOF);
	lastend =  stkey == SEND;
	crunch();
	tokno = 0;
	lexstate = FIRSTTOKEN;
	yystno = stno;
	stno = nxtstno;
	toklen = 0;
	return(SLABEL);

first:
case FIRSTTOKEN :	/* first step on a statement */
	analyz();
	lexstate = OTHERTOKEN;
	tokno = 1;
	return(stkey);

case OTHERTOKEN :	/* return next token */
	if(nextch > lastch)
		goto reteos;
	++tokno;
	if( (stkey==SLOGIF || stkey==SELSEIF) && parlev==0 && tokno>3)
		goto first;

	if(stkey==SASSIGN && tokno==3 && nextch<lastch &&
		nextch[0]=='t' && nextch[1]=='o')
			{
			nextch+=2;
			return(STO);
			}
	return(gettok());

reteos:
case RETEOS:
	lexstate = NEWSTMT;
	return(SEOS);
	}
fatali("impossible lexstate %d", lexstate);
/* NOTREACHED */
}

LOCAL getcds()
{
register char *p, *q;

	if (newname)
		{
		free(infname);
		infname = newname;
		newname = NULL;
		}

top:
	if(nextcd == NULL)
		{
		code = getcd( nextcd = s );
		stno = nxtstno;
		if (newname)
			{
			free(infname);
			infname = newname;
			newname = NULL;
			}
		prevlin = thislin;
		}
	if(code == STEOF)
		if( popinclude() )
			goto top;
		else
			return(STEOF);

	if(code == STCONTINUE)
		{
		if (newname)
			{
			free(infname);
			infname = newname;
			newname = NULL;
			}
		lineno = thislin;
		err("illegal continuation card ignored");
		nextcd = NULL;
		goto top;
		}

	if(nextcd > s)
		{
		q = nextcd;
		p = s;
		while(q < endcd)
			*p++ = *q++;
		endcd = p;
		}
	for(nextcd = endcd ;
		nextcd+66<=send && (code = getcd(nextcd))==STCONTINUE ;
		nextcd = endcd )
			;
	nextch = s;
	lastch = nextcd - 1;
	if(nextcd >= send)
		nextcd = NULL;
	lineno = prevlin;
	prevlin = thislin;
	return(STINITIAL);
}

LOCAL getcd(b)
register char *b;
{
register int c;
register char *p, *bend;
int speclin;
static char a[6];
static char *aend	= a+6;
int num;

top:
	endcd = b;
	bend = b+66;
	speclin = NO;

	if( (c = getc(infile)) == '&')
		{
		a[0] = BLANK;
		a[5] = 'x';
		speclin = YES;
		bend = send;
		}
	else if(c=='c' || c=='C' || c=='*')
		{
		while( (c = getc(infile)) != '\n')
			if(c == EOF)
				return(STEOF);
		++thislin;
		goto top;
		}
	else if(c == '#')
		{
		c = getc(infile);
		while (c == BLANK || c == '\t')
			c = getc(infile);

		num = 0;
		while (isdigit(c))
			{
			num = 10*num + c - '0';
			c = getc(infile);
			}
		thislin = num - 1;

		while (c == BLANK || c == '\t')
			c = getc(infile);

		if (c == '"')
			{
			char fname[1024];
			int len = 0;

			c = getc(infile);
			while (c != '"' && c != '\n')
				{
				fname[len++] = c;
				c = getc(infile);
				}
			fname[len++] = '\0';

			if (newname)
				free(newname);
			newname = (char *) ckalloc(len);
			strcpy(newname, fname);
			}

		while (c != '\n')
			if (c == EOF)
				return (STEOF);
			else
				c = getc(infile);
		goto top;
		}

	else if(c != EOF)
		{
		/* a tab in columns 1-6 skips to column 7 */
		ungetc(c, infile);
		for(p=a; p<aend && (c=getc(infile)) != '\n' && c!=EOF; )
			if(c == '\t')
				{
				while(p < aend)
					*p++ = BLANK;
				speclin = YES;
				bend = send;
				}
			else
				*p++ = c;
		}
	if(c == EOF)
		return(STEOF);
	if(c == '\n')
		{
		while(p < aend)
			*p++ = BLANK;
		if( ! speclin )
			while(endcd < bend)
				*endcd++ = BLANK;
		}
	else	{	/* read body of line */
		while( endcd<bend && (c=getc(infile)) != '\n' && c!=EOF )
			*endcd++ = c;
		if(c == EOF)
			return(STEOF);
		if(c != '\n')
			{
			while( (c=getc(infile)) != '\n')
				if(c == EOF)
					return(STEOF);
			}

		if( ! speclin )
			while(endcd < bend)
				*endcd++ = BLANK;
		}
	++thislin;
	if( !isspace(a[5]) && a[5]!='0')
		return(STCONTINUE);
	for(p=a; p<aend; ++p)
		if( !isspace(*p) ) goto initline;
	for(p = b ; p<endcd ; ++p)
		if( !isspace(*p) ) goto initline;
	goto top;

initline:
	nxtstno = 0;
	for(p = a ; p<a+5 ; ++p)
		if( !isspace(*p) )
			if(isdigit(*p))
				nxtstno = 10*nxtstno + (*p - '0');
			else	{
				if (newname)
					{
					free(infname);
					infname = newname;
					newname = NULL;
					}
				lineno = thislin;
				err("nondigit in statement number field");
				nxtstno = 0;
				break;
				}
	return(STINITIAL);
}

LOCAL crunch()
{
register char *i, *j, *j0, *j1, *prvstr;
int ten, nh, quote;

/* i is the next input character to be looked at
j is the next output character */
parlev = 0;
expcom = 0;	/* exposed ','s */
expeql = 0;	/* exposed equal signs */
j = s;
prvstr = s;
for(i=s ; i<=lastch ; ++i)
	{
	if(isspace(*i) )
		continue;
	if(*i=='\'' ||  *i=='"')
		{
		quote = *i;
		*j = MYQUOTE; /* special marker */
		for(;;)
			{
			if(++i > lastch)
				{
				err("unbalanced quotes; closing quote supplied");
				break;
				}
			if(*i == quote)
				if(i<lastch && i[1]==quote) ++i;
				else break;
			else if(*i=='\\' && i<lastch)
				switch(*++i)
					{
					case 't':
						*i = '\t'; break;
					case 'b':
						*i = '\b'; break;
					case 'n':
						*i = '\n'; break;
					case 'f':
						*i = '\f'; break;
					case 'v':
						*i = '\v'; break;
					case '0':
						*i = '\0'; break;
					default:
						break;
					}
			*++j = *i;
			}
		j[1] = MYQUOTE;
		j += 2;
		prvstr = j;
		}
	else if( (*i=='h' || *i=='H')  && j>prvstr)	/* test for Hollerith strings */
		{
		if( ! isdigit(j[-1])) goto copychar;
		nh = j[-1] - '0';
		ten = 10;
		j1 = prvstr - 1;
		if (j1<j-5) j1=j-5;
		for(j0=j-2 ; j0>j1; -- j0)
			{
			if( ! isdigit(*j0 ) ) break;
			nh += ten * (*j0-'0');
			ten*=10;
			}
		if(j0 <= j1) goto copychar;
/* a hollerith must be preceded by a punctuation mark.
   '*' is possible only as repetition factor in a data statement
   not, in particular, in character*2h
*/

		if( !(*j0=='*'&&s[0]=='d') && *j0!='/' && *j0!='(' &&
			*j0!=',' && *j0!='=' && *j0!='.')
				goto copychar;
		if(i+nh > lastch)
			{
			erri("%dH too big", nh);
			nh = lastch - i;
			}
		j0[1] = MYQUOTE; /* special marker */
		j = j0 + 1;
		while(nh-- > 0)
			{
			if(*++i == '\\')
				switch(*++i)
					{
					case 't':
						*i = '\t'; break;
					case 'b':
						*i = '\b'; break;
					case 'n':
						*i = '\n'; break;
					case 'f':
						*i = '\f'; break;
					case '0':
						*i = '\0'; break;
					default:
						break;
					}
			*++j = *i;
			}
		j[1] = MYQUOTE;
		j+=2;
		prvstr = j;
		}
	else	{
		if(*i == '(') ++parlev;
		else if(*i == ')') --parlev;
		else if(parlev == 0)
			if(*i == '=') expeql = 1;
			else if(*i == ',') expcom = 1;
copychar:		/*not a string or space -- copy, shifting case if necessary */
		if(shiftcase && isupper(*i))
			*j++ = tolower(*i);
		else	*j++ = *i;
		}
	}
lastch = j - 1;
nextch = s;
}

LOCAL analyz()
{
register char *i;

	if(parlev != 0)
		{
		err("unbalanced parentheses, statement skipped");
		stkey = SUNKNOWN;
		return;
		}
	if(nextch+2<=lastch && nextch[0]=='i' && nextch[1]=='f' && nextch[2]=='(')
		{
/* assignment or if statement -- look at character after balancing paren */
		parlev = 1;
		for(i=nextch+3 ; i<=lastch; ++i)
			if(*i == (MYQUOTE))
				{
				while(*++i != MYQUOTE)
					;
				}
			else if(*i == '(')
				++parlev;
			else if(*i == ')')
				{
				if(--parlev == 0)
					break;
				}
		if(i >= lastch)
			stkey = SLOGIF;
		else if(i[1] == '=')
			stkey = SLET;
		else if( isdigit(i[1]) )
			stkey = SARITHIF;
		else	stkey = SLOGIF;
		if(stkey != SLET)
			nextch += 2;
		}
	else if(expeql) /* may be an assignment */
		{
		if(expcom && nextch<lastch &&
			nextch[0]=='d' && nextch[1]=='o')
				{
				stkey = SDO;
				nextch += 2;
				}
		else	stkey = SLET;
		}
/* otherwise search for keyword */
	else	{
		stkey = getkwd();
		if(stkey==SGOTO && lastch>=nextch)
			if(nextch[0]=='(')
				stkey = SCOMPGOTO;
			else if(isalpha(nextch[0]))
				stkey = SASGOTO;
		}
	parlev = 0;
}



LOCAL getkwd()
{
register char *i, *j;
register struct Keylist *pk, *pend;
int k;

if(! isalpha(nextch[0]) )
	return(SUNKNOWN);
k = nextch[0] - 'a';
if(pk = keystart[k])
	for(pend = keyend[k] ; pk<=pend ; ++pk )
		{
		i = pk->keyname;
		j = nextch;
		while(*++i==*++j && *i!='\0')
			;
		if(*i=='\0' && j<=lastch+1)
			{
			nextch = j;
			if(no66flag && pk->notinf66)
				errstr("Not a Fortran 66 keyword: %s",
					pk->keyname);
			return(pk->keyval);
			}
		}
return(SUNKNOWN);
}



initkey()
{
extern struct Keylist keys[];
register struct Keylist *p;
register int i,j;

for(i = 0 ; i<26 ; ++i)
	keystart[i] = NULL;

for(p = keys ; p->keyname ; ++p)
	{
	j = p->keyname[0] - 'a';
	if(keystart[j] == NULL)
		keystart[j] = p;
	keyend[j] = p;
	}
}

LOCAL gettok()
{
int havdot, havexp, havdbl;
int radix, val;
extern struct Punctlist puncts[];
struct Punctlist *pp;
extern struct Fmtlist fmts[];
extern struct Dotlist dots[];
struct Dotlist *pd;

char *i, *j, *n1, *p;

	if(*nextch == (MYQUOTE))
		{
		++nextch;
		p = token;
		while(*nextch != MYQUOTE)
			*p++ = *nextch++;
		++nextch;
		toklen = p - token;
		*p = '\0';
		return (SHOLLERITH);
		}
/*
	if(stkey == SFORMAT)
		{
		for(pf = fmts; pf->fmtchar; ++pf)
			{
			if(*nextch == pf->fmtchar)
				{
				++nextch;
				if(pf->fmtval == SLPAR)
					++parlev;
				else if(pf->fmtval == SRPAR)
					--parlev;
				return(pf->fmtval);
				}
			}
		if( isdigit(*nextch) )
			{
			p = token;
			*p++ = *nextch++;
			while(nextch<=lastch && isdigit(*nextch) )
				*p++ = *nextch++;
			toklen = p - token;
			*p = '\0';
			if(nextch<=lastch && *nextch=='p')
				{
				++nextch;
				return(SSCALE);
				}
			else	return(SICON);
			}
		if( isalpha(*nextch) )
			{
			p = token;
			*p++ = *nextch++;
			while(nextch<=lastch &&
				(*nextch=='.' || isdigit(*nextch) || isalpha(*nextch) ))
					*p++ = *nextch++;
			toklen = p - token;
			*p = '\0';
			return(SFIELD);
			}
		goto badchar;
		}
/* Not a format statement */

if(needkwd)
	{
	needkwd = 0;
	return( getkwd() );
	}

	for(pp=puncts; pp->punchar; ++pp)
		if(*nextch == pp->punchar)
			{
			if( (*nextch=='*' || *nextch=='/') &&
				nextch<lastch && nextch[1]==nextch[0])
					{
					if(*nextch == '*')
						val = SPOWER;
					else	val = SCONCAT;
					nextch+=2;
					}
			else	{
				val = pp->punval;
				if(val==SLPAR)
					++parlev;
				else if(val==SRPAR)
					--parlev;
				++nextch;
				}
			return(val);
			}
	if(*nextch == '.')
		if(nextch >= lastch) goto badchar;
		else if(isdigit(nextch[1])) goto numconst;
		else	{
			for(pd=dots ; (j=pd->dotname) ; ++pd)
				{
				for(i=nextch+1 ; i<=lastch ; ++i)
					if(*i != *j) break;
					else if(*i != '.') ++j;
					else	{
						nextch = i+1;
						return(pd->dotval);
						}
				}
			goto badchar;
			}
	if( isalpha(*nextch) )
		{
		p = token;
		*p++ = *nextch++;
		while(nextch<=lastch)
			if( isalpha(*nextch) || isdigit(*nextch) )
				*p++ = *nextch++;
			else break;
		toklen = p - token;
		*p = '\0';
		if(inioctl && nextch<=lastch && *nextch=='=')
			{
			++nextch;
			return(SNAMEEQ);
			}
		if(toklen>8 && eqn(8,token,"function") && isalpha(token[8]) &&
			nextch<lastch && nextch[0]=='(' &&
			(nextch[1]==')' | isalpha(nextch[1])) )
				{
				nextch -= (toklen - 8);
				return(SFUNCTION);
				}
		if(toklen > VL)
			{
			char buff[30];
			sprintf(buff, "name %s too long, truncated to %d",
				token, VL);
			err(buff);
			toklen = VL;
			token[VL] = '\0';
			}
		if(toklen==1 && *nextch==MYQUOTE)
			{
			switch(token[0])
				{
				case 'z':  case 'Z':
				case 'x':  case 'X':
					radix = 16; break;
				case 'o':  case 'O':
					radix = 8; break;
				case 'b':  case 'B':
					radix = 2; break;
				default:
					err("bad bit identifier");
					return(SNAME);
				}
			++nextch;
			for(p = token ; *nextch!=MYQUOTE ; )
				if ( *nextch == BLANK || *nextch == '\t')
					nextch++;
				else
					{
					if (isupper(*nextch))
						*nextch = tolower(*nextch);
					if (hextoi(*p++ = *nextch++) >= radix)
						{
						err("invalid binary character");
						break;
						}
					}
			++nextch;
			toklen = p - token;
			return( radix==16 ? SHEXCON :
				(radix==8 ? SOCTCON : SBITCON) );
			}
		return(SNAME);
		}
	if( ! isdigit(*nextch) ) goto badchar;
numconst:
	havdot = NO;
	havexp = NO;
	havdbl = NO;
	for(n1 = nextch ; nextch<=lastch ; ++nextch)
		{
		if(*nextch == '.')
			if(havdot) break;
			else if(nextch+2<=lastch && isalpha(nextch[1])
				&& isalpha(nextch[2]))
					break;
			else	havdot = YES;
		else if( !intonly && (*nextch=='d' || *nextch=='e') )
			{
			p = nextch;
			havexp = YES;
			if(*nextch == 'd')
				havdbl = YES;
			if(nextch<lastch)
				if(nextch[1]=='+' || nextch[1]=='-')
					++nextch;
			if( (nextch >= lastch) || ! isdigit(*++nextch) )
				{
				nextch = p;
				havdbl = havexp = NO;
				break;
				}
			for(++nextch ;
				nextch<=lastch && isdigit(*nextch);
				++nextch);
			break;
			}
		else if( ! isdigit(*nextch) )
			break;
		}
	p = token;
	i = n1;
	while(i < nextch)
		*p++ = *i++;
	toklen = p - token;
	*p = '\0';
	if(havdbl) return(SDCON);
	if(havdot || havexp) return(SRCON);
	return(SICON);
badchar:
	s[0] = *nextch++;
	return(SUNKNOWN);
}

/* KEYWORD AND SPECIAL CHARACTER TABLES
*/

struct Punctlist puncts[ ] =
	{
	'(', SLPAR,
	')', SRPAR,
	'=', SEQUALS,
	',', SCOMMA,
	'+', SPLUS,
	'-', SMINUS,
	'*', SSTAR,
	'/', SSLASH,
	'$', SCURRENCY,
	':', SCOLON,
	0, 0 } ;

/*
LOCAL struct Fmtlist  fmts[ ] =
	{
	'(', SLPAR,
	')', SRPAR,
	'/', SSLASH,
	',', SCOMMA,
	'-', SMINUS,
	':', SCOLON,
	0, 0 } ;
*/

LOCAL struct Dotlist  dots[ ] =
	{
	"and.", SAND, 
	"or.", SOR, 
	"not.", SNOT, 
	"true.", STRUE, 
	"false.", SFALSE, 
	"eq.", SEQ, 
	"ne.", SNE, 
	"lt.", SLT, 
	"le.", SLE, 
	"gt.", SGT, 
	"ge.", SGE, 
	"neqv.", SNEQV, 
	"eqv.", SEQV, 
	0, 0 } ;

LOCAL struct Keylist  keys[ ] =
	{
	 	{ "assign",  SASSIGN  },
	 	{ "automatic",  SAUTOMATIC, YES  },
	 	{ "backspace",  SBACKSPACE  },
	 	{ "blockdata",  SBLOCK  },
	 	{ "call",  SCALL  },
	 	{ "character",  SCHARACTER, YES  },
	 	{ "close",  SCLOSE, YES  },
	 	{ "common",  SCOMMON  },
	 	{ "complex",  SCOMPLEX  },
	 	{ "continue",  SCONTINUE  },
	 	{ "data",  SDATA  },
	 	{ "dimension",  SDIMENSION  },
	 	{ "doubleprecision",  SDOUBLE  },
	 	{ "doublecomplex", SDCOMPLEX, YES  },
	 	{ "elseif",  SELSEIF, YES  },
	 	{ "else",  SELSE, YES  },
	 	{ "endfile",  SENDFILE  },
	 	{ "endif",  SENDIF, YES  },
	 	{ "end",  SEND  },
	 	{ "entry",  SENTRY, YES  },
	 	{ "equivalence",  SEQUIV  },
	 	{ "external",  SEXTERNAL  },
	 	{ "format",  SFORMAT  },
	 	{ "function",  SFUNCTION  },
	 	{ "goto",  SGOTO  },
	 	{ "implicit",  SIMPLICIT, YES  },
	 	{ "include",  SINCLUDE, YES  },
	 	{ "inquire",  SINQUIRE, YES  },
	 	{ "intrinsic",  SINTRINSIC, YES  },
	 	{ "integer",  SINTEGER  },
	 	{ "logical",  SLOGICAL  },
#ifdef NAMELIST
		{ "namelist", SNAMELIST, YES },
#endif
		{ "none", SUNDEFINED, YES },
	 	{ "open",  SOPEN, YES  },
	 	{ "parameter",  SPARAM, YES  },
	 	{ "pause",  SPAUSE  },
	 	{ "print",  SPRINT  },
	 	{ "program",  SPROGRAM, YES  },
	 	{ "punch",  SPUNCH, YES  },
	 	{ "read",  SREAD  },
	 	{ "real",  SREAL  },
	 	{ "return",  SRETURN  },
	 	{ "rewind",  SREWIND  },
	 	{ "save",  SSAVE, YES  },
	 	{ "static",  SSTATIC, YES  },
	 	{ "stop",  SSTOP  },
	 	{ "subroutine",  SSUBROUTINE  },
	 	{ "then",  STHEN, YES  },
	 	{ "undefined", SUNDEFINED, YES  },
	 	{ "write",  SWRITE  },
			{ 0, 0 }
	};
