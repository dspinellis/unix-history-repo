/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)gmatch.c	1.1 */

/*
 *  gmatch - match Shell expression patterns
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 *  Derived from Bourne Shell
 */

/*
 * gmatch compares the string s with the shell pattern p.
 * returns 1 for match, 0 otherwise.
 * The ESCAPE character is used to remove special meaning in pattern only.
 */

#define ESCAPE	'\\'

#ifdef MULTIBYTE
# include	"national.h"
# define C_MASK (3<<(7*ESS_MAXCHAR))	/* character classes */
# define peekchar(x)	(_save=x,_c=getchar(x),x=_save,_c)
# define getchar(x)	ja_getchar((unsigned char**)(&(x)))
int ja_getchar();
static char	*_save;
static int	_c;
#else
# define getchar(x)	(*x++)
# define peekchar(x)	(*x)
#endif /* MULTIBYTE */

gmatch(s, p)
#ifdef MULTIBYTE
char *p;
#else
register char *p;
#endif /* MULTIBYTE */
char *s;
{
	register int 	scc,c;
	register int d;
	char *olds;
	while((olds=s,scc = getchar(s)))
	{
		switch(c = getchar(p))
		{
			case '[':
			{
				char ok = 0;
				int lc = -1;
				int notflag=0;
				if(*p == '!' )
				{
					notflag=1;
					p++;
				}
				while(c = getchar(p))
				{
					if(c==']' && lc>=0)
						return(ok?gmatch(s,p):0);
					else if(c=='-' && lc>=0 && *p!=']')
						/*character range */
					{
						c = getchar(p);
#ifdef MULTIBYTE
						/* must be in same char set */
						if((c&C_MASK) != (lc&C_MASK))
						{
							int match;
							match = (scc==c||scc==lc);
					 		if(notflag)
							{
								if(!match)
									ok++;
								else
									return(0);
							}
							else
							{
								if(match)
									ok++;
							}
							lc = c;
						}
#endif /* MULTIBYTE */
						if(notflag)
						{
							if(lc>scc || scc>c)
								ok++;
							else
								return(0);
						}
						else
							if(lc<scc && scc<=c)
								ok++;
					}
					else
					{
						if(c == ESCAPE)
							c = *p++;
				 		if(notflag)
						{
							if(scc!=c)
								ok++;
							else
								return(0);
						}
						else
						{
							if(scc==c)
								ok++;
						}
						lc = c;
					}
				}
				return(0);
			}
			case '\\':
				c = getchar(p);	/* need exact match */
			default:
				if(c != scc)
					return(0);
			case '?':
				break;
			case '*':
			/* several asterisks are the same as one */
				while(*p=='*' )
					p++;
				if(*p==0)
					return(1);
				d = scc;
				c = peekchar(p);
				scc = (c!='?' && c !='[');
				if(c==ESCAPE)
					c = *(p+1);
				while(d)
				{
					if(scc && c != d)
						;
					else if(gmatch(olds,p))
						return(1);
					olds = s;
					d = getchar(s);
					
				}
				return(0);

			case 0:
				return(scc==0);
		}
	}
	while(*p == '*')
		p++;
	return(*p==0);
}

#ifdef MULTIBYTE

/*
 * This character read from one to three bytes and returns a character
 * The character set designation is in the bits defined by C_MASK
 */

int ja_getchar(address)
unsigned char **address;
{
	register unsigned char *cp = *(unsigned char**)address;
	register int c = *cp++;
	register int size;
	int d;
	if(size = echarset(c))
	{
		d = (size==1?c:0);
		c = size;
		size = in_csize(c);
		c <<= 7*(ESS_MAXCHAR-size);
		if(d)
		{
			size--;
			c = (c<<7) | (d&~HIGHBIT);
		}
		while(size-- >0)
			c = (c<<7) | ((*cp++)&~HIGHBIT);
	}
	*address = cp;
	return(c);
}
#endif /*MULTIBYTE*/
