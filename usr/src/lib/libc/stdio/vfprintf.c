From stevesu@copper.UUCP Wed Mar 25 23:35:32 1987
Path: seismo!ut-sally!husc6!bacchus!mit-eddie!genrad!decvax!tektronix!teklds!copper!stevesu
From: stevesu@copper.TEK.COM (Steve Summit)
Newsgroups: net.sources
Subject: Public Domain _doprnt in C
Message-ID: <938@copper.TEK.COM>
Date: 26 Mar 87 04:35:32 GMT
Reply-To: stevesu@copper.UUCP (Steve Summit)
Distribution: world
Organization: Tektronix, Inc., Beaverton, OR.
Lines: 420

Mark Pulver is looking for a C version of _doprnt, so I thought
I'd pass mine along.  I wrote this from the ground up; it is
absolutely underived from anything proprietary.

This version is not complete, and has the following two key
omissions:

	It doesn't do floating point (%f, %e, or %g).

	It will handle %ld (%lx, etc.) incorrectly on machines
	where sizeof(long) != sizeof(int).

It also does not implement the %# stuff which appeared in the 4.2
documentation but which I haven't seen in any implementation yet.

I believe it handles everything else correctly, although I have
not tested it exhaustively.

There are two "fun" additions: %b is binary, and %r is roman.

You are free to use this code as you wish, but please leave the
identification comment intact.  I can offer no support for this
code, although if I ever implement floating point or pdp11
support (I'm acutely embarrassed to admit making the typical VAX
int/long equivalence assumption) I'll try to remember to post
those additions.

                                           Steve Summit
                                           stevesu@copper.tek.com

--------------------- cut here for doprnt.c ---------------------
/*
 *  Common code for printf et al.
 *
 *  The calling routine typically takes a variable number of arguments,
 *  and passes the address of the first one.  This implementation
 *  assumes a straightforward, stack implementation, aligned to the
 *  machine's wordsize.  Increasing addresses are assumed to point to
 *  successive arguments (left-to-right), as is the case for a machine
 *  with a downward-growing stack with arguments pushed right-to-left.
 *
 *  To write, for example, fprintf() using this routine, the code
 *
 *	fprintf(fd, format, args)
 *	FILE *fd;
 *	char *format;
 *	{
 *	_doprnt(format, &args, fd);
 *	}
 *
 *  would suffice.  (This example does not handle the fprintf's "return
 *  value" correctly, but who looks at the return value of fprintf
 *  anyway?)
 *
 *  This version implements the following printf features:
 *
 *	%d	decimal conversion
 *	%u	unsigned conversion
 *	%x	hexadecimal conversion
 *	%X	hexadecimal conversion with capital letters
 *	%o	octal conversion
 *	%c	character
 *	%s	string
 *	%m.n	field width, precision
 *	%-m.n	left adjustment
 *	%0m.n	zero-padding
 *	%*.*	width and precision taken from arguments
 *
 *  This version does not implement %f, %e, or %g.  It accepts, but
 *  ignores, an `l' as in %ld, %lo, %lx, and %lu, and therefore will not
 *  work correctly on machines for which sizeof(long) != sizeof(int).
 *  It does not even parse %D, %O, or %U; you should be using %ld, %o and
 *  %lu if you mean long conversion.
 *
 *  This version implements the following nonstandard features:
 *
 *	%b	binary conversion
 *	%r	roman numeral conversion
 *	%R	roman numeral conversion with capital letters
 *
 *  As mentioned, this version does not return any reasonable value.
 *
 *  Permission is granted to use, modify, or propagate this code as
 *  long as this notice is incorporated.
 *
 *  Steve Summit 3/25/87
 */

#include <stdio.h>

#define TRUE 1
#define FALSE 0

#define ROMAN

#define isdigit(d) ((d) >= '0' && (d) <= '9')
#define Ctod(c) ((c) - '0')

#define MAXBUF (sizeof(long int) * 8)		 /* enough for binary */

#ifdef ROMAN
static tack();
static doit();
#endif

_doprnt(fmt, argp, fd)
register char *fmt;
register int *argp;
FILE *fd;
{
register char *p;
char *p2;
int size;
int length;
int prec;
int ladjust;
char padc;
int n;
unsigned int u;
int base;
char buf[MAXBUF];
int negflag;
char *digs;
#ifdef ROMAN
char *rdigs;
int d;
#endif

while(*fmt != '\0')
	{
	if(*fmt != '%')
		{
		putc(*fmt++, fd);
		continue;
		}

	fmt++;

	if(*fmt == 'l')
		fmt++;	     /* need to use it if sizeof(int) < sizeof(long) */

	length = 0;
	prec = -1;
	ladjust = FALSE;
	padc = ' ';

	if(*fmt == '-')
		{
		ladjust = TRUE;
		fmt++;
		}

	if(*fmt == '0')
		{
		padc = '0';
		fmt++;
		}

	if(isdigit(*fmt))
		{
		while(isdigit(*fmt))
			length = 10 * length + Ctod(*fmt++);
		}
	else if(*fmt == '*')
		{
		length = *argp++;
		fmt++;
		if(length < 0)
			{
			ladjust = !ladjust;
			length = -length;
			}
		}

	if(*fmt == '.')
		{
		fmt++;
		if(isdigit(*fmt))
			{
			prec = 0;
			while(isdigit(*fmt))
				prec = 10 * prec + Ctod(*fmt++);
			}
		else if(*fmt == '*')
			{
			prec = *argp++;
			fmt++;
			}
		}

	negflag = FALSE;
	digs = "0123456789abcdef";
#ifdef ROMAN
	rdigs = "  mdclxvi";
#endif

	switch(*fmt)
		{
		case 'b':
		case 'B':
			u = *argp++;
			base = 2;
			goto donum;

		case 'c':
			putc(*argp++, fd);
			break;

		case 'd':
		case 'D':
			n = *argp++;

			if(n >= 0)
				u = n;
			else	{
				u = -n;
				negflag = TRUE;
				}

			base = 10;

			goto donum;

		case 'o':
		case 'O':
			u = *argp++;
			base = 8;
			goto donum;
#ifdef ROMAN
		case 'R':
			rdigs = "  MDCLXVI";
		case 'r':
			n = *argp++;
			p2 = &buf[MAXBUF - 1];

			d = n % 10;
			tack(d, &rdigs[6], &p2);
			n = n / 10;

			d = n % 10;
			tack(d, &rdigs[4], &p2);
			n = n / 10;

			d = n % 10;
			tack(d, &rdigs[2], &p2);
			n /= 10;

			d = n % 10;
			tack(d, rdigs, &p2);

			p = p2;

			goto putpad;
#endif
		case 's':
			p = (char *)(*argp++);

			if(p == NULL)
				p = "(NULL)";

			if(length > 0 && !ladjust)
				{
				n = 0;
				p2 = p;

				for(; *p != '\0' &&
						(prec == -1 || n < prec); p++)
					n++;

				p = p2;

				while(n < length)
					{
					putc(' ', fd);
					n++;
					}
				}

			n = 0;

			while(*p != '\0')
				{
				if(++n > prec && prec != -1)
					break;

				putc(*p++, fd);
				}

			if(n < length && ladjust)
				{
				while(n < length)
					{
					putc(' ', fd);
					n++;
					}
				}

			break;

		case 'u':
		case 'U':
			u = *argp++;
			base = 10;
			goto donum;

		case 'X':
			digs = "0123456789ABCDEF";
		case 'x':
			u = *argp++;
			base = 16;

donum:			p = &buf[MAXBUF - 1];

			do	{
				*p-- = digs[u % base];
				u /= base;
				} while(u != 0);

			if(negflag)
				putc('-', fd);
putpad:
			size = &buf[MAXBUF - 1] - p;

			if(size < length && !ladjust)
				{
				while(length > size)
					{
					putc(padc, fd);
					length--;
					}
				}

			while(++p != &buf[MAXBUF])
				putc(*p, fd);

			if(size < length)	/* must be ladjust */
				{
				while(length > size)
					{
					putc(padc, fd);
					length--;
					}
				}

			break;

		case '\0':
			fmt--;
			break;

		default:
			putc(*fmt, fd);
		}
	fmt++;
	}
}

#ifdef ROMAN

static
tack(d, digs, p)
int d;
char *digs;
char **p;
{
if(d == 0) return;
if(d >= 1 && d <= 3)
	{
	doit(d, digs[2], p);
	return;
	}

if(d == 4 || d == 5)
	{
	**p = digs[1];
	(*p)--;
	}

if(d == 4)
	{
	**p = digs[2];
	(*p)--;
	return;
	}

if(d == 5) return;

if(d >= 6 && d <= 8)
	{
	doit(d - 5, digs[2], p);
	**p = digs[1];
	(*p)--;
	return;
	}

/* d == 9 */

**p = digs[0];
(*p)--;
**p = digs[2];
(*p)--;
return;
}

static
doit(d, one, p)
int d;
char one;
char **p;
{
int i;

for(i = 0; i < d; i++)
	{
	**p = one;
	(*p)--;
	}
}

#endif
