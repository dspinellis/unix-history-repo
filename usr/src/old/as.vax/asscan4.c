/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)asscan4.c 4.3 %G%";
#endif not lint

#include "asscanl.h"

#define	reg	register
#define	NUMSIZE	128	/* how many characters long a number can be */
#define	FLTCHAR(x)	(INCHARSET((x),(DIGIT|SIGN|FLOATEXP|POINT)))

static char	numbuf[NUMSIZE];

int number(ch, cpp)
	reg	int	ch;
		char	**cpp;
{
		int	radix;
		int	digit;		/* part of number being constructed */
	reg	int	intval;		/* number being constructed */
	reg	char	*cp;
	reg	char	*inbufptr;
		char	ch1;
		Bignum	floatnumber();
		Ovf	overflow;	/* overflow flag */
		int	maxstrlg;

	inbufptr = *cpp;
	cp = numbuf;
	radix = 10;

	switch(ch){
	case '0':
		switch(ch = getchar()){
		case 'b':
			yylval = -1;
			*cpp = inbufptr;
			return(BFINT);
		case 'f':
			/*
			 * Check if it is a local label by peeking ahead
			 */
			ch1 = getchar();
			ungetc(ch1);
			if (!FLTCHAR(ch1)){
				yylval = 1;
				*cpp = inbufptr;
				return(BFINT);
			}
			/*FALLTHROUGH*/
		case 'F': ch = 'f';	goto floatnum;
		case 'd':
		case 'D': ch = 'd';	goto floatnum;
		case 'h':
		case 'H': ch = 'h';	goto floatnum;
		case 'g':
		case 'G': ch = 'g';	goto floatnum;

		case 'x':
		case 'X':
			ch = '0';
			radix = 16;
			break;
		case '0':
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8':
		case '9':
			radix = 8;
			break;
		default:	/* single 0 */
			ungetc(ch);
			intval = 0;
			goto smallnum;
		}
		break;

	case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8':
	case '9':
		switch(ch1 = getchar()){
		case 'f':
			yylval = ((ch - '0') + 1);
			*cpp = inbufptr;
			return(BFINT);
		case 'b':
			yylval = -((ch - '0') + 1);
			*cpp = inbufptr;
			return(BFINT);
		default:
			ungetc(ch1);	/* put back non zero */
		}
		radix = 10;
		break;
	}
	intval = 0;
	/*
	 *	There is a character in ch that must be used to
	 *	cons up the number; we can't ungetc it
	 */
	do{
		digit = ch - '0';
		switch(radix){
		case 8:
			intval <<= 3;
			break;
		case 10:
			intval *= 10;
			break;
		case 16:
			intval <<= 4;
			if (INCHARSET(ch, HEXLDIGIT)){
				digit = (ch - 'a') + 10;
				break;
			}
			if (INCHARSET(ch, HEXUDIGIT)){
				digit = (ch - 'A') + 10;
				break;
			}
		}
		*cp++ = ch;
		/*
		 *	Build a negative number, then negate it
		 */
		intval -= digit;

		ch = getchar();
		if(!INCHARSET(ch, DIGIT)){
			if (radix != 16)
				break;
			if(!INCHARSET(ch, (HEXLDIGIT|HEXUDIGIT)))
				break;
		}
	} while (1);
	ungetc(ch);
	*cp = 0;
	maxstrlg = cp - numbuf;
	/*
	 *	See if the number is too large for our previous calculation
	 */
	switch(radix){
	case 16:
		if (maxstrlg > 8)
			goto bignum;
		break;
	case 10:
		if (maxstrlg >= 10)
			goto bignum;
		break;
	case 8:
		if (maxstrlg > 11)
			goto bignum;
		if (maxstrlg == 11 && numbuf[0] > 3)
			goto bignum;
		break;
	}
	/*
	 *	Negate the number
	 */
  smallnum: ;
	yylval = -intval;
	*cpp = inbufptr;
	return(INT);
  bignum: ;
	yybignum = as_atoi(numbuf, radix, &overflow);
	*cpp = inbufptr;
	return(BIGNUM);
  floatnum: ;
	*cpp = inbufptr;
	yybignum = floatnumber(ch, cpp);
	return(BIGNUM);
}

#define	TOOLONG	if(cp == &numbuf[NUMSIZE]){if (passno == 2)yywarning(toolong); goto process;}
#define	scanit(sign) *cpp = inbufptr; error |= scanint(sign, &cp, cpp); inbufptr = *cpp; ch = getchar(); TOOLONG;

Bignum floatnumber(fltradix, cpp)
	int	fltradix;
	char	**cpp;		/* call by copy return semantics */
{
		char	*cp;
		int	ch;
		char	*toolong = "Floating number too long.";
		char	*prologue =
			"Floating 0%c conflicts with exponent %c; choose %c";
		/*
		 *	This is not implemented yet:
		 *	overflow is set on floating overflow.
		 */
		Ovf	overflow;
		int	error;
		int	fractOK;
	reg	char	*inbufptr;

	inbufptr = *cpp;
	cp = numbuf;
	error = 0;
	fractOK = 0;

	scanit(1);
	if(INCHARSET(ch, POINT)){
		fractOK++;
		*cp++ = '.';
		scanit(0);
	}
	if(INCHARSET(ch, FLOATEXP)){
		fractOK++;
		if(ch != fltradix){
			if (passno == 2)
				yywarning(prologue, fltradix, ch, fltradix);
		}
		switch(fltradix){
		case 'd':
		case 'f':
			*cp++ = 'e';		/* will be read by atof() */
			break;
		default:
			*cp++ = fltradix;	/* will be read by bigatof() */
			break;
		}
		scanit(1);
	}
	if (error || fractOK == 0){
		yyerror("Badly formatted floating point number.");
	}
	ungetc(ch);
	*cp++ = 0;

  process: ;
	switch(fltradix){
	case 'f':	fltradix = TYPF;	break;
	case 'd':	fltradix = TYPD;	break;
	case 'g':	fltradix = TYPG;	nGHnumbers++; break;
	case 'h':	fltradix = TYPH;	nGHnumbers++; break;
	}
	/*
	 *	The overflow value is lost in the call to as_atof
	 */
	*cpp = inbufptr;
	return(as_atof(numbuf, fltradix, &overflow));
}
/*
 *	Scan an optionally signed integer, putting back the lookahead
 *	character when finished scanning.
 */
int scanint(signOK, dstcpp, srccpp)
	int	signOK;
	char	**dstcpp;
	char	**srccpp;	/* call by copy return */
{
	int	ch;
	int	back = 0;
	reg	char	*inbufptr = *srccpp;

	ch = getchar();
	while (INCHARSET(ch, SIGN)){
		if (signOK && !back)
			*((*dstcpp)++) = ch;
		else
			back = 1;
		ch = getchar();
	}
	while (INCHARSET(ch, DIGIT)){
		*((*dstcpp)++) = ch;
		ch = getchar();
	}
	ungetc(ch);
	*srccpp = inbufptr;
	return(back);
}
