/*
 * PARTIME		parse date/time string into a TM structure
 *
 * Usage:
 *      #include "time.h"             -- expanded tm structure
 *	char *str; struct tm *tp;
 *	partime(str,tp);
 * Returns:
 *	0 if parsing failed
 *	else time values in specified TM structure (unspecified values
 *		set to TMNULL)
 * Notes:
 *	This code is quasi-public; it may be used freely in like software.
 *	It is not to be sold, nor used in licensed software without
 *	permission of the author.
 *	For everyone's benefit, please report bugs and improvements!
 * 	Copyright 1980 by Ken Harrenstien, SRI International.
 *	(ARPANET: KLH @ SRI)
 */

/* Hacknotes:
 *	If parsing changed so that no backup needed, could perhaps modify
 *		to use a FILE input stream.  Need terminator, though.
 *	Perhaps should return 0 on success, else a non-zero error val?
 *	Flush AMPM from TM structure and handle locally within PARTIME,
 *		like midnight/noon?
 */

#ifndef lint
static char rcsid[]=
"$Header: /arthur/src/local/bin/rcs/src/RCS/partime.c,v 1.2 87/03/27 14:21:53 jenkins Exp $";
#endif

/* $Log:	partime.c,v $
 * Revision 1.2  87/03/27  14:21:53  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:07  kcs
 * Initial revision
 * 
 * Revision 1.1  82/05/06  11:38:26  wft
 * Initial revision
 * 
 */

#include <stdio.h>
#include <ctype.h>
#include "time.h"

#ifndef lint
static char timeid[] = TIMEID;
#endif

struct tmwent {
	char *went;
	long wval;	/* must be big enough to hold pointer or integer */
	char wflgs;
	char wtype;
};
	/* wflgs */
#define TWSPEC 01	/* Word wants special processing */
#define TWTIME 02	/* Word is a time value (absence implies date) */
#define TWDST  04	/* Word is a DST-type timezone */
#define TW1200 010	/* Word is NOON or MIDNIGHT (sigh) */

int pt12hack();
int ptnoise();
struct tmwent tmwords [] = {
	{"january",      0, 0, TM_MON},
	{"february",     1, 0, TM_MON},
	{"march",        2, 0, TM_MON},
	{"april",        3, 0, TM_MON},
	{"may",          4, 0, TM_MON},
	{"june",         5, 0, TM_MON},
	{"july",         6, 0, TM_MON},
	{"august",       7, 0, TM_MON},
	{"september",    8, 0, TM_MON},
	{"october",      9, 0, TM_MON},
	{"november",     10, 0, TM_MON},
	{"december",     11, 0, TM_MON},

	{"sunday",       0, 0, TM_WDAY},
	{"monday",       1, 0, TM_WDAY},
	{"tuesday",      2, 0, TM_WDAY},
	{"wednesday",    3, 0, TM_WDAY},
	{"thursday",     4, 0, TM_WDAY},
	{"friday",       5, 0, TM_WDAY},
	{"saturday",     6, 0, TM_WDAY},

	{"gmt",          0*60, TWTIME, TM_ZON},   /* Greenwich */
	{"gst",          0*60, TWTIME, TM_ZON},
	{"gdt",          0*60, TWTIME+TWDST, TM_ZON},     /* ?? */

	{"ast",          4*60, TWTIME, TM_ZON},   /* Atlantic */
	{"est",          5*60, TWTIME, TM_ZON},   /* Eastern */
	{"cst",          6*60, TWTIME, TM_ZON},   /* Central */
	{"mst",          7*60, TWTIME, TM_ZON},   /* Mountain */
	{"pst",          8*60, TWTIME, TM_ZON},   /* Pacific */
	{"yst",          9*60, TWTIME, TM_ZON},   /* Yukon */
	{"hst",          10*60, TWTIME, TM_ZON},  /* Hawaii */
	{"bst",          11*60, TWTIME, TM_ZON},  /* Bering */

	{"adt",          4*60, TWTIME+TWDST, TM_ZON},     /* Atlantic */
	{"edt",          5*60, TWTIME+TWDST, TM_ZON},     /* Eastern */
	{"cdt",          6*60, TWTIME+TWDST, TM_ZON},     /* Central */
	{"mdt",          7*60, TWTIME+TWDST, TM_ZON},     /* Mountain */
	{"pdt",          8*60, TWTIME+TWDST, TM_ZON},     /* Pacific */
	{"ydt",          9*60, TWTIME+TWDST, TM_ZON},     /* Yukon */
	{"hdt",          10*60, TWTIME+TWDST, TM_ZON},    /* Hawaii */
	{"bdt",          11*60, TWTIME+TWDST, TM_ZON},    /* Bering */

	{"daylight",     1, TWTIME+TWDST, TM_ZON},        /* Local Daylight */
	{"standard",     1, TWTIME, TM_ZON},      /* Local Standard */
	{"std",          1, TWTIME, TM_ZON},      /*   "       "    */

	{"am",           1, TWTIME, TM_AMPM},
	{"pm",           2, TWTIME, TM_AMPM},
	{"noon",         12,TWTIME+TW1200, 0},    /* Special frobs */
	{"midnight",     0, TWTIME+TW1200, 0},
	{"at",           (long)ptnoise, TWSPEC, 0},    /* Noise word */

	{0, 0, 0, 0},             /* Zero entry to terminate searches */
};

#define TMWILD (-2)	/* Value meaning item specified as wild-card */
			/* (May use someday...) */

struct token {
	char *tcp;	/* pointer to string */
	int tcnt;	/* # chars */
	char tbrk;	/* "break" char */
	char tbrkl;	/* last break char */
	char tflg;	/* 0 = alpha, 1 = numeric */
	union {         /* Resulting value; */
		int tnum;/* either a #, or */
		struct tmwent *ttmw;/* ptr to a tmwent. */
	} tval;
};

partime(astr, atm)
char *astr;
struct tm *atm;
{	register int *tp;
	register struct tmwent *twp;
	register int i;
	struct token btoken, atoken;
	char *cp, ch;
	int ord, midnoon;
	int (*aproc)();

	tp = (int *)atm;
	zaptime(tp);			 /* Initialize the TM structure */
	midnoon = TMNULL;		/* and our own temp stuff */
	btoken.tcnt = btoken.tbrkl = 0;
	btoken.tcp = astr;

domore:
	if(!ptitoken(btoken.tcp+btoken.tcnt,&btoken))	/* Get a token */
	  {     if(btoken.tval.tnum) return(0);         /* Read error? */
		if(midnoon != TMNULL)			/* EOF, wrap up */
			return(pt12hack(tp, midnoon));
		return(1);				/* Win return! */
	  }
	if(btoken.tflg == 0)		/* Alpha? */
	  {     twp = btoken.tval.ttmw;         /* Yes, get ptr to entry */
		if(twp->wflgs&TWSPEC)		/* Special alpha crock */
		  {     aproc = (int (*) ()) (twp->wval);
			if(!(*aproc)(tp, twp, &btoken))
				return(0);	/* ERR: special word err */
			goto domore;
		  }
		if(twp->wflgs&TW1200)
			if(ptstash(&midnoon,(int)twp->wval))
				return(0);	/* ERR: noon/midnite clash */
			else goto domore;
		if(ptstash(&tp[twp->wtype],(int)twp->wval))
			return(0);		/* ERR: val already set */
		if(twp->wtype == TM_ZON)	/* If was zone, hack DST */
			if(ptstash(&tp[TM_ISDST],(twp->wflgs&TWDST)))
				return(0);	/* ERR: DST conflict */
		goto domore;
	  }

	/* Token is number.  Lots of hairy heuristics. */
	if(btoken.tcnt >= 7)	/* More than 6 digits in string? */
		return(0);	/* ERR: number too big */
	if(btoken.tcnt == 6)	/* 6 digits = HHMMSS.  Needs special crock */
	  {			/* since 6 digits are too big for integer! */
		i = (btoken.tcp[0]-'0')*10	/* Gobble 1st 2 digits */
		   + btoken.tcp[1]-'0';
		btoken.tcnt = 2;		/* re-read last 4 chars */
		goto coltime;
	  }

	i = btoken.tval.tnum;   /* Value now known to be valid; get it. */
	if( btoken.tcnt == 5	/*  5 digits = HMMSS */
	 || btoken.tcnt == 3)	/*  3 digits = HMM   */
	  {	if(btoken.tcnt != 3)
			if(ptstash(&tp[TM_SEC], i%100))
				return(0);	/* ERR: sec conflict */
			else i /= 100;
hhmm4:		if(ptstash(&tp[TM_MIN], i%100))
			return(0);		/* ERR: min conflict */
		i /= 100;
hh2:            if(ptstash(&tp[TM_HOUR], i))
			return(0);		/* ERR: hour conflict */
		goto domore;
	  }

	if(btoken.tcnt == 4)	/* 4 digits = YEAR or HHMM */
	  {	if(tp[TM_YEAR] != TMNULL) goto hhmm4;	/* Already got yr? */
		if(tp[TM_HOUR] != TMNULL) goto year4;	/* Already got hr? */
		if((i%100) > 59) goto year4;		/* MM >= 60? */
		if(btoken.tbrk == ':')			/* HHMM:SS ? */
			if( ptstash(&tp[TM_HOUR],i/100)
			 || ptstash(&tp[TM_MIN], i%100))
				return(0);		/* ERR: hr/min clash */
			else goto coltm2;		/* Go handle SS */
		if(btoken.tbrk != ',' && btoken.tbrk != '/'
		  && ptitoken(btoken.tcp+btoken.tcnt,&atoken)	/* Peek */
		  && atoken.tflg == 0			/* alpha */
		  && (atoken.tval.ttmw->wflgs&TWTIME))  /* HHMM-ZON */
			goto hhmm4;
		if(btoken.tbrkl == '-'		/* DD-Mon-YYYY */
		  || btoken.tbrkl == ','	/* Mon DD, YYYY */
		  || btoken.tbrkl == '/'	/* MM/DD/YYYY */
		  || btoken.tbrkl == '.'	/* DD.MM.YYYY */
		  || btoken.tbrk == '-'		/* YYYY-MM-DD */
			) goto year4;
		goto hhmm4;			/* Give up, assume HHMM. */
	  }

	/* From this point on, assume tcnt == 1 or 2 */
	/* 2 digits = YY, MM, DD, or HH (MM and SS caught at coltime) */
	if(btoken.tbrk == ':')		/* HH:MM[:SS] */
		goto coltime;		/*  must be part of time. */
	if(i > 31) goto yy2;		/* If >= 32, only YY poss. */

	/* Check for numerical-format date */
	for (cp = "/-."; ch = *cp++;)
	  {	ord = (ch == '.' ? 0 : 1);	/* n/m = D/M or M/D */
		if(btoken.tbrk == ch)			/* "NN-" */
		  {	if(btoken.tbrkl != ch)
			  {	if(ptitoken(btoken.tcp+btoken.tcnt,&atoken)
				  && atoken.tflg == 0
				  && atoken.tval.ttmw->wtype == TM_MON)
					goto dd2;
				if(ord)goto mm2; else goto dd2; /* "NN-" */
			  }				/* "-NN-" */
			if(tp[TM_DAY] == TMNULL
			&& tp[TM_YEAR] != TMNULL)	/* If "YY-NN-" */
				goto mm2;		/* then always MM */
			if(ord)goto dd2; else goto mm2;
		  }
		if(btoken.tbrkl == ch			/* "-NN" */
		  && tp[ord ? TM_MON : TM_DAY] != TMNULL)
			if(tp[ord ? TM_DAY : TM_MON] == TMNULL)	/* MM/DD */
				if(ord)goto dd2; else goto mm2;
			else goto yy2;			/* "-YY" */
	  }

	/* At this point only YY, DD, and HH are left.
	 * YY is very unlikely since value is <= 32 and there was
	 * no numerical format date.  Make one last try at YY
	 * before dropping through to DD vs HH code.
	 */
	if(btoken.tcnt == 2		/* If 2 digits */
	  && tp[TM_HOUR] != TMNULL	/* and already have hour */
	  && tp[TM_DAY] != TMNULL	/* and day, but  */
	  && tp[TM_YEAR] == TMNULL)	/* no year, then assume */
		goto yy2;		/* that's what we have. */

	/* Now reduced to choice between HH and DD */
	if(tp[TM_HOUR] != TMNULL) goto dd2;	/* Have hour? Assume day. */
	if(tp[TM_DAY] != TMNULL) goto hh2;	/* Have day? Assume hour. */
	if(i > 24) goto dd2;			/* Impossible HH means DD */
	if(!ptitoken(btoken.tcp+btoken.tcnt, &atoken))	/* Read ahead! */
		if(atoken.tval.tnum) return(0); /* ERR: bad token */
		else goto dd2;			/* EOF, assume day. */
	if( atoken.tflg == 0		/* If next token is an alpha */
	 && atoken.tval.ttmw->wflgs&TWTIME)  /* time-spec, assume hour */
		goto hh2;		/* e.g. "3 PM", "11-EDT"  */

dd2:	if(ptstash(&tp[TM_DAY],i))	/* Store day (1 based) */
		return(0);
	goto domore;

mm2:	if(ptstash(&tp[TM_MON], i-1))	/* Store month (make zero based) */
		return(0);
	goto domore;

yy2:	i += 1900;
year4:	if(ptstash(&tp[TM_YEAR],i))	/* Store year (full number) */
		return(0);		/* ERR: year conflict */
	goto domore;

	/* Hack HH:MM[[:]SS] */
coltime:
	if(ptstash(&tp[TM_HOUR],i)) return(0);
	if(!ptitoken(btoken.tcp+btoken.tcnt,&btoken))
		return(!btoken.tval.tnum);
	if(!btoken.tflg) return(0);	/* ERR: HH:<alpha> */
	if(btoken.tcnt == 4)		/* MMSS */
		if(ptstash(&tp[TM_MIN],btoken.tval.tnum/100)
		  || ptstash(&tp[TM_SEC],btoken.tval.tnum%100))
			return(0);
		else goto domore;
	if(btoken.tcnt != 2
	  || ptstash(&tp[TM_MIN],btoken.tval.tnum))
		return(0);		/* ERR: MM bad */
	if(btoken.tbrk != ':') goto domore;	/* Seconds follow? */
coltm2:	if(!ptitoken(btoken.tcp+btoken.tcnt,&btoken))
		return(!btoken.tval.tnum);
	if(!btoken.tflg || btoken.tcnt != 2	/* Verify SS */
	  || ptstash(&tp[TM_SEC], btoken.tval.tnum))
		return(0);		/* ERR: SS bad */
	goto domore;
}

/* Store date/time value, return 0 if successful.
 * Fails if entry already set to a different value.
 */
ptstash(adr,val)
int *adr;
{	register int *a;
	if( *(a=adr) != TMNULL)
		return(*a != val);
	*a = val;
	return(0);
}

/* This subroutine is invoked for NOON or MIDNIGHT when wrapping up
 * just prior to returning from partime.
 */
pt12hack(atp, aval)
int *atp, aval;
{	register int *tp, i, h;
	tp = atp;
	if (((i=tp[TM_MIN]) && i != TMNULL)	/* Ensure mins, secs */
	 || ((i=tp[TM_SEC]) && i != TMNULL))	/* are 0 or unspec'd */
		return(0);			/* ERR: MM:SS not 00:00 */
	i = aval;			/* Get 0 or 12 (midnite or noon) */
	if ((h = tp[TM_HOUR]) == TMNULL	/* If hour unspec'd, win */
	 || h == 12)			/* or if 12:00 (matches either) */
		tp[TM_HOUR] = i;	/* Then set time */
	else if(!(i == 0		/* Nope, but if midnight and */
		&&(h == 0 || h == 24)))	/* time matches, can pass. */
			return(0);	/* ERR: HH conflicts */
	tp[TM_AMPM] = TMNULL;		/* Always reset this value if won */
	return(1);
}

/* Null routine for no-op tokens */

ptnoise() { return(1); }

/* Get a token and identify it to some degree.
 * Returns 0 on failure; token.tval will be 0 for normal EOF, otherwise
 * hit error of some sort
 */

ptitoken(astr, tkp)
register struct token *tkp;
char *astr;
{
	register char *cp;
	register int i;

	tkp->tval.tnum = 0;
	if(pttoken(astr,tkp) == 0)
#ifdef DEBUG
	VOID printf("EOF\n");
#endif DEBUG
		return(0);
	cp = tkp->tcp;

#ifdef DEBUG
	i = cp[tkp->tcnt];
	cp[tkp->tcnt] = 0;
	VOID printf("Token: \"%s\" ",cp);
	cp[tkp->tcnt] = i;
#endif DEBUG

	if(tkp->tflg)
		for(i = tkp->tcnt; i > 0; i--)
			tkp->tval.tnum = (int)tkp->tval.tnum*10 + ((*cp++)-'0');
	else
	  {     i = ptmatchstr(cp, tkp->tcnt, tmwords);
		tkp->tval.tnum = i ? i : -1;         /* Set -1 for error */

#ifdef DEBUG
		if(!i) VOID printf("Not found!\n");
#endif DEBUG

		if(!i) return(0);
	  }

#ifdef DEBUG
	if(tkp->tflg)
		VOID printf("Val: %d.\n",tkp->tval.tnum);
	else VOID printf("Found: \"%s\", val: %d., type %d\n",
		tkp->tval.ttmw->went,tkp->tval.ttmw->wval,tkp->tval.ttmw->wtype);
#endif DEBUG

	return(1);
}

/* Read token from input string into token structure */
pttoken(astr,tkp)
register struct token *tkp;
char *astr;
{
	register char *cp;
	register int c;

	tkp->tcp = cp = astr;
	tkp->tbrkl = tkp->tbrk;		/* Set "last break" */
	tkp->tcnt = tkp->tbrk = tkp->tflg = 0;

	while(c = *cp++)
	  {	switch(c)
		  {	case ' ': case '\t':	/* Flush all whitespace */
				while((c = *cp++) && isspace(c));
				cp--;		/* Drop thru to handle brk */
			case '(': case ')':	/* Perhaps any non-alphanum */
			case '-': case ',':	/* shd qualify as break? */
			case '/': case ':': case '.':	/* Break chars */
				if(tkp->tcnt == 0)	/* If no token yet */
				  {	tkp->tcp = cp;	/* ignore the brk */
					tkp->tbrkl = c;
				  	continue;	/* and go on. */
				  }
				tkp->tbrk = c;
				return(tkp->tcnt);
		  }
		if(tkp->tcnt == 0)		/* If first char of token, */
			tkp->tflg = isdigit(c);	/*    determine type */
	  	if(( isdigit(c) &&  tkp->tflg)	/* If not first, make sure */
		 ||(!isdigit(c) && !tkp->tflg))	/*    char matches type */
			tkp->tcnt++;		/* Win, add to token. */
		else {
			cp--;			/* Wrong type, back up */
			tkp->tbrk = c;
			return(tkp->tcnt);
		  }
	  }
	return(tkp->tcnt);		/* When hit EOF */
}


ptmatchstr(astr,cnt,astruc)
char *astr;
int cnt;
struct tmwent *astruc;
{	register char *cp, *mp;
	register int c;
	struct tmwent *lastptr;
	struct integ { int word; };   /* For getting at array ptr */
	int i;

	lastptr = 0;
	for(;mp = (char *)((struct integ *)astruc)->word; astruc += 1)
	  {	cp = astr;
		for(i = cnt; i > 0; i--)
		  {	switch((c = *cp++) ^ *mp++)	/* XOR the chars */
			  {	case 0: continue;	/* Exact match */
				case 040: if(isalpha(c))
					continue;
			  }
			break;
		  }
		if(i==0)
			if(*mp == 0) return((unsigned int)astruc);    /* Exact match */
			else if(lastptr) return(0);	/* Ambiguous */
			else lastptr = astruc;		/* 1st ambig */
	  }
	return((unsigned int)lastptr);
}



zaptime(tp)
register int *tp;
/* clears tm structure pointed to by tp */
{	register int i;
	i = (sizeof (struct tm))/(sizeof (int));
	do *tp++ = TMNULL;		/* Set entry to "unspecified" */
	while(--i);			/* Faster than FOR */
}
