/*
 * Copyright (c) 1985 University of Alberta *
 *
 * This file contains two fuctions for parameter decoding;
 * "inicodes" is an initialization function and must be called once,
 * before "decode" can be used; "decode" is the actual parameter
 * decoding function;
 */

#ifndef lint
static char *rcsid_decode_c = "$Header: decode.c,v 10.2 86/02/01 15:59:43 tony Rel $";
#endif

#include "imPdefs.h"
#include "imPcodes.h"
#define CMDPLEN(code)	(017&cmdpars[code&0177])
#define	CMDPMIX(code)	(017&(cmdpars[code&0177]>>4))
char	cmdpars[128];

#define	NOPMSK	(15<<4)
#define	B	(0<<4)



#define	BA	(0<<4)
#define	W	(1<<4)
#define	BB	(2<<4)
#define	BBB	(3<<4)
#define	WWW	(4<<4)
#define	BBA	(5<<4)
#define	WBBBBBA	(6<<4)
#define	WWWWWWA	(7<<4)
#define BWA	(8<<4)
#define BBBA	(9<<4)
#define WA	(10<<4)
#define	NOPARS	NOPMSK+0

char	*cmdpmsk[16] = {
	"B", "W", "BB", "BBB", "WWW", "BBA", "WBBBBBA", "WWWWWWA",
	"BWA", "BBBA", "WA", "", "", "", "", ""};

inicodes ()
{	
	int	i;

	for (i=0; i<128; i++)
		cmdpars[i] = NOPARS;

	cmdpars[ASP0&0177] = NOPARS;
	cmdpars[ASP1&0177] = NOPARS;

	cmdpars[ASRULE&0177] = BBB+3;
	cmdpars[ABRULE&0177] = WWW+6;

	cmdpars[AM&0177] = BB+2;
	cmdpars[AMP&0177]= NOPARS;
	cmdpars[AMM&0177]= NOPARS;
	cmdpars[AH&0177] = W+2;
	cmdpars[AV&0177] = W+2;
	cmdpars[AN&0177] = NOPARS;

	cmdpars[ASGLY&0177] = WBBBBBA+7;
	cmdpars[ABGLY&0177] = WWWWWWA+12;
	cmdpars[ADELG&0177] = W+2;
	cmdpars[ADELC&0177] = W+2;
	cmdpars[ADELF&0177] = B+1;
	cmdpars[AFONT&0177] = BA+1;

	cmdpars[APAGOR&0177] = B+1;
	cmdpars[AROTMS&0177] = B+1;

	cmdpars[AF&0177] = B+1;
	cmdpars[ABSKIP&0177] = W+2;
	cmdpars[AMARGIN&0177] = W+2;
	cmdpars[ASETSP&0177] = W+2;

	cmdpars[APUSH&0177] = NOPARS;
	cmdpars[APOP&0177] = NOPARS;

	cmdpars[APAGE&0177] = NOPARS;
	cmdpars[AEND&0177] = NOPARS;

	cmdpars[AEOF&0177] = NOPARS;
	cmdpars[ANOP&0177] = NOPARS;

	cmdpars[ASET_HV_SYS&0177] = B+1;
	cmdpars[ASET_ABS_H&0177] = W+2;
	cmdpars[ASET_ABS_V&0177] = W+2;
	cmdpars[ASET_REL_H&0177] = W+2;
	cmdpars[ASET_REL_V&0177] = W+2;

	cmdpars[AMMOVE&0177] = W+2;
	cmdpars[ASMOVE&0177] = W+2;

	cmdpars[ACREATE_MAP&0177] = BB+2;
	cmdpars[ACREATE_FAMILY&0177] = BB+2;
	cmdpars[AFORCE_GLY_DELETE&0177] = NOPARS;

	cmdpars[ASET_PATH&0177] = WA+2;
	cmdpars[ASET_TEXTURE&0177] = BB+2;
	cmdpars[ASET_PEN&0177] = B+1;
	cmdpars[ADRAW_PATH&0177] = B+1;
	cmdpars[AFILL_PATH&0177] = B+1;
	cmdpars[ABIT_MAP&0177] = BBBA+3;
	cmdpars[ASET_MAGNIFICATION&0177] = B+1;
	cmdpars[ASET_PUSH_MASK&0177] = W+2;

	cmdpars[ADEFINE_MACRO&0177] = BWA+3;
	cmdpars[AEXECUTE_MACRO&0177] = B+1;
}

decode(ccode)
char	ccode;
{	
	register parva	*parap;
	register char	*sp;
	register int	i;
	register char	*Cinpcurr;
	char	inp[20];

	Cinpcurr = inp;
	for (i=CMDPLEN(ccode); i; i--)
		*Cinpcurr++ = gc();
	Cinpcurr = inp;

	parap = param+1;
	sp = cmdpmsk[CMDPMIX(ccode)];
	while (*sp!=0) {
		if (*sp == 'B') {
			parap->pval2.hival = 0;
			parap->pval2.loval = *Cinpcurr;
			Cinpcurr++;
		}
		else if (*sp == 'W') {
			parap->pval = ((*Cinpcurr & 0377) << 8)
			    | (Cinpcurr[1] & 0377);
			Cinpcurr += 2;
		}
		else parap->pptr = Cinpcurr;
		sp++;
		parap++;
	}
}

getint()
{	
	register parva	*parap;
	register int c = gc();

	parap = param+1;
	parap->pval = ((c & 0377) << 8) | (gc() & 0377);
	return(V(1));
}
