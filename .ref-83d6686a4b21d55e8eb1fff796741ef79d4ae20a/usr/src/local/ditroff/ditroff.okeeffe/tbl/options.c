#ifndef lint
static char sccsid[] = "@(#)options.c	1.2 (CWI) 85/10/02";
#endif lint

/*
 * Threat the option part of the table
 */

#include "defs.h"
#include "ext.h"

struct optstr {
	char   *optnam;
	int    *optadd;
}options[] = {
	"expand",	&expflg,
	"EXPAND",	&expflg,
	"center",	&ctrflg,
	"CENTER",	&ctrflg,
	"centre",	&ctrflg,	/* proper English added by jaap */
	"CENTRE",	&ctrflg,	/* proper English added by jaap */
	"box",		&boxflg,
	"BOX",		&boxflg,
	"allbox",	&allflg,
	"ALLBOX",	&allflg,
	"doublebox",	&dboxflg,
	"DOUBLEBOX",	&dboxflg,
	"frame",	&boxflg,
	"FRAME",	&boxflg,
	"doubleframe",	&dboxflg,
	"DOUBLEFRAME",	&dboxflg,
	"tab",		&tab,
	"TAB",		&tab,
	"linesize",	&linsize,
	"LINESIZ",	&linsize,
	"delim",	&delim1,
	"DELIM",	&delim1,
	0,		0
};

#define NBSIZ 25

getcomm(){
	char line[BUFSIZ], *cp, nb[NBSIZ], *t;
	register struct optstr *lp;
	register int c, ci;
	int found;

	for(lp = options; lp -> optnam; lp++)

		*(lp -> optadd) = 0;
	texname = texstr[texct = 0];
	tab = '\t';
	printf(".nr %d \\n(.s\n", LSIZE);
	gets1(line);
	/*
	 * see if this is a command line
	 */
	if(index(line, ';') == NULL){
		backrest(line);
		return;
	}
	for(cp = line; (c = *cp) != ';'; cp++){
		if(!isalpha(c))
			continue;
		found = 0;
		for(lp = options; lp -> optadd; lp++){
			if(prefix(lp -> optnam, cp)){
				*(lp -> optadd) = 1;
				cp += strlen(lp -> optnam);
				if(isalpha(*cp))
					error("Misspelled global option");
				while(*cp == ' ')
					cp++;
				t = nb;
				if(*cp == '(')
					while((ci = *++cp) != ')'){
						*t++ = ci;
						if(t >= &nb[NBSIZ-3])
							error("buffer overflow");
				}
				else
					cp--;
				*t++ = 0;
				*t = 0;
				if(lp -> optadd == &tab){
					if(nb[0])
						*(lp -> optadd) = nb[0];
				}
				if(lp -> optadd == &linsize)
					printf(".nr %d %s\n", LSIZE, nb);
				if(lp -> optadd == &delim1){
					delim1 = nb[0];
					delim2 = nb[1];
				}
				found = 1;
				break;
			}
		}
		if(!found)
			error("Illegal option");
	}
	cp++;
	backrest(cp);
	return;
}
