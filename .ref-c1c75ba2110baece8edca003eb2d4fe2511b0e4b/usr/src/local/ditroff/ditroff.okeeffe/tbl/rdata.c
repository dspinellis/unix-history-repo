#ifndef lint
static char sccsid[] = "@(#)rdata.c	1.2 (CWI) 85/10/02";
#endif lint


/*
 * read data for table
 */

#include "defs.h"
#include "ext.h"
#include <signal.h>

gettbl(){
	int icol, ch;
	extern interr();
	extern char *chspace();
	extern char *maknew();
	extern int *alocv();
	extern char *gettext();
	int (*savsign)();

	cstore = cspace = chspace();
	textflg = 0;
	for(nlin = nslin = 0; gets1(cstore); nlin++){
		stynum[nlin] = nslin;
		if(prefix(".TE", cstore)){
			leftover = 0;
			break;
		}
		if(prefix(".TC", cstore) || prefix(".T&", cstore)){
			readspec();
			nslin++;
		}
		if(nlin >= MAXLIN){
			leftover = (int)cstore;
			break;
		}
		fullbot[nlin] = 0;
		if(cstore[0] == '.' && !isdigit(cstore[1])){
			instead[nlin] = cstore;
			while(*cstore++)
				;
			continue;
		} else
			instead[nlin] = 0;
		if(nodata(nlin)){
			if(ch = oneh(nlin))
				fullbot[nlin] = ch;
#ifdef FIX
	This FIX didn't work, so commented out for the time being,
	problem temporarily solved with signal catching...
dprint(".\\\" FIX nlin = %d\n", nlin);
dprint(".\\\" FIXgettbl: alocv %d\n", (ncol + 2) * sizeof(table[0][0]));
			/*
			 * start of FIX?
			 *
			 * Need to allocate pointers as well, in case
			 * of vertical spanning.
			 * I hope this works
			 */
			table[nlin] = (struct colstr *) alocv((ncol + 2) *
							sizeof(*table[0]));
			/*
			 * Does alocv clears the pointers?
			 */
		{	int tmp;
			for( tmp = 0; tmp < ncol +2; tmp++) {
		table[nlin][tmp].col = "";
		table[nlin][tmp].rcol = (char*)0;
printf(".\\\"FIX table[%d][%d].col: <%s>\n", nlin, tmp, table[nlin][tmp].col);
printf(".\\\"FIX table[%d][%d].rcol: <%s>\n", nlin, tmp, table[nlin][tmp].rcol);
			}
		}
			/*
			 * End of FIX
			 */
#endif FIX
			nlin++;
			nslin++;
			instead[nlin] = (char *) 0;
			fullbot[nlin] = 0;
dprint(".\\\" gettbl continue, due to nodata\n");
		}
		table[nlin] = (struct colstr *) alocv((ncol + 2) *
							sizeof(*table[0]));
		if(cstore[1] == 0) {
			switch(cstore[0]){

			case '_': 
				fullbot[nlin] = '-';
				continue;
			case '=': 
				fullbot[nlin] = '=';
				continue;
			}
		}
		stynum[nlin] = nslin;
		nslin = min(nslin + 1, nclin - 1);
		for(icol = 0; icol < ncol; icol++){
			table[nlin][icol].col = cstore;
			table[nlin][icol].rcol = (char*)0;
			ch = 1;
			if(strcmp(cstore, "T{") == 0) {
				/*
				 * text follows
				 */
				table[nlin][icol].col =
						gettext(cstore, nlin, icol,
						font[stynum[nlin]][icol],
						csize[stynum[nlin]][icol]);
			} else {
				for(; (ch = *cstore) != '\0' && ch != tab; cstore++)
					;
				*cstore++ = '\0';
				switch(ctype(nlin, icol)){
 					/*
					 * numerical or alpha, subcol
					 */
				case 'n': 
					table[nlin][icol].rcol = maknew(table[nlin][icol].col);
					break;
				case 'a': 
					table[nlin][icol].rcol = table[nlin][icol].col;
					table[nlin][icol].col = "";
					break;
				}
			}
			while(ctype(nlin, icol + 1) == 's'){
				/*
				 * spanning
				 */
				table[nlin][++icol].col = "";
			}
			if(ch == '\0')
				break;
		}
		while(++icol < ncol + 2){
			table[nlin][icol].col = "";
			table[nlin][icol].rcol = (char*)0;
		}
		while(*cstore != '\0')
			cstore++;
		if(cstore - cspace > MAXCHS)
			cstore = cspace = chspace();
	}
	last = cstore;
	/*
	 * the next example is weird & legal tbl input.
	 * however, it generates a bus error.
.TS
linesize(24) tab(@);
ct| c cf3
^ | _ _
^ | cf3 cf3
^ | c s.
0,0@0,1@0,2
@1,1@1,2
@2,1@2,2
.TE
	 * This works:
.TS
linesize(24) tab(@);
ct| c cf3
^ | cf3 cf3
^ | c s.
0,0@0,1@0,2
@1,1@1,2
@2,1@2,2
.TE
	 * So it is the vertical spanning of an empty column which
	 * cuases problems
	 */
	savsign = signal(SIGBUS, interr);
	permute();
	signal(SIGBUS, savsign);
	if(textflg)
		untext();
	return;
}

/*
 * return 1 if no type of column specified for this line
 */
nodata(il){
	int c;

	for(c = 0; c < ncol; c++){
		switch(ctype(il, c)){

		case 'c': 
		case 'n': 
		case 'r': 
		case 'l': 
		case 's': 
		case 'a': 
			return(0);
		}
	}
	return(1);
}

/*
 * returns the type of heading if they are all the same for the table
 * line?
 */
oneh(lin){
	int k, icol;

	k = ctype(lin, 0);
	for(icol = 1; icol < ncol; icol++){
		if(k != ctype(lin, icol))
			return(0);
	}
	return(k);
}

#define SPAN "\\^"

permute(){
	register int irow, jcol, is;
	char *start, *strig;

	for(jcol = 0; jcol < ncol; jcol++){
		for(irow = 1; irow < nlin; irow++){
			if(vspand(irow, jcol, 0)){
				is = prev(irow);
				if(is < 0)
					error("Vertical spanning in first row not allowed");
				start = table[is][jcol].col;
				strig = table[is][jcol].rcol;
				while(irow < nlin && vspand(irow, jcol, 0)){
					irow++;
				}
				table[--irow][jcol].col = start;
				table[irow][jcol].rcol = strig;
				while(is < irow){
					table[is][jcol].col = SPAN;
					table[is][jcol].rcol = (char*)0;
					is = next(is);
				}
			}
		}
	}
}

/*
 * return 1 if vertical spanning is row ir, column ij, from position ifrom
 */
vspand(ir, ij, ifform)
{
	if(ir < 0)
		return(0);
	if(ir >= nlin)
		return(0);
	if(instead[ir])
		return(0);
	if(ifform == 0 && ctype(ir, ij) == '^'){
		return(1);
	}
	if(table[ir][ij].rcol != (char*)0)
		return(0);
	if(fullbot[ir])
		return(0);
	return(vspen(table[ir][ij].col));
}

/*
 * return 1 if the string is the same as SPAN
 */
vspen(s)
char   *s;
{
	if(s == 0)
		return(0);
	if(!point(s))
		return(0);
	return(strcmp(s, SPAN) == 0);
}

static
interr()
{
	error("internal tbl error -- function: permute");
}
