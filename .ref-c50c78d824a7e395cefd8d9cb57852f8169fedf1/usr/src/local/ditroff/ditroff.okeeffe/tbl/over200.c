#ifndef lint
static char sccsid[] = "@(#)over200.c	1.2 (CWI) 85/10/02";
#endif lint

 /*
  * write lines for tables over 200 lines (actually MAXLIN lines)
  *
  * Note that there is aserious bug here:
  *	The T{ ... T} doesn't work after MAXLIN entries.
  */
#include "defs.h"
#include "ext.h"

static int useln;

extern char *maknew();

yetmore()
{
	dprint(".\\\" -- yetmore\n");
	for(useln = 0; useln < MAXLIN && table[useln] == 0; useln++)
		;
	if(useln >= MAXLIN)
		error("Wierd.  No data in table.");
	table[0] = table[useln];
	for(useln = nlin - 1; useln >= 0 && (fullbot[useln] || instead[useln]);
								useln--)
		;
	if(useln < 0)
		error("Wierd.  No real lines in table.");
	domore(leftover);
	while(gets1(cstore = cspace) && domore(cstore))
		;
	last = cstore;
	return;
}

domore(dataln)
char *dataln;
{
	register int icol, ch;

	dprint(".\\\" -- domore\n");

	if(prefix(".TE", dataln))
		return(0);
	if(dataln[0] == '.' && !isdigit(dataln[1])){
		puts(dataln);
		return(1);
	}
	instead[0] = (char *) 0;
	fullbot[0] = 0;
	if(dataln[1] == 0){
		switch(dataln[0]){

		case '_': 
			fullbot[0] = '-';
			putline(useln, 0);
			return(1);
		case '=': 
			fullbot[0] = '=';
			putline(useln, 0);
			return(1);
		}
	}
	for(icol = 0; icol < ncol; icol++){
		table[0][icol].col = dataln;
		table[0][icol].rcol = 0;
		for(; (ch = *dataln) != '\0' && ch != tab; dataln++)
			;
		*dataln++ = '\0';
		switch(ctype(useln, icol)){

		case 'n': 
			table[0][icol].rcol = maknew(table[0][icol].col);
			break;
		case 'a': 
			table[0][icol].rcol = table[0][icol].col;
			table[0][icol].col = "";
			break;
		}
		while(ctype(useln, icol + 1) == 's'){
			/*
			 * spanning
			 */
			table[0][++icol].col = "";
		}
		if(ch == '\0')
			break;
	}
	while(++icol < ncol)
		table[0][icol].col = "";
	putline(useln, 0);
	/*
	 * Reuse space for numerical items (bwk)
	 * exstore = exspace;
	 */
	return(1);
}
