/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)asscan1.c 4.1 %G%";
#endif not lint

#include "asscanl.h"

inittmpfile()
{
	if (passno == 1){
		if (useVM){
			bufstart = &tokbuf[0];
			buftail = &tokbuf[1];
			bufstart->tok_next = buftail;
			buftail->tok_next = 0;
		}
		tokbuf[0].tok_count = -1;
		tokbuf[1].tok_count = -1;
	}
	tok_temp = 0;
	tok_free = 0;
	bufno = 0;
	emptybuf = &tokbuf[bufno];
	tokptr = 0;
	tokub = 0;
}

closetmpfile()
{
	if (passno == 1){
		if (useVM){
			emptybuf->toks[emptybuf->tok_count++] = PARSEEOF;
		} else {
			/*
			 *	Clean up the buffers that haven't been
			 *	written out yet
			 */
			if (tokbuf[bufno ^ 1].tok_count >= 0){
				if (writeTEST((char *)&tokbuf[bufno ^ 1], sizeof *emptybuf, 1, tmpfil)){
				  badwrite:
					yyerror("Unexpected end of file writing the interpass tmp file");
				exit(2);
				}
			}
			/*
			 *	Ensure that we will read an End of file,
			 *	if there are more than one file names
			 *	in the argument list
			 */
			tokbuf[bufno].toks[tokbuf[bufno].tok_count++] = PARSEEOF;
			if (writeTEST((char *)&tokbuf[bufno], sizeof *emptybuf, 1, tmpfil))
				goto badwrite;
		}
	}	/*end of being pass 1*/
}

inttoktype yylex()
{
	register	ptrall	bufptr;	
	register	inttoktype		val;	
	register	struct	exp	*locxp;
	/*
	 *	No local variables to be allocated; this saves
	 *	one piddling instruction..
	 */
	static	int	Lastjxxx;

	bufptr = tokptr;		/*copy in the global value*/
   top:
	if (bufptr < tokub){
		gtoken(val, bufptr);
		switch(yylval = val){
		case	PARSEEOF:
				yylval = val = PARSEEOF;
				break;
		case	BFINT:
		case	INT:
				if (xp >= &explist[NEXP])
				     yyerror("Too many expressions; try simplyfing");
				else
				    locxp = xp++;
				locxp->e_number = Znumber;
				locxp->e_number.num_tag = TYPL;
				glong(locxp->e_xvalue, bufptr);
			  makevalue:
				locxp->e_xtype = XABS;
				locxp->e_xloc = 0;
				locxp->e_xname = NULL;
				yylval = (int)locxp;
				break;
		case	BIGNUM:	
				if (xp >= &explist[NEXP])
				     yyerror("Too many expressions; try simplyfing");
				else
				    locxp = xp++;
				gnumber(locxp->e_number, bufptr);
				goto makevalue;
		case	NAME:
				gptr(yylval, bufptr);
				lastnam = (struct symtab *)yylval;
				break;
		case	SIZESPEC:
		case 	REG:
				gchar(yylval, bufptr);
				break;
		case	INSTn:
		case	INST0:
				gopcode(yyopcode, bufptr);
				break;
		case	IJXXX:
				gopcode(yyopcode, bufptr);
				/* We can't cast Lastjxxx into (int *) here.. */
				gptr(Lastjxxx, bufptr);
				lastjxxx = (struct symtab *)Lastjxxx;
				break;
		case	ILINESKIP:
				gint(yylval, bufptr);
				lineno += yylval;
				goto top;
		case	SKIP:	
				eatskiplg(bufptr);
				goto top;
		case	VOID:	
				goto top;
		case 	STRING:
				strptr = &strbuf[strno ^= 1];
				strptr->str_lg = *((lgtype *)bufptr);
				movestr(&strptr->str[0],
					(char *)bufptr + sizeof(lgtype),
					strptr->str_lg);
				eatstrlg(bufptr);
				yylval = (int)strptr;
				break;
		case 	ISTAB:
		case	ISTABSTR:
		case	ISTABNONE:
		case	ISTABDOT:
		case	IALIGN:
				gptr(yylval, bufptr);
				break;
		} 
#ifdef DEBUG
		if (toktrace){
		char	*tok_to_name();
		printf("P: %d T#: %4d, %s ",
			passno, bufptr -  firsttoken, tok_to_name(val));
		switch(val){
		case 	INT:	printf("val %d",
					((struct exp *)yylval)->e_xvalue);
				break;
		case	BFINT:	printf("val %d",
					((struct exp *)yylval)->e_xvalue);
				break;
		case 	BIGNUM: bignumprint(((struct exp*)yylval)->e_number);
				break;
		case	NAME:	printf("\"%.8s\"",
					((struct symtab *)yylval)->s_name);
				break;
		case	REG:	printf(" r%d",
					yylval);
				break;
		case	IJXXX:
		case	INST0:	
		case	INSTn:	if (ITABCHECK(yyopcode))
					printf("%.8s", ITABFETCH(yyopcode)->s_name);
				else
					printf("IJXXX or INST0 or INSTn can't get into the itab\n");
				break;
		case	STRING:	printf("length %d ",
					((struct strdesc *)yylval)->str_lg);
				printf("value\"%s\"",
					((struct strdesc *)yylval)->str);
				break;
		}  		/*end of the debug switch*/
		printf("\n");
		}
#endif DEBUG

	} else {	/* start a new buffer */
	    if (useVM){
		if (passno == 2){
			tok_temp = emptybuf->tok_next;
			emptybuf->tok_next = tok_free;
			tok_free = emptybuf;
			emptybuf = tok_temp;
		} else {
			emptybuf = emptybuf->tok_next;
		}
		bufno += 1;
		if (emptybuf == 0){
			struct	tokbufdesc *newdallop;
			int	i;
			if (passno == 2)
				goto badread;
			emptybuf = newdallop = (struct tokbufdesc *)
			  Calloc(TOKDALLOP, sizeof (struct tokbufdesc));
			for (i=0; i < TOKDALLOP; i++){
				buftail->tok_next = newdallop;
				buftail = newdallop;
				newdallop += 1;
			}
			buftail->tok_next = 0;
		}	/*end of need to get more buffers*/
		(bytetoktype *)bufptr = &(emptybuf->toks[0]);
		if (passno == 1)
			scan_dot_s(emptybuf);
	    } else {	/*don't use VM*/
		bufno ^= 1;
		emptybuf = &tokbuf[bufno];
		((bytetoktype *)bufptr) = &(emptybuf->toks[0]);
		if (passno == 1){
			/*
			 *	First check if there are things to write
			 *	out at all
			 */
			if (emptybuf->tok_count >= 0){
			    if (writeTEST((char *)emptybuf, sizeof *emptybuf, 1, tmpfil)){
				yyerror("Unexpected end of file writing the interpass tmp file");
				exit(2);
			    }
			}
			scan_dot_s(emptybuf);
		} else {	/*pass 2*/
		    if (readTEST((char *)emptybuf, sizeof *emptybuf, 1, tmpfil)){
			 badread:
			     yyerror("Unexpected end of file while reading the interpass tmp file");
			     exit(1);
		    }
		}
	    }	/*end of using a real live file*/
	    (char *)tokub = (char *)bufptr + emptybuf->tok_count;
#ifdef DEBUG
	    firsttoken = bufptr;
	    if (debug)
		printf("created buffernumber %d with %d tokens\n",
			bufno, emptybuf->tok_count);
#endif DEBUG
	    goto top;
	}	/*end of reading/creating a new buffer*/
	tokptr = bufptr;		/*copy back the global value*/
	return(val);
}	/*end of yylex*/


buildskip(from, to)
	register	ptrall	from, to;
{
	int	diff;
	register	struct	tokbufdesc *middlebuf;
	/*
	 *	check if from and to are in the same buffer
	 *	from and to DIFFER BY AT MOST 1 buffer and to is
	 *	always ahead of from, with to being in the buffer emptybuf
	 *	points to.
	 *	The hard part here is accounting for the case where the
	 *	skip is to cross a buffer boundary; we must construct
	 *	two skips.
	 *
	 *	Figure out where the buffer boundary between from and to is
	 *	It's easy in VM, as buffers increase to high memory, but
	 *	w/o VM, we alternate between two buffers, and want
	 *	to look at the exact middle of the contiguous buffer region.
	 */
	middlebuf = useVM ? emptybuf : &tokbuf[1];
	if (  ( (bytetoktype *)from > (bytetoktype *)middlebuf)
	    ^ ( (bytetoktype *)to > (bytetoktype *)middlebuf)
	   ){	/*split across a buffer boundary*/
		ptoken(from, SKIP);
		/*
		 *	Set the skip so it lands someplace beyond
		 *	the end of this buffer.
		 *	When we pull this skip out in the second pass,
		 *	we will temporarily move the current pointer
		 *	out beyond the end of the buffer, but immediately
		 *	do a compare and fail the compare, and then reset
		 *	all the pointers correctly to point into the next buffer.
		 */
		bskiplg(from,  TOKBUFLG + 1);
		/*
		 *	Now, force from to be in the same buffer as to
		 */
		(bytetoktype *)from = (bytetoktype *)&(emptybuf->toks[0]);
	}
	/*
	 *	Now, to and from are in the same buffer
	 */
	if (from > to)
		yyerror("Internal error: bad skip construction");
	else {
		if ( (diff = (bytetoktype *)to - (bytetoktype *)from) >= 
			(sizeof(bytetoktype) + sizeof(lgtype) + 1)) {
				ptoken(from, SKIP);
				bskipfromto(from, to);
		} else {
			for ( ; diff > 0; --diff)
				ptoken(from, VOID);
		}
	}
}

movestr(to, from, lg)
	register	char	*to, *from;
	register	int	lg;
{
	if (lg <= 0) return;
	do
		*to++ = *from++;
	while (--lg);
}

new_dot_s(namep)
	char	*namep;
{
	newfflag = 1;
	newfname = namep;
	dotsname = namep;
	lineno = 1;
	scanlineno = 1;
}
