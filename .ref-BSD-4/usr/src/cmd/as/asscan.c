/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)asscan.c 4.6 9/8/80";
#include <stdio.h>
#include "as.h"
#include "asscan.h"

/*
 *	NOTE:
 *		This version of the assembler does not use fread and fwrite
 *	for the token buffering.  The token buffers are integrals of BUFSIZ
 *	at all times, so we use direct read and write.  fread and fwrite
 *	as supplied from BTL in stdio are HORRENDOUSLY inefficient,
 *	as they use putchar for each character, nested two deep in loops.
 */
#define writeTEST(pointer, size, nelements, ioptr) \
	write(ioptr->_file, pointer, nelements * size) != nelements * size

#define readTEST(pointer, size, nelements, ioptr) \
	read(ioptr->_file, pointer, nelements * size) != nelements * size
/*
 *	Variables to manage the token buffering.
 *	We scan (lexically analyze) a large number of tokens, and
 *	then parse all of the tokens in the scan buffer.
 *	This reduces procedure call overhead when the parser
 *	demands a token, allows for an efficient reread during
 *	the second pass, and confuses the line number reporting
 *	for errors encountered in the scanner and in the parser.
 */
#define TOKDALLOP	8
struct	tokbufdesc *bufstart;	/*where the buffer list begins*/
struct	tokbufdesc *buftail;	/*last one on the list*/
struct	tokbufdesc *emptybuf;	/*the one being filled*/
/*
 *	If we are using VM, during the second pass we reclaim the used
 *	token buffers for saving the relocation information
 */
struct	tokbufdesc *tok_free;	/* free pool */
struct	tokbufdesc *tok_temp;	/* temporary for doing list manipulation */
/*
 *	Other token buffer managers
 */
int	bufno;			/*which buffer number: 0,1 for tmp file*/
struct 	tokbufdesc tokbuf[2];	/*our initial increment of buffers*/
ptrall	tokptr;			/*where the current token comes from*/
ptrall	tokub;			/*the last token in the current token buffer*/

/*
 *	Variables to manage the string buffering
 *	declared in asscan.h.
 */
int	strno;			/*the current string being filled*/
struct	strdesc	strbuf[3];	/*the string buffers; the first for nulls*/
struct	strdesc	*strptr;	/*current string buffer being filled*/
	
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

#define bstrlg(from, length) \
	*(lgtype *)from = length; \
	(char *)from += sizeof(lgtype) + length 

#define bstrfromto(from,to) \
	*(lgtype *)from = (char *)to - (char *)from - sizeof(lgtype); \
	(char *)from += sizeof(lgtype) + (char *)to - (char *)from

#define eatstrlg(from) \
	(char *)from +=  sizeof(lgtype) + *(lgtype *)from

#define bskiplg(from, length) \
	*(lgtype *)from = length; \
	(char *)from += sizeof(lgtype) + length

#define bskipfromto(from, to) \
	*(lgtype *)from = (toktype *)to - (toktype *)from - sizeof(lgtype); \
	(char *)from += sizeof (lgtype) + (toktype *)to - (toktype *)from

#define eatskiplg(from) \
	(toktype *)from += sizeof(lgtype) + *(lgtype *)from

#ifdef DEBUG
	ptrall	firsttoken;
#endif DEBUG

extern	int		yylval;		/*global communication with parser*/
static	int		Lastjxxx;	/*this ONLY shuts up cc; see below*/

toktype yylex()
{
	register	ptrall	bufptr;	
	register	toktype		val;	
	register	struct	exp	*locxp;

	bufptr = tokptr;		/*copy in the global value*/
   top:
	if (bufptr < tokub){
		gtoken(val, bufptr);
		switch(yylval = val){
		case	PARSEEOF :
				yylval = val = PARSEEOF;
				break;
		case	BFINT:
		case	INT:
				if (xp >= &explist[NEXP])
				     yyerror("Too many expressions; try simplyfing");
				else
				    locxp = xp++;
				glong(locxp->e_xvalue, bufptr);
				locxp->e_yvalue = 0;
			  makevalue:
				locxp->e_xtype = XABS;
				locxp->e_xloc = 0;
				locxp->e_xname = NULL;
				yylval = (int)locxp;
				break;
		case	FLTNUM:	
				if (xp >= &explist[NEXP])
				     yyerror("Too many expressions; try simplyfing");
				else
				    locxp = xp++;
				gdouble( ( (union Double *)locxp)->dvalue, bufptr);
				goto makevalue;
		case	QUAD:
				if (xp >= &explist[NEXP])
				     yyerror("Too many expressions; try simplyfing");
				else
				    locxp = xp++;
				glong(locxp->e_xvalue, bufptr);
				glong(locxp->e_yvalue, bufptr);
				yylval = val = INT;
				goto makevalue;
		case	NAME:
				gptr(yylval, bufptr);
				lastnam = (struct symtab *)yylval;
				break;
		case	SIZESPEC:
		case 	REG:
		case	INSTn:
		case	INST0:
				gchar(yylval, bufptr);
				break;
		case	IJXXX:
				gchar(yylval, bufptr);
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
		case	QUAD:	printf("val[msd] = 0x%x, val[lsd] = 0x%x.",
				((struct exp *)yylval)->e_xvalue,
				((struct exp *)yylval)->e_yvalue);
				break;
		case 	FLTNUM: printf("value %20.17f",
				((union Double *)yylval)->dvalue);
				break;
		case	NAME:	printf("\"%.8s\"",
					((struct symtab *)yylval)->s_name);
				break;
		case	REG:	printf(" r%d",
					yylval);
				break;
		case	IJXXX:
		case	INST0:	
		case	INSTn:	printf("%.8s",
					itab[0xFF &yylval]->s_name);
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
		(toktype *)bufptr = &(emptybuf->toks[0]);
		if (passno == 1)
			scan_dot_s(emptybuf);
	    } else {	/*don't use VM*/
		bufno ^= 1;
		emptybuf = &tokbuf[bufno];
		((toktype *)bufptr) = &(emptybuf->toks[0]);
		if (passno == 1){
			/*
			 *	First check if there are things to write
			 *	out at all
			 */
			if (emptybuf->tok_count >= 0){
			    if (writeTEST((char *)emptybuf, sizeof *emptybuf, 1, tmpfil)){
			      badwrite:
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
	register	int	frombufno;
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
	if (  ( (toktype *)from > (toktype *)middlebuf)
	    ^ ( (toktype *)to > (toktype *)middlebuf)
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
		(toktype *)from = (toktype *)&(emptybuf->toks[0]);
	}
	/*
	 *	Now, to and from are in the same buffer
	 */
	if (from > to)
		yyerror("Internal error: bad skip construction");
	else {
		if ( (diff = (toktype *)to - (toktype *)from) >= 
			(sizeof(toktype) + sizeof(lgtype) + 1)) {
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
static	int	newfflag = 0;
static	char	*newfname;
int	scanlineno;		/*the scanner's linenumber*/

new_dot_s(namep)
	char	*namep;
{
	newfflag = 1;
	newfname = namep;
	dotsname = namep;
	lineno = 1;
	scanlineno = 1;
}

/*
 *	Maps characters to their use in assembly language
 */
#define EOFCHAR	(-1)
#define	NEEDCHAR (-2)

readonly short type[] = {
	NEEDSBUF,		/*fill up the input buffer*/
	SCANEOF,		/*hit the hard end of file*/
	SP,	BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,   /*\0..^G*/
	BADCHAR,SP,	NL,	BADCHAR,BADCHAR,SP,	BADCHAR,BADCHAR,   /*BS..SI*/
	BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,   /*DLE..ETB*/
	BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,BADCHAR,   /*CAN..US*/
	SP,	ORNOT,	DQ,	SH,	LITOP,	REGOP,	AND,	SQ,  /*sp .. '*/
	LP,	RP,	MUL,	PLUS,	CM,	MINUS,	ALPH,	DIV, /*( .. /*/
	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG, /*0 .. 7*/
	DIG,	DIG,	COLON,	SEMI,	LSH,	BADCHAR,RSH,	BADCHAR, /*8 .. ?*/
	BADCHAR,ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*@ .. G*/
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*H .. BADCHAR*/
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*P .. V*/
	ALPH,	ALPH,	ALPH,	LB,	BADCHAR,RB,	XOR,	ALPH,/*W .. _*/
	SIZEQUOTE,ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*` .. g*/
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*h .. o*/
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,/*p .. v*/
	ALPH,	ALPH,	ALPH,	BADCHAR,IOR,	BADCHAR,TILDE,	BADCHAR,/*x .. del*/
};

/*
 *	The table of possible uses for each character to test set inclusion.
 *	Different than the above table, which knows about tokens yylex
 *	is to return.
 */
#define	HEXFLAG		01		/* 'x' or 'X' */
#define	HEXLDIGIT	02		/* 'a' .. 'f' */
#define	HEXUDIGIT	04		/* 'A' .. 'F' */
#define	ALPHA		010		/* 'A' .. 'Z', 'a' .. 'z', '_'*/
#define	DIGIT		020		/* '0' .. '9' */
#define	FLOATEXP	040		/* 'd' 'e' 'D' 'E' */
#define	SIGN		0100		/* '+' .. '-'*/
#define	REGDIGIT	0200		/* '0' .. '5' */
#define	SZSPECBEGIN	0400		/* 'b', 'B', 'l', 'L', 'w', 'W' */
#define	POINT		01000		/* '.' */
#define	SPACE		02000		/* '\t' or ' ' */
#define	BSESCAPE	04000		/* bnrtf */
#define	STRESCAPE	010000		/* '"', '\\', '\n' */
#define	OCTDIGIT	020000		/* '0' .. '7' */
#define	FLOATFLAG	040000		/* 'd', 'D', 'f', 'F' */
						/*after leading 0*/

readonly short charsets[] = {
	0,	0,	0,	0,	0,	0,	0,	0,   /*\0..^G*/
	0,	SPACE,	STRESCAPE,0,	0,	0,	0,	0,   /*BS..SI*/
	0,	0,	0,	0,	0,	0,	0,	0,   /*DLE..ETB*/
	0,	0,	0,	0,	0,	0,	0,	0,   /*CAN..US*/
/* dollar is an alpha character */
	SPACE,	0,	STRESCAPE,0,	ALPHA,	0,	0,	0,   /*sp.. '*/
	0,	0,	0,	SIGN,	0,	SIGN,	POINT+ALPHA,0, /*( .. /*/
	DIGIT+REGDIGIT+OCTDIGIT,	DIGIT+REGDIGIT+OCTDIGIT,     /*0..1*/
	DIGIT+REGDIGIT+OCTDIGIT,	DIGIT+REGDIGIT+OCTDIGIT,     /*2..3*/
	DIGIT+REGDIGIT+OCTDIGIT,	DIGIT+REGDIGIT+OCTDIGIT,     /*4..5*/
	DIGIT+OCTDIGIT,			DIGIT+OCTDIGIT,		     /*6..7*/
	DIGIT,	DIGIT,	0,	0,	0,	0,	0,	0,   /*8..?*/
	0,							     /*@*/
	ALPHA+HEXUDIGIT,ALPHA+HEXUDIGIT+SZSPECBEGIN,		     /*A..B*/
	ALPHA+HEXUDIGIT,ALPHA+HEXUDIGIT+FLOATEXP+FLOATFLAG,	     /*C..D*/
	ALPHA+HEXUDIGIT+FLOATEXP,ALPHA+HEXUDIGIT+FLOATFLAG,	     /*E..F*/
	ALPHA,							     /*G*/
	ALPHA,			ALPHA,	ALPHA, 	ALPHA,		     /*H..K*/
	ALPHA+SZSPECBEGIN, 	ALPHA,	ALPHA,	ALPHA,		     /*L..O*/
	ALPHA,			ALPHA,	ALPHA,	ALPHA,		     /*P..S*/
	ALPHA,			ALPHA,	ALPHA,	ALPHA+SZSPECBEGIN,   /*T..W*/
	ALPHA+HEXFLAG,	ALPHA,	ALPHA,	0,STRESCAPE,0,	0,	ALPHA,/*X.._*/

	0,
	ALPHA+HEXLDIGIT,ALPHA+HEXLDIGIT+BSESCAPE+SZSPECBEGIN,	      /*a..b*/
	ALPHA+HEXLDIGIT,ALPHA+HEXLDIGIT+FLOATEXP+FLOATFLAG,	      /*c..d*/
	ALPHA+HEXLDIGIT+FLOATEXP,ALPHA+HEXLDIGIT+BSESCAPE+FLOATFLAG,  /*e..f*/
	ALPHA,							      /*g*/
	ALPHA,			ALPHA,	ALPHA,		ALPHA,	      /*h..k*/
	ALPHA+SZSPECBEGIN,	ALPHA,	ALPHA+BSESCAPE,	ALPHA,	      /*l..o*/
	ALPHA,			ALPHA,	ALPHA+BSESCAPE,	ALPHA,	      /*p..s*/
	ALPHA+BSESCAPE,		ALPHA,	ALPHA,		ALPHA+SZSPECBEGIN,/*t..w*/
	ALPHA+HEXFLAG,	ALPHA,	ALPHA,	0,0,	0,	0,	0,    /*x..del*/
0};

#define	INCHARSET(val, kind) (charsets[val] & (kind) )
static	toktype	oval = NL;

#define	NINBUFFERS	2
#define	INBUFLG		NINBUFFERS*BUFSIZ + 2
	/*
	 *	We have two input buffers; the first one is reserved
	 *	for catching the tail of a line split across a buffer
	 *	boundary; the other one are used for snarfing a buffer
	 *	worth of .s source.
	 */
static	char	inbuffer[INBUFLG];
static	char	*InBufPtr = 0;

#ifdef	getchar
#undef		getchar
#endif
#define	getchar() *inbufptr++

#ifdef	ungetc
#undef		ungetc
#endif
#define	ungetc(char) *--inbufptr = char

/*
 *	fill the inbuffer from the standard input.
 *	Assert: there are always n COMPLETE! lines in the buffer area.
 *	Assert: there is always a \n terminating the last line
 *		in the buffer area.
 *	Assert: after the \n, there is an EOFCHAR (hard end of file)
 *		or a NEEDCHAR (end of buffer)
 *	Assert:	fgets always null pads the string it reads.
 *	Assert:	no ungetc's are done at the end of a line or at the
 *		beginning of a line.
 *	
 *	We read a complete buffer of characters in one single read.
 *	We then back scan within this buffer to find the end of the
 *	last complete line, and force the assertions, and save a pointer
 *	to the incomplete line.
 *	The next call to fillinbuffer will move the unread characters
 *	to the end of the first buffer, and then read another two buffers,
 *	completing the cycle.
 */

static	char	p_swapped = '\0';			
static	char	*p_start = &inbuffer[NINBUFFERS * BUFSIZ];
static	char	*p_stop = &inbuffer[NINBUFFERS * BUFSIZ];
char *fillinbuffer()
{
	register	char	*to;
	register	char	*from;
			char	*inbufptr;
	int		nread;

	*p_start = p_swapped;
	inbufptr = &inbuffer[1*BUFSIZ] - (p_stop - p_start);

	for (to = inbufptr, from = p_start; from < p_stop;)
		*to++ = *from++;
	/*
	 *	Now, go read two full buffers (hopefully)
	 */
	nread = read(stdin->_file, &inbuffer[1*BUFSIZ], (NINBUFFERS - 1)*BUFSIZ);
	if (nread == 0)
		return(0);
	p_stop = from = &inbuffer[1*BUFSIZ + nread];
	*from = '\0';
	while (*--from != '\n')		/* back over the partial line */
		continue;
	from++;				/* first char of partial line */
	p_start = from;
	p_swapped = *p_start;
	*p_start = NEEDCHAR;		/* force assertion */
	return(inbufptr);
}

scan_dot_s(bufferbox)
	struct tokbufdesc *bufferbox;
{
	register int		yylval;/*lexical value*/
	register toktype	val;	/*the value returned; the character read*/
	register int	base;		/*the base of the number also counter*/
	register	char	*cp;	
	register	char	*inbufptr;
	register	struct 		symtab	*op;
	register	unsigned	char	tag;
	int		forb;

	register	ptrall	bufptr;		/*where to stuff tokens*/
			ptrall	lgbackpatch;	/*where to stuff a string length*/
			ptrall	bufub;		/*where not to stuff tokens*/
	register	int	maxstrlg;	/*how long a string can be*/
			long	intval;		/*value of int*/
			char	fltchr[64];	/*buffer for floating values*/
		union	Double	fltval;		/*floating value returned*/
		struct	Quad	quadval;	/*quad returned from immediate constant */
			int	linescrossed;	/*when doing strings and comments*/

	(toktype *)bufptr = (toktype *) & (bufferbox->toks[0]);	
	(toktype *)bufub = &(bufferbox->toks[AVAILTOKS]);

	inbufptr = InBufPtr;
	if (inbufptr == 0){
		inbufptr = fillinbuffer();
		if (inbufptr == 0){	/*end of file*/
   		  endoffile:
			inbufptr = 0;
			ptoken(bufptr, PARSEEOF);
			goto done;
		}
	}

	if (newfflag){
		ptoken(bufptr, IFILE);
		ptoken(bufptr, STRING);
		val = strlen(newfname) + 1;
		movestr( (char *)&( ( (lgtype *)bufptr)[1]), newfname, val);
		bstrlg(bufptr, val);

		ptoken(bufptr, ILINENO);
		ptoken(bufptr, INT);
		pint(bufptr,  1);
		newfflag = 0;
	}

	while (bufptr < bufub){
   loop:
        switch(yylval = (type+2)[val = getchar()]) {
	case SCANEOF:
		inbufptr = 0;
		goto endoffile;

	case NEEDSBUF:
		inbufptr = fillinbuffer();
		if (inbufptr == 0)
			goto endoffile;
		goto loop;

	case DIV:		/*process C style comments*/
		if ( (val = getchar()) == '*') {  /*comment prelude*/
			int	incomment;
			linescrossed = 0;
			incomment = 1;
			val = getchar();	/*skip over the * */
			do{
				while ( (val != '*') &&
					(val != '\n') &&
					(val != EOFCHAR) &&
					(val != NEEDCHAR))
						val = getchar();
				if (val == '\n'){
					scanlineno++;
					linescrossed++;
				} else
				if (val == EOFCHAR)
					goto endoffile;
				if (val == NEEDCHAR){
					inbufptr = fillinbuffer();
					if (inbufptr == 0)
						goto endoffile;
					lineno++;
					incomment = 1;
					val = getchar(); /*pull in the new char*/
				} else { 	/*its a star */
					val = getchar();
					incomment = val != '/';
				}
			} while (incomment);
			val = ILINESKIP;
			yylval = linescrossed;
			goto ret;
		} else {	/*just an ordinary DIV*/
			ungetc(val);
			val = yylval = DIV;
			goto ret;
		}
	case SH:
		if (oval == NL){
			/*
			 *	Attempt to recognize a C preprocessor
			 *	style comment '^#[ \t]*[0-9]*[ \t]*".*"
			 */
			val = getchar();	/*bump the #*/
			while (INCHARSET(val, SPACE))
				val = getchar();/*bump white */
			if (INCHARSET(val, DIGIT)){
				intval = 0;
				while(INCHARSET(val, DIGIT)){
					intval = intval *10 + val - '0';
					val = getchar();
				}
				while (INCHARSET(val, SPACE))
					val = getchar();
				if (val == '"'){
					ptoken(bufptr, ILINENO);
					ptoken(bufptr, INT);
					pint(bufptr, intval - 1);
					ptoken(bufptr, IFILE);
					/*
					 *	The '"' has already been
					 *	munched
					 *	
					 *	eatstr will not eat
					 *	the trailing \n, so
					 *	it is given to the parser
					 *	and counted.
					 */
					goto eatstr;
				}
			}
		}
		/*
		 *	Well, its just an ordinary decadent comment
		 */
		while ((val != '\n') && (val != EOFCHAR)) 
			val = getchar();
		if (val == EOFCHAR)
			goto endoffile;
		val = yylval = oval = NL;
		scanlineno++;
		goto ret;

	case NL:
		scanlineno++;
		val = yylval;
		goto ret;

	case SP:
		oval = SP;	/*invalidate ^# meta comments*/
		goto loop;

	case REGOP:		/* % , could be used as modulo, or register*/
		val = getchar();
		if (INCHARSET(val, DIGIT)){
			yylval = val-'0';
			if (val=='1') {
				if (INCHARSET( (val = getchar()), REGDIGIT))
					yylval = 10+val-'0';
				else
					ungetc(val);
			}
			/*
			 *	God only knows what the original author
			 *	wanted this undocumented feature to
			 *	do.
			 *		%5++ is really  r7
			 */
			while(INCHARSET( (val = getchar()), SIGN)) {
				if (val=='+')
					yylval++;
				else
					yylval--;
			}
			ungetc(val);
			val = REG;
		} else {
			ungetc(val);
			val = REGOP;
		}
		goto ret;

	case ALPH:
		yylval = val;
		if (INCHARSET(val, SZSPECBEGIN)){
			if( (val = getchar()) == '`' || val == '^'){
				yylval |= 0100;	/*convert to lower*/
				if (yylval == 'b') yylval = 1;
				else if (yylval == 'w') yylval = 2;
				else if (yylval == 'l') yylval = 4;
				else			yylval = d124;
				val = SIZESPEC;
				goto ret;
			} else {
				ungetc(val);
				val = yylval;	/*restore first character*/
			}
		}
		cp = yytext;
		do {
			if (cp < &yytext[NCPS])
				*cp++ = val;
		} while (INCHARSET ( (val = getchar()), ALPHA | DIGIT));
		*cp = '\0';
		while (INCHARSET(val, SPACE))
			val = getchar();
		ungetc(val);
	doit:
		tag = (op = *lookup(1))->s_tag;
		if (tag && tag != LABELID){
			yylval = ( (struct instab *)op)->i_opcode;
			val = op->s_tag ;
			goto ret;
		} else {
			/*
			 *	Its a name... (Labels are subsets ofname)
			 */
			yylval = (int)op;
			val = NAME;
			goto ret;
		}

	case DIG:
		base = 10;
		cp = fltchr;
		intval = 0;
		if (val=='0') {
			val = getchar();
			if (val == 'b') {
				yylval = -1;
				val = BFINT;
				goto ret;
			} 
			if (val == 'f') {
				/*
				 *	Well, it appears to be a local label
				 *	reference, but check to see if
				 *	the next character makes it a floating
				 *	point constant.
				 */
				forb = getchar();
				ungetc(forb);
				if (!(INCHARSET(forb,(DIGIT|SIGN|FLOATEXP|POINT)))){
					yylval = 1;
					val = BFINT;
					goto ret;
				}
			}
			if (INCHARSET(val, HEXFLAG)){
				base = 16;
			} else
			if (INCHARSET(val, FLOATFLAG)){
				double atof();
				while ( (cp < &fltchr[63]) &&
				        INCHARSET(
						(val=getchar()),
						(DIGIT|SIGN|FLOATEXP|POINT)
					      )
				      ) *cp++ = val;
				if (cp == fltchr) {
					yylval = 1;
					val = BFINT;
					goto ret;
				}
				ungetc(val);
				*cp++ = '\0';
				fltval.dvalue = atof(fltchr);
				val = FLTNUM;
				goto ret;
			} else {
				ungetc(val);
				base = 8;
			}
		} else {
			forb = getchar();
			if (forb == 'f' || forb == 'b') {
				yylval = val - '0' + 1;
				if (forb == 'b')
					yylval = -yylval;
				val = BFINT;
				goto ret;
			}
			ungetc(forb);	/* put back non zero */
			goto middle;
		}
		while ( (val = getchar()) == '0')
			continue;
		ungetc(val);
		while ( INCHARSET( (val = getchar()), DIGIT) || 
		    	(base==16 && (INCHARSET(val, HEXLDIGIT|HEXUDIGIT) )
		      	   )
		      ){
			if (base==8)
				intval <<= 3;
			else if (base==10)
				intval *= 10;
			else {
				intval <<= 4;
				if (INCHARSET(val, HEXLDIGIT))
					val -= 'a' - 10 - '0';
				else if (INCHARSET(val, HEXUDIGIT))
					val -= 'A' - 10 - '0';
			}
middle:
			*cp++ = (val -= '0');
			intval += val;
		}
		ungetc(val);
		*cp = 0;
		maxstrlg = cp - fltchr;
		if (   (maxstrlg > 8)
		    && (   (   (base == 8)
			    && (   (maxstrlg>11)
				|| (   (maxstrlg == 11)
				    && (*fltchr > 3)
				   )
				)
			   )
			|| (   (base == 16)
			    && (maxstrlg > 8)
			   )
			|| (   (base == 10)
			    && (maxstrlg >= 10)
			   )
			)
		) {
			val = QUAD;
			get_quad(base, fltchr, cp, &quadval);
		} else
			val = INT;
		goto ret;

	case LSH:
	case RSH:
		/*
		 *	We allow the C style operators
		 *	<< and >>, as well as < and >
		 */
		if ( (base = getchar()) != val)
			ungetc(base);
		val = yylval;
		goto ret;

	case MINUS:
		if ( (val = getchar()) =='(')
			yylval=val=MP;
		else {
			ungetc(val);
			val=MINUS;
		}
		goto ret;

	case SQ:
		if ((yylval = getchar()) == '\n')
			scanlineno++;		/*not entirely correct*/
		intval = yylval;
		val = INT;
		goto ret;

	case DQ:
	   eatstr:
		linescrossed = 0;
		maxstrlg = (char *)bufub - (char *)bufptr;

		if (maxstrlg < MAXSTRLG) {
			ungetc('"');
			*(toktype *)bufptr = VOID ;
			bufub = bufptr;
			goto done;
		}
		if (maxstrlg > MAXSTRLG)
			maxstrlg = MAXSTRLG;
		
		ptoken(bufptr, STRING);
		lgbackpatch = bufptr;	/*this is where the size goes*/
		bufptr += sizeof(lgtype);
		/*
		 *	bufptr is now set to
		 *	be stuffed with characters from
		 *	the input
		 */

		while (   (maxstrlg > 0)
		       && !(INCHARSET( (val = getchar()), STRESCAPE))
		      ){
			stuff:
				maxstrlg-= 1;
				pchar(bufptr, val);
			}
		if (maxstrlg <= 0){	/*enough characters to fill a string buffer*/
			ungetc('"');		/*will read it next*/
		}
		else if (val == '"');		/*done*/
		else if (val == '\n'){
			yywarning("New line embedded in a string constant.");
			scanlineno++;
			linescrossed++;
			val = getchar();
			if (val == EOFCHAR){
			  do_eof:
				pchar(bufptr, '\n');
				ungetc(EOFCHAR);
			} else
			if (val == NEEDCHAR){
				if ( (inbufptr = fillinbuffer()) == 0)
					goto do_eof;
				val = '\n';
				goto stuff;
			} else {	/* simple case */
				ungetc(val);
				val = '\n';
				goto stuff;
			}
		} else {
			val = getchar();		/*skip the '\\'*/
			if ( INCHARSET(val, BSESCAPE)){
				switch (val){
				  case 'b':  val = '\b'; goto stuff;
				  case 'f':  val = '\f'; goto stuff;
				  case 'n':  val = '\n'; goto stuff;
				  case 'r':  val = '\r'; goto stuff;
				  case 't':  val = '\t'; goto stuff;
				}
			}
			if ( !(INCHARSET(val,OCTDIGIT)) )  goto stuff;
			base = 0;
			intval = 0;
			while ( (base < 3) && (INCHARSET(val, OCTDIGIT))){
				base++;intval <<= 3;intval += val - '0';
				val = getchar();
			}
			ungetc(val);
			val = (char)intval;
			goto stuff;
		}
		/*
		 *	bufptr now points at the next free slot
		 */
		bstrfromto(lgbackpatch, bufptr);
		if (linescrossed){
			val = ILINESKIP;
			yylval = linescrossed;
			goto ret;
		} else
			goto builtval;

	case BADCHAR:
		linescrossed = lineno;
		lineno = scanlineno;
		yyerror("Illegal character mapped: %d, char read:(octal) %o",
			yylval, val);
		lineno = linescrossed;
		val = BADCHAR;
		goto ret;

	default:
		val = yylval;
		goto ret;
	}	/*end of the switch*/
	/*
	 *	here with one token, so stuff it
	 */
	ret:	
	oval = val;
	ptoken(bufptr, val);
	switch(val){
		case	ILINESKIP:
				pint(bufptr, yylval);
				break;
		case	SIZESPEC:
				pchar(bufptr, yylval);
				break;
		case	BFINT:	plong(bufptr, yylval);
				break;
		case	INT:	plong(bufptr, intval);
				break;
		case	QUAD:	plong(bufptr, quadval.quad_low_long);
				plong(bufptr, quadval.quad_high_long);
				break;
		case 	FLTNUM:	pdouble(bufptr, fltval.dvalue);
				break;
		case	NAME:	pptr(bufptr, (int)(struct symtab *)yylval);
				break;
		case	REG:	pchar(bufptr, yylval);
				break;	
		case	INST0:
		case	INSTn:
				pchar(bufptr, yylval);
				break;
		case 	IJXXX:
				pchar(bufptr, yylval);
				pptr(bufptr, (int)(struct symtab *)symalloc());
				break;
		case	ISTAB:
		case	ISTABSTR:
		case	ISTABNONE:
		case	ISTABDOT:
		case	IALIGN:
				pptr(bufptr, (int)(struct symtab *)symalloc());
				break;
	/*
	 *	default:
	 */
	 }
	 builtval: ;
   }			/*end of the while to stuff the buffer*/
   done:
	bufferbox->tok_count = (toktype *)bufptr - &(bufferbox->toks[0]);

	/*
	 *	This is a real kludge:
	 *
	 *	We put the last token in the buffer to be  a MINUS
	 *	symbol.  This last token will never be picked up
	 *	in the normal way, but can be looked at during
	 *	a peekahead look that the short circuit expression
	 *	evaluator uses to see if an expression is complicated.
	 *
	 *	Consider the following situation:
	 *
	 *	.word	45		+	47
	 *        buffer 1      |  buffer 0
	 *	the peekahead would want to look across the buffer,
	 *	but will look in the buffer end zone, see the minus, and
	 *	fail.
	 */
	ptoken(bufptr, MINUS);
	InBufPtr = inbufptr;		/*copy this back*/
}

struct	Quad _quadtemp;
get_quad(radix, cp_start, cp_end, quadptr)
	int	radix;
	char	*cp_start, *cp_end;
	struct	Quad *quadptr;
{
	register		char	*cp = cp_start;	/* r11 */
	register	struct	Quad	*qp = quadptr;	/* r10 */
	register	long	temp;			/* r9 */

	asm("clrq (r10)");
	for (; cp < cp_end; cp++){
		switch (radix) {
			case 8:
				asm ("ashq $3, (r10), (r10)");
				break;
			case 16:
				asm ("ashq $4, (r10), (r10)");
				break;
			case 10:
				asm ("ashq	$1, (r10), __quadtemp");
				asm ("ashq	$3, (r10), (r10)");
				asm ("addl2	__quadtemp, (r10)");
				asm ("adwc	__quadtemp+4, 4(r10)");
				break;
		}
		asm ("cvtbl	(r11), r9");
		asm ("addl2	r9, (r10)");
		asm ("adwc	$0, 4(r10)");
	}
}
