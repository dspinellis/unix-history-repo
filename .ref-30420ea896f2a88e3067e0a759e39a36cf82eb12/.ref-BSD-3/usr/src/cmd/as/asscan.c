/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include "as.h"
#include "asscan.h"

extern	int	d124;
extern 	struct	exp	*xp;

struct	tokbufdesc *bufstart;		/*where the buffer list begins*/
struct	tokbufdesc *buftail;		/*last one on the list*/
struct	tokbufdesc *emptybuf;		/*the one being filled*/

#define TOKDALLOP	8

int	useVM;				/*keep `tmp' file in virtual memory*/
int	bufno;				/*which buffer number: 0,1 for tmp file*/
struct 	tokbufdesc tokbuf[2];		/*our initial increment of buffers*/
	
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
				if (fwrite(&tokbuf[bufno ^ 1], sizeof *emptybuf, 1, tmpfil) != 1){
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
			if (fwrite(&tokbuf[bufno], sizeof *emptybuf, 1, tmpfil)
					!= 1) goto	badwrite;
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
#endif

extern	int		yylval;		/*global communication with parser*/

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
			case	INT:
					locxp = xp++;
					glong(locxp->xvalue, bufptr);
				  makevalue:
					locxp->xtype = XABS;
					locxp->xloc = 0;
					locxp->xname = NULL;
					yylval = (int)locxp;
					break;
			case	FLTNUM:	/*case patched on 3-Jan-80*/
					locxp = xp++;
					gdouble(locxp->doubval.dvalue, bufptr);
					/*
					 *	We make sure that locxp->xvalue
					 *	is not in the range suitable for
					 *	a short literal.  The field
					 *	xvalue is only used for
					 *	integers, not doubles, but when
					 *	we test for short literals
					 *	in ascode.c, we look
					 *	at the field xvalue when
					 *	it encounters an in line
					 *	floating number. Ergo,
					 *	give it a bad value.
					 */
					locxp->xvalue = -1;
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
					gptr(lastjxxx, bufptr);
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
		} /*end of the switch*/

#ifdef DEBUG

		if (toktrace)
		switch(val){
			case 	INT:	printf("Class integer val %d\n",
						((struct exp *)yylval)->xvalue);
					break;
			case 	FLTNUM: printf("Class floating point num value %4.3f\n",
					((struct exp *)yylval) -> doubval.dvalue);
					break;
			case	NAME:	printf("Class name, \"%.8s\"\n",
						((struct symtab *)yylval)->name);
					break;
			case	REG:	printf("Class register, number %d\n",
						yylval);
					break;
			case	INSTn:	printf("Class INSTn, %.8s\n",
						itab[0xFF &yylval]->name);
					break;
			case	IJXXX:	printf("Class IJXXX, %.8s\n",
						itab[0xFF &yylval]->name);
					break;
			case	INST0:	printf("Class INST0, %.8s\n",
						itab[0xFF &yylval]->name);
					break;
			case	STRING:	printf("Class string, length %d\n",
						((struct strdesc *)yylval)->str_lg);
					break;
			default:	printf("Pass: %d Tok: %d Other class: %d, 0%o, '%c'\n",
						passno,
						bufptr -  firsttoken,
						val,val, val);
					break;
		}		/*end of the debug switch*/
#endif

	}	/*end of this buffer*/
	else {
		if (useVM){
			bufno += 1;
			emptybuf = emptybuf->tok_next;
			if (emptybuf == 0){
				struct	tokbufdesc *newdallop;
				int	i;
				if (passno == 2)
					goto badread;
				emptybuf = newdallop = 
				  (struct tokbufdesc *)sbrk(
					TOKDALLOP*sizeof (struct tokbufdesc));
				if (emptybuf == (struct tokbufdesc *)-1)
					goto badwrite;
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
					if (fwrite(emptybuf, sizeof *emptybuf, 1, tmpfil) != 1){
					  badwrite:
						yyerror("Unexpected end of file writing the interpass tmp file");
						exit(2);
					}
				}
				scan_dot_s(emptybuf);
			} else {	/*pass 2*/
				if (fread(emptybuf, sizeof *emptybuf, 1, tmpfil) != 1){
				  badread:
					yyerror("Unexpected end of file while reading the interpass tmp file");
					exit(1);
				}
			}	/*end of pass2*/
		}	/*end of using a real live file*/
		(char *)tokub = (char *)bufptr + emptybuf->tok_count;
#ifdef DEBUG
		firsttoken = bufptr;
		if (debug)
			printf("created buffernumber %d with %d tokens\n",
				bufno, emptybuf->tok_count);
#endif
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
#define HEXUDIGIT	04		/* 'A' .. 'F' */
#define	ALPHA		010		/* 'A' .. 'Z', 'a' .. 'z', '_'*/
#define DIGIT		020		/* '0' .. '9' */
#define	FLOATEXP	040		/* 'd' 'e' 'D' 'E' */
						/*exponent field*/
#define SIGN		0100		/* '+' .. '-'*/
#define REGDIGIT	0200		/* '0' .. '5' */
#define SZSPECBEGIN	0400		/* 'b', 'B', 'l', 'L', 'w', 'W' */
#define POINT		01000		/* '.' */
#define SPACE		02000		/* '\t' or ' ' */
#define BSESCAPE	04000		/* bnrtf */
#define STRESCAPE	010000		/* '"', '\\', '\n' */
#define OCTDIGIT	020000		/* '0' .. '7' */
#define	FLOATFLAG	040000		/* 'd', 'D', 'f', 'F' */
						/*after leading 0*/

readonly short charsets[] = {
	0,	0,	0,	0,	0,	0,	0,	0,   /*\0..^G*/
	0,	SPACE,	STRESCAPE,0,	0,	0,	0,	0,   /*BS..SI*/
	0,	0,	0,	0,	0,	0,	0,	0,   /*DLE..ETB*/
	0,	0,	0,	0,	0,	0,	0,	0,   /*CAN..US*/
	SPACE,	0,	STRESCAPE,0,	0,	0,	0,	0,   /*sp.. '*/
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

#define INCHARSET(val, kind) (charsets[val] & (kind) )
static	toktype	oval = NL;

#define INBUFLG 2 + 2*BUFSIZ + 128
static	char	inbuffer[INBUFLG];
static	char	*InBufPtr = 0;

#ifdef  getchar
#undef	getchar
#endif
#define getchar() *inbufptr++

#ifdef  ungetc
#undef	ungetc
#endif
#define	ungetc(char, fileptr) *--inbufptr = char

char *fillinbuffer()
{
	register	char	*cp, *inbufptr;
	int		nread;

	inbufptr = &inbuffer[2];	/*allow enough room for two ungetcs*/
	nread = fread(inbufptr, 1, 2*BUFSIZ, stdin);
	if (nread == 2*BUFSIZ){
		cp = fgets(inbufptr+2*BUFSIZ, 128, stdin);	/*get next whole line*/
		if (cp != 0){
			while(*cp++);	/*find the trailing null*/
			*--cp = NEEDCHAR;	/*clobber with a NEED character*/
			return(inbufptr);
		} else {
			*(inbufptr + 2*BUFSIZ) = EOFCHAR;
			return(inbufptr);
		}
	} else {
		if (nread == 0)		/*hard end of file*/
			return(0);
		inbuffer[2+nread] = EOFCHAR;
		return(inbufptr);
	}
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

	register	ptrall	bufptr;		/*where to stuff tokens*/
			ptrall	lgbackpatch;	/*where to stuff a string length*/
			ptrall	bufub;		/*where not to stuff tokens*/
	register	int	maxstrlg;	/*how long a string can be*/
			long	intval;		/*value of int*/
			char	fltchr[64];	/*buffer for floating values*/
			double	fltval;		/*floating value returned*/
			int	linescrossed;	/*when doing strings and comments*/

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

	(toktype *)bufptr = (toktype *) & (bufferbox->toks[0]);	
	(toktype *)bufub = &(bufferbox->toks[AVAILTOKS]);

	if (newfflag){
#ifdef DEBUG
	if (debug)
		printf(">>>>>>>>>>>>>(scanner) Starting to insert tokens into a new file: %s\n",
				newfname);
#endif
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
				ungetc(val, stdin);
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
						ungetc(val, stdin);
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
				ungetc(val, stdin);
				val = REG;
			} else {
				ungetc(val, stdin);
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
					ungetc(val, stdin);
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
			ungetc(val, stdin);
			tag = (op = *lookup(1))->tag;
			if (tag && tag != LABELID){
				yylval = ( (struct instab *)op)->opcode;
				val = op->tag ;
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
			intval = val-'0';
			if (val=='0') {
				val = getchar();
				if (INCHARSET(val, HEXFLAG)){
					base = 16;
				} else
				if (INCHARSET(val, FLOATFLAG)){
					char *p = fltchr;
					double atof();
	
					while ( (p < &fltchr[63]) &&
					        INCHARSET(
							(val=getchar()),
							(DIGIT|SIGN|FLOATEXP|POINT)
						      )
					      ) *p++ = val;
					ungetc(val, stdin);
					*p++ = '\0';
					fltval = atof(fltchr);
					val = FLTNUM;
					goto ret;
				} else {
					ungetc(val, stdin);
					base = 8;
				}
			} else
				base = 10;
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
				intval += val-'0';
			}
			ungetc(val, stdin);
			val = INT;
			goto ret;
	
		case LSH:
		case RSH:
			/*
			 *	We allow the C style operators
			 *	<< and >>, as well as < and >
			 */
			if ( (base = getchar()) != val)
				ungetc(base, stdin);
			val = yylval;
			goto ret;

		case MINUS:
			if ( (val = getchar()) =='(')
				yylval=val=MP;
			else {
				ungetc(val,stdin);
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
				ungetc('"', stdin);
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
				ungetc('"', stdin);		/*will read it next*/
			}
			else if (val == '"');		/*done*/
			else if (val == '\n'){
				scanlineno++;
				linescrossed++;
				goto stuff;
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
				ungetc(val, stdin);
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
		case	INT:	plong(bufptr, intval);
				break;
		case 	FLTNUM:	pdouble(bufptr, fltval);
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
