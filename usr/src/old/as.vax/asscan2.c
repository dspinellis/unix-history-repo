/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)asscan2.c 4.2 %G%";
#endif not lint

#include "asscanl.h"
static	inttoktype	oval = NL;

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
	reg	int	ryylval;	/* local copy of lexical value */
	extern	int	yylval;		/* global copy of lexical value */
	reg	int	val;		/* the value returned */
		int	i;		/* simple counter */
	reg	char	*rcp;	
		char	*cp;		/* can have address taken */
	reg	int	ch;		/* treated as a character */
		int	ch1;		/* shadow value */
	reg	char	*inbufptr;
		struct 	symtab	*op;

	reg	ptrall	bufptr;		/* where to stuff tokens */
		ptrall	lgbackpatch;	/* where to stuff a string length */
		ptrall	bufub;		/* where not to stuff tokens */
		int	maxstrlg;	/* how long a string can be */
		long	intval;		/* value of int */
		int	linescrossed;	/* when doing strings and comments */
		struct	Opcode		opstruct;

	(bytetoktype *)bufptr = (bytetoktype *) & (bufferbox->toks[0]);	
	(bytetoktype *)bufub = &(bufferbox->toks[AVAILTOKS]);

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
        switch(ryylval = (type+2)[ch = getchar()]) {
	case SCANEOF:
		inbufptr = 0;
		goto endoffile;

	case NEEDSBUF:
		inbufptr = fillinbuffer();
		if (inbufptr == 0)
			goto endoffile;
		goto loop;

	case DIV:		/*process C style comments*/
		if ( (ch = getchar()) == '*') {  /*comment prelude*/
			int	incomment;
			linescrossed = 0;
			incomment = 1;
			ch = getchar();	/*skip over the * */
			while(incomment){
				switch(ch){
				case '*':
					ch = getchar();
					incomment = (ch != '/');
					break;
				case '\n':
					scanlineno++;
					linescrossed++;
					ch = getchar();
					break;
				case EOFCHAR:
					goto endoffile;
				case NEEDCHAR:
					inbufptr = fillinbuffer();
					if (inbufptr == 0)
						goto endoffile;
					lineno++;
					ch = getchar();
					break;
				default:
					ch = getchar();
					break;
				}
			}
			val = ILINESKIP;
			ryylval = linescrossed;
			goto ret;
		} else {	/*just an ordinary DIV*/
			ungetc(ch);
			val = ryylval = DIV;
			goto ret;
		}
	case SH:
		if (oval == NL){
			/*
			 *	Attempt to recognize a C preprocessor
			 *	style comment '^#[ \t]*[0-9]*[ \t]*".*"
			 */
			ch = getchar();	/*bump the #*/
			while (INCHARSET(ch, SPACE))
				ch = getchar();/*bump white */
			if (INCHARSET(ch, DIGIT)){
				intval = 0;
				while(INCHARSET(ch, DIGIT)){
					intval = intval*10 + ch - '0';
					ch = getchar();
				}
				while (INCHARSET(ch, SPACE))
					ch = getchar();
				if (ch == '"'){
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
		while ((ch != '\n') && (ch != EOFCHAR)) 
			ch = getchar();
		if (ch == EOFCHAR)
			goto endoffile;
		val = ryylval = oval = NL;
		scanlineno++;
		goto ret;

	case NL:
		scanlineno++;
		val = ryylval;
		goto ret;

	case SP:
		oval = SP;	/*invalidate ^# meta comments*/
		goto loop;

	case REGOP:		/* % , could be used as modulo, or register*/
		ch = getchar();
		if (INCHARSET(ch, DIGIT)){
			ryylval = ch-'0';
			if (ch=='1') {
				if (INCHARSET( (ch = getchar()), REGDIGIT))
					ryylval = 10+ch-'0';
				else
					ungetc(ch);
			}
			/*
			 *	God only knows what the original author
			 *	wanted this undocumented feature to
			 *	do.
			 *		%5++ is really  r7
			 */
			while(INCHARSET( (ch = getchar()), SIGN)) {
				if (ch=='+')
					ryylval++;
				else
					ryylval--;
			}
			ungetc(ch);
			val = REG;
		} else {
			ungetc(ch);
			val = REGOP;
		}
		goto ret;

	case ALPH:
		ch1 = ch;
		if (INCHARSET(ch, SZSPECBEGIN)){
			if( (ch = getchar()) == '`' || ch == '^'){
				ch1 |= 0100;	/*convert to lower*/
				switch(ch1){
				case 'b':	ryylval = 1;	break;
				case 'w':	ryylval = 2;	break;
				case 'l':	ryylval = 4;	break;
				default:	ryylval = d124;	break;
				}
				val = SIZESPEC;
				goto ret;
			} else {
				ungetc(ch);
				ch = ch1;	/*restore first character*/
			}
		}
		rcp = yytext;
		do {
			if (rcp < &yytext[NCPS])
				*rcp++ = ch;
		} while (INCHARSET ( (ch = getchar()), ALPHA | DIGIT));
		*rcp = '\0';
		while (INCHARSET(ch, SPACE))
			ch = getchar();
		ungetc(ch);
	
		switch((op = *lookup(1))->s_tag){
		case 0:
		case LABELID:
			/*
			 *	Its a name... (Labels are subsets ofname)
			 */
			ryylval = (int)op;
			val = NAME;
			break;
		case INST0:
		case INSTn:
		case IJXXX:
			opstruct.Op_popcode = ( (struct instab *)op)->i_popcode;
			opstruct.Op_eopcode = ( (struct instab *)op)->i_eopcode;
			val = op->s_tag;
			break;
		default:
			ryylval = ( (struct instab *)op)->i_popcode;
			val = op->s_tag;
			break;
		}
		goto ret;

	case DIG:
		/*
		 *	Implement call by reference on a reg variable
		 */
		cp = inbufptr;
		val = number(ch, &cp);
		/*
		 *	yylval or yybignum has been stuffed as a side
		 *	effect to number(); get the global yylval
		 *	into our fast local copy in case it was an INT.
		 */
		ryylval = yylval;
		inbufptr = cp;
		goto ret;

	case LSH:
	case RSH:
		/*
		 *	We allow the C style operators
		 *	<< and >>, as well as < and >
		 */
		if ( (ch1 = getchar()) != ch)
			ungetc(ch1);
		val = ryylval;
		goto ret;

	case MINUS:
		if ( (ch = getchar()) =='(')
			ryylval=val=MP;
		else {
			ungetc(ch);
			val=MINUS;
		}
		goto ret;

	case SQ:
		if ((ryylval = getchar()) == '\n')
			scanlineno++;		/*not entirely correct*/
		val = INT;
		goto ret;

	case DQ:
	   eatstr:
		linescrossed = 0;
		maxstrlg = (char *)bufub - (char *)bufptr;

		if (maxstrlg < MAXSTRLG) {
			ungetc('"');
			*(bytetoktype *)bufptr = VOID ;
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
		       && !(INCHARSET( (ch = getchar()), STRESCAPE))
		      ){
			stuff:
				maxstrlg-= 1;
				pchar(bufptr, ch);
			}
		if (maxstrlg <= 0){	/*enough characters to fill a string buffer*/
			ungetc('"');		/*will read it next*/
		}
		else if (ch == '"');		/*done*/
		else if (ch == '\n'){
			yywarning("New line embedded in a string constant.");
			scanlineno++;
			linescrossed++;
			ch = getchar();
			if (ch == EOFCHAR){
			  do_eof:
				pchar(bufptr, '\n');
				ungetc(EOFCHAR);
			} else
			if (ch == NEEDCHAR){
				if ( (inbufptr = fillinbuffer()) == 0)
					goto do_eof;
				ch = '\n';
				goto stuff;
			} else {	/* simple case */
				ungetc(ch);
				ch = '\n';
				goto stuff;
			}
		} else {
			ch = getchar();		/*skip the '\\'*/
			if ( INCHARSET(ch, BSESCAPE)){
				switch (ch){
				  case 'b':  ch = '\b'; goto stuff;
				  case 'f':  ch = '\f'; goto stuff;
				  case 'n':  ch = '\n'; goto stuff;
				  case 'r':  ch = '\r'; goto stuff;
				  case 't':  ch = '\t'; goto stuff;
				}
			}
			if ( !(INCHARSET(ch,OCTDIGIT)) )  goto stuff;
			i = 0;
			intval = 0;
			while ( (i < 3) && (INCHARSET(ch, OCTDIGIT))){
				i++;intval <<= 3;intval += ch - '0';
				ch = getchar();
			}
			ungetc(ch);
			val = (char)intval;
			goto stuff;
		}
		/*
		 *	bufptr now points at the next free slot
		 */
		bstrfromto(lgbackpatch, bufptr);
		if (linescrossed){
			val = ILINESKIP;
			ryylval = linescrossed;
			goto ret;
		} else
			goto builtval;

	case BADCHAR:
		linescrossed = lineno;
		lineno = scanlineno;
		yyerror("Illegal character mapped: %d, char read:(octal) %o",
			ryylval, ch);
		lineno = linescrossed;
		val = BADCHAR;
		goto ret;

	default:
		val = ryylval;
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
				pint(bufptr, ryylval);
				break;
		case	SIZESPEC:
				pchar(bufptr, ryylval);
				break;
		case	BFINT:	plong(bufptr, ryylval);
				break;
		case	INT:	plong(bufptr, ryylval);
				break;
		case 	BIGNUM:	pnumber(bufptr, yybignum);
				break;
		case	NAME:	pptr(bufptr, (int)(struct symtab *)ryylval);
				break;
		case	REG:	pchar(bufptr, ryylval);
				break;	
		case	INST0:
		case	INSTn:
				popcode(bufptr, opstruct);
				break;
		case 	IJXXX:
				popcode(bufptr, opstruct);
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
	bufferbox->tok_count = (bytetoktype *)bufptr - &(bufferbox->toks[0]);

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
