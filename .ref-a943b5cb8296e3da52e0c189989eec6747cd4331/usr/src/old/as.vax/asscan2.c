/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)asscan2.c	5.1 (Berkeley) %G%";
#endif not lint

#include "asscanl.h"

static	inttoktype	oval = NL;
#define	ASINBUFSIZ	4096
char	inbufunget[8];
char	inbuffer[ASINBUFSIZ];
char	*Ginbufptr = inbuffer;
int	Ginbufcnt = 0;
int	scannerhadeof;

fillinbuffer()
{
		int	nread;
		int	goal;
		int	got;

	nread = 0;
	if (scannerhadeof == 0){
		goal = sizeof(inbuffer);
		do {
			got = read(stdin->_file, inbuffer + nread, goal);
			if (got == 0)
				scannerhadeof = 1;
			if (got <= 0)
				break;
			nread += got;
			goal -= got;
		} while (goal);
	} else {
		scannerhadeof = 0;
	}
	/*
	 *	getchar assumes that Ginbufcnt and Ginbufptr
	 *	are adjusted as if one character has been removed
	 *	from the input.
	 */
	if (nread == 0){
		inbuffer[0] = EOFCHAR;
		nread = 1;
	}
	Ginbufcnt = nread - 1;
	Ginbufptr = inbuffer + 1;
}

scan_dot_s(bufferbox)
	struct tokbufdesc *bufferbox;
{
	reg	char	*inbufptr;
	reg	int	inbufcnt;
	reg	int	ryylval;	/* local copy of lexical value */
	extern	int	yylval;		/* global copy of lexical value */
	reg	int	val;		/* the value returned */
		int	i;		/* simple counter */
	reg	char	*rcp;	
		int	ch;		/* treated as a character */
		int	ch1;		/* shadow value */
		struct 	symtab	*op;
		ptrall	lgbackpatch;	/* where to stuff a string length */
	reg	ptrall	bufptr;		/* where to stuff tokens */
		ptrall	bufub;		/* where not to stuff tokens */
		long	intval;		/* value of int */
		int	linescrossed;	/* when doing strings and comments */
		struct	Opcode		opstruct;
	reg	int	strlg;		/* the length of a string */

	(bytetoktype *)bufptr = (bytetoktype *) & (bufferbox->toks[0]);	
	(bytetoktype *)bufub = &(bufferbox->toks[AVAILTOKS]);

	MEMTOREGBUF;
	if (newfflag){
		newfflag = 0;
		ryylval = (int)savestr(newfname, strlen(newfname)+1, STR_BOTH);

		ptoken(bufptr, IFILE);
		ptoken(bufptr, STRING);
		pptr(bufptr, ryylval);

		ptoken(bufptr, ILINENO);
		ptoken(bufptr, INT);
		pint(bufptr,  1);
	}

	while (bufptr < bufub){
   loop:
        switch(ryylval = (type+1)[ch = getchar()]) {
	case SCANEOF:
	endoffile: ;
		inbufptr = 0;
		ptoken(bufptr, PARSEEOF);
		goto done;

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
			if (rcp < &yytext[NCPName])
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
			 *	Its a name... (Labels are subsets of name)
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
		 *	restore local inbufptr and inbufcnt
		 */
		REGTOMEMBUF;
		val = number(ch);
		MEMTOREGBUF;
		/*
		 *	yylval or yybignum has been stuffed as a side
		 *	effect to number(); get the global yylval
		 *	into our fast local copy in case it was an INT.
		 */
		ryylval = yylval;
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
		for (strlg = 0; /*VOID*/; strlg++){
		    switch(ch = getchar()){
		    case '"':
			goto tailDQ;
		    default:
		    stuff:
			putc(ch, strfile);
			break;
		    case '\n':
			yywarning("New line in a string constant");
			scanlineno++;
			linescrossed++;
			ch = getchar();
			switch(ch){
			case EOFCHAR:
				putc('\n', strfile);
				ungetc(EOFCHAR);
				goto tailDQ;
			default:
				ungetc(ch);
				ch = '\n';
				goto stuff;
			}
			break;

		    case '\\':
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
			if ( !(INCHARSET(ch, OCTDIGIT)) ) 
				goto stuff;
			i = 0;
			intval = 0;
			while ( (i < 3) && (INCHARSET(ch, OCTDIGIT))){
				i++;
				intval <<= 3;
				intval += ch - '0';
				ch = getchar();
			}
			ungetc(ch);
			ch = (char)intval;
			goto stuff;
		    }
		}
	tailDQ: ;
		/*
		 *	account for any lines that were crossed
		 */
		if (linescrossed){
			ptoken(bufptr, ILINESKIP);
			pint(bufptr, linescrossed);
		}
		/*
		 *	Cheat: append a trailing null to the string
		 *	and then adjust the string length to ignore
		 *	the trailing null.  If any STRING client requires
		 *	the trailing null, the client can just change STRLEN
		 */
		putc(0, strfile);
		ryylval = (int)savestr((char *)0, strlg + 1, STR_FILE);
		val = STRING;
		((struct strdesc *)ryylval)->sd_strlen -= 1;
		goto ret;

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
		case	STRING:	pptr(bufptr, (int)(char *)ryylval);
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
	REGTOMEMBUF;
}
