/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)put.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "opcode.h"
#include "0.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#   include	"align.h"
#else
    short	*obufp	= obuf;
#endif

/*
 * If DEBUG is defined, include the table
 * of the printing opcode names.
 */
#ifdef DEBUG
#include "OPnames.h"
#endif

char showit[] = "'x'";

#ifdef OBJ
/*
 * Put is responsible for the interpreter equivalent of code
 * generation.  Since the interpreter is specifically designed
 * for Pascal, little work is required here.
 *
 * FIXME, this should be converted to use <varargs.h> or <stdarg.h>.
 */
/*VARARGS*/
put(a)
{
	register int *p, i;
	register char *cp;
	register short *sp;
	register long *lp;
	int n, subop, suboppr, op, oldlc;
	char *string;
	static int casewrd;

	/*
	 * It would be nice to do some more
	 * optimizations here.  The work
	 * done to collapse offsets in lval
	 * should be done here, the IFEQ etc
	 * relational operators could be used
	 * etc.
	 */
	oldlc = (int) lc; /* its either this or change put to return a char * */
	if ( !CGENNING )
		/*
		 * code disabled - do nothing
		 */
		return (oldlc);
	p = &a;
	n = *p++;
	suboppr = subop = (*p >> 8) & 0377;
	op = *p & 0377;
	string = 0;
#ifdef DEBUG
	if ((cp = otext[op]) == NIL) {
		printf("op= %o\n", op);
		panic("put");
	}
#endif
	switch (op) {
		case O_ABORT:
			cp = "*";
			break;
		case O_AS:
			switch(p[1]) {
			case 0:
				break;
			case 2:
				op = O_AS2;
				n = 1;
				break;
			case 4:
				op = O_AS4;
				n = 1;
				break;
			case 8:
				op = O_AS8;
				n = 1;
				break;
			default:
				goto pack;
			}
#			ifdef DEBUG
				cp = otext[op];
#			endif DEBUG
			break;
		case O_FOR1U:
		case O_FOR2U:
		case O_FOR4U:
		case O_FOR1D:
		case O_FOR2D:
		case O_FOR4D:
			/* relative addressing */
			p[1] -= ( unsigned ) lc + sizeof(short);
			/* try to pack the jump */
			if (p[1] <= 127 && p[1] >= -128) {
				suboppr = subop = p[1];
				p++;
				n--;
			} else {
				/* have to allow for extra displacement */
				p[1] -= sizeof(short);
			}
			break;
		case O_CONG:
		case O_LVCON:
		case O_CON:
		case O_LINO:
		case O_NEW:
		case O_DISPOSE:
		case O_DFDISP:
		case O_IND:
		case O_OFF:
		case O_INX2:
		case O_INX4:
		case O_CARD:
		case O_ADDT:
		case O_SUBT:
		case O_MULT:
		case O_IN:
		case O_CASE1OP:
		case O_CASE2OP:
		case O_CASE4OP:
		case O_FRTN:
		case O_WRITES:
		case O_WRITEC:
		case O_WRITEF:
		case O_MAX:
		case O_MIN:
		case O_ARGV:
		case O_CTTOT:
		case O_INCT:
		case O_RANG2:
		case O_RSNG2:
		case O_RANG42:
		case O_RSNG42:
		case O_SUCC2:
		case O_SUCC24:
		case O_PRED2:
		case O_PRED24:
			if (p[1] == 0)
				break;
		case O_CON2:
		case O_CON24:
		pack:
			if (p[1] <= 127 && p[1] >= -128) {
				suboppr = subop = p[1];
				p++;
				n--;
				if (op == O_CON2) {
					op = O_CON1;
#					ifdef DEBUG
						cp = otext[O_CON1];
#					endif DEBUG
				}
				if (op == O_CON24) {
					op = O_CON14;
#					ifdef DEBUG
						cp = otext[O_CON14];
#					endif DEBUG
				}
			}
			break;
		case O_CON8:
		    {
			short	*sp = (short *) (&p[1]);

#ifdef	DEBUG
			if ( opt( 'k' ) )
			    printf ( "%5d\tCON8\t%22.14e\n" ,
					lc - HEADER_BYTES ,
					* ( ( double * ) &p[1] ) );
#endif
#			ifdef DEC11
			    word(op);
#			else
			    word(op << 8);
#			endif DEC11
			for ( i = 1 ; i <= 4 ; i ++ )
			    word ( *sp ++ );
			return ( oldlc );
		    }
		default:
			if (op >= O_REL2 && op <= O_REL84) {
				if ((i = (subop >> INDX) * 5 ) >= 30)
					i -= 30;
				else
					i += 2;
#ifdef DEBUG
				string = &"IFEQ\0IFNE\0IFLT\0IFGT\0IFLE\0IFGE"[i];
#endif
				suboppr = 0;
			}
			break;
		case O_IF:
		case O_TRA:
/*****
			codeline = 0;
*****/
			/* relative addressing */
			p[1] -= ( unsigned ) lc + sizeof(short);
			break;
		case O_CONC:
#ifdef DEBUG
			(string = showit)[1] = p[1];
#endif
			suboppr = 0;
			op = O_CON1;
#			ifdef DEBUG
				cp = otext[O_CON1];
#			endif DEBUG
			subop = p[1];
			goto around;
		case O_CONC4:
#ifdef DEBUG
			(string = showit)[1] = p[1];
#endif
			suboppr = 0;
			op = O_CON14;
			subop = p[1];
			goto around;
		case O_CON1:
		case O_CON14:
			suboppr = subop = p[1];
around:
			n--;
			break;
		case O_CASEBEG:
			casewrd = 0;
			return (oldlc);
		case O_CASEEND:
			if ((unsigned) lc & 1) {
				lc--;
				word(casewrd);
			}
			return (oldlc);
		case O_CASE1:
#ifdef DEBUG
			if (opt('k'))
				printf("%5d\tCASE1\t%d\n"
					, lc - HEADER_BYTES, p[1]);
#endif
			/*
			 * this to build a byte size case table 
			 * saving bytes across calls in casewrd
			 * so they can be put out by word()
			 */
			lc++;
			if ((unsigned) lc & 1)
#				ifdef DEC11
				    casewrd = p[1] & 0377;
#				else
				    casewrd = (p[1] & 0377) << 8;
#				endif DEC11
			else {
				lc -= 2;
#				ifdef DEC11
				    word(((p[1] & 0377) << 8) | casewrd);
#				else
				    word((p[1] & 0377) | casewrd);
#				endif DEC11
			}
			return (oldlc);
		case O_CASE2:
#ifdef DEBUG
			if (opt('k'))
				printf("%5d\tCASE2\t%d\n"
					, lc - HEADER_BYTES , p[1]);
#endif
			word(p[1]);
			return (oldlc);
		case O_PUSH:
			lp = (long *)&p[1];
			if (*lp == 0)
				return (oldlc);
			/* and fall through */
		case O_RANG4:
		case O_RANG24:
		case O_RSNG4:
		case O_RSNG24:
		case O_SUCC4:
		case O_PRED4:
			/* sub opcode optimization */
			lp = (long *)&p[1];
			if (*lp < 128 && *lp >= -128 && *lp != 0) {
				suboppr = subop = *lp;
				p += (sizeof(long) / sizeof(int));
				n--;
			}
			goto longgen;
		case O_TRA4:
		case O_CALL:
		case O_FSAV:
		case O_GOTO:
		case O_NAM:
		case O_READE:
			/* absolute long addressing */
			lp = (long *)&p[1];
			*lp -= HEADER_BYTES;
			goto longgen;
		case O_RV1:
		case O_RV14:
		case O_RV2:
		case O_RV24:
		case O_RV4:
		case O_RV8:
		case O_RV:
		case O_LV:
			/*
			 * positive offsets represent arguments
			 * and must use "ap" display entry rather
			 * than the "fp" entry
			 */
			if (p[1] >= 0) {
				subop++;
				suboppr++;
			}
#			ifdef PDP11
			    break;
#			else
			    /*
			     * offsets out of range of word addressing
			     * must use long offset opcodes
			     */
			    if (p[1] < SHORTADDR && p[1] >= -SHORTADDR)
				    break;
			    else {
				op += O_LRV - O_RV;
#				ifdef DEBUG
				    cp = otext[op];
#				endif DEBUG
			    }
			    /* and fall through */
#			endif PDP11
		case O_BEG:
		case O_NODUMP:
		case O_CON4:
		case O_CASE4:
		longgen:
			n = (n << 1) - 1;
			if ( op == O_LRV ) {
				n--;
#				if defined(ADDR32) && !defined(DEC11)
				    p[n / 2] <<= 16;
#				endif
			}
#ifdef DEBUG
			if (opt('k')) {
				printf("%5d\t%s", lc - HEADER_BYTES, cp+1);
				if (suboppr)
					printf(":%d", suboppr);
				for ( i = 2, lp = (long *)&p[1]; i < n 
				    ; i += sizeof ( long )/sizeof ( short ) )
					printf( "\t%D " , *lp ++ );
				if (i == n) {
					sp = (short *)lp;
					printf( "\t%d ", *sp );
				}
				pchr ( '\n' );
			}
#endif
			if ( op != O_CASE4 )
#				ifdef DEC11
			    	    word((op & 0377) | subop << 8);
#				else
				    word(op << 8 | (subop & 0377));
#				endif DEC11
			for ( i = 1, sp = (short *)&p[1]; i < n; i++)
				word ( *sp ++ );
			return ( oldlc );
	}
#ifdef DEBUG
	if (opt('k')) {
		printf("%5d\t%s", lc - HEADER_BYTES, cp+1);
		if (suboppr)
			printf(":%d", suboppr);
		if (string)
			printf("\t%s",string);
		if (n > 1)
			pchr('\t');
		for (i=1; i<n; i++)
			printf("%d ", p[i]);
		pchr('\n');
	}
#endif
	if (op != NIL)
#		ifdef DEC11
		    word((op & 0377) | subop << 8);
#		else
		    word(op << 8 | (subop & 0377));
#		endif DEC11
	for (i=1; i<n; i++)
		word(p[i]);
	return (oldlc);
}
#endif OBJ

/*
 * listnames outputs a list of enumerated type names which
 * can then be selected from to output a TSCAL
 * a pointer to the address in the code of the namelist
 * is kept in value[ NL_ELABEL ].
 */
listnames(ap)

	register struct nl *ap;
{
	struct nl *next;
#ifdef OBJ
	register int oldlc;
#endif
	register int len;
	register unsigned w;
	register char *strptr;

	if ( !CGENNING )
		/* code is off - do nothing */
		return(NIL);
	if (ap->class != TYPE)
		ap = ap->type;
	if (ap->value[ NL_ELABEL ] != 0) {
		/* the list already exists */
		return( ap -> value[ NL_ELABEL ] );
	}
#	ifdef OBJ
	    oldlc = (int) lc; /* same problem as put */
	    (void) put(2, O_TRA, lc);
	    ap->value[ NL_ELABEL ] = (int) lc;
#	endif OBJ
#	ifdef PC
	    putprintf("	.data", 0);
	    aligndot(A_STRUCT);
	    ap -> value[ NL_ELABEL ] = (int) getlab();
	    (void) putlab((char *) ap -> value[ NL_ELABEL ] );
#	endif PC
	/* number of scalars */
	next = ap->type;
	len = next->range[1]-next->range[0]+1;
#	ifdef OBJ
	    (void) put(2, O_CASE2, len);
#	endif OBJ
#	ifdef PC
	    putprintf( "	.word %d" , 0 , len );
#	endif PC
	/* offsets of each scalar name */
	len = (len+1)*sizeof(short);
#	ifdef OBJ
	    (void) put(2, O_CASE2, len);
#	endif OBJ
#	ifdef PC
	    putprintf( "	.word %d" , 0 , len );
#	endif PC
	next = ap->chain;
	do	{
		for(strptr = next->symbol;  *strptr++;  len++)
			continue;
		len++;
#		ifdef OBJ
		    (void) put(2, O_CASE2, len);
#		endif OBJ
#		ifdef PC
		    putprintf( "	.word %d" , 0 , len );
#		endif PC
	} while (next = next->chain);
	/* list of scalar names */
	strptr = getnext(ap, &next);
#	ifdef OBJ
	    do	{
#		    ifdef DEC11
			w = (unsigned) *strptr;
#		    else
			w = *strptr << 8;
#		    endif DEC11
		    if (!*strptr++)
			    strptr = getnext(next, &next);
#		    ifdef DEC11
			w |= *strptr << 8;
#		    else
			w |= (unsigned) *strptr;
#		    endif DEC11
		    if (!*strptr++)
			    strptr = getnext(next, &next);
		    word((int) w);
	    } while (next);
	    /* jump over the mess */
	    patch((PTR_DCL) oldlc);
#	endif OBJ
#	ifdef PC
	    while ( next ) {
		while ( *strptr ) {
		    putprintf( "	.byte	0%o" , 1 , *strptr++ );
		    for ( w = 2 ; ( w <= 8 ) && *strptr ; w ++ ) {
			putprintf( ",0%o" , 1 , *strptr++ );
		    }
		    putprintf( "" , 0 );
		}
		putprintf( "	.byte	0" , 0 );
		strptr = getnext( next , &next );
	    }
	    putprintf( "	.text" , 0 );
#	endif PC
	return( ap -> value[ NL_ELABEL ] );
}

char *
getnext(next, new)

	struct nl *next, **new;
{
	if (next != NIL) {
		next = next->chain;
		*new = next;
	}
	if (next == NLNIL)
		return("");
#ifdef OBJ
	if (opt('k') && CGENNING )
		printf("%5d\t\t\"%s\"\n", lc-HEADER_BYTES, next->symbol);
#endif OBJ
	return(next->symbol);
}

#ifdef OBJ
/*
 * Putspace puts out a table
 * of nothing to leave space
 * for the case branch table e.g.
 */
putspace(n)
	int n;
{
	register i;

	if ( !CGENNING )
		/*
		 * code disabled - do nothing
		 */
		return;
#ifdef DEBUG
	if (opt('k'))
		printf("%5d\t.=.+%d\n", lc - HEADER_BYTES, n);
#endif
	for (i = n; i > 0; i -= 2)
		word(0);
}

putstr(sptr, padding)

	char *sptr;
	int padding;
{
	register unsigned short w;
	register char *strptr = sptr;
	register int pad = padding;

	if ( !CGENNING )
		/*
		 * code disabled - do nothing
		 */
		return;
#ifdef DEBUG
	if (opt('k'))
		printf("%5d\t\t\"%s\"\n", lc-HEADER_BYTES, strptr);
#endif
	if (pad == 0) {
		do	{
#			ifdef DEC11
			    w = (unsigned short) * strptr;
#			else
			    w = (unsigned short)*strptr<<8;
#			endif DEC11
			if (w)
#				ifdef DEC11
				    w |= *++strptr << 8;
#				else
				    w |= *++strptr;
#				endif DEC11
			word((int) w);
		} while (*strptr++);
	} else {
#		ifdef DEC11
		    do 	{
			    w = (unsigned short) * strptr;
			    if (w) {
				    if (*++strptr)
					    w |= *strptr << 8;
				    else {
					    w |= ' ' << 8;
					    pad--;
				    }
				    word((int) w);
			    }
		    } while (*strptr++);
#		else
		    do 	{
			    w = (unsigned short)*strptr<<8;
			    if (w) {
				    if (*++strptr)
					    w |= *strptr;
				    else {
					    w |= ' ';
					    pad--;
				    }
				    word(w);
			    }
		    } while (*strptr++);
#		endif DEC11
		while (pad > 1) {
#			ifdef DEC11
			    word(' ' | (' ' << 8));
#			else
			    word((' ' << 8) | ' ');
#			endif DEC11
			pad -= 2;
		}
		if (pad == 1)
#			ifdef DEC11
			    word(' ');
#			else
			    word(' ' << 8);
#			endif DEC11
		else
			word(0);
	}
}
#endif OBJ

#ifndef PC
lenstr(sptr, padding)

	char *sptr;
	int padding;

{
	register int cnt;
	register char *strptr = sptr;

	cnt = padding;
	do	{
		cnt++;
	} while (*strptr++);
	return((++cnt) & ~1);
}
#endif

/*
 * Patch repairs the branch
 * at location loc to come
 * to the current location.
 *	for PC, this puts down the label
 *	and the branch just references that label.
 *	lets here it for two pass assemblers.
 */
patch(loc)
    PTR_DCL loc;
{

#	ifdef OBJ
	    patchfil(loc, (long)(lc-loc-2), 1);
#	endif OBJ
#	ifdef PC
	    (void) putlab((char *) loc );
#	endif PC
}

#ifdef OBJ
patch4(loc)
PTR_DCL loc;
{
	patchfil(loc, (long)(lc - HEADER_BYTES), 2);
}

/*
 * Patchfil makes loc+2 have jmploc
 * as its contents.
 */
patchfil(loc, jmploc, words)
	PTR_DCL loc;
	long jmploc;
	int words;
{
	register i;
	extern long lseek();
	short val;

	if ( !CGENNING )
		return;
	if (loc > (unsigned) lc)
		panic("patchfil");
#ifdef DEBUG
	if (opt('k'))
		printf("\tpatch %u %D\n", loc - HEADER_BYTES, jmploc);
#endif
	val = jmploc;
	do {
#		ifndef DEC11
		    if (words > 1)
			    val = jmploc >> 16;
		    else
			    val = jmploc;
#		endif DEC11
		i = ((unsigned) loc + 2 - ((unsigned) lc & ~01777))/2;
		if (i >= 0 && i < 1024) {
			obuf[i] = val;
		} else {
			(void) lseek(ofil, (long) loc+2, 0);
			write(ofil, (char *) (&val), 2);
			(void) lseek(ofil, (long) 0, 2);
		}
		loc += 2;
#		ifdef DEC11
		    val = jmploc >> 16;
#		endif DEC11
	} while (--words);
}

/*
 * Put the word o into the code
 */
word(o)
	int o;
{

	*obufp = o;
	obufp++;
	lc += 2;
	if (obufp >= obuf+512)
		pflush();
}

extern char	*obj;
/*
 * Flush the code buffer
 */
pflush()
{
	register i;

	i = (obufp - ( ( short * ) obuf ) ) * 2;
	if (i != 0 && write(ofil, (char *) obuf, i) != i)
		perror(obj), pexit(DIED);
	obufp = obuf;
}
#endif OBJ

/*
 * Getlab - returns the location counter.
 * included here for the eventual code generator.
 *	for PC, thank you!
 */
char *
getlab()
{
#	ifdef OBJ

	    return (lc);
#	endif OBJ
#	ifdef PC
	    static long	lastlabel;

	    return ( (char *) ++lastlabel );
#	endif PC
}

/*
 * Putlab - lay down a label.
 *	for PC, just print the label name with a colon after it.
 */
char *
putlab(l)
	char *l;
{

#	ifdef PC
	    putprintf( PREFIXFORMAT , 1 , (int) LABELPREFIX , (int) l );
	    putprintf( ":" , 0 );
#	endif PC
	return (l);
}
