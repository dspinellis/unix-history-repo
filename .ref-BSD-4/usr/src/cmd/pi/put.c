/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)put.c 1.3 10/2/80";

#include "whoami.h"
#include "opcode.h"
#include "0.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#endif PC

short	*obufp	= obuf;

/*
 * If DEBUG is defined, include the table
 * of the printing opcode names.
 */
#ifdef DEBUG
#include "OPnames.h"
#endif

#ifdef OBJ
/*
 * Put is responsible for the interpreter equivalent of code
 * generation.  Since the interpreter is specifically designed
 * for Pascal, little work is required here.
 */
put(a)
{
	register int *p, i;
	register char *cp;
	int n, subop, suboppr, op, oldlc, w;
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
	oldlc = lc;
	if (cgenflg < 0)
		/*
		 * code disabled - do nothing
		 */
		return (oldlc);
	p = &a;
	n = *p++;
	suboppr = subop = (*p>>8) & 0377;
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
		case O_LINO:
/*****
			if (line == codeline)
				return (oldlc);
			codeline = line;
*****/
		case O_NEW:
		case O_DISPOSE:
		case O_AS:
		case O_IND:
		case O_LVCON:
		case O_CON:
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
		case O_WRITEF:
		case O_MAX:
		case O_MIN:
		case O_PACK:
		case O_UNPACK:
		case O_ARGV:
		case O_CTTOT:
		case O_INCT:
		case O_RANG2:
		case O_RSNG2:
		case O_RANG42:
		case O_RSNG42:
			if (p[1] == 0)
				break;
		case O_CON2:
		case O_CON24:
			if (p[1] < 128 && p[1] >= -128) {
				suboppr = subop = p[1];
				p++;
				n--;
				if (op == O_CON2) {
					op = O_CON1;
					cp = otext[O_CON1];
				}
				if (op == O_CON24) {
					op = O_CON14;
					cp = otext[O_CON14];
				}
			}
			break;
		case O_CON8:
		    {
			short	*sp = &p[1];

#ifdef	DEBUG
			if ( opt( 'k' ) )
			    printf ( ")#%5d\tCON8\t%10.3f\n" ,
					lc - HEADER_BYTES ,
					* ( ( double * ) &p[1] ) );
#endif
			word ( op );
			for ( i = 1 ; i <= 4 ; i ++ )
			    word ( *sp ++ );
			return ( oldlc );
		    }
		default:
			if (op >= O_REL2 && op <= O_REL84) {
				if ((i = (subop >> 1) * 5 ) >= 30)
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
		case O_FOR1U:
		case O_FOR2U:
		case O_FOR4U:
		case O_FOR1D:
		case O_FOR2D:
		case O_FOR4D:
			/* relative addressing */
			p[1] -= ( unsigned ) lc + 2;
			break;
		case O_CONG:
			i = p[1];
			cp = * ( ( char ** ) &p[2] ) ;
#ifdef DEBUG
			if (opt('k'))
				printf(")#%5d\tCONG:%d\t%s\n",
					lc - HEADER_BYTES, i, cp);
#endif
			if (i <= 127)
				word(O_CON | i << 8);
			else {
				word(O_CON);
				word(i);
			}
			while (i > 0) {
				w = *cp ? *cp++ : ' ';
				w |= (*cp ? *cp++ : ' ') << 8;
				word(w);
				i -= 2;
			}
			return (oldlc);
		case O_CONC:
#ifdef DEBUG
			(string = "'x'")[1] = p[1];
#endif
			suboppr = 0;
			op = O_CON1;
			cp = otext[O_CON1];
			subop = p[1];
			goto around;
		case O_CONC4:
#ifdef DEBUG
			(string = "'x'")[1] = p[1];
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
				printf(")#%5d\tCASE1\t%d\n"
					, lc - HEADER_BYTES
					, ( int ) *( ( long * ) &p[1] ) );
#endif
			/*
			 * this to build a byte size case table 
			 * saving bytes across calls in casewrd
			 * so they can be put out by word()
			 */
			lc++;
			if ((unsigned) lc & 1)
				casewrd = *( ( long * ) &p[1] ) & 0377;
			else {
				lc -= 2;
				word (   casewrd
				       | ( ( int ) *( ( long * ) &p[1] ) << 8 ) );
			}
			return (oldlc);
		case O_CASE2:
#ifdef DEBUG
			if (opt('k'))
				printf(")#%5d\tCASE2\t%d\n"
					, lc - HEADER_BYTES
					, ( int ) *( ( long * ) &p[1] ) );
#endif
			word( ( short ) *( ( long * ) &p[1] ) );
			return (oldlc);
		case O_FCALL:
			if (p[1] == 0)
				goto longgen;
			/* and fall through */
		case O_PUSH:
			if (p[1] == 0)
				return (oldlc);
			if (p[1] < 128 && p[1] >= -128) {
				suboppr = subop = p[1];
				p++;
				n--;
				break;
			}
			goto longgen;
		case O_TRA4:
		case O_CALL:
		case O_FSAV:
		case O_GOTO:
		case O_NAM:
		case O_READE:
			/* absolute long addressing */
			p[1] -= HEADER_BYTES;
			goto longgen;
		case O_RV1:
		case O_RV14:
		case O_RV2:
		case O_RV24:
		case O_RV4:
		case O_RV8:
		case O_RV:
		case O_LV:
			if (p[1] < SHORTADDR && p[1] >= -SHORTADDR)
				break;
			else {
				op += O_LRV - O_RV;
				cp = otext[op];
			}
		case O_BEG:
		case O_NODUMP:
		case O_CON4:
		case O_CASE4:
		case O_RANG4:
		case O_RANG24:
		case O_RSNG4:
		case O_RSNG24:
		longgen:
		    {
			short	*sp = &p[1];
			long	*lp = &p[1];

			n = (n << 1) - 1;
			if ( op == O_LRV )
				n--;
#ifdef DEBUG
			if (opt('k'))
			    {
				printf( ")#%5d\t%s" , lc - HEADER_BYTES , cp+1 );
				if (suboppr)
					printf(":%1d", suboppr);
				for ( i = 1 ; i < n 
				    ; i += sizeof ( long )/sizeof ( short ) )
					printf( "\t%D " , *lp ++ );
				pchr ( '\n' );
			    }
#endif
			if ( op != O_CASE4 )
			    word ( op | subop<<8 );
			for ( i = 1 ; i < n ; i ++ )
			    word ( *sp ++ );
			return ( oldlc );
		    }
	}
#ifdef DEBUG
	if (opt('k')) {
		printf(")#%5d\t%s", lc - HEADER_BYTES, cp+1);
		if (suboppr)
			printf(":%d", suboppr);
		if (string)
			printf("\t%s",string);
		if (n > 1)
			pchr('\t');
		for (i=1; i<n; i++)
			printf("%d ", ( short ) p[i]);
		pchr('\n');
	}
#endif
	if (op != NIL)
		word(op | subop << 8);
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
	register int oldlc, len;
	register unsigned w;
	register char *strptr;

	if (cgenflg < 0)
		/* code is off - do nothing */
		return(NIL);
	if (ap->class != TYPE)
		ap = ap->type;
	if (ap->value[ NL_ELABEL ] != 0) {
		/* the list already exists */
		return( ap -> value[ NL_ELABEL ] );
	}
#	ifdef OBJ
	    oldlc = lc;
	    put(2, O_TRA, lc);
	    ap->value[ NL_ELABEL ] = lc;
#	endif OBJ
#	ifdef PC
	    putprintf( "	.data" , 0 );
	    putprintf( "	.align 1" , 0 );
	    ap -> value[ NL_ELABEL ] = getlab();
	    putlab( ap -> value[ NL_ELABEL ] );
#	endif PC
	/* number of scalars */
	next = ap->type;
	len = next->range[1]-next->range[0]+1;
#	ifdef OBJ
	    put(2, O_CASE2, len);
#	endif OBJ
#	ifdef PC
	    putprintf( "	.word %d" , 0 , len );
#	endif PC
	/* offsets of each scalar name */
	len = (len+1)*sizeof(short);
#	ifdef OBJ
	    put(2, O_CASE2, len);
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
		    put(2, O_CASE2, len);
#		endif OBJ
#		ifdef PC
		    putprintf( "	.word %d" , 0 , len );
#		endif PC
	} while (next = next->chain);
	/* list of scalar names */
	strptr = getnext(ap, &next);
#	ifdef OBJ
	    do	{
		    w = (unsigned) *strptr;
		    if (!*strptr++)
			    strptr = getnext(next, &next);
		    w |= *strptr << 8;
		    if (!*strptr++)
			    strptr = getnext(next, &next);
		    word(w);
	    } while (next);
	    /* jump over the mess */
	    patch(oldlc);
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

getnext(next, new)

	struct nl *next, **new;
{
	if (next != NIL) {
		next = next->chain;
		*new = next;
	}
	if (next == NIL)
		return("");
#ifdef OBJ
	if (opt('k') && cgenflg >= 0)
		printf(")#%5d\t\t\"%s\"\n", lc-HEADER_BYTES, next->symbol);
#endif
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

	if (cgenflg < 0)
		/*
		 * code disabled - do nothing
		 */
		return(lc);
#ifdef DEBUG
	if (opt('k'))
		printf(")#%5d\t.=.+%d\n", lc - HEADER_BYTES, n);
#endif
	for (i = even(n); i > 0; i -= 2)
		word(0);
}

putstr(sptr, padding)

	char *sptr;
	int padding;
{
	register unsigned short w;
	register char *strptr = sptr;
	register int pad = padding;

	if (cgenflg < 0)
		/*
		 * code disabled - do nothing
		 */
		return(lc);
#ifdef DEBUG
	if (opt('k'))
		printf(")#%5D\t\t\"%s\"\n", lc-HEADER_BYTES, strptr);
#endif
	if (pad == 0) {
		do	{
			w = (unsigned short) * strptr;
			if (w)
				w |= *++strptr << 8;
			word(w);
		} while (*strptr++);
	} else {
		do 	{
			w = (unsigned short) * strptr;
			if (w) {
				if (*++strptr)
					w |= *strptr << 8;
				else {
					w |= ' ' << 8;
					pad--;
				}
				word(w);
			}
		} while (*strptr++);
		while (pad > 1) {
			word('  ');
			pad -= 2;
		}
		if (pad == 1)
			word(' ');
		else
			word(0);
	}
}
#endif OBJ

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

/*
 * Patch repairs the branch
 * at location loc to come
 * to the current location.
 *	for PC, this puts down the label
 *	and the branch just references that label.
 *	lets here it for two pass assemblers.
 */
patch(loc)
{

#	ifdef OBJ
	    patchfil(loc, lc-loc-2, 1);
#	endif OBJ
#	ifdef PC
	    putlab( loc );
#	endif PC
}

#ifdef OBJ
patch4(loc)
{

	patchfil(loc, lc - HEADER_BYTES, 2);
}

/*
 * Patchfil makes loc+2 have value
 * as its contents.
 */
patchfil(loc, value, words)
	PTR_DCL loc;
	int value, words;
{
	register i;

	if (cgenflg < 0)
		return;
	if (loc > (unsigned) lc)
		panic("patchfil");
#ifdef DEBUG
	if (opt('k'))
		printf(")#\tpatch %u %d\n", loc - HEADER_BYTES, value);
#endif
	do {
		i = ((unsigned) loc + 2 - ((unsigned) lc & ~01777))/2;
		if (i >= 0 && i < 1024)
			obuf[i] = value;
		else {
			lseek(ofil, (long) loc+2, 0);
			write(ofil, &value, 2);
			lseek(ofil, (long) 0, 2);
		}
		loc += 2;
		value = value >> 16;
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
	if (i != 0 && write(ofil, obuf, i) != i)
		perror(obj), pexit(DIED);
	obufp = obuf;
}
#endif OBJ

/*
 * Getlab - returns the location counter.
 * included here for the eventual code generator.
 *	for PC, thank you!
 */
getlab()
{
#	ifdef OBJ

	    return (lc);
#	endif OBJ
#	ifdef PC
	    static long	lastlabel;

	    return ( ++lastlabel );
#	endif PC
}

/*
 * Putlab - lay down a label.
 *	for PC, just print the label name with a colon after it.
 */
putlab(l)
	int l;
{

#	ifdef PC
	    putprintf( PREFIXFORMAT , 1 , LABELPREFIX , l );
	    putprintf( ":" , 0 );
#	endif PC
	return (l);
}

