#ifndef lint
static char sccsid[] = "@(#)n8.c	2.1 (CWI) 85/07/18";
#endif lint
#include	<ctype.h>
#include	"tdef.h"
#include <sgtty.h>
#include "ext.h"
#define	HY_BIT	0200	/* stuff in here only works for ascii */

/*
 * troff8.c
 * 
 * hyphenation
 */

char	hbuf[NHEX];
char	*nexth = hbuf;
tchar	*hyend;

hyphen(wp)
	tchar *wp;
{
	register j;
	register tchar *i;

	i = wp;
	while (punct(cbits(*i++)))
		;
	if (!alph(cbits(*--i)))
		return;
	wdstart = i++;
	while (alph(cbits(*i++)))
		;
	hyend = wdend = --i - 1;
	while (punct(cbits(*i++)))
		;
	if (*--i)
		return;
	if ((wdend - wdstart - 4) < 0)
		return;
	hyp = hyptr;
	*hyp = 0;
	hyoff = 2;
/*
	if (!exword() && !suffix())
		digram();
*/
	if (!exword()) {
		if (hyalg == ORIGINAL && !suffix())
			digram();
		if (hyalg == DUTCH)
			split(wdstart, wdend);
	}
	*hyp++ = 0;
	if (*hyptr) 
		for (j = 1; j; ) {
			j = 0;
			for (hyp = hyptr + 1; *hyp != 0; hyp++) {
				if (*(hyp - 1) > *hyp) {
					j++;
					i = *hyp;
					*hyp = *(hyp - 1);
					*(hyp - 1) = i;
				}
			}
		}
}


punct(i)
{
	if (!i || alph(i))
		return(0);
	else
		return(1);
}


alph(i)
{
	if (i >= 'a' && i <= 'z' || i >= 'A' && i <= 'Z')
		return(1);
	else
		return(0);
}

/*
 * set the hyphenation algorithm
 *
 * jna
 */

caseha()
{	register i;

	if ( skip())
		i = hyalg1;
	else {
		noscale++;
		noscale = 0;
		i = max(atoi(), 0);
		if (nonumb)
			return;
		if (i > MAXDIALECTS) {
			errprint("Unknown dialect %d", i);
			return;
		}
	}
	hyalg1 = hyalg;
	hyalg = i;
	if( hyalg == DUTCH)
		thresh = DUTCH_THRESH;
}

caseht()
{
	switch(hyalg) {
		case ORIGINAL:
			thresh = THRESH;
			break;
		case DUTCH:
			thresh = DUTCH_THRESH;
			break;
	}
	if (skip())
		return;
	noscale++;
	if (hyalg == DUTCH)
		thresh = max(atoi(), 1);
	else
		thresh = atoi();
	noscale = 0;
}


casehw()
{
	register i, k;
	register char	*j;
	tchar t;

	k = 0;
	while (!skip()) {
		if ((j = nexth) >= (hbuf + NHEX - 2))
			goto full;
		for (; ; ) {
			if (ismot(t = getch()))
				continue;
			i = cbits(t);
			if (i == ' ' || i == '\n') {
				*j++ = 0;
				nexth = j;
				*j = 0;
				if (i == ' ')
					break;
				else
					return;
			}
			if (i == '-') {
				k = HY_BIT;
				continue;
			}
			*j++ = maplow(i) | k;
			k = 0;
			if (j >= (hbuf + NHEX - 2))
				goto full;
		}
	}
	return;
full:
	errprint("exception word list full.");
	*nexth = 0;
}


exword()
{
	register tchar *w;
	register char	*e;
	char	*save;

	e = hbuf;
	while (1) {
		save = e;
		if (*e == 0)
			return(0);
		w = wdstart;
		while (*e && w <= hyend && (*e & 0177) == maplow(cbits(*w))) {
			e++; 
			w++;
		};
		if (!*e) {
			if (w-1 == hyend ||
			   (hyalg == ORIGINAL /* s-extension only in original */
				&& (w == wdend && maplow(cbits(*w)) == 's'))) {
				w = wdstart;
				for (e = save; *e; e++) {
					if (*e & HY_BIT)
						*hyp++ = w;
					if (hyp > (hyptr + NHYP - 1))
						hyp = hyptr + NHYP - 1;
					w++;
				}
				return(1);
			} else {
				e++; 
				continue;
			}
		} else 
			while (*e++)
				;
	}
}


suffix()
{
	register tchar *w;
	register char	*s, *s0;
	tchar i;
	extern char	*suftab[];
	extern tchar *chkvow();

again:
	if (!alph(cbits(i = cbits(*hyend))))
		return(0);
	if (i < 'a')
		i -= 'A' - 'a';
	if ((s0 = suftab[i-'a']) == 0)
		return(0);
	for (; ; ) {
		if ((i = *s0 & 017) == 0)
			return(0);
		s = s0 + i - 1;
		w = hyend - 1;
		while (s > s0 && w >= wdstart && (*s & 0177) == maplow(cbits(*w))) {
			s--;
			w--;
		}
		if (s == s0)
			break;
		s0 += i;
	}
	s = s0 + i - 1;
	w = hyend;
	if (*s0 & HY_BIT) 
		goto mark;
	while (s > s0) {
		w--;
		if (*s-- & HY_BIT) {
mark:
			hyend = w - 1;
			if (*s0 & 0100)
				continue;
			if (!chkvow(w))
				return(0);
			*hyp++ = w;
		}
	}
	if (*s0 & 040)
		return(0);
	if (exword())
		return(1);
	goto again;
}


maplow(i)
register int	i;
{
	if (isupper(i)) 
		i = tolower(i);
	return(i);
}


vowel(i)
int	i;
{
	switch (maplow(i)) {
	case 'a':
	case 'e':
	case 'i':
	case 'o':
	case 'u':
	case 'y':
		return(1);
	default:
		return(0);
	}
}


tchar *chkvow(w)
tchar *w;
{
	while (--w >= wdstart)
		if (vowel(cbits(*w)))
			return(w);
	return(0);
}


digram() 
{
	register tchar *w;
	register val;
	tchar * nhyend, *maxw;
	int	maxval;
	extern char	bxh[26][13], bxxh[26][13], xxh[26][13], xhx[26][13], hxx[26][13];

again:
	if (!(w = chkvow(hyend + 1)))
		return;
	hyend = w;
	if (!(w = chkvow(hyend)))
		return;
	nhyend = w;
	maxval = 0;
	w--;
	while ((++w < hyend) && (w < (wdend - 1))) {
		val = 1;
		if (w == wdstart)
			val *= dilook('a', cbits(*w), bxh);
		else if (w == wdstart + 1)
			val *= dilook(cbits(*(w-1)), cbits(*w), bxxh);
		else 
			val *= dilook(cbits(*(w-1)), cbits(*w), xxh);
		val *= dilook(cbits(*w), cbits(*(w+1)), xhx);
		val *= dilook(cbits(*(w+1)), cbits(*(w+2)), hxx);
		if (val > maxval) {
			maxval = val;
			maxw = w + 1;
		}
	}
	hyend = nhyend;
	if (maxval > thresh)
		*hyp++ = maxw;
	goto again;
}


dilook(a, b, t)
int	a, b;
char	t[26][13];
{
	register i, j;

	i = t[maplow(a)-'a'][(j = maplow(b)-'a')/2];
	if (!(j & 01))
		i >>= 4;
	return(i & 017);
}


/*
 * All these jazz is to have the dialect dutch being hyphenated
 * It first appeared in the dutch version of troff (nltroff), due to
 * teus hagen.
 * The original program has converted from Algol60 to C by, I think
 * bert ijsselstein.
 * It's a mess, anyway.
 *
 * Planted in this version of troff by jaap akkerhuis (jna).
 *
 * Note that this is licensed software!
 *
 */

#ifndef	NULL
#define NULL	0
#endif
#define MAXLETT 50  /* at most the first MAXLETT characters of a word
                       will be processed */
#define MAXSYLL 20  /* at most the first MAXSYLL syllables of a word
		       will be processed */

#define LETTEREE 27
#define LETTERJ 41
#define LETTERV 55
#define LETTERX 57
#define LETTERZ 58

/*
 * split(..) needs to be cleaned up, could install hjt's version...
 */

split( aword, anend ) register tchar *aword, *anend;
{	register tchar *place;
	extern tchar *bestsplit1();

	place = bestsplit1( aword, anend );
	if( place != (tchar *) NULL )
	{	*hyp++ = place;
		if( place - aword > thresh && anend - place > thresh )
			split( aword, place+1 );
		if( anend - place > thresh && place - aword > thresh )
			split( place, anend );
	}
}

tchar *
bestsplit1( tosplit , aend )
tchar *tosplit, *aend;
{
/* This function determines the "best" place to split into two parts the
 * Dutch word contained in a string of <size> characters which starts at
 * the address <tosplit> .
 * The input characters should be in ASCII code .
 * The function returns as value the number of characters of the first
 * of the two parts .
 * If the returned value exceeds the character count of the line the
 * user may try to invoke bestsplit1 again but now with <size> equal to
 * the returned value plus one .
 * The algorithm is adapted from the Mathematical Centre report NR 28/72,
 * "BESTESPLITS1, EEN PROCEDURE VOOR HET AUTOMATISCH AFBREKEN VAN NEDER-
 * LANDSE WOORDEN" , which has been written by J.C. VAN VLIET.
 */
	extern char translate[], comprimation[][14], consonant[][23],
		prefix[][3] ;
	short woord[ MAXLETT +1], reference[ MAXLETT +1], vowel[ MAXSYLL ],
            turn[ MAXSYLL ] , letter, nextlett, vowel1, vowel2,
            l0, l1, l2 ;
        short numlett, numsyll, turnindex, differ, start1, start2, stop,
	    level, bp ;
	register int i, j, help ;
	short size = aend - tosplit + 1;

	/* translate into bestsplit code : */
	woord[0] = 0 ;
	i = 1 ;
	help = -1 ;
	while ( (++help < size) && (i <  MAXLETT ) ) {
		reference[i] = i;
		woord[i++] = translate[maplow(cbits(tosplit[help])) - 'a'] ;
	}
	/* end of translation : */

	numlett = i ;
	if ( numlett < 4 ) goto nosplit ;
	i = j = 1 ;
	help = 0 ;
	while ( i < numlett ) {
		letter = woord[i] ;
 		/* comprimation of vowels : */
 		if ( (25 < letter) && (letter < 41) ) {
 			nextlett = woord[i+1] ;
 			if ( (28 < nextlett) && (nextlett < 43) ) {
 				letter = comprimation[letter-26][nextlett-29] ;
 				if (letter > 0) {
 					i++ ;
 					help++ ;
 					woord[i] = letter ;
					continue ;
 				}
 			}
 		} /* end of comprimation */

 		woord[j] = woord[i] ;
 		j++ ;
 		i++ ;
 		reference[j] += help ;
	}
	woord[j] = woord[numlett] ;
	numlett = j ;


	/* determination of the number of syllables */
	j = -1 ;
	i = 0 ;
	while ( ( ++i <= numlett ) && ( j < MAXSYLL ) ) {
		if (woord[i] < 39) {
			j++ ;
			vowel[j] = i ;
		}
	}
	numsyll = j+1 ;

	if ( numsyll < 2 ) goto nosplit ;
	turnindex = 0 ;
	differ = 1 ;
	start1 = 0 ;
	start2 = numsyll - 1 ;
	stop = start2 ;

	while ( turnindex < stop ) {
		vowel1 = vowel[stop] ;
		for ( i = stop - 1 ; i >= 0 ; i-- ) {
			vowel2 = vowel[i] ;
			if ( vowel1 - vowel2 == differ) {
				turn[turnindex] = i ;
				turnindex++ ;
			}
			vowel1 = vowel2 ;
		}
		if ( differ == 1 ) start1 = turnindex ;
		else if ( differ == 2 ) start2 = turnindex ;
		differ++ ;
	}

	turnindex = start2 - 1 ;
	stop = numsyll - 1 ;
	level = 1 ;

next :
	turnindex++ ;
	if ( turnindex >= stop ) {
		if ( level == 1 ) turnindex = start2 ;
		else if ( level == 2 ) {
				turnindex = start1 ;
				stop = start2 ;
			}
			else goto nosplit ;
		level++ ;
		if ( turnindex >= stop ) goto next ;
	}
	j = turn[turnindex] ;
	vowel1 = vowel[j] ;
	vowel2 = vowel[j+1] ;

	switch ( level ) {
	case 1 :
		for ( j = vowel2-2 ; j >= vowel1+1 ; j-- ) {
			help = consonant[woord[j]-39][woord[j+1]-39] ;
			if ( abs(help) == 1 ) goto splitafterj ;
			if ( help < 0 ) goto next ;
		}
		break ;   /* end of first phase */

	case 2 :
		for ( i = vowel2-2 ; i >= vowel1+1 ; i-- ) {
			help = consonant[woord[i]-39][woord[i+1]-39] ;
			if ( abs(help) == 2 ) {
				j = i ;
				goto splitafterj ;
			}
			if ( abs(help) == 3 ) {
				if ( i == vowel1+1 ) { 
					j = vowel1 ;
					goto splitafterj ;
				}
				help = abs(consonant[woord[i-1]-39][woord[i]-39]) ;
				if ( help == 2 ) {
					j = i - 1 ;
					goto splitafterj ;
				}
				if ( help == 3 ) {
					j = i - 2 ;
					goto splitafterj ;
				}
			}
			else if ( ( abs(help) == 4 ) && 
						( i == vowel2-2 ) ) {
				j = i ;
				goto splitafterj ;
			}
			if ( help < 0 ) goto next ;
		}
		break ;   /* end of second phase */

	case 3 :
		j = vowel1 ;
		help = woord[j+1] ;
		if ( (help == LETTERJ) || (help == LETTERV) ||  
				  (help == LETTERZ) ) goto splitafterj ;
		if ( help == LETTERX ) goto next ;
		l1 = woord[j] ;
		if ( l1 == LETTEREE ) goto next ;
		if ( ( l1 > 24 ) && ( l1 < 29 ) ) {
			j++ ;
			goto splitafterj ;
		}
		l0 = woord[j-1] ;
		l2 = woord[j+1] ;
		for ( i = 0 ; i < 7 ; i++ )
			if ( ( l0 == prefix[i][0] ) &&
			     ( l1 == prefix[i][1] ) &&
			     ( l2 == prefix[i][2] ) ) goto next ;
		goto splitafterj ;
		break ;   /* end of third phase */

	}


	goto next ;

splitafterj :
	bp = reference[j+1] - 1 ;
	if((bp < size-1) && (bp > 0))
		goto away;
	else
		goto next;

nosplit :
	bp = 0 ;
	level = 4 ;
away :
	return(bp == 0? (tchar *) NULL : tosplit+bp) ;
}
