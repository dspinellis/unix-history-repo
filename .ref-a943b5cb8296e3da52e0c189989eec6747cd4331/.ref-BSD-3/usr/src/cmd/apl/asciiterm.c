#include "apl.h"

asciich() {
	extern unsigned char	*iline;
	extern struct asoperbox charconv[OPERBOXSIZE];
	register D,E,F,I;
	
	F = *iline++;
	if ( ( F == '.') && ( digit(*iline) == 0 ) ) {
		D = (int)*iline++;
		E = (int)*iline++;
		for (I = 0; I <= OPERBOXSIZE;I++) {
			if (( D== (charconv[I].letters[0]) ) &&
			    ( E== (charconv[I].letters[1]) ) ) {
				F = charconv[I].returnchar;
				goto out;
			}
		}
		--iline;--iline;
	}
out:	return(F);
};

/* eventually this should be replaces with arrays that hae a better	*/
/*	method of selecting and returning the lexical value, but	*/
/*	for now, this is a quick implementation 			*/
/*									*/
/*	use char as subscript from ascii - get apl character hopefully  */
/*	for character conversion from ascii to apl char set ---		*/
/* see files write2.c and write3.c for more recent version 		*/

struct asoperbox charconv[OPERBOXSIZE] = {
/* ascii mnemonic ( from HP  APL ascii char mnemonics and 		*/
/*	equivalent apl character value.					*/
/* 	all are two lowercase letters preceeded by a dot.		*/
/*	letters for identifiers are lowercase or uppercase		*/
/*	lowercase corresponding to apl uppercase and			*/
/*	uppercase corresponding to apl underscored letters.		*/

"le",	/* less than or equal	*/ '$',
"ge",	/* greater than or equal*/ '^',
"ne",	/* not equal		*/ '*',
"om",	/* omega ( not used )	*/ 'W',
"ep",	/* epsilon		*/ 'E',
"rh",	/* shape (rho)		*/ 'R',
"nt",	/* not ( also '~' )	*/ 'T',
"tk",	/* take ( also '^' )	*/ 'Y',
"dr",	/* drop			*/ 'U',
"it",	/* iota			*/ 'I',
"ci",	/* circular function	*/ 'O',
"al",	/* alpha ( not used )	*/ 'A',
"cl",	/* maximum ( ceiling )  */ 'S',
"fl",	/* minimum ( floor )	*/ 'D',
"dl",	/* del ( not used )	*/ 'G',
"de",	/* upside down del	*/ 'H',
"jt",	/* small circle ( null )*/ 'J',
"qd",	/* quad			*/ 'L',
"ss",	/* right U ( not used )	*/ 'Z',
"sc",	/* left U ( not used )	*/ 'X',
"si",	/* Down U		*/ 'C',
"su",	/* U ( not used )	*/ 'V',
"[^",	/* upside-down del	*/ 'H',
"bv",	/* decode ( base )	*/ 'B',
"rp",	/* encode ( rep )	*/ 'N',
"br",	/* residue ( mod )	*/ 'M',
"sp",	/* assignment		*/ '[',
"go",	/* goto			*/ ']',
"or",	/* or			*/ '(',
"nn",	/* nand			*/ 0205,
"nr",	/* nor			*/ 0206,
"lg",	/* log			*/ 0207, 
"rv",	/* reversal		*/ 0217,
"tr",	/* transpose		*/ 0212,
"rb",	/* reverse bar		*/ 0214,
"cb",	/* comma bar ( not used)*/ 0,
"sb",	/* slash bar		*/ 0200,
"bb",	/* blackslash bar	*/ 0201,
"gu",	/* grade up		*/ 0215,
"gd",	/* grade down		*/ 0216,
"qq",	/* quote quad		*/ 0202,
"dm",	/* domino		*/ 0214,
"lm",	/* lamp			*/ 0204,
"ib",	/* i - beam		*/ 0213,
"ex",	/* execute ( not used )	*/ 0,
"fr",	/* format( not used )	*/ 0,
"di",	/* diamond ( not used )	*/ 0,
"ot",	/* out ( not used ) 	*/ 0,
"ld",	/* locked del (not used)*/ 0,
"[a",	/* alias for 'A'	*/ 0220,
"[b",	/* alias for 'B'	*/ 0221,
"[c",	/* alias for 'C'	*/ 0222,
"[d",	/* alias for 'D'	*/ 0223,
"[e",	/* alias for 'E'	*/ 0224,
"[f",	/* alias for 'F'	*/ 0225,
"[g",	/* alias for 'G'	*/ 0226,
"[h",	/* alias for 'H'	*/ 0227,
"[i",	/* alias for 'I'	*/ 0230,
"[j",	/* alias for 'J'	*/ 0231,
"[k",	/* alias for 'K'	*/ 0232,
"[l",	/* alias for 'L'	*/ 0233,
"[m",	/* alias for 'M'	*/ 0234,
"[n",	/* alias for 'N'	*/ 0235,
"[o",	/* alias for 'O'	*/ 0236,
"[p",	/* alias for 'P'	*/ 0237,
"[q",	/* alias for 'Q'	*/ 0240,
"[r",	/* alias for 'R'	*/ 0241,
"[s",	/* alias for 'S'	*/ 0242,
"[t",	/* alias for 'T'	*/ 0243,
"[u",	/* alias for 'U'	*/ 0244,
"[v",	/* alias for 'V'	*/ 0245,
"[w",	/* alias for 'W'	*/ 0246,
"[x",	/* alias for 'X'	*/ 0247,
"[y",	/* alias for 'Y'	*/ 0250,
"[z",	/* alias for 'Z'	*/ 0251
};
