static	char sccsid[] = "@(#)c22.c 4.2 %G%";
#include "c2.h"
/* char c22[] = "@(#)c22.c 4.2 %G%"; */
#define readonly

readonly char revbr[] = {
	JNE, JEQ, JGT, JLT, JGE, JLE,
	JNE, JEQ, JHI, JLO, JHIS, JLOS,
	JBS, JBC, JLBS, JLBC, JBSC, JBCC, JBSS, JBCS };

#define	TYPB	BYTE
#define	TYPW	WORD
#define	TYPL	LONG
#define	TYPQ	QUAD
#define	TYPO	OCTA
#define	TYPF	FFLOAT
#define	TYPD	DFLOAT
#define	TYPG	GFLOAT
#define	TYPH	HFLOAT

readonly struct optab optab[] = {

#include "./instrs.c2"

0,	0};
