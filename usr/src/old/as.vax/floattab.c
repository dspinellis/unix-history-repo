/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)floattab.c 4.1 %G%";
#endif not lint

#include <stdio.h>
#include "as.h"
#define		N	NOTAKE

struct ty_bigdesc ty_bigdesc[] = {
{	/* TYPB */
	{ 15, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N },
	{  N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, 0 },
	{ N, N },
	{ N, N },
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	8,	/* mantissa sig bits */
	0,	/* exponent sig bits */
	8,	/* exponent excess */
},
{	/* TYPW */
	{ 14, 15, N, N, N, N, N, N, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, N, N, N, N, N, N, 0, 1 },
	{ N, N},
	{ N, N},
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	16,	/* mantissa sig bits */
	0,	/* exponent sig bits */
	16,	/* exponent excess */
},
{	/* TYPL */
	{ 12, 13, 14, 15, N, N, N, N, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, N, N, N, N, 0, 1, 2, 3 },
	{ N, N },
	{ N, N },
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	32,	/* mantissa sig bits */
	0,	/* exponent sig bits */
	32,	/* exponent excess */
},
{	/* TYPQ */
	{ 8, 9, 10, 11, 12, 13, 14, 15, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, 0, 1, 2, 3, 4, 5, 6, 7 },
	{ N, N },
	{ N, N },
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	64,	/* mantissa sig bits */
	0,	/* exponent sig bits */
	64,	/* exponent excess */
},
{	/* TYPO */
	{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
	{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
	{ N, N },
	{ N, N },
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	128,	/* mantissa sig bits */
	0,	/* exponent sig bits */
	128,	/* exponent excess */
},
{	/* TYPF */
	{ 15, N, 13, 14, N, N, N, N, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, N, N, N, N, N, 2, 3, 0 },
	{ 0, 1 },
	{ 0, 1 },
	1,	/* mantissa lshift */
	7,	/* exponent rshift */
	24,	/* mantissa sig bits */
	8,	/* exponent sig bits */
	128	/* exponent excess */
},
{	/* TYPD */
	{ 15, N, 13, 14, 11, 12, 9, 10, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, N, 6, 7, 4, 5, 2, 3, 0 },
	{ 0, 1 },
	{ 0, 1 },
	1,	/* mantissa lshift */
	7,	/* exponent rshift */
	56,	/* mantissa sig bits */
	8,	/* exponent sig bits */
	128	/* exponent excess */
},
{	/* TYPG */
	{ 15, N, 13, 14, 11, 12, 9, 10, N, N, N, N, N, N, N, N },
	{ N, N, N, N, N, N, N, N, N, 6, 7, 4, 5, 2, 3, 0 },
	{ 0, 1 },
	{ 0, 1 },
	4,	/* mantissa lshift */
	4,	/* exponent rshift */
	53,	/* mantissa sig bits */
	11,	/* exponent sig bits */
	1024	/* exponent excess */
},
{	/* TYPH */
	{ N, N, 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3 },
	{ N, N, 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3 },
	{ 0, 1 },
	{ 0, 1 },
	0,	/* mantissa lshift */
	0,	/* exponent rshift */
	113,	/* mantissa sig bits */
	15,	/* exponent sig bits */
	16384	/* exponent excess */
},
{0}		/* TYPNONE */
};

cfloattab()
{
	reg	struct ty_bigdesc	*p;
	reg	int	i;
	reg	int	j;
	reg	int	k;
		extern	int	ty_float[];

	for (i = 0; i < TYPNONE - 1; i++){
		p = &ty_bigdesc[i];
		for (j = 0; j < 16; j++){
			if (((char)p->b_upmmap[j]) != NOTAKE){
				k = p->b_pmmap[p->b_upmmap[j]];
				if (j != k)
					printf("%s:p[up[%d]] == %d\n",
						ty_string[i],j,k);
			}

			if (((char)p->b_pmmap[j]) != NOTAKE){
				k = p->b_upmmap[p->b_pmmap[j]];
				if (j != k)
					printf("%s:up[p[%d]] == %d\n",
						ty_string[i],j,k);
			}
		}
		if (!ty_float[i])
			continue;
		k = (p->b_msigbits - 1) + p->b_mlshift;
		if (k % 8)
			printf("sigbits: %d, lshift: %d, excess: %d\n",
				p->b_msigbits, p->b_mlshift, k % 8);
		if ((15 - (k / 8)) < 0)
			printf("lsbyte <= 0\n");
		if ( (8 - (p->b_mlshift + 2)) <= 0)
			printf("Shifted byte 15 into byte 14");
		if ( 1 << (p->b_esigbits - 1) != p->b_eexcess)
			printf("sigbits == %d, excess = %d\n",
				p->b_esigbits, p->b_eexcess);
	}
}
