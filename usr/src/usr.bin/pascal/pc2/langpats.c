/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)langpats.c 1.3 %G%";

#include <stdio.h>
#include <ctype.h>
/*
 * The hash table must be at least twice as big as the number
 * of patterns, preferably bigger. It must also be a prime number
 */
#define HSHSIZ	101

struct pats {
	char	*name;
	char	*replace;
} ptab[] = {

	{ "1,_ACTFILE\n",
"	movl	(sp)+,r1\n\
	movl	12(r1),r0\n" },

	{ "1,_fgetc\n",
"	sobgeq	*(sp),1f\n\
	calls	$1,__filbuf\n\
	jbr     2f\n\
1:\n\
	addl3	$4,(sp)+,r1\n\
	movzbl	*(r1),r0\n\
	incl	(r1)\n\
2:\n" },

	{ "2,_fputc\n",
"	sobgeq	*4(sp),1f\n\
	calls	$2,__flsbuf\n\
	jbr	2f\n\
1:\n\
	movl	(sp)+,r0\n\
	addl3	$4,(sp)+,r1\n\
	movb	r0,*(r1)\n\
	incl	(r1)\n\
2:\n" },

	{ "3,_blkcpy\n",
"	movl	4(sp),r1\n\
	movl	8(sp),r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jleq	1f\n\
	subl2	r0,(sp)\n\
	movc3	r0,(r1),(r3)\n\
	jbr	1b\n\
1:\n\
	movl	(sp)+,r0\n\
	addl2	$8,sp\n\
	movc3	r0,(r1),(r3)\n" },

	{ "2,_blkclr\n",
"	movl	4(sp),r3\n\
	subl3	r3,r3,-4(sp)\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	movc5	$0,(r3),$0,r0,(r3)\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movq	(sp)+,r0\n\
	movc5	$0,(r3),$0,r0,(r3)\n" },

	{ "3,_LOCC\n",
"	movl	(sp)+,r5\n\
	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	1f\n\
	subl2	r0,r4\n\
	locc	r5,r0,(r1)\n\
	jeql	1b\n\
	addl2	r4,r0\n\
	jbr	2f\n\
1:\n\
	locc	r5,r4,(r1)\n\
2:\n" },

	{ "1,_ROUND\n",
"	cvtrdl	(sp)+,r0\n" },

	{ "1,_TRUNC\n",
"	cvtdl	(sp)+,r0\n" },

	{ "1,_FCALL\n",
"	movl	(sp),r0\n\
	ashl	$3,4(r0),r1\n\
	movc3	r1,__disply+8,8(r0)[r1]\n\
	movl	(sp),r0\n\
	ashl	$3,4(r0),r1\n\
	movc3	r1,8(r0),__disply+8\n\
	movl	*(sp)+,r0\n" },

	{ "2,_FRTN\n",
"	movl	(sp)+,r0\n\
	ashl	$3,4(r0),r1\n\
	movc3	r1,8(r0)[r1],__disply+8\n\
	movl	(sp)+,r0\n" },

	{ "3,_FSAV\n",
"	movl	8(sp),r0\n\
	movl	(sp)+,(r0)\n\
	movl	(sp)+,4(r0)\n\
	ashl	$3,4(r0),r1\n\
	movc3	r1,__disply+8,8(r0)\n\
	movl	(sp)+,r0\n" },

	{ "3,_RELEQ\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
	jneq	3f\n\
	incl	r0\n\
	jbr	4f\n\
3:\n\
	clrl	r0\n\
4:\n" },

	{ "3,_RELNE\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
	jeql	4f\n\
3:\n\
	movl	$1,r0\n\
4:\n" },

	{ "3,_RELSLT\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jlss	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSLE\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jleq	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSGT\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jgtr	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSGE\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	2f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
	jbr	3f\n\
2:\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jgeq	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "4,_ADDT\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movl	r0,r3\n\
	movl	(sp)+,r4\n\
1:\n\
	bisl3	(r1)+,(r2)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_SUBT\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movl	r0,r3\n\
	movl	(sp)+,r4\n\
1:\n\
	bicl3	(r2)+,(r1)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_MULT\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movl	r0,r3\n\
	movl	(sp)+,r4\n\
1:\n\
	mcoml	(r1)+,r5\n\
	bicl3	r5,(r2)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_IN\n",
"	clrl	r0\n\
	movl	(sp)+,r1\n\
	subl2	(sp)+,r1\n\
	cmpl	r1,(sp)+\n\
	jgtru	1f\n\
	jbc	r1,*(sp),1f\n\
	movl	$1,r0\n\
1:\n\
	addl2	$4,sp\n" }
};

struct pats		*htbl[HSHSIZ];


#define	CHK(c)	if (*cp++ != c) goto copy;

#define HASH(cp, hp) {\
	hash = 0; rehash = 1; ccp = cp; \
	do	{ \
		hash *= (int)*ccp++; \
	} while (*ccp && *ccp != '\n'); \
	hash >>= 7; hash %= HSHSIZ; hp = &htbl[hash]; size = ccp - cp + 1; \
	}

#define REHASH(hp) {\
	hp += rehash; rehash += 2; \
	if (hp >= &htbl[HSHSIZ]) \
		hp -= HSHSIZ; \
	}


main(argc, argv)

	int	argc;
	char	**argv;
{
	register struct pats	*pp;
	register struct pats	**hp;
	register char		*cp, *ccp;
	register int		hash, rehash, size;
	char			line[BUFSIZ];

	if (argc > 1)
		freopen(argv[1], "r", stdin);
	if (argc > 2)
		freopen(argv[2], "w", stdout);
	/*
	 * set up the hash table
	 */
	for(pp = ptab; pp < &ptab[sizeof ptab/sizeof ptab[0]]; pp++) {
		HASH(pp->name, hp);
		while (*hp)
			REHASH(hp);
		*hp = pp;
	}
	/*
	 * check each line and replace as appropriate
	 */
	while (fgets(line, BUFSIZ, stdin)) {
		for (cp = line; *cp && *cp == '\t'; )
			cp++;
		CHK('c'); CHK('a'); CHK('l'); CHK('l'); CHK('s');
		CHK('\t'); CHK('$');
		HASH(cp, hp);
		while (*hp) {
			if (RELEQ(size, (*hp)->name, cp)) {
				fputs((*hp)->replace, stdout);
				goto nextline;
			}
			REHASH(hp);
		}
copy:
		fputs(line, stdout);
nextline:;
	}
	exit(0);
}
