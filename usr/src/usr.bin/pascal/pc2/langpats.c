/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)langpats.c 1.11 %G%";

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

/*
 * C library routines
 */
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
	popr	$0x3\n\
	movb	r0,*4(r1)\n\
	incl	4(r1)\n\
2:\n" },

/*
 * VAX special instructions
 */
	{ "3,_blkcpy\n",
"	popr	$0xa\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	movc3	r0,(r1),(r3)\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movl	(sp)+,r0\n\
	movc3	r0,(r1),(r3)\n" },

	{ "2,_blkclr\n",
"	movl	(sp)+,r3\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	movc5	$0,(r3),$0,r0,(r3)\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movl	(sp)+,r0\n\
	movc5	$0,(r3),$0,r0,(r3)\n" },

	{ "3,_LOCC\n",
"	popr	$0x30\n\
	movl	(sp)+,r1\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r5,r0\n\
	jleq	1f\n\
	subl2	r0,r5\n\
	locc	r4,r0,(r1)\n\
	jeql	1b\n\
	addl2	r5,r0\n\
	jbr	2f\n\
1:\n\
	locc	r4,r5,(r1)\n\
2:\n" },

	{ "2,_ROUND\n",
"	cvtrdl	(sp)+,r0\n" },

	{ "2,_TRUNC\n",
"	cvtdl	(sp)+,r0\n" },

/*
 * General Pascal library routines
 */
	{ "1,_ACTFILE\n",
"	movl	(sp)+,r1\n\
	movl	12(r1),r0\n" },

	{ "2,_FCALL\n",
"	movl	4(sp),r0\n\
	movc3	4(r0),__disply+8,*(sp)+\n\
	movl	(sp)+,r0\n\
	movc3	4(r0),8(r0),__disply+8\n" },

	{ "2,_FRTN\n",
"	movl	(sp)+,r0\n\
	movc3	4(r0),*(sp)+,__disply+8\n" },

	{ "3,_FSAV\n",
"	movl	8(sp),r0\n\
	movl	(sp)+,(r0)\n\
	ashl	$3,(sp)+,4(r0)\n\
	movc3	4(r0),__disply+8,8(r0)\n\
	movl	(sp)+,r0\n" },

/*
 * Pascal relational comparisons
 */
	{ "3,_RELEQ\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	3f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
2:\n\
	clrl	r0\n\
	jbr	4f\n\
3:\n\
	cmpc3	r4,(r1),(r3)\n\
	jneq	2b\n\
	incl	r0\n\
4:\n" },

	{ "3,_RELNE\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jleq	3f\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jeql	1b\n\
2:\n\
	movl	$1,r0\n\
	jbr	4f\n\
3:\n\
	cmpc3	r4,(r1),(r3)\n\
	jneq	2b\n\
4:\n" },

	{ "3,_RELSLT\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
	jbr	2f\n\
1:\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jneq	3f\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jgtr	1b\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jlss	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSLE\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
	jbr	2f\n\
1:\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jneq	3f\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jgtr	1b\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jleq	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSGT\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
	jbr	2f\n\
1:\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jneq	3f\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jgtr	1b\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jgtr	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

	{ "3,_RELSGE\n",
"	popr	$0xb\n\
	movl	r0,r4\n\
	jbr	2f\n\
1:\n\
	subl2	r0,r4\n\
	cmpc3	r0,(r1),(r3)\n\
	jneq	3f\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	r4,r0\n\
	jgtr	1b\n\
	cmpc3	r4,(r1),(r3)\n\
3:\n\
	jgeq	4f\n\
	clrl	r0\n\
	jbr	5f\n\
4:\n\
	movl	$1,r0\n\
5:\n" },

/*
 * Pascal set operations.
 */
	{ "4,_ADDT\n",
"	popr	$0x17\n\
	movl	r0,r3\n\
1:\n\
	bisl3	(r1)+,(r2)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_SUBT\n",
"	popr	$0x17\n\
	movl	r0,r3\n\
1:\n\
	bicl3	(r2)+,(r1)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_MULT\n",
"	popr	$0x17\n\
	movl	r0,r3\n\
1:\n\
	mcoml	(r1)+,r5\n\
	bicl3	r5,(r2)+,(r3)+\n\
	sobgtr	r4,1b\n" },

	{ "4,_IN\n",
"	popr	$0x1e\n\
	clrl	r0\n\
	subl2	r2,r1\n\
	cmpl	r1,r3\n\
	jgtru	1f\n\
	jbc	r1,(r4),1f\n\
	incl	r0\n\
1:\n" },

/*
 * Pascal runtime checks
 */
	{ "1,_ASRT\n",
"	tstl	(sp)+\n\
	jneq	1f\n\
	pushl	$0\n\
	pushl	$_EASRT\n\
	calls	$2,_ERROR\n\
1:\n" },

	{ "2,_ASRTS\n",
"	popr	$0x03\n\
	tstl	r0\n\
	jneq	1f\n\
	pushl	r1\n\
	pushl	$_EASRTS\n\
	calls	$2,_ERROR\n\
1:\n" },

	{ "1,_CHR\n",
"	movl	(sp)+,r0\n\
	cmpl	r0,$127\n\
	jlequ	1f\n\
	pushl	r0\n\
	pushl	$_ECHR\n\
	calls	$2,_ERROR\n\
1:\n" },

	{ "0,_LINO\n",
"	incl	__stcnt\n\
	cmpl	__stcnt,__stlim\n\
	jlss	1f\n\
	pushl	__stcnt\n\
	pushl	$_ELINO\n\
	calls	$2,_ERROR\n\
1:\n" },

	{ "1,_NIL\n",
"	movl	(sp)+,r0\n\
	cmpl	r0,__maxptr\n\
	jgtr	1f\n\
	cmpl	r0,__minptr\n\
	jgeq	2f\n\
1:\n\
	pushl	$0\n\
	pushl	$_ENIL\n\
	calls	$2,_ERROR\n\
2:\n" },

	{ "2,_RANDOM\n",
"	addl2	$8,sp\n\
	emul	__seed,$1103515245,$0,r0\n\
	ediv	$0x7fffffff,r0,r1,r0\n\
	movl	r0,__seed\n\
	cvtld	r0,r0\n\
	divd2	$0d2.147483647e+09,r0\n" },

	{ "3,_RANG4\n",
"	popr	$0x07\n\
	cmpl	r0,r1\n\
	jlss	1f\n\
	cmpl	r0,r2\n\
	jleq	2f\n\
1:\n\
	pushl	r0\n\
	pushl	$_ERANG\n\
	calls	$2,_ERROR\n\
2:\n" },

	{ "2,_RSNG4\n",
"	popr	$0x03\n\
	cmpl	r0,r1\n\
	jlequ	1f\n\
	pushl	r0\n\
	pushl	$_ERANG\n\
	calls	$2,_ERROR\n\
1:\n" },

	{ "1,_SEED\n",
"	movl	__seed,r0\n\
	movl	(sp)+,__seed\n" },

	{ "3,_SUBSC\n",
"	popr	$0x07\n\
	cmpl	r0,r1\n\
	jlss	1f\n\
	cmpl	r0,r2\n\
	jleq	2f\n\
1:\n\
	pushl	r0\n\
	pushl	$_ESUBSC\n\
	calls	$2,_ERROR\n\
2:\n" },

	{ "2,_SUBSCZ\n",
"	popr	$0x03\n\
	cmpl	r0,r1\n\
	jlequ	1f\n\
	pushl	r0\n\
	pushl	$_ESUBSC\n\
	calls	$2,_ERROR\n\
1:\n" },

};

struct pats		*htbl[HSHSIZ];


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
	register char		*cp, *ccp, *lp;
	register int		hash, rehash, size;
	char			line[BUFSIZ];
	extern char		*index();

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
		lp = index(line, ':');
		for (cp = (lp != NULL) ? ++lp : line; *cp == '\t'; )
			cp++;
		if (strcmpn(cp, "calls\t$", 7) != 0) {
			fputs(line, stdout);
			continue;
		}
		cp += 7;
		HASH(cp, hp);
		while (*hp) {
			if (strcmpn((*hp)->name, cp, size)==NULL) {
				if (lp != NULL) {
					*lp++ = '\n';
					*lp = '\0';
					fputs(line, stdout);
				}
				fputs((*hp)->replace, stdout);
				goto nextline;
			}
			REHASH(hp);
		}
		fputs(line, stdout);
nextline:;
	}
	exit(0);
}
