#ifndef lint
static char sccsid[] = "@(#)langpats.c	1.4 (Berkeley) %G%";
#endif

/*
 * In-line assembly code expander for
 * the kernel.  This code is based on
 * pc2 and the old asm.sed script.
 */
#include <stdio.h>
#include <ctype.h>

/*
 * The hash table must be at least twice as big as the number
 * of patterns, preferably bigger. It must also be a prime number
 */
#define HSHSIZ	139

/*
 * Pattern table.
 */
struct pats {
	char	*name;
	char	*replace;
} ptab[] = {
	{ "_spl0\n",
"	mfpr	$18,r0\n\
	mtpr	$0,$18\n" },

	{ "_spl1\n",
"	mfpr	$18,r0\n\
	mtpr	$1,$18\n" },

	{ "_splsoftclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x8,$18\n" },

	{ "_splnet\n",
"	mfpr	$18,r0\n\
	mtpr	$0xc,$18\n" },

	{ "_splimp\n",
"	mfpr	$18,r0\n\
	mtpr	$0x16,$18\n" },

	{ "_spl4\n",
"	mfpr	$18,r0\n\
	mtpr	$0x14,$18\n" },

	{ "_splbio\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ "_spltty\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ "_spl5\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ "_splclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },

	{ "_spl6\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },

	{ "_spl7\n",
"	mfpr	$18,r0\n\
	mtpr	$0x1f,$18\n" },

	{ "_splhigh\n",
"	mfpr	$18,r0\n\
	mtpr	$0x1f,$18\n" },

	{ "_splx\n",
"	mfpr	$18,r0\n\
	mtpr	(sp)+,$18\n" },

	{ "_mfpr\n",
"	mfpr	(sp)+,r0\n" },

	{ "_mtpr\n",
"	mtpr	4(sp),(sp)\n\
	addl2	$8,sp\n" },

	{ "_setsoftclock\n",
"	mtpr	$0x8,$0x14\n" },

	{ "_resume\n",
"	ashl	$9,(sp)+,r0 \n\
	movpsl	-(sp) \n\
	jsb	_Resume\n" },

	{ "_bcopy\n", 
"	movc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "_ovbcopy\n",
"	movc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "_bzero\n",
"	movc5	$0,(r0),$0,4(sp),*(sp)\n\
	addl2	$8,sp\n" },

	{ "_bcmp\n",
"	popr	$0x7\n\
	cmpc3	r2,(r0),(r1)\n" },

	{ "_strncmp\n",
"	cmpc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "_blkclr\n",
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

	{ "_strlen\n",
"	movl	(sp),r1\n\
1:\n\
	locc	$0,$65535,(r1)\n\
	jeql	1b\n\
	subl3	(sp)+,r1,r0\n" },

	{ "_scanc\n",
"	popr	$0xf\n\
	scanc	r0,(r1),(r2),r3\n" },

	{ "_skpc\n",
"	popr	$0x7\n\
	skpc	r0,r1,(r2)\n" },

	{ "_copyin\n",
"	jsb	_Copyin\n\
	addl2	$12,sp\n" },

	{ "_copyout\n",
"	jsb	_Copyout\n\
	addl2	$12,sp\n" },

	{ "_fubyte\n",
"	movl	(sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ "_fuibyte\n",
"	movl (sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ "_fuword\n",
"	movl (sp)+,r0\n\
	jsb	_Fuword\n" },

	{ "_fuiword\n",
"	movl (sp)+,r0\n\
	jsb	_Fuword\n" },

	{ "_subyte\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ "_suibyte\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ "_suword\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ "_suiword\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ "_setrq\n",
"	movl	(sp)+,r0 \n\
	jsb	_Setrq\n" },

	{ "_remrq\n",
"	movl	(sp)+,r0 \n\
	jsb	_Remrq\n" },

	{ "_swtch\n",
"	movpsl	-(sp)\n\
	jsb	_Swtch\n" },

	{ "_setjmp\n",
"	movl	(sp)+,r0 \n\
	jsb	_Setjmp\n" },

	{ "_longjmp\n",
"	movl	(sp)+,r0 \n\
	jsb	_Longjmp\n" },

	{ "_ffs\n",
"	movl	(sp)+,r1\n\
	ffs	$0,$32,r1,r0 \n\
	bneq	1f \n\
	mnegl	$1,r0 \n\
1: \n\
	incl	r0\n" },

	{ "_htons\n",
"	rotl	$8,(sp),r0\n\
	movb	1(sp),r0\n\
	movzwl	r0,r0\n\
	addl2	$4,sp\n" },

	{ "_ntohs\n",
"	rotl	$8,(sp),r0\n\
	movb	1(sp),r0\n\
	movzwl	r0,r0\n\
	addl2	$4,sp\n" },

	{ "_htonl\n",
"	rotl	$-8,(sp),r0\n\
	insv	r0,$16,$8,r0\n\
	movb	3(sp),r0\n\
	addl2	$4,sp\n" },

	{ "_ntohl\n",
"	rotl	$-8,(sp),r0\n\
	insv	r0,$16,$8,r0\n\
	movb	3(sp),r0\n\
	addl2	$4,sp\n" },

	{ "__insque\n",
"	insque	*(sp)+,*(sp)+\n" },

	{ "__remque\n",
"	remque	*(sp)+,r0\n" },

	{ "__queue\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	insque	r1,*4(r0)\n" },

	{ "__dequeue\n",
"	movl	(sp)+,r0\n\
	remque	*(r0),r0\n" },
};
struct pats *htbl[HSHSIZ];

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
	char *argv[];
{
	register struct pats *pp, **hp;
	register char *cp, *ccp, *lp;
	register int hash, rehash, size;
	char line[BUFSIZ];
	extern char *index();

	if (argc > 1)
		freopen(argv[1], "r", stdin);
	if (argc > 2)
		freopen(argv[2], "w", stdout);
	/*
	 * Set up the hash table.
	 */
	for (pp = ptab; pp < &ptab[sizeof (ptab)/sizeof (ptab[0])]; pp++) {
		HASH(pp->name, hp);
		while (*hp)
			REHASH(hp);
		*hp = pp;
	}
	/*
	 * Check each line and replace as appropriate.
	 */
	while (fgets(line, BUFSIZ, stdin)) {
		lp = index(line, ':');
		for (cp = (lp != NULL) ? ++lp : line; *cp == '\t'; )
			cp++;
#define	CALLS	"calls\t"
		if (strncmp(cp, CALLS, sizeof (CALLS) - 1) != 0) {
			fputs(line, stdout);
			continue;
		}
		for (cp += sizeof (CALLS) - 1; *cp && *cp != ','; cp++)
			;
		if (*cp != ',') {
			fputs(line, stdout);
			continue;
		}
		cp++;
		HASH(cp, hp);
		while (*hp) {
			if (strncmp((*hp)->name, cp, size) == NULL) {
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
nextline:
		;
	}
	exit(0);
}
