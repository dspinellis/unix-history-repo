#ifndef lint
static char sccsid[] = "@(#)langpats.c	1.1 (Berkeley) %G%";
#endif

/*
 * In-line assembly code expander for
 * the kernel.  This code is stolen from
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
	{ "$0,_spl0\n",
"	mfpr	$18,r0\n\
	mtpr	$0,$18\n" },

	{ "$0,_spl1\n",
"	mfpr	$18,r0\n\
	mtpr	$1,$18\n" },

	{ "$0,_splsoftclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x8,$18\n" },

	{ "$0,_splnet\n",
"	mfpr	$18,r0\n\
	mtpr	$0xc,$18\n" },

	{ "$0,_splimp\n",
"	mfpr	$18,r0\n\
	mtpr	$0x16,$18\n" },

	{ "$0,_spl4\n",
"	mfpr	$18,r0\n\
	mtpr	$0x14,$18\n" },

#ifdef notdef
	{ "r[0-9]*,_spl4\n",
"	mfpr	$18,r0\n\
	mtpr	$0x14,$18\n" },
#endif

	{ "$0,_splbio",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

#ifdef notdef
	{ "r[0-9]*,_splbio",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },
#endif

	{ "$0,_spltty",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

#ifdef notdef
	{ "r[0-9]*,_spltty\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },
#endif

	{ "$0,_spl5\n", 
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

#ifdef notdef
	{ "r[0-9]*,_spl5\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },
#endif

	{ "$0,_splclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },

	{ "$0,_spl6\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },

#ifdef notdef
	{ "r[0-9]*,_spl6\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },
#endif

	{ "$0,_spl7\n",
"	mfpr	$18,r0\n\
	mtpr	$0x1f,$18\n" },

	{ "$1,_splx\n",
"	mfpr	$18,r0\n\
	mtpr	(sp)+,$18\n" },

	{ "$1,_mfpr\n",
"	mfpr	(sp)+,r0\n" },

	{ "$2,_mtpr\n",
"	mtpr	4(sp),(sp)\n\
	addl2	$8,sp\n" },

	{ "$0,_setsoftclock\n",
"	mtpr	$0x8,$0x14\n" },

	{ "$1,_resume\n",
"	ashl	$9,(sp)+,r0 \n\
	movpsl	-(sp) \n\
	jsb	_Resume\n" },

	{ "$3,_bcopy\n", 
"	movc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "$3,_ovbcopy\n",
"	movc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "$2,_bzero\n",
"	movc5	$0,(r0),$0,4(sp),*(sp)\n\
	addl2	$8,sp\n" },

	{ "$3,_bcmp\n",
"	popr	$0x7\n\
	cmpc3	r2,(r0),(r1)\n" },

	{ "$3,_strncmp\n",
"	cmpc3	8(sp),*(sp),*4(sp)\n\
	addl2	$12,sp\n" },

	{ "$2,_blkclr\n",
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

	{ "$1,_strlen\n",
"	movl	(sp),r1\n\
1:\n\
	locc	$0,$65535,(r1)\n\
	jeql	1b\n\
	subl3	(sp)+,r1,r0\n" },

	{ "$4,_scanc\n",
"	popr	$0xf\n\
	scanc	r0,(r1),(r2),r3\n" },

	{ "$3,_copyin\n",
"	jsb	_Copyin\n\
	addl2	$12,sp\n" },

	{ "$3,_copyout\n",
"	jsb	_Copyout\n\
	addl2	$12,sp\n" },

	{ "$1,_fubyte\n",
"	movl	(sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ "$1,_fuibyte\n",
"	movl (sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ "$1,_fuword\n",
"	movl (sp)+,r0\n\
	jsb	_Fuword\n" },

	{ "$1,_fuiword\n",
"	movl (sp)+,r0\n\
	jsb	_Fuword\n" },

	{ "$2,_subyte\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ "$2,_suibyte\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ "$2,_suword\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ "$2,_suiword\n",
"	movl (sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ "$1,_setrq\n",
"	movl	(sp)+,r0 \n\
	jsb	_Setrq\n" },

	{ "$1,_remrq\n",
"	movl	(sp)+,r0 \n\
	jsb	_Remrq\n" },

	{ "$0,_swtch\n",
"	movpsl	-(sp)\n\
	jsb	_Swtch\n" },

	{ "$1,_setjmp\n",
"	movl	(sp)+,r0 \n\
	jsb	_Setjmp\n" },

	{ "$1,_longjmp\n",
"	movl	(sp)+,r0 \n\
	jsb	_Longjmp\n" },

	{ "$1,_ffs\n",
"	movl	(sp)+,r1\n\
	ffs	$0,$32,r1,r0 \n\
	bneq	1f \n\
	mnegl	$1,r0 \n\
1: \n\
	incl	r0\n" },

	{ "$1,_htons\n",
"	rotl	$8,(sp),r0\n\
	movb	1(sp),r0\n\
	movzwl	r0,r0\n\
	addl2	$4,sp\n" },

	{ "$1,_ntohs\n",
"	rotl	$8,(sp),r0\n\
	movb	1(sp),r0\n\
	movzwl	r0,r0\n\
	addl2	$4,sp\n" },

	{ "$1,_htonl\n",
"	rotl	$-8,(sp),r0\n\
	insv	r0,$16,$8,r0\n\
	movb	3(sp),r0\n\
	addl2	$4,sp\n" },

	{ "$1,_ntohl\n",
"	rotl	$-8,(sp),r0\n\
	insv	r0,$16,$8,r0\n\
	movb	3(sp),r0\n\
	addl2	$4,sp\n" },

	{ "$2,__insque\n",
"	insque	*(sp)+,*(sp)+\n" },

	{ "$1,__remque\n",
"	remque	*(sp)+,r0\n" },

	{ "$2,__queue\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	insque	r1,*4(r0)\n" },

	{ "$1,__dequeue\n",
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
		cp += sizeof (CALLS) - 1;
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
