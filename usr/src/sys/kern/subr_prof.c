/*	subr_prof.c	4.3	82/12/17	*/

#ifdef GPROF
#include "../h/crt0.h"
#include "../h/param.h"
#include "../h/systm.h"

/*
 * Froms is actually a bunch of unsigned shorts indexing tos
 */
int	profiling = 3;
u_short	*froms = 0;
struct	tostruct *tos = 0;
u_short	tolimit = 0;
#ifdef vax
char	*s_lowpc = (char *)0x80000000;
#endif
#ifdef sun
char	*s_lowpc = (char *)0x4000;
#endif
extern	char etext;
char	*s_highpc = &etext;
u_long	s_textsize = 0;
int	ssiz;
u_short	*sbuf;
u_short	*kcount;

kmstartup()
{
	u_long	limit;

	s_textsize = s_highpc - s_lowpc;
	ssiz = s_textsize + sizeof(struct phdr);
	printf("Profiling kernel, s_textsize=%d [%x..%x]\n",
		s_textsize, s_lowpc, s_highpc);
	sbuf = (u_short *)wmemall(memall, ssiz);
	if (sbuf == 0) {
		printf("No space for monitor buffer(s)\n");
		return;
	}
	blkclr((caddr_t)sbuf, ssiz);
	froms = (u_short *)wmemall(memall, s_textsize);
	if (froms == 0) {
		printf("No space for monitor buffer(s)\n");
		wmemfree(sbuf, ssiz);
		sbuf = 0;
		return;
	}
	blkclr((caddr_t)froms, s_textsize);
	tos = (struct tostruct *)wmemall(memall, s_textsize);
	if (tos == 0) {
		printf("No space for monitor buffer(s)\n");
		wmemfree(sbuf, ssiz);
		sbuf = 0;
		wmemfree(froms, s_textsize);
		froms = 0;
		return;
	}
	blkclr((caddr_t)tos, s_textsize);
	tos[0].link = 0;
	limit = s_textsize / sizeof(struct tostruct);
	/*
	 * Tolimit is what mcount checks to see if
	 * all the data structures are ready!!!
	 * Make sure it won't overflow.
	 */
	tolimit = limit > 65534 ? 65534 : limit;
	((struct phdr *)sbuf)->lpc = s_lowpc;
	((struct phdr *)sbuf)->hpc = s_highpc;
	((struct phdr *)sbuf)->ncnt = ssiz;
	kcount = (u_short *)(((int)sbuf) + sizeof(struct phdr));
#ifdef notdef
	profiling = 0;		/* patch by hand when you're ready */
#endif
}

#ifdef vax
/*
 * This routine is massaged so that it may be jsb'ed to
 */
asm("#define _mcount mcount");
mcount()
{
	register char *selfpc;		/* r11 */
	register u_short *frompcindex;	/* r10 */
	register struct tostruct *top;	/* r9 */

	asm("	forgot to run ex script on gcrt0.s");
	asm("#define r11 r5");
	asm("#define r10 r4");
	asm("#define r9 r3");
#ifdef lint
	selfpc = (char *) 0;
	frompcindex = 0;
#else not lint
	/*
	 * Find the return address for mcount,
	 * and the return address for mcount's caller.
	 */
	asm("	movl (sp), r11");	/* selfpc = ... (jsb frame) */
	asm("	movl 16(fp), r10");	/* frompcindex =     (calls frame) */
#endif not lint
	/*
	 * Check that we are profiling
	 * and that we aren't recursively invoked.
	 */
	if (tolimit == 0)
		goto out;
	if (profiling)
		goto out;
	profiling++;
	/*
	 * Check that frompcindex is a reasonable pc value.
	 * For example:	signal catchers get called from the stack,
	 * 		not from text space.  too bad.
	 */
	frompcindex = (u_short *)((long)frompcindex - (long)s_lowpc);
	if ((u_long)frompcindex > s_textsize)
		goto done;
	frompcindex = &froms[((long)frompcindex) >> 1];
	if (*frompcindex != 0)
		top = &tos[*frompcindex];
	else {
		*frompcindex = ++tos[0].link;
		if (*frompcindex >= tolimit)
			goto overflow;
		top = &tos[*frompcindex];
		top->selfpc = selfpc;
		top->count = 0;
		top->link = 0;
	}
	for (; /* break */; top = &tos[top->link]) {
		if (top->selfpc == selfpc) {
			top->count++;
			break;
		}
		if (top->link != 0)
			continue;
		top->link = ++tos[0].link;
		if (top->link >= tolimit)
			goto overflow;
		top = &tos[top->link];
		top->selfpc = selfpc;
		top->count = 1;
		top->link = 0;
		break;
	}
done:
	profiling--;
	/* and fall through */
out:
	asm("	rsb");
	asm("#undef r11");
	asm("#undef r10");
	asm("#undef r9");
	asm("#undef _mcount");

overflow:
	tolimit = 0;
	printf("mcount: tos overflow\n");
	goto out;
}
#endif
#endif GPROF
