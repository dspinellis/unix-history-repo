/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_print.c	7.20 (Berkeley) %G%
 */

#include "machine/mtpr.h"
#undef ISP
#undef TB
#include "../kdb/defs.h"
#undef CTRL
#include "ioctl.h"
#include "tty.h"
#include "vnode.h"
#include "mount.h"

char	*kdbBADRAD;

char	*kdbBADMOD;
char	*kdblp;
long	kdbmaxpos;
int	kdbradix;
char	kdblastc;

/* breakpoints */
BKPTR	kdbbkpthead;

extern	REGLIST kdbreglist[];

/* general printing routines ($) */

kdbprinttrace(modif)
{
	register int i;
	register BKPTR bkptr;
	register char *comptr;
	register struct nlist *sp;
	register struct proc *p;
	extern struct proc *allproc;

	if (kdbcntflg==0)
		kdbcntval = -1;
	switch (modif) {

	case 'd':
		if (kdbadrflg) {
			if (kdbadrval < 2 || kdbadrval > 16)
				kdberror(kdbBADRAD);
			kdbradix = kdbadrval;
		}
		kdbprintf("radix=%d base ten", kdbradix);
		break;

	case 'w': case 'W':
		kdbprintf("maxpos=%d", kdbmaxpos=(kdbadrflg?kdbadrval:MAXPOS));
		break;

	case 's': case 'S':
		kdbprintf("maxoff=%d", kdbmaxoff=(kdbadrflg?kdbadrval:MAXOFF));
		break;

	case 'V':
		kdbprintf("variables\n");
		for (i=0;i<=35;i++)
			if (kdbvar[i]) {
				kdbprintc((i<=9 ? '0' : 'a'-10) + i);
				kdbprintf(" = %R\n",kdbvar[i]);
			}
		break;

	case 0: case '?':
		if (p = (struct proc *)kdbvar[kdbvarchk('p')])
			kdbprintf("pid = %d\n", p->p_pid);
		else
			kdbprintf("in idle loop\n");
		kdbprinttrap(kdbvar[kdbvarchk('t')], kdbvar[kdbvarchk('c')]);
		/* fall thru... */
	case 'r': case 'R':
		kdbprintregs(modif);
		return;

	case 'c': case 'C':
		kdbstacktrace(modif == 'C');
		break;

		/*print externals*/
	case 'e': case 'E':
		for (sp = kdbsymtab; sp < kdbesymtab; sp++)
			if (sp->n_type==(N_DATA|N_EXT) ||
			    sp->n_type==(N_BSS|N_EXT))
				kdbprintf("%s:%12t%R\n", sp->n_un.n_name,
					kdbget((off_t)sp->n_value, DSP));
		break;

		/*print breakpoints*/
	case 'b': case 'B':
		kdbprintf("breakpoints\ncount%8tbkpt%24tcommand\n");
		for (bkptr=kdbbkpthead; bkptr; bkptr=bkptr->nxtbkpt)
			if (bkptr->flag) {
		   		kdbprintf("%-8.8d",bkptr->count);
				kdbpsymoff((long)bkptr->loc,ISYM,"%24t");
				comptr=bkptr->comm;
				while (*comptr)
					kdbprintc(*comptr++);
			}
		break;

	case 'k':
		panic("kdb requested panic");
		/* NOTREACHED */

	case 'l': {
		struct pte savemmap;
		extern char vmmap[];

		savemmap = mmap[0];
		for (p = allproc; p; p = p->p_nxt) {
			kdbprintf("%X pid %5d%c%5d %c ", p, p->p_pid,
				p == (struct proc *)kdbvar[kdbvarchk('p')]? '*' : ' ',
				p->p_ppid,
				p->p_stat == SSLEEP ? 'S' :
				p->p_stat == SRUN ? 'R':
				p->p_stat == SIDL ? 'I':
				p->p_stat == SSTOP ? 'T' : '?');
			if (p->p_wchan)
				kdbpsymoff((long)p->p_wchan, ISYM, "");
			if ((p->p_flag & SLOAD) && p->p_addr) {
				int i;
				*(int *)mmap = *(int *)p->p_addr;
				mtpr(TBIS, vmmap);
#define U	((struct user *)vmmap)
#ifdef not_until_uarea_completely_mapped
				if (U->u_ttyp)
					kdbprintf(" ctty %x ", U->u_ttyp);
#endif
				kdbprintf(" %.8s ", p->p_comm);
#undef U
			}

			kdbprintc(EOR);
		}
		mmap[0] = savemmap;
		mtpr(TBIS, vmmap);
		break;
	}

	case 't':	/* XXX - debug */
		if (kdbadrflg) {
		      kdbprintf("dev       state  rawq   canq  outq  lwat hwat\n");

#define T	((struct tty *)kdbadrval)	
			kdbprintf("%x  %x %d %d %d %d %d\n", T->t_dev, 
				T->t_state, T->t_rawq.c_cc, 
				T->t_canq.c_cc, T->t_outq.c_cc,
				T->t_lowat, T->t_hiwat);
	       kdbprintf(" &rawq    &canq      &outq    &outq.c_cf  &rawq.c_cf\n");
	       		kdbprintf(" %x %x  %x %x %x \n", &T->t_rawq, 
				&T->t_canq, &T->t_outq, &T->t_outq.c_cf, 
				&T->t_rawq.c_cf);
#undef T
		}

	case 'v': {
		register struct mount *mp;
		register struct vnode *vp;

		kdbprintf("Locked vnodes\n");
		mp = rootfs;
		do {
			for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf)
				if (VOP_ISLOCKED(vp))
					vprint((char *)0, vp);
			mp = mp->mnt_next;
		} while (mp != rootfs);
		break;
	}

	default:
		kdberror(kdbBADMOD);
	}
}

static
kdbprintregs(c)
{
	register REGPTR	p;
	ADDR v;

	for (p = kdbreglist; p->rname; p++) {
		if (c != 'R' && ishiddenreg(p))
			continue;
		v = *p->rkern;
		kdbprintf("%s%6t%R %16t", p->rname, v);
		kdbvalpr((long)v, p->rkern == &kdbpcb.pcb_pc ? ISYM : DSYM);
		kdbprintc(EOR);
	}
	kdbprintpc();
}

kdbgetreg(regnam)
{
	register REGPTR	p;
	register char *regptr;
	char *olp;

	olp = kdblp;
	for (p = kdbreglist; p->rname; p++) {
		regptr = p->rname;
		if (regnam == *regptr++) {
			while (*regptr)
				if (kdbreadchar() != *regptr++) {
					--regptr;
					break;
				}
			if (*regptr)
				kdblp = olp;
			else
				return ((int)p->rkern);
		}
	}
	kdblp = olp;
	return (-1);
}

kdbprintpc()
{

	kdbpsymoff((long)kdbpcb.pcb_pc, ISYM, ":%16t");
	kdbprintins(ISP, (long)kdbchkget((off_t)kdbpcb.pcb_pc, ISP));
	kdbprintc(EOR);
}
