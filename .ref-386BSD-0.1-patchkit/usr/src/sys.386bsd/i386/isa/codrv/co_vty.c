/*      Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 *	@(#) $RCSfile: co_vty.c,v $	$Revision: 1.6 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:58 $
 *
 * This file includes the data table for virtual terminals
 */
static char *rcsid = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_vty.c,v 1.6 93/01/23 23:14:58 root Exp Locker: root $";

/*
 * History: see CO_HISTORY
 */

#include <machine/stdarg.h>
#include "co_hdr.h"

#ifdef MINITERM
#define NVTY 1
#else
#include "vty.h"
#endif

#if NVTY > 0

#if NVTY > 12
#undef NVTY
#define NVTY 12
#endif

#else /* ! NVTY > 0 */
#undef NVTY
#define NVTY 1
#endif /* ! NVTY > 0 */

struct vty vtys[NVTY];
struct tty pccons[NVTY];
int nvty = NVTY;

/*
 * returns a pointer to a vty, when called with a 
 * vty devmajor/minor, 0 when called with /dev/kbd
 */
struct vty *dev2vty(dev_t dev) 
{
	register m = minor(dev);

	/* check whether called from a vty or kbd:
	 * kbd has global scope 
	 */
	if (major(dev)==consoftc.cs_mymajor) 
		return 0;

	return &vtys[m>=nvty?0:m];
}

/*
 * initialize the vty data structure 
 */
void vty_init(int first)
{
	int i,adr;
	struct vty *vp;
	struct outmode *std,*kern;

	/* first mode: only 1 vty for startup, must be extended
	 * when remainder of system is up
	 */
	nvty = first ? 1 : NVTY;

	for (i=0; i<nvty; i++) {
		vp = &vtys[i];
		std = &vp->om[0];
		kern= &vp->om[1];
		vp->op = std;

		vp->vtynum = i;

		std->escstate = 
		kern->escstate = 0;		/* "ESC_NONE" */
		std->parcnt = 
		kern->parcnt = 0;

		emul_setattributes(vp, 0, 0);	/* set std attributes */
		emul_setattributes(vp, 0, 1);	/* set kernel attributes */
		emul_setattributes(vp, 4, 0);	/* set so attributes */
		std->f2 =
		kern->f2 = 0;
		vp->so = 0;

		vp->pitch = 0x31b;		/* default 1.500 kHz */
		vp->duration = hz/4;

		/* initialize vty tty structure */
		vp->ttyp = &pccons[i];
		vp->ttycnt = 0;

		/* set default # of rows and columns */
		vp->ncol = DEFAULTCOL;
		vp->nrow = DEFAULTROW;
		vp->size = vp->ncol * vp->nrow;
		vp->visible = 
		vp->altgrlock = 
		vp->shiftlock = 
		vp->scroll = 
		vp->num =
		vp->caps = 0;

		/* set Screen */
		if (first) {
			/* set console to screen */
			vp->Crtat = vp->crtat = Crtat;
			vp->col = vp->row = 0;
			vp->vbuf = 0;
			actvty = vp;
			emul_clearcursor(vp,2);
		}
		else {
			/* now also vm is initialized */
			vp->vbuf = (u_short*)malloc(vp->size*CHR, M_TEMP, M_NOWAIT);
			if (i > 0) {
				vp->crtat = vp->Crtat = vp->vbuf;
				vp->col = vp->row = 0;
				actvty = vp;
				emul_clearcursor(vp,2);
			}
		}
	}

	/* execute the local vt_emulator initializations */
	vtemul_init();

	/* set active vty */
	vty_setactive(0,2);
}

/*
 * set the active vty, which writes to screen (-1: suspend writing to screen)
 * sw=0: only set page for writing
 * sw=1: do a physical switch (save/restore pages first)
 * sw=2: do a logical switch (don't save/restore pages)
 * sw=3: restore page without saving old page
 */
void vty_setactive(int vtyno,int sw)
{
	struct vty *oldvty;
	register struct vty *vp;
	int pos,s;

	/* protect switch */
	vtswsema = 1;

	/* to disable writing to screen entirely */
	if (vtyno == -1) {
		pos = actvty->crtat - actvty->Crtat;
		actvty->Crtat = actvty->vbuf;
		actvty->crtat = actvty->Crtat + pos;
		return;
	}

	if (vtyno < nvty) {

		/* save old vty, and set new one */
		oldvty = actvty;
		vp = actvty = &vtys[vtyno];

		switch (sw) {
		case 0:
			break;
		case 1:			
			/* save screen */
			bcopy(Crtat,oldvty->vbuf,oldvty->size*CHR);
			/*FALLTHRU*/
		case 3:
			/* restore videopage of new vty */
			bcopy(vp->vbuf,Crtat,vp->size*CHR);
			/*FALLTHRU*/
		case 2:
			/* adjust old pointers */
			pos = oldvty->crtat - oldvty->Crtat;
			oldvty->Crtat = oldvty->vbuf;
			oldvty->crtat = oldvty->vbuf + pos;
			oldvty->visible = 0;

			/* set the write pointers 
			 * these normally point to the vbuf, but on a visible
			 * active screen they point to video memory
			 */
			pos = vp->crtat - vp->Crtat;
			vp->Crtat = Crtat;
			vp->crtat = vp->Crtat + pos;

			/* set cursor */
			vga_setcursorpos(pos);

			/* restore LEDS */
			kbd_setleds(leds(vp));

			actvty->visible = 1;
		}
	}

	vtswsema = 0;
}

#ifndef MINITERM
/*
 *	switch to next vty
 */ 
void vty_next()
{
	int n = actvty->vtynum;
	if (nvty==1) return;

	n = (n+1) % nvty;
	
	vty_setactive(n,1);
}

/*
 *	switch to previous vty
 */
void vty_previous()
{
	int n = actvty->vtynum;
	if (nvty==1) return;

	n = (n==0) ? nvty-1 : n-1;
	
	vty_setactive(n,1);
}
#endif /*!MINITERM*/

/* 
 * vty_broadcast: Send a message to all vtys
 */
void
#ifdef __STDC__
vty_broadcast(const char *fmt, ...)
#else
vty_broadcast(fmt /*, va_alist */)
	char *fmt;
#endif
{
	int i;
	va_list ap;

#define TOCONS	0x01	/* inherited from /sys/kern/subr_prf.c */
#define TOTTY	0x02	/* inherited from /sys/kern/subr_prf.c */
	va_start(ap, fmt);
	for (i=0; i<nvty; i++) {
		kprintf(fmt,TOCONS, vtys[i].ttyp, ap);
	}
	va_end(ap);
}
