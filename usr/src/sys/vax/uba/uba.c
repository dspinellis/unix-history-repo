/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uba.c	6.5 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "conf.h"
#include "dk.h"
#include "kernel.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vax/nexus.h"
#include "ubareg.h"
#include "ubavar.h"

#if VAX780
char	ubasr_bits[] = UBASR_BITS;
#endif

/*
 * Do transfer on device argument.  The controller
 * and uba involved are implied by the device.
 * We queue for resource wait in the uba code if necessary.
 * We return 1 if the transfer was started, 0 if it was not.
 * If you call this routine with the head of the queue for a
 * UBA, it will automatically remove the device from the UBA
 * queue before it returns.  If some other device is given
 * as argument, it will be added to the request queue if the
 * request cannot be started immediately.  This means that
 * passing a device which is on the queue but not at the head
 * of the request queue is likely to be a disaster.
 */
ubago(ui)
	register struct uba_device *ui;
{
	register struct uba_ctlr *um = ui->ui_mi;
	register struct uba_hd *uh;
	register int s, unit;

	uh = &uba_hd[um->um_ubanum];
	s = spl6();
	if (um->um_driver->ud_xclu && uh->uh_users > 0 || uh->uh_xclu)
		goto rwait;
	um->um_ubinfo = ubasetup(um->um_ubanum, um->um_tab.b_actf->b_actf,
	    UBA_NEEDBDP|UBA_CANTWAIT);
	if (um->um_ubinfo == 0)
		goto rwait;
	uh->uh_users++;
	if (um->um_driver->ud_xclu)
		uh->uh_xclu = 1;
	splx(s);
	if (ui->ui_dk >= 0) {
		unit = ui->ui_dk;
		dk_busy |= 1<<unit;
		dk_xfer[unit]++;
		dk_wds[unit] += um->um_tab.b_actf->b_actf->b_bcount>>6;
	}
	if (uh->uh_actf == ui)
		uh->uh_actf = ui->ui_forw;
	(*um->um_driver->ud_dgo)(um);
	return (1);
rwait:
	if (uh->uh_actf != ui) {
		ui->ui_forw = NULL;
		if (uh->uh_actf == NULL)
			uh->uh_actf = ui;
		else
			uh->uh_actl->ui_forw = ui;
		uh->uh_actl = ui;
	}
	splx(s);
	return (0);
}

ubadone(um)
	register struct uba_ctlr *um;
{
	register struct uba_hd *uh = &uba_hd[um->um_ubanum];

	if (um->um_driver->ud_xclu)
		uh->uh_xclu = 0;
	uh->uh_users--;
	ubarelse(um->um_ubanum, &um->um_ubinfo);
}

/*
 * Allocate and setup UBA map registers, and bdp's
 * Flags says whether bdp is needed, whether the caller can't
 * wait (e.g. if the caller is at interrupt level).
 *
 * Return value:
 *	Bits 0-8	Byte offset
 *	Bits 9-17	Start map reg. no.
 *	Bits 18-27	No. mapping reg's
 *	Bits 28-31	BDP no.
 */
ubasetup(uban, bp, flags)
	struct buf *bp;
{
	register struct uba_hd *uh = &uba_hd[uban];
	int pfnum, temp;
	int npf, reg, bdp;
	unsigned v;
	register struct pte *pte, *io;
	struct proc *rp;
	int a, o, ubinfo;

#if VAX730
	if (cpu == VAX_730)
		flags &= ~UBA_NEEDBDP;
#endif
	v = btop(bp->b_un.b_addr);
	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = btoc(bp->b_bcount + o) + 1;
	a = spl6();
	while ((reg = rmalloc(uh->uh_map, (long)npf)) == 0) {
		if (flags & UBA_CANTWAIT) {
			splx(a);
			return (0);
		}
		uh->uh_mrwant++;
		sleep((caddr_t)&uh->uh_mrwant, PSWP);
	}
	if ((flags & UBA_NEED16) && reg + npf > 128) {
		/*
		 * Could hang around and try again (if we can ever succeed).
		 * Won't help any current device...
		 */
		rmfree(uh->uh_map, (long)npf, (long)reg);
		splx(a);
		return (0);
	}
	bdp = 0;
	if (flags & UBA_NEEDBDP) {
		while ((bdp = ffs(uh->uh_bdpfree)) == 0) {
			if (flags & UBA_CANTWAIT) {
				rmfree(uh->uh_map, (long)npf, (long)reg);
				splx(a);
				return (0);
			}
			uh->uh_bdpwant++;
			sleep((caddr_t)&uh->uh_bdpwant, PSWP);
		}
		uh->uh_bdpfree &= ~(1 << (bdp-1));
	} else if (flags & UBA_HAVEBDP)
		bdp = (flags >> 28) & 0xf;
	splx(a);
	reg--;
	ubinfo = (bdp << 28) | (npf << 18) | (reg << 9) | o;
	temp = (bdp << 21) | UBAMR_MRV;
	if (bdp && (o & 01))
		temp |= UBAMR_BO;
	rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
	if ((bp->b_flags & B_PHYS) == 0)
		pte = &Sysmap[btop(((int)bp->b_un.b_addr)&0x7fffffff)];
	else if (bp->b_flags & B_UAREA)
		pte = &rp->p_addr[v];
	else if (bp->b_flags & B_PAGET)
		pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
	else
		pte = vtopte(rp, v);
	io = &uh->uh_uba->uba_map[reg];
	while (--npf != 0) {
		pfnum = pte->pg_pfnum;
		if (pfnum == 0)
			panic("uba zero uentry");
		pte++;
		*(int *)io++ = pfnum | temp;
	}
	*(int *)io++ = 0;
	return (ubinfo);
}

/*
 * Non buffer setup interface... set up a buffer and call ubasetup.
 */
uballoc(uban, addr, bcnt, flags)
	int uban;
	caddr_t addr;
	int bcnt, flags;
{
	struct buf ubabuf;

	ubabuf.b_un.b_addr = addr;
	ubabuf.b_flags = B_BUSY;
	ubabuf.b_bcount = bcnt;
	/* that's all the fields ubasetup() needs */
	return (ubasetup(uban, &ubabuf, flags));
}
 
/*
 * Release resources on uba uban, and then unblock resource waiters.
 * The map register parameter is by value since we need to block
 * against uba resets on 11/780's.
 */
ubarelse(uban, amr)
	int *amr;
{
	register struct uba_hd *uh = &uba_hd[uban];
	register int bdp, reg, npf, s;
	int mr;
 
	/*
	 * Carefully see if we should release the space, since
	 * it may be released asynchronously at uba reset time.
	 */
	s = spl6();
	mr = *amr;
	if (mr == 0) {
		/*
		 * A ubareset() occurred before we got around
		 * to releasing the space... no need to bother.
		 */
		splx(s);
		return;
	}
	*amr = 0;
	bdp = (mr >> 28) & 0x0f;
	if (bdp) {
		switch (cpu) {
#if VAX780
		case VAX_780:
			uh->uh_uba->uba_dpr[bdp] |= UBADPR_BNE;
			break;
#endif
#if VAX750
		case VAX_750:
			uh->uh_uba->uba_dpr[bdp] |=
			    UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE;
			break;
#endif
		}
		uh->uh_bdpfree |= 1 << (bdp-1);		/* atomic */
		if (uh->uh_bdpwant) {
			uh->uh_bdpwant = 0;
			wakeup((caddr_t)&uh->uh_bdpwant);
		}
	}
	/*
	 * Put back the registers in the resource map.
	 * The map code must not be reentered,
	 * nor can the registers be freed twice.
	 * Unblock interrupts once this is done.
	 */
	npf = (mr >> 18) & 0x3ff;
	reg = ((mr >> 9) & 0x1ff) + 1;
	rmfree(uh->uh_map, (long)npf, (long)reg);
	splx(s);

	/*
	 * Wakeup sleepers for map registers,
	 * and also, if there are processes blocked in dgo(),
	 * give them a chance at the UNIBUS.
	 */
	if (uh->uh_mrwant) {
		uh->uh_mrwant = 0;
		wakeup((caddr_t)&uh->uh_mrwant);
	}
	while (uh->uh_actf && ubago(uh->uh_actf))
		;
}

ubapurge(um)
	register struct uba_ctlr *um;
{
	register struct uba_hd *uh = um->um_hd;
	register int bdp = (um->um_ubinfo >> 28) & 0x0f;

	switch (cpu) {
#if VAX780
	case VAX_780:
		uh->uh_uba->uba_dpr[bdp] |= UBADPR_BNE;
		break;
#endif
#if VAX750
	case VAX_750:
		uh->uh_uba->uba_dpr[bdp] |= UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE;
		break;
#endif
	}
}

ubainitmaps(uhp)
	register struct uba_hd *uhp;
{

	rminit(uhp->uh_map, (long)NUBMREG, (long)1, "uba", UAMSIZ);
	switch (cpu) {
#if VAX780
	case VAX_780:
		uhp->uh_bdpfree = (1<<NBDP780) - 1;
		break;
#endif
#if VAX750
	case VAX_750:
		uhp->uh_bdpfree = (1<<NBDP750) - 1;
		break;
#endif
#if VAX730
	case VAX_730:
		break;
#endif
	}
}

/*
 * Generate a reset on uba number uban.  Then
 * call each device in the character device table,
 * giving it a chance to clean up so as to be able to continue.
 */
ubareset(uban)
	int uban;
{
	register struct cdevsw *cdp;
	register struct uba_hd *uh = &uba_hd[uban];
	int s;

	s = spl6();
	uh->uh_users = 0;
	uh->uh_zvcnt = 0;
	uh->uh_xclu = 0;
	uh->uh_actf = uh->uh_actl = 0;
	uh->uh_bdpwant = 0;
	uh->uh_mrwant = 0;
	ubainitmaps(uh);
	wakeup((caddr_t)&uh->uh_bdpwant);
	wakeup((caddr_t)&uh->uh_mrwant);
	printf("uba%d: reset", uban);
	ubainit(uh->uh_uba);
	ubameminit(uban);
	for (cdp = cdevsw; cdp < cdevsw + nchrdev; cdp++)
		(*cdp->d_reset)(uban);
#ifdef INET
	ifubareset(uban);
#endif
	printf("\n");
	splx(s);
}

/*
 * Init a uba.  This is called with a pointer
 * rather than a virtual address since it is called
 * by code which runs with memory mapping disabled.
 * In these cases we really don't need the interrupts
 * enabled, but since we run with ipl high, we don't care
 * if they are, they will never happen anyways.
 */
ubainit(uba)
	register struct uba_regs *uba;
{

	switch (cpu) {
#if VAX780
	case VAX_780:
		uba->uba_cr = UBACR_ADINIT;
		uba->uba_cr = UBACR_IFS|UBACR_BRIE|UBACR_USEFIE|UBACR_SUEFIE;
		while ((uba->uba_cnfgr & UBACNFGR_UBIC) == 0)
			;
		break;
#endif
#if VAX750
	case VAX_750:
#endif
#if VAX730
	case VAX_730:
#endif
#if defined(VAX750) || defined(VAX730)
		mtpr(IUR, 0);
		/* give devices time to recover from power fail */
/* THIS IS PROBABLY UNNECESSARY */
		DELAY(500000);
/* END PROBABLY UNNECESSARY */
		break;
#endif
	}
}

#ifdef VAX780
int	ubawedgecnt = 10;
int	ubacrazy = 500;
int	zvcnt_max = 5000;	/* in 8 sec */
int	zvcnt_total;
long	zvcnt_time;
/*
 * This routine is called by the locore code to
 * process a UBA error on an 11/780.  The arguments are passed
 * on the stack, and value-result (through some trickery).
 * In particular, the uvec argument is used for further
 * uba processing so the result aspect of it is very important.
 * It must not be declared register.
 */
/*ARGSUSED*/
ubaerror(uban, uh, ipl, uvec, uba)
	register int uban;
	register struct uba_hd *uh;
	int ipl, uvec;
	register struct uba_regs *uba;
{
	register sr, s;

	if (uvec == 0) {
		long	dt = time.tv_sec - zvcnt_time;
		zvcnt_total++;
		if (dt > 8) {
			zvcnt_time = time.tv_sec;
			uh->uh_zvcnt = 0;
		}
		if (++uh->uh_zvcnt > zvcnt_max) {
			printf("uba%d: too many zero vectors (%d in <%d sec)\n",
				uban, uh->uh_zvcnt, dt + 1);
			printf("\tIPL 0x%x\n\tcnfgr: %b  Adapter Code: 0x%x\n",
				ipl, uba->uba_cnfgr&(~0xff), UBACNFGR_BITS,
				uba->uba_cnfgr&0xff);
			printf("\tsr: %b\n\tdcr: %x (MIC %sOK)\n",
				uba->uba_sr, ubasr_bits, uba->uba_dcr,
				(uba->uba_dcr&0x8000000)?"":"NOT ");
			ubareset(uban);
		}
		return;
	}
	if (uba->uba_cnfgr & NEX_CFGFLT) {
		printf("uba%d: sbi fault sr=%b cnfgr=%b\n",
		    uban, uba->uba_sr, ubasr_bits,
		    uba->uba_cnfgr, NEXFLT_BITS);
		ubareset(uban);
		uvec = 0;
		return;
	}
	sr = uba->uba_sr;
	s = spl7();
	printf("uba%d: uba error sr=%b fmer=%x fubar=%o\n",
	    uban, uba->uba_sr, ubasr_bits, uba->uba_fmer, 4*uba->uba_fubar);
	splx(s);
	uba->uba_sr = sr;
	uvec &= UBABRRVR_DIV;
	if (++uh->uh_errcnt % ubawedgecnt == 0) {
		if (uh->uh_errcnt > ubacrazy)
			panic("uba crazy");
		printf("ERROR LIMIT ");
		ubareset(uban);
		uvec = 0;
		return;
	}
	return;
}
#endif

/*
 * Look for devices with unibus memory, allow them to configure, then disable
 * map registers as necessary.  Called during autoconfiguration and ubareset.
 * The device ubamem routine returns 0 on success, 1 on success if it is fully
 * configured (has no csr or interrupt, so doesn't need to be probed),
 * and -1 on failure.
 */
ubameminit(uban)
{
	register struct uba_device *ui;
	register struct uba_hd *uh = &uba_hd[uban];
	caddr_t umembase = umem[uban] + 0x3e000, addr;
#define	ubaoff(off)	((int)(off) & 0x1fff)

	uh->uh_lastmem = 0;
	for (ui = ubdinit; ui->ui_driver; ui++) {
		if (ui->ui_ubanum != uban && ui->ui_ubanum != '?')
			continue;
		if (ui->ui_driver->ud_ubamem) {
			/*
			 * During autoconfiguration, need to fudge ui_addr.
			 */
			addr = ui->ui_addr;
			ui->ui_addr = umembase + ubaoff(addr);
			switch ((*ui->ui_driver->ud_ubamem)(ui, uban)) {
			case 1:
				ui->ui_alive = 1;
				/* FALLTHROUGH */
			case 0:
				ui->ui_ubanum = uban;
				break;
			}
			ui->ui_addr = addr;
		}
	}
#if VAX780
	/*
	 * On a 780, throw away any map registers disabled by rounding
	 * the map disable in the configuration register
	 * up to the next 8K boundary, or below the last unibus memory.
	 */
	if (cpu == VAX_780) {
		register i;

		i = btop(((uh->uh_lastmem + 8191) / 8192) * 8192);
		while (i)
			(void) rmget(uh->uh_map, 1, i--);
	}
#endif
}

/*
 * Allocate UNIBUS memory.  Allocates and initializes
 * sufficient mapping registers for access.  On a 780,
 * the configuration register is setup to disable UBA
 * response on DMA transfers to addresses controlled
 * by the disabled mapping registers.
 * On a 780, should only be called from ubameminit, or in ascending order
 * from 0 with 8K-sized and -aligned addresses; freeing memory that isn't
 * the last unibus memory would free unusable map registers.
 * Doalloc is 1 to allocate, 0 to deallocate.
 */
ubamem(uban, addr, npg, doalloc)
	int uban, addr, npg, doalloc;
{
	register struct uba_hd *uh = &uba_hd[uban];
	register int a;
	int s;

	a = (addr >> 9) + 1;
	s = spl6();
	if (doalloc)
		a = rmget(uh->uh_map, npg, a);
	else
		rmfree(uh->uh_map, (long)npg, (long)a);
	splx(s);
	if (a) {
		register int i, *m;

		m = (int *)&uh->uh_uba->uba_map[a - 1];
		for (i = 0; i < npg; i++)
			*m++ = 0;	/* All off, especially 'valid' */
		i = addr + npg * 512;
		if (doalloc && i > uh->uh_lastmem)
			uh->uh_lastmem = i;
		else if (doalloc == 0 && i == uh->uh_lastmem)
			uh->uh_lastmem = addr;
#if VAX780
		/*
		 * On a 780, set up the map register disable
		 * field in the configuration register.  Beware
		 * of callers that request memory ``out of order''
		 * or in sections other than 8K multiples.
		 * Ubameminit handles such requests properly, however.
		 */
		if (cpu == VAX_780) {
			i = uh->uh_uba->uba_cr &~ 0x7c000000;
			i |= ((uh->uh_lastmem + 8191) / 8192) << 26;
			uh->uh_uba->uba_cr = i;
		}
#endif
	}
	return (a);
}

#include "ik.h"
#if NIK > 0
/*
 * Map a virtual address into users address space. Actually all we
 * do is turn on the user mode write protection bits for the particular
 * page of memory involved.
 */
maptouser(vaddress)
	caddr_t vaddress;
{

	Sysmap[(((unsigned)(vaddress))-0x80000000) >> 9].pg_prot = (PG_UW>>27);
}

unmaptouser(vaddress)
	caddr_t vaddress;
{

	Sysmap[(((unsigned)(vaddress))-0x80000000) >> 9].pg_prot = (PG_KW>>27);
}
#endif
