/*
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)uba.c	7.7 (Berkeley) 2/15/89
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "conf.h"
#include "dkstat.h"
#include "kernel.h"

#include "../vax/pte.h"
#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vax/nexus.h"
#include "ubareg.h"
#include "ubavar.h"

#ifdef DW780
char	ubasr_bits[] = UBASR_BITS;
#endif

#define	spluba	spl7		/* IPL 17 */

/*
 * Do transfer on device argument.  The controller
 * and uba involved are implied by the device.
 * We queue for resource wait in the uba code if necessary.
 * We return 1 if the transfer was started, 0 if it was not.
 *
 * The onq argument must be zero iff the device is not on the
 * queue for this UBA.  If onq is set, the device must be at the
 * head of the queue.  In any case, if the transfer is started,
 * the device will be off the queue, and if not, it will be on.
 *
 * Drivers that allocate one BDP and hold it for some time should
 * set ud_keepbdp.  In this case um_bdp tells which BDP is allocated
 * to the controller, unless it is zero, indicating that the controller
 * does not now have a BDP.
 */
ubaqueue(ui, onq)
	register struct uba_device *ui;
	int onq;
{
	register struct uba_ctlr *um = ui->ui_mi;
	register struct uba_hd *uh;
	register struct uba_driver *ud;
	register int s, unit;

	uh = &uba_hd[um->um_ubanum];
	ud = um->um_driver;
	s = spluba();
	/*
	 * Honor exclusive BDP use requests.
	 */
	if (ud->ud_xclu && uh->uh_users > 0 || uh->uh_xclu)
		goto rwait;
	if (ud->ud_keepbdp) {
		/*
		 * First get just a BDP (though in fact it comes with
		 * one map register too).
		 */
		if (um->um_bdp == 0) {
			um->um_bdp = uballoc(um->um_ubanum,
				(caddr_t)0, 0, UBA_NEEDBDP|UBA_CANTWAIT);
			if (um->um_bdp == 0)
				goto rwait;
		}
		/* now share it with this transfer */
		um->um_ubinfo = ubasetup(um->um_ubanum,
			um->um_tab.b_actf->b_actf,
			um->um_bdp|UBA_HAVEBDP|UBA_CANTWAIT);
	} else
		um->um_ubinfo = ubasetup(um->um_ubanum,
			um->um_tab.b_actf->b_actf, UBA_NEEDBDP|UBA_CANTWAIT);
	if (um->um_ubinfo == 0)
		goto rwait;
	uh->uh_users++;
	if (ud->ud_xclu)
		uh->uh_xclu = 1;
	splx(s);
	if (ui->ui_dk >= 0) {
		unit = ui->ui_dk;
		dk_busy |= 1<<unit;
		dk_xfer[unit]++;
		dk_wds[unit] += um->um_tab.b_actf->b_actf->b_bcount>>6;
	}
	if (onq)
		uh->uh_actf = ui->ui_forw;
	(*ud->ud_dgo)(um);
	return (1);
rwait:
	if (!onq) {
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
	if (um->um_driver->ud_keepbdp)
		um->um_ubinfo &= ~BDPMASK;	/* keep BDP for misers */
	ubarelse(um->um_ubanum, &um->um_ubinfo);
}

/*
 * Allocate and setup UBA map registers, and bdp's
 * Flags says whether bdp is needed, whether the caller can't
 * wait (e.g. if the caller is at interrupt level).
 * Return value encodes map register plus page offset,
 * bdp number and number of map registers.
 */
ubasetup(uban, bp, flags)
	int uban;
	register struct buf *bp;
	register int flags;
{
	register struct uba_hd *uh = &uba_hd[uban];
	register struct pte *pte, *io;
	register int npf;
	int pfnum, temp;
	int reg, bdp;
	unsigned v;
	struct proc *rp;
	int a, o, ubinfo;

#ifdef DW730
	if (uh->uh_type == DW730)
		flags &= ~UBA_NEEDBDP;
#endif
#ifdef QBA
	if (uh->uh_type == QBA)
		flags &= ~UBA_NEEDBDP;
#endif
	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = btoc(bp->b_bcount + o) + 1;
	if (npf > UBA_MAXNMR)
		panic("uba xfer too big");
	a = spluba();
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
		while ((bdp = ffs((long)uh->uh_bdpfree)) == 0) {
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
	ubinfo = UBAI_INFO(o, reg, npf, bdp);
	temp = (bdp << 21) | UBAMR_MRV;
	if (bdp && (o & 01))
		temp |= UBAMR_BO;
	if ((bp->b_flags & B_PHYS) == 0)
		pte = kvtopte(bp->b_un.b_addr);
	else if (bp->b_flags & B_PAGET)
		pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
	else {
		rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
		v = btop(bp->b_un.b_addr);
		if (bp->b_flags & B_UAREA)
			pte = &rp->p_addr[v];
		else
			pte = vtopte(rp, v);
	}
	io = &uh->uh_mr[reg];
	while (--npf > 0) {
		pfnum = pte->pg_pfnum;
		if (pfnum == 0)
			panic("uba zero uentry");
		pte++;
		*(int *)io++ = pfnum | temp;
	}
	*(int *)io = 0;
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
	s = spluba();
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
	bdp = UBAI_BDP(mr);
	if (bdp) {
		switch (uh->uh_type) {
#ifdef DWBUA
		case DWBUA:
			BUA(uh->uh_uba)->bua_dpr[bdp] |= BUADPR_PURGE;
			break;
#endif
#ifdef DW780
		case DW780:
			uh->uh_uba->uba_dpr[bdp] |= UBADPR_BNE;
			break;
#endif
#ifdef DW750
		case DW750:
			uh->uh_uba->uba_dpr[bdp] |=
			    UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE;
			break;
#endif
		default:
			break;
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
	npf = UBAI_NMR(mr);
	reg = UBAI_MR(mr) + 1;
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
	while (uh->uh_actf && ubaqueue(uh->uh_actf, 1))
		;
}

ubapurge(um)
	register struct uba_ctlr *um;
{
	register struct uba_hd *uh = um->um_hd;
	register int bdp = UBAI_BDP(um->um_ubinfo);

	switch (uh->uh_type) {
#ifdef DWBUA
	case DWBUA:
		BUA(uh->uh_uba)->bua_dpr[bdp] |= BUADPR_PURGE;
		break;
#endif
#ifdef DW780
	case DW780:
		uh->uh_uba->uba_dpr[bdp] |= UBADPR_BNE;
		break;
#endif
#ifdef DW750
	case DW750:
		uh->uh_uba->uba_dpr[bdp] |= UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE;
		break;
#endif
	default:
		break;
	}
}

ubainitmaps(uhp)
	register struct uba_hd *uhp;
{

	if (uhp->uh_memsize > UBA_MAXMR)
		uhp->uh_memsize = UBA_MAXMR;
	rminit(uhp->uh_map, (long)uhp->uh_memsize, (long)1, "uba", UAMSIZ);
	switch (uhp->uh_type) {
#ifdef DWBUA
	case DWBUA:
		uhp->uh_bdpfree = (1<<NBDPBUA) - 1;
		break;
#endif
#ifdef DW780
	case DW780:
		uhp->uh_bdpfree = (1<<NBDP780) - 1;
		break;
#endif
#ifdef DW750
	case DW750:
		uhp->uh_bdpfree = (1<<NBDP750) - 1;
		break;
#endif
	default:
		break;
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

	s = spluba();
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
	ifubareset(uban);
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
 * SHOULD GET POINTER TO UBA_HD INSTEAD OF UBA.
 */
ubainit(uba)
	register struct uba_regs *uba;
{
	register struct uba_hd *uhp;
#ifdef QBA
	int isphys = 0;
#endif

	for (uhp = uba_hd; uhp < uba_hd + numuba; uhp++) {
		if (uhp->uh_uba == uba)
			break;
		if (uhp->uh_physuba == uba) {
#ifdef QBA
			isphys++;
#endif
			break;
		}
	}
	if (uhp >= uba_hd + numuba) {
		printf("init unknown uba\n");
		return;
	}

	switch (uhp->uh_type) {
#ifdef DWBUA
	case DWBUA:
		BUA(uba)->bua_csr |= BUACSR_UPI;
		/* give devices time to recover from power fail */
		DELAY(500000);
		break;
#endif
#ifdef DW780
	case DW780:
		uba->uba_cr = UBACR_ADINIT;
		uba->uba_cr = UBACR_IFS|UBACR_BRIE|UBACR_USEFIE|UBACR_SUEFIE;
		while ((uba->uba_cnfgr & UBACNFGR_UBIC) == 0)
			;
		break;
#endif
#ifdef DW750
	case DW750:
#endif
#ifdef DW730
	case DW730:
#endif
#ifdef QBA
	case QBA:
#endif
#if DW750 || DW730 || QBA
		mtpr(IUR, 0);
		/* give devices time to recover from power fail */
/* THIS IS PROBABLY UNNECESSARY */
		DELAY(500000);
/* END PROBABLY UNNECESSARY */
#ifdef QBA
		/*
		 * Re-enable local memory access
		 * from the Q-bus.
		 */
		if (uhp->uh_type == QBA) {
			if (isphys)
				*((char *)QIOPAGE630 + QIPCR) = Q_LMEAE;
			else
				*(uhp->uh_iopage + QIPCR) = Q_LMEAE;
		}
#endif QBA
		break;
#endif DW750 || DW730 || QBA
	}
}

#ifdef QBA
/*
 * Determine the interrupt priority of a Q-bus
 * peripheral.  The device probe routine must spl6(),
 * attempt to make the device request an interrupt,
 * delaying as necessary, then call this routine
 * before resetting the device.
 */
qbgetpri()
{
	int pri;
	extern int cvec;

	for (pri = 0x17; pri > 0x14; ) {
		if (cvec && cvec != 0x200)	/* interrupted at pri */
			break;
		pri--;
		splx(pri - 1);
	}
	(void) spl0();
	return (pri);
}
#endif

#ifdef DW780
int	ubawedgecnt = 10;
int	ubacrazy = 500;
int	zvcnt_max = 5000;	/* in 8 sec */
/*
 * This routine is called by the locore code to process a UBA
 * error on an 11/780 or 8600.  The arguments are passed
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
		/*
		 * Declare dt as unsigned so that negative values
		 * are handled as >8 below, in case time was set back.
		 */
		u_long	dt = time.tv_sec - uh->uh_zvtime;

		uh->uh_zvtotal++;
		if (dt > 8) {
			uh->uh_zvtime = time.tv_sec;
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
	s = spluba();
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
#ifdef DW780
	/*
	 * On a DW780, throw away any map registers disabled by rounding
	 * the map disable in the configuration register
	 * up to the next 8K boundary, or below the last unibus memory.
	 */
	if (uh->uh_type == DW780) {
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
 * On a DW780, should only be called from ubameminit, or in ascending order
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
	s = spluba();
	if (doalloc)
		a = rmget(uh->uh_map, npg, a);
	else
		rmfree(uh->uh_map, (long)npg, (long)a);
	splx(s);
	if (a) {
		register int i, *m;

		m = (int *)&uh->uh_mr[a - 1];
		for (i = 0; i < npg; i++)
			*m++ = 0;	/* All off, especially 'valid' */
		i = addr + npg * 512;
		if (doalloc && i > uh->uh_lastmem)
			uh->uh_lastmem = i;
		else if (doalloc == 0 && i == uh->uh_lastmem)
			uh->uh_lastmem = addr;
#ifdef DW780
		/*
		 * On a 780, set up the map register disable
		 * field in the configuration register.  Beware
		 * of callers that request memory ``out of order''
		 * or in sections other than 8K multiples.
		 * Ubameminit handles such requests properly, however.
		 */
		if (uh->uh_type == DW780) {
			i = uh->uh_uba->uba_cr &~ 0x7c000000;
			i |= ((uh->uh_lastmem + 8191) / 8192) << 26;
			uh->uh_uba->uba_cr = i;
		}
#endif
	}
	return (a);
}

#include "ik.h"
#include "vs.h"
#if NIK > 0 || NVS > 0
/*
 * Map a virtual address into users address space. Actually all we
 * do is turn on the user mode write protection bits for the particular
 * page of memory involved.
 */
maptouser(vaddress)
	caddr_t vaddress;
{

	kvtopte(vaddress)->pg_prot = (PG_UW >> 27);
}

unmaptouser(vaddress)
	caddr_t vaddress;
{

	kvtopte(vaddress)->pg_prot = (PG_KW >> 27);
}
#endif
