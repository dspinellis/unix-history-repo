/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: Utah $Hdr: autoconf.c 1.36 92/12/20$
 *
 *	@(#)autoconf.c	8.2 (Berkeley) 1/12/94
 */

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/dkstat.h>
#include <sys/conf.h>
#include <sys/dmap.h>
#include <sys/reboot.h>

#include <machine/vmparam.h>
#include <machine/cpu.h>
#include <hp300/hp300/pte.h>
#include <hp300/hp300/isr.h>
#include <hp/dev/device.h>
#include <hp/dev/grfreg.h>
#include <hp/dev/hilreg.h>

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold;		    /* if 1, still working on cold-start */
int	dkn;		    /* number of iostat dk numbers assigned so far */
int	cpuspeed = 0;	    /* relative cpu speed -- can be patched */	
struct	isr isrqueue[NISR];
struct	hp_hw sc_table[MAXCTLRS];

/* XXX must be allocated statically because of early console init */
struct	map extiomap[EIOMAPSIZE/16];

extern	caddr_t internalhpib;
extern	char *extiobase;

#ifdef DEBUG
int	acdebug = 0;
#endif

/*
 * Determine mass storage and memory configuration for a machine.
 */
configure()
{
	register struct hp_hw *hw;
	int found;

	/*
	 * XXX: these should be consolidated into some kind of table
	 */
	hilsoftinit(0, HILADDR);
	hilinit(0, HILADDR);
	isrinit();
	dmainit();

	/*
	 * Look over each hardware device actually found and attempt
	 * to match it with an ioconf.c table entry.
	 */
	for (hw = sc_table; hw->hw_type; hw++) {
		if (HW_ISCTLR(hw))
			found = find_controller(hw);
		else
			found = find_device(hw);
#ifdef DEBUG
		if (!found) {
			int sc = patosc(hw->hw_pa);

			printf("unconfigured card id %x ", hw->hw_id);
			if (sc < 256)
				printf("at sc%d\n", sc);
			else
				printf("csr at %x\n", sc);
		}
#endif
	}

#if GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	swapconf();
	cold = 0;
}

#define dr_type(d, s)	\
	(strcmp((d)->d_name, (s)) == 0)

#define same_hw_ctlr(hw, hc) \
	(HW_ISHPIB(hw) && dr_type((hc)->hp_driver, "hpib") || \
	 HW_ISSCSI(hw) && dr_type((hc)->hp_driver, "scsi"))

find_controller(hw)
	register struct hp_hw *hw;
{
	register struct hp_ctlr *hc;
	struct hp_ctlr *match_c;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_controller: hw: id%x at sc%d (%x), type %x...",
		       hw->hw_id, hw->hw_sc, hw->hw_kva, hw->hw_type);
#endif
	sc = hw->hw_sc;
	match_c = NULL;
	for (hc = hp_cinit; hc->hp_driver; hc++) {
		if (hc->hp_alive)
			continue;
		/*
		 * Make sure we are looking at the right
		 * controller type.
		 */
		if (!same_hw_ctlr(hw, hc))
			continue;
		/*
		 * Exact match; all done
		 */
		if ((int)hc->hp_addr == sc) {
			match_c = hc;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if (hc->hp_addr == NULL && match_c == NULL)
			match_c = hc;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_c)
			printf("found %s%d\n",
			       match_c->hp_driver->d_name,
			       match_c->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece of hardware,
	 * just ignore it.
	 */
	if (match_c == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize and configure all attached
	 * slaves.  Note, we can still fail if HW won't initialize.
	 */
	hc = match_c;
	oaddr = hc->hp_addr;
	hc->hp_addr = hw->hw_kva;
	if ((*hc->hp_driver->d_init)(hc)) {
		hc->hp_alive = 1;
		printf("%s%d", hc->hp_driver->d_name, hc->hp_unit);
		sc = patosc(hw->hw_pa);
		if (sc < 256)
			printf(" at sc%d,", sc);
		else
			printf(" csr 0x%x,", sc);
		printf(" ipl %d", hc->hp_ipl);
		if (hc->hp_flags)
			printf(" flags 0x%x", hc->hp_flags);
		printf("\n");
		find_slaves(hc);
	} else
		hc->hp_addr = oaddr;
	return(1);
}

find_device(hw)
	register struct hp_hw *hw;
{
	register struct hp_device *hd;
	struct hp_device *match_d;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_device: hw: id%x at sc%d (%x), type %x...",
		       hw->hw_id, hw->hw_sc, hw->hw_kva, hw->hw_type);
#endif
	match_d = NULL;
	for (hd = hp_dinit; hd->hp_driver; hd++) {
		if (hd->hp_alive)
			continue;
		/* Must not be a slave */
		if (hd->hp_cdriver)
			continue;
		/*
		 * XXX: A graphics device that was found as part of the
		 * console init will have the hp_addr field already set
		 * (i.e. no longer the select code).  Gotta perform a
		 * slightly different check for an exact match.
		 */
		if (HW_ISDEV(hw, D_BITMAP) && hd->hp_addr >= intiobase) {
			/* must be an exact match */
			if (hd->hp_addr == hw->hw_kva) {
				match_d = hd;
				break;
			}
			continue;
		}
		sc = (int) hd->hp_addr;
		/*
		 * Exact match; all done.
		 */
		if (sc > 0 && sc == hw->hw_sc) {
			match_d = hd;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if (sc == 0 && same_hw_device(hw, hd) && match_d == NULL)
			match_d = hd;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_d)
			printf("found %s%d\n",
			       match_d->hp_driver->d_name,
			       match_d->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece
	 * of hardware, just ignore it.
	 */
	if (match_d == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize.
	 * Note, we can still fail if HW won't initialize.
	 */
	hd = match_d;
	oaddr = hd->hp_addr;
	hd->hp_addr = hw->hw_kva;
	if ((*hd->hp_driver->d_init)(hd)) {
		hd->hp_alive = 1;
		printf("%s%d", hd->hp_driver->d_name, hd->hp_unit);
		sc = patosc(hw->hw_pa);
		if (sc < 256)
			printf(" at sc%d", sc);
		else
			printf(" csr 0x%x", sc);
		if (hd->hp_ipl)
			printf(", ipl %d", hd->hp_ipl);
		if (hd->hp_flags)
			printf(", flags 0x%x", hd->hp_flags);
		printf("\n");
	} else
		hd->hp_addr = oaddr;
	return(1);
}

find_slaves(hc)
	struct hp_ctlr *hc;
{
	/*
	 * The SCSI bus is structured very much like the HP-IB 
	 * except that the host adaptor is slave 7 so we only want
	 * to look at the first 6 slaves.
	 */
	if (dr_type(hc->hp_driver, "hpib"))
		find_busslaves(hc, 0, MAXSLAVES-1);
	else if (dr_type(hc->hp_driver, "scsi"))
#ifdef SCSI_REVPRI
		/*
		 * Later releases of the HP boot ROM start searching for
		 * boot devices starting with slave 6 and working down.
		 * This is apparently the order in which priority is given
		 * to slaves on the host adaptor.
		 */
		find_busslaves(hc, MAXSLAVES-2, 0);
#else
		find_busslaves(hc, 0, MAXSLAVES-2);
#endif
}

/*
 * Search each BUS controller found for slaves attached to it.
 * The bad news is that we don't know how to uniquely identify all slaves
 * (e.g. PPI devices on HP-IB).  The good news is that we can at least
 * differentiate those from slaves we can identify.  At worst (a totally
 * wildcarded entry) this will cause us to locate such a slave at the first
 * unused position instead of where it really is.  To save grief, non-
 * identifing devices should always be fully qualified.
 */
find_busslaves(hc, startslave, endslave)
	register struct hp_ctlr *hc;
	int startslave, endslave;
{
	register int s;
	register struct hp_device *hd;
	struct hp_device *match_s;
	int new_s, new_c, old_s, old_c;
	int rescan;
	
#define NEXTSLAVE(s) (startslave < endslave ? (s)++ : (s)--)
#define LASTSLAVE(s) (startslave < endslave ? (s)-- : (s)++)
#ifdef DEBUG
	if (acdebug)
		printf("find_busslaves: for %s%d\n",
		       hc->hp_driver->d_name, hc->hp_unit);
#endif
	NEXTSLAVE(endslave);
	for (s = startslave; s != endslave; NEXTSLAVE(s)) {
		rescan = 1;
		match_s = NULL;
		for (hd = hp_dinit; hd->hp_driver; hd++) {
			/*
			 * Rule out the easy ones:
			 * 1. slave already assigned or not a slave
			 * 2. not of the proper type
			 * 3. controller specified but not this one
			 * 4. slave specified but not this one
			 */
			if (hd->hp_alive || hd->hp_cdriver == NULL)
				continue;
			if (!dr_type(hc->hp_driver, hd->hp_cdriver->d_name))
				continue;
			if (hd->hp_ctlr >= 0 && hd->hp_ctlr != hc->hp_unit)
				continue;
			if (hd->hp_slave >= 0 && hd->hp_slave != s)
				continue;
			/*
			 * Case 0: first possible match.
			 * Remember it and keep looking for better.
			 */
			if (match_s == NULL) {
				match_s = hd;
				new_c = hc->hp_unit;
				new_s = s;
				continue;
			}
			/*
			 * Case 1: exact match.
			 * All done.  Note that we do not attempt any other
			 * matches if this one fails.  This allows us to
			 * "reserve" locations for dynamic addition of
			 * disk/tape drives by fully qualifing the location.
			 */
			if (hd->hp_slave == s && hd->hp_ctlr == hc->hp_unit) {
				match_s = hd;
				rescan = 0;
				break;
			}
			/*
			 * Case 2: right controller, wildcarded slave.
			 * Remember first and keep looking for an exact match.
			 */
			if (hd->hp_ctlr == hc->hp_unit &&
			    match_s->hp_ctlr < 0) {
				match_s = hd;
				new_s = s;
				continue;
			}
			/*
			 * Case 3: right slave, wildcarded controller.
			 * Remember and keep looking for a better match.
			 */
			if (hd->hp_slave == s &&
			    match_s->hp_ctlr < 0 && match_s->hp_slave < 0) {
				match_s = hd;
				new_c = hc->hp_unit;
				continue;
			}
			/*
			 * OW: we had a totally wildcarded spec.
			 * If we got this far, we have found a possible
			 * match already (match_s != NULL) so there is no
			 * reason to remember this one.
			 */
			continue;
		}
		/*
		 * Found a match.  We need to set hp_ctlr/hp_slave properly
		 * for the init routines but we also need to remember all
		 * the old values in case this doesn't pan out.
		 */
		if (match_s) {
			hd = match_s;
			old_c = hd->hp_ctlr;
			old_s = hd->hp_slave;
			if (hd->hp_ctlr < 0)
				hd->hp_ctlr = new_c;
			if (hd->hp_slave < 0)
				hd->hp_slave = new_s;
#ifdef DEBUG
			if (acdebug)
				printf("looking for %s%d at slave %d...",
				       hd->hp_driver->d_name,
				       hd->hp_unit, hd->hp_slave);
#endif

			if ((*hd->hp_driver->d_init)(hd)) {
#ifdef DEBUG
				if (acdebug)
					printf("found\n");
#endif
				printf("%s%d at %s%d, slave %d",
				       hd->hp_driver->d_name, hd->hp_unit,
				       hc->hp_driver->d_name, hd->hp_ctlr,
				       hd->hp_slave);
				if (hd->hp_flags)
					printf(" flags 0x%x", hd->hp_flags);
				printf("\n");
				hd->hp_alive = 1;
				if (hd->hp_dk && dkn < DK_NDRIVE)
					hd->hp_dk = dkn++;
				else
					hd->hp_dk = -1;
				rescan = 1;
			} else {
#ifdef DEBUG
				if (acdebug)
					printf("not found\n");
#endif
				hd->hp_ctlr = old_c;
				hd->hp_slave = old_s;
			}
			/*
			 * XXX: This should be handled better.
			 * Re-scan a slave.  There are two reasons to do this.
			 * 1. It is possible to have both a tape and disk
			 *    (e.g. 7946) or two disks (e.g. 9122) at the
			 *    same slave address.  Here we need to rescan
			 *    looking only at entries with a different
			 *    physical unit number (hp_flags).
			 * 2. It is possible that an init failed because the
			 *    slave was there but of the wrong type.  In this
			 *    case it may still be possible to match the slave
			 *    to another ioconf entry of a different type.
			 *    Here we need to rescan looking only at entries
			 *    of different types.
			 * In both cases we avoid looking at undesirable
			 * ioconf entries of the same type by setting their
			 * alive fields to -1.
			 */
			if (rescan) {
				for (hd = hp_dinit; hd->hp_driver; hd++) {
					if (hd->hp_alive)
						continue;
					if (match_s->hp_alive == 1) {	/* 1 */
						if (hd->hp_flags == match_s->hp_flags)
							hd->hp_alive = -1;
					} else {			/* 2 */
						if (hd->hp_driver == match_s->hp_driver)
							hd->hp_alive = -1;
					}
				}
				LASTSLAVE(s);
				continue;
			}
		}
		/*
		 * Reset bogon alive fields prior to attempting next slave
		 */
		for (hd = hp_dinit; hd->hp_driver; hd++)
			if (hd->hp_alive == -1)
				hd->hp_alive = 0;
	}
#undef NEXTSLAVE
#undef LASTSLAVE
}

caddr_t
sctopa(sc)
	register int sc;
{
	register caddr_t addr;

	if (sc == 7 && internalhpib)
		addr = internalhpib;
	else if (sc < 32)
		addr = (caddr_t) (DIOBASE + sc * DIOCSIZE);
	else if (sc >= 132)
		addr = (caddr_t) (DIOIIBASE + (sc - 132) * DIOIICSIZE);
	else
		addr = 0;
	return(addr);
}

patosc(addr)
	register caddr_t addr;
{
	if (addr == (caddr_t)0x478000)
		return(7);
	if (addr >= (caddr_t)DIOBASE && addr < (caddr_t)DIOTOP)
		return(((unsigned)addr - DIOBASE) / DIOCSIZE);
	if (addr >= (caddr_t)DIOIIBASE && addr < (caddr_t)DIOIITOP)
		return(((unsigned)addr - DIOIIBASE) / DIOIICSIZE + 132);
	return((int)addr);
}

caddr_t
sctova(sc)
	register int sc;
{
	register struct hp_hw *hw;

	for (hw = sc_table; hw->hw_type; hw++)
		if (sc == hw->hw_sc)
			return(hw->hw_kva);
	return((caddr_t)sc);
}

vatosc(addr)
	register caddr_t addr;
{
	register struct hp_hw *hw;

	for (hw = sc_table; hw->hw_type; hw++)
		if (addr == hw->hw_kva)
			return(hw->hw_sc);
	return((int)addr);
}

same_hw_device(hw, hd)
	struct hp_hw *hw;
	struct hp_device *hd;
{
	int found = 0;

	switch (hw->hw_type & ~B_MASK) {
	case C_HPIB:
		found = dr_type(hd->hp_driver, "hpib");
		break;
	case C_SCSI:
		found = dr_type(hd->hp_driver, "scsi");
		break;
	case D_BITMAP:
		found = dr_type(hd->hp_driver, "grf");
		break;
	case D_LAN:
		found = dr_type(hd->hp_driver, "le");
		break;
	case D_COMMDCA:
		found = dr_type(hd->hp_driver, "dca");
		break;
	case D_COMMDCL:
		found = dr_type(hd->hp_driver, "dcl");
		break;
	case D_COMMDCM:
		found = dr_type(hd->hp_driver, "dcm");
		break;
	default:
		break;
	}
	return(found);
}

char notmappedmsg[] = "WARNING: no space to map IO card, ignored\n";

/*
 * Scan the IO space looking for devices.
 */
find_devs()
{
	short sc;
	u_char *id_reg;
	register caddr_t addr;
	register struct hp_hw *hw;
	int didmap, sctop;

	/*
	 * Initialize IO resource map for iomap().
	 */
	rminit(extiomap, (long)EIOMAPSIZE, (long)1, "extio", EIOMAPSIZE/16);
	hw = sc_table;
	/*
	 * Probe all select codes + internal display addr
	 */
	sctop = machineid == HP_320 ? 32 : 256;
	for (sc = -1; sc < sctop; sc++) {
		/*
		 * Invalid select codes
		 */
		if (sc >= 32 && sc < 132)
			continue;

		if (sc == -1) {
			hw->hw_pa = (caddr_t) GRFIADDR;
			addr = (caddr_t) IIOV(hw->hw_pa);
			didmap = 0;
		} else if (sc == 7 && internalhpib) {
			hw->hw_pa = (caddr_t) 0x478000;
			addr = internalhpib = (caddr_t) IIOV(hw->hw_pa);
			didmap = 0;
		} else {
			hw->hw_pa = sctopa(sc);
			addr = iomap(hw->hw_pa, NBPG);
			if (addr == 0) {
				printf(notmappedmsg);
				continue;
			}
			didmap = 1;
		}
		if (badaddr(addr)) {
			if (didmap)
				iounmap(addr, NBPG);
			continue;
		}
		id_reg = (u_char *) addr;
		if (sc >= 132)
			hw->hw_size = (id_reg[0x101] + 1) * 0x100000;
		else
			hw->hw_size = DIOCSIZE;
		hw->hw_kva = addr;
		hw->hw_id = id_reg[1];
		hw->hw_sc = sc;
		/*
		 * Internal HP-IB on some machines (345/375) doesn't return
		 * consistant id info so we use the info gleaned from the
		 * boot ROMs SYSFLAG.
		 */
		if (sc == 7 && internalhpib) {
			hw->hw_type = C_HPIB;
			hw++;
			continue;
		}
		/*
		 * XXX: the following could be in a big static table
		 */
		switch (hw->hw_id) {
		/* Null device? */
		case 0:
			break;
		/* 98644A */
		case 2:
		case 2+128:
			hw->hw_type = D_COMMDCA;
			break;
		/* 98622A */
		case 3:
			hw->hw_type = D_MISC;
			break;
		/* 98623A */
		case 4:
			hw->hw_type = D_MISC;
			break;
		/* 98642A */
		case 5:
		case 5+128:
			hw->hw_type = D_COMMDCM;
			break;
		/* 345/375 builtin parallel port */
		case 6:
			hw->hw_type = D_PPORT;
			break;
		/* 98625A */
		case 7:
		case 7+32:
		case 7+64:
		case 7+96:
			hw->hw_type = C_SCSI;
			break;
		/* 98625B */
		case 8:
			hw->hw_type = C_HPIB;
			break;
		/* 98287A */
		case 9:
			hw->hw_type = D_KEYBOARD;
			break;
		/* 98635A */
		case 10:
			hw->hw_type = D_FPA;
			break;
		/* timer */
		case 11:
			hw->hw_type = D_MISC;
			break;
		/* 98640A */
		case 18:
			hw->hw_type = D_MISC;
			break;
		/* 98643A */
		case 21:
			hw->hw_type = D_LAN;
			break;
		/* 98659A */
		case 22:
			hw->hw_type = D_MISC;
			break;
		/* 237 display */
		case 25:
			hw->hw_type = D_BITMAP;
			break;
		/* quad-wide card */
		case 26:
			hw->hw_type = D_MISC;
			hw->hw_size *= 4;
			sc += 3;
			break;
		/* 98253A */
		case 27:
			hw->hw_type = D_MISC;
			break;
		/* 98627A */
		case 28:
			hw->hw_type = D_BITMAP;
			break;
		/* 98633A */
		case 29:
			hw->hw_type = D_BITMAP;
			break;
		/* 98259A */
		case 30:
			hw->hw_type = D_MISC;
			break;
		/* 8741 */
		case 31:
			hw->hw_type = D_MISC;
			break;
		/* 98577A */
		case 49:
			hw->hw_type = C_VME;
			if (sc < 132) {
				hw->hw_size *= 2;
				sc++;
			}
			break;
		/* 98628A */
		case 52:
		case 52+128:
			hw->hw_type = D_COMMDCL;
			break;
		/* bitmap display */
		case 57:
			hw->hw_type = D_BITMAP;
			hw->hw_secid = id_reg[0x15];
			switch (hw->hw_secid) {
			/* 98700/98710 */
			case 1:
				break;
			/* 98544-547 topcat */
			case 2:
				break;
			/* 98720/721 renassiance */
			case 4:
				if (sc < 132) {
					hw->hw_size *= 2;
					sc++;
				}
				break;
			/* 98548-98556 catseye */
			case 5:
			case 6:
			case 7:
			case 9:
				break;
			/* 98730/731 davinci */
			case 8:
				if (sc < 132) {
					hw->hw_size *= 2;
					sc++;
				}
				break;
			/* A1096A hyperion */
			case 14:
				break;
			/* 987xx */
			default:
				break;
			}
			break;
		/* 98644A */
		case 66:
		case 66+128:
			hw->hw_type = D_COMMDCA;
			break;
		/* 98624A */
		case 128:
			hw->hw_type = C_HPIB;
			break;
		default:
			hw->hw_type = D_MISC;
			break;
		}
		/*
		 * Re-map to proper size
		 */
		if (didmap) {
			iounmap(addr, NBPG);
			addr = iomap(hw->hw_pa, hw->hw_size);
			if (addr == 0) {
				printf(notmappedmsg);
				continue;
			}
			hw->hw_kva = addr;
		}
		/*
		 * Encode bus type
		 */
		if (sc >= 132)
			hw->hw_type |= B_DIOII;
		else
			hw->hw_type |= B_DIO;
		hw++;
	}
}

/*
 * Allocate/deallocate a cache-inhibited range of kernel virtual address
 * space mapping the indicated physical address range [pa - pa+size)
 */
caddr_t
iomap(pa, size)
	caddr_t pa;
	int size;
{
	int ix, npf;
	caddr_t kva;

#ifdef DEBUG
	if (((int)pa & PGOFSET) || (size & PGOFSET))
		panic("iomap: unaligned");
#endif
	npf = btoc(size);
	ix = rmalloc(extiomap, npf);
	if (ix == 0)
		return(0);
	kva = extiobase + ctob(ix-1);
	physaccess(kva, pa, size, PG_RW|PG_CI);
	return(kva);
}

iounmap(kva, size)
	caddr_t kva;
	int size;
{
	int ix;

#ifdef DEBUG
	if (((int)kva & PGOFSET) || (size & PGOFSET))
		panic("iounmap: unaligned");
	if (kva < extiobase || kva >= extiobase + ctob(EIOMAPSIZE))
		panic("iounmap: bad address");
#endif
	physunaccess(kva, size);
	ix = btoc(kva - extiobase) + 1;
	rmfree(extiomap, btoc(size), ix);
}

isrinit()
{
	register int i;

	for (i = 0; i < NISR; i++)
		isrqueue[i].isr_forw = isrqueue[i].isr_back = &isrqueue[i];
}

void
isrlink(isr)
	register struct isr *isr;
{
	int i = ISRIPL(isr->isr_ipl);

	if (i < 0 || i >= NISR) {
		printf("bad IPL %d\n", i);
		panic("configure");
	}
	insque(isr, isrqueue[i].isr_back);
}

/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev != NODEV; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}

#define	DOSWAP			/* Change swdevt and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	0,0,		/* 0 = ct */
	0,0,		/* 1 = xx */
	'r','d',	/* 2 = rd */
	0,0,		/* 3 = sw */
	's','d',	/* 4 = rd */
};

#define	PARTITIONMASK	0x7
#define	PARTITIONSHIFT	3

/*
 * Attempt to find the device from which we were booted.
 * If we can do so, and not instructed not to do so,
 * change rootdev to correspond to the load device.
 */
setroot()
{
	register struct hp_ctlr *hc;
	register struct hp_device *hd;
	int  majdev, mindev, unit, part, controller, adaptor;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != (u_long)B_DEVMAGIC)
		return;
	majdev = B_TYPE(bootdev);
	if (majdev >= sizeof(devname) / sizeof(devname[0]))
		return;
	adaptor = B_ADAPTOR(bootdev);
	controller = B_CONTROLLER(bootdev);
	part = B_PARTITION(bootdev);
	unit = B_UNIT(bootdev);
	/*
	 * First, find the controller type which supports this device.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_driver->d_name[0] == devname[majdev][0] &&
		    hd->hp_driver->d_name[1] == devname[majdev][1])
			break;
	if (hd->hp_driver == 0)
		return;
	/*
	 * Next, find the "controller" (bus adaptor) of that type
	 * corresponding to the adaptor number.
	 */
	for (hc = hp_cinit; hc->hp_driver; hc++)
		if (hc->hp_alive && hc->hp_unit == adaptor &&
		    hc->hp_driver == hd->hp_cdriver)
			break;
	if (hc->hp_driver == 0)
		return;
	/*
	 * Finally, find the "device" (controller or slave) in question
	 * attached to that "controller".
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_alive && hd->hp_slave == controller &&
		    hd->hp_cdriver == hc->hp_driver &&
		    hd->hp_ctlr == hc->hp_unit)
			break;
	if (hd->hp_driver == 0)
		return;
	/*
	 * XXX note that we are missing one level, the unit, here.
	 * Most HP drives come with one controller per disk.  There
	 * are some older drives (e.g. 7946) which have two units
	 * on the same controller but those are typically a disk as
	 * unit 0 and a tape as unit 1.  This would have to be
	 * rethought if you ever wanted to boot from other than unit 0.
	 */
	if (unit != 0)
		printf("WARNING: using device at unit 0 of controller\n");

	mindev = hd->hp_unit;
	/*
	 * Form a new rootdev
	 */
	mindev = (mindev << PARTITIONSHIFT) + part;
	orootdev = rootdev;
	rootdev = makedev(majdev, mindev);
	/*
	 * If the original rootdev is the same as the one
	 * just calculated, don't need to adjust the swap configuration.
	 */
	if (rootdev == orootdev)
		return;

	printf("Changing root device to %c%c%d%c\n",
		devname[majdev][0], devname[majdev][1],
		mindev >> PARTITIONSHIFT, part + 'a');

#ifdef DOSWAP
	mindev &= ~PARTITIONMASK;
	for (swp = swdevt; swp->sw_dev != NODEV; swp++) {
		if (majdev == major(swp->sw_dev) &&
		    mindev == (minor(swp->sw_dev) & ~PARTITIONMASK)) {
			temp = swdevt[0].sw_dev;
			swdevt[0].sw_dev = swp->sw_dev;
			swp->sw_dev = temp;
			break;
		}
	}
	if (swp->sw_dev == NODEV)
		return;

	/*
	 * If dumpdev was the same as the old primary swap
	 * device, move it to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
#endif
}
