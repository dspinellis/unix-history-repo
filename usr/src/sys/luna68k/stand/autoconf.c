/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *	@(#)autoconf.c	8.1 (Berkeley) 6/10/93
 */

/* 
 * autoconf.c -- Determine mass storage and memory configuration for a machine.
 *          by A.Fujita, NOV-30-1991
 *
 * Modified by A.Fujita, FEB-04-1992
 */


#include <sys/param.h>
#include <sys/dkstat.h>
#include <luna68k/stand/device.h>

int	dkn;		    /* number of iostat dk numbers assigned so far */
struct	hp_hw sc_table[MAX_CTLR];

#ifdef DEBUG
int	acdebug = 1;
#endif

/*
 * Determine mass storage and memory configuration for a machine.
 */
configure()
{
	register struct hp_hw *hw;
	int found;

	/*
	 * Look over each hardware device actually found and attempt
	 * to match it with an ioconf.c table entry.
	 */
	for (hw = sc_table; hw->hw_type; hw++) {
		if (hw->hw_type & CONTROLLER)
			found = find_controller(hw);
		else
			found = find_device(hw);
#ifdef DEBUG
		if (!found) {
			printf("unconfigured %s ", hw->hw_name);
			printf("at 0x%x\n", hw->hw_addr);
		}
#endif
	}

}

#define dr_type(d, s)	\
	(strcmp((d)->d_name, (s)) == 0)

#define same_hw_ctlr(hw, hc) \
	((hw)->hw_type == SCSI && dr_type((hc)->hp_driver, "sc"))

find_controller(hw)
	register struct hp_hw *hw;
{
	register struct hp_ctlr *hc;
	struct hp_ctlr *match_c;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_controller: hw: %s at sc%d (%x), type %x...",
		       hw->hw_name, hw->hw_sc, hw->hw_addr, hw->hw_type);
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
		if ((int)hc->hp_addr == WILD_CARD_CTLR && match_c == NULL)
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
	hc->hp_addr = hw->hw_addr;
	if ((*hc->hp_driver->d_init)(hc)) {
		hc->hp_alive = 1;
		printf("%s%d", hc->hp_driver->d_name, hc->hp_unit);
		printf(" at 0x%x,", hc->hp_addr);
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
		printf("find_device: hw: %s at sc%d (%x), type %x...",
		       hw->hw_name, hw->hw_sc, hw->hw_addr, hw->hw_type);
#endif
	match_d = NULL;
	for (hd = hp_dinit; hd->hp_driver; hd++) {
		if (hd->hp_alive)
			continue;
		/* Must not be a slave */
		if (hd->hp_cdriver)
			continue;
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
	hd->hp_addr = hw->hw_addr;
	if ((*hd->hp_driver->d_init)(hd)) {
		hd->hp_alive = 1;
		printf("%s%d", hd->hp_driver->d_name, hd->hp_unit);
		printf(" at 0x%x", hd->hp_addr);
		if (hd->hp_ipl)
			printf(", ipl %d", hd->hp_ipl);
		if (hd->hp_flags)
			printf(", flags 0x%x", hd->hp_flags);
		printf("\n");
	} else
		hd->hp_addr = oaddr;
	return(1);
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
find_slaves(hc)
	register struct hp_ctlr *hc;
{
	register int s;
	register struct hp_device *hd;
	struct hp_device *match_s;
	int maxslaves = MAXSLAVES-1;
	int new_s, new_c, old_s, old_c;
	int rescan;

#ifdef DEBUG
	if (acdebug)
		printf("find_slaves: for %s%d\n",
		       hc->hp_driver->d_name, hc->hp_unit);
#endif
	for (s = 0; s < maxslaves; s++) {
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
				s--;
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
}

same_hw_device(hw, hd)
	struct hp_hw *hw;
	struct hp_device *hd;
{
	int found = 0;

	switch (hw->hw_type) {
	case NET:
		found = dr_type(hd->hp_driver, "le");
		break;
	case SCSI:
		found = dr_type(hd->hp_driver, "scsi");
		break;
	case VME:
	case MISC:
		break;
	}
	return(found);
}

#define setup_hw(hw, addr, sc, type, id, name) \
	(hw)->hw_addr = addr; \
	(hw)->hw_sc   = sc; \
	(hw)->hw_type = type; \
	(hw)->hw_id   = id; \
	(hw)->hw_name = name

find_devs()
{
	register struct hp_hw *hw = sc_table;
	
	setup_hw(hw, (char *) 0x51000000, 0x5, SIO,      0x5, "uPD7201A (SIO)");
	hw->hw_id2 = 0; hw++;
	
	setup_hw(hw, (char *) 0x51000004, 0x5, KEYBOARD, 0x5, "uPD7201A (KBD)");
	hw->hw_id2 = 1; hw++;
	
	setup_hw(hw, (char *) 0xe1000000, 0xe, SCSI,     0xe, "MB89352  (SPC)");
	hw++;

	if (!badaddr((caddr_t) 0xf1000000)) {
		setup_hw(hw, (char *) 0xf1000000, 0xf, NET,      0xf, "Am7990 (LANCE)");
		hw++;
	}
}

strcmp(s1, s2)
	register char *s1, *s2;
{
	while (*s1 == *s2)
		if (*s1++=='\0' || *s2++ == '\0')
			return (0);
	return (*s1 - *s2);
}
