/*
 *	@(#)ka820.c	7.1 (UofMD/Berkeley) %G%
 */

#if VAX8200

/*
 * KA820 specific CPU code.  (Note that the VAX8200 uses a KA820, not
 * a KA8200.  Sigh.)
 */

#include "param.h"
#include "time.h"
#include "kernel.h"
#include "vmmac.h"

#include "cpu.h"
#include "clock.h"
#include "ka820.h"
#include "mem.h"
#include "mtpr.h"
#include "pte.h"

#include "../vaxbi/bireg.h"

extern struct pte Clockmap[];
extern struct pte RX50map[];
extern struct pte Ka820map[];
struct ka820clock ka820clock;
struct ka820port ka820port;

#ifdef notyet
extern struct pte BRAMmap[];
extern struct pte EEPROMmap[];
char bootram[KA820_BRPAGES * NBPG];
char eeprom[KA820_EEPAGES * NBPG];
#endif

ka820_init()
{
	register int csr;

	/* map in the various devices */
	*(int *)&Ka820map[0] = PG_V|PG_KW|btop(KA820_PORTADDR);
	*(int *)&RX50map[0] = PG_V|PG_KW|btop(KA820_RX50ADDR);
	*(int *)&Clockmap[0] = PG_V|PG_KW|btop(KA820_CLOCKADDR);
#ifdef notyet
	ioaccess(bootram, BRAMmap, KA820_BRPAGES * NBPG);
	ioaccess(eeprom, EEPROMmap, KA820_EEPAGES * NBPG);
#else
	mtpr(TBIA, 0);
#endif

	/* reset the console and enable the RX50 */
	csr = ka820port.csr;
	csr &= ~KA820PORT_RSTHALT;	/* ??? */
	csr |= KA820PORT_CONSCLR | KA820PORT_CRDCLR | KA820PORT_CONSEN |
		KA820PORT_RXIE;
	ka820port.csr = csr;
}

/* Set system time from clock */
/* ARGSUSED */
ka820_clkread(base)
	time_t base;
{
	register struct ka820clock *clock = &ka820clock;
	struct chiptime c;
	int s, rv;

	rv = CLKREAD_OK;
	/* I wish I knew the differences between these */
	if ((clock->csr3 & KA820CLK_3_VALID) == 0) {
		printf("WARNING: TOY clock not marked valid\n");
		rv = CLKREAD_WARN;
	}
	if ((clock->csr1 & KA820CLK_1_GO) != KA820CLK_1_GO) {
		printf("WARNING: TOY clock stopped\n");
		rv = CLKREAD_WARN;
	}
	/* THIS IS NOT RIGHT (clock may change on us) */
	s = splhigh();
	while (clock->csr0 & KA820CLK_0_BUSY)
		/* void */;
	c.sec = clock->sec;
	c.min = clock->min;
	c.hour = clock->hr;
	c.day = clock->day;
	c.mon = clock->mon;
	c.year = clock->yr;
	splx(s);

	/* the darn thing needs tweaking! */
	c.sec >>= 1;		/* tweak */
	c.min >>= 1;		/* tweak */
	c.hour >>= 1;		/* tweak */
	c.day >>= 1;		/* tweak */
	c.mon >>= 1;		/* tweak */
	c.year >>= 1;		/* tweak */

	time.tv_sec = chiptotime(&c);
	return (time.tv_sec ? rv : CLKREAD_BAD);
}

/* store time into clock */
ka820_clkwrite()
{
	register struct ka820clock *clock = &ka820clock;
	struct chiptime c;
	int s;

	timetochip(&c);

	/* play it again, sam (or mike or kirk or ...) */
	c.sec <<= 1;		/* tweak */
	c.min <<= 1;		/* tweak */
	c.hour <<= 1;		/* tweak */
	c.day <<= 1;		/* tweak */
	c.mon <<= 1;		/* tweak */
	c.year <<= 1;		/* tweak */

	s = splhigh();
	clock->csr1 = KA820CLK_1_SET;
	while (clock->csr0 & KA820CLK_0_BUSY)
		/* void */;
	clock->sec = c.sec;
	clock->min = c.min;
	clock->hr = c.hour;
	clock->day = c.day;
	clock->mon = c.mon;
	clock->yr = c.year;
	/* should we set a `rate'? */
	clock->csr1 = KA820CLK_1_GO;
	splx(s);
}

/*
 * MS820 support.
 */
struct ms820regs {
	struct	biiregs biic;		/* BI interface chip */
	u_long	ms_gpr[4];		/* the four gprs (unused) */
	int	ms_csr1;		/* control/status register 1 */
	int	ms_csr2;		/* control/status register 2 */
};

/*
 * Bits in CSR1.
 */
#define	MS1_ERRSUM	0x80000000	/* error summary (ro) */
#define	MS1_ECCDIAG	0x40000000	/* ecc diagnostic (rw) */
#define	MS1_ECCDISABLE	0x20000000	/* ecc disable (rw) */
#define	MS1_MSIZEMASK	0x1ffc0000	/* mask for memory size (ro) */
#define	MS1_RAMTYMASK	0x00030000	/* mask for ram type (ro) */
#define	MS1_RAMTY64K	0x00000000	/* 64K chips */
#define	MS1_RAMTY256K	0x00010000	/* 256K chips */
					/* types 2 and 3 reserved */
#define	MS1_CRDINH	0x00008000	/* inhibit crd interrupts (rw) */
#define	MS1_MEMVALID	0x00004000	/* memory has been written (ro) */
#define	MS1_INTLK	0x00002000	/* interlock flag (ro) */
#define	MS1_BROKE	0x00001000	/* broken (rw) */
#define	MS1_MBZ		0x00000880	/* zero */
#define	MS1_MWRITEERR	0x00000400	/* rds during masked write (rw) */
#define	MS1_CNTLERR	0x00000200	/* internal timing busted (rw) */
#define	MS1_INTLV	0x00000100	/* internally interleaved (ro) */
#define	MS1_DIAGC	0x0000007f	/* ecc diagnostic bits (rw) */

/*
 * Bits in CSR2.
 */
#define	MS2_RDSERR	0x80000000	/* rds error (rw) */
#define	MS2_HIERR	0x40000000	/* high error rate (rw) */
#define	MS2_CRDERR	0x20000000	/* crd error (rw) */
#define	MS2_ADRSERR	0x10000000	/* rds due to addr par err (rw) */
#define	MS2_MBZ		0x0f000080	/* zero */
#define	MS2_ADDR	0x00fffe00	/* address in error (relative) (ro) */
#define	MS2_INTLVADDR	0x00000100	/* error was in bank 1 (ro) */
#define	MS2_SYN		0x0000007f	/* error syndrome (ro, rw diag) */


ka820_memenable()
{
	register struct ms820regs *mcr;
	register int m;

	for (m = 0; m < nmcr; m++) {
		mcr = (struct ms820regs *)mcraddr[m];
		/*
		 * This will be noisy.  Should we do anything
		 * about that?
		 */
		if ((mcr->biic.bi_csr & BICSR_STS) == 0)
			printf("mcr%d: failed self test\n", m);
		else {
			mcr->ms_csr1 = MS1_MWRITEERR | MS1_CNTLERR;
			mcr->ms_csr2 = MS2_RDSERR | MS2_HIERR |
				MS2_CRDERR | MS2_ADRSERR;
		}
	}
}

ka820_memerr()
{
	register struct ms820regs *mcr;
	register int m, hard;
	register char *type;
static char b1[] = "\20\40ERRSUM\37ECCDIAG\36ECCDISABLE\20CRDINH\17VALID\
\16INTLK\15BROKE\13MWRITEERR\12CNTLERR\11INTLV";
static char b2[] = "\20\40RDS\37HIERR\36CRD\35ADRS";

	for (m = 0; m < nmcr; m++) {
		mcr = (struct ms820regs *)mcraddr[m];
printf("mcr%d: csr1=%b csr2=%b\n", m, mcr->ms_csr1, b1, mcr->ms_csr2, b2);
		if ((mcr->ms_csr1 & MS1_ERRSUM) == 0)
			continue;
		hard = 1;
		if (mcr->ms_csr1 & MS1_BROKE)
			type = "broke";
		else if (mcr->ms_csr1 & MS1_CNTLERR)
			type = "cntl err";
		else if (mcr->ms_csr2 & MS2_ADRSERR)
			type = "address parity err";
		else if (mcr->ms_csr2 & MS2_RDSERR)
			type = "rds err";
		else if (mcr->ms_csr2 & MS2_CRDERR) {
			hard = 0;
			type = "";
		} else
			type = "mysterious error";
		printf("mcr%d: %s%s%s addr %x bank %x syn %x\n", m,
			hard ? "hard error: " : "soft ecc",
			type, mcr->ms_csr2 & MS2_HIERR ?
			" (+ other rds or crd err)" : "",
			((mcr->ms_csr2 & MS2_ADDR) + mcr->biic.bi_sadr) >> 9,
			(mcr->ms_csr2 & MS2_INTLVADDR) != 0,
			mcr->ms_csr2 & MS2_SYN);
		mcr->ms_csr1 = mcr->ms_csr1 | MS1_CRDINH;
		mcr->ms_csr2 = mcr->ms_csr2;
	}
}

/* these are bits 0 to 6 in the summary field */
char *mc8200[] = {
	"cpu bad ipl",		"ucode lost err",
	"ucode par err",	"DAL par err",
	"BI bus err",		"BTB tag par",
	"cache tag par",
};
#define	MC8200_BADIPL	0x01
#define	MC8200_UERR	0x02
#define	MC8200_UPAR	0x04
#define	MC8200_DPAR	0x08
#define	MC8200_BIERR	0x10
#define	MC8200_BTAGPAR	0x20
#define	MC8200_CTAGPAR	0x40

struct mc8200frame {
	int	mc82_bcnt;		/* byte count == 0x20 */
	int	mc82_summary;		/* summary parameter */
	int	mc82_param1;		/* parameter 1 */
	int	mc82_va;		/* va register */
	int	mc82_vap;		/* va prime register */
	int	mc82_ma;		/* memory address */
	int	mc82_status;		/* status word */
	int	mc82_epc;		/* error pc */
	int	mc82_upc;		/* micro pc */
	int	mc82_pc;		/* current pc */
	int	mc82_psl;		/* current psl */
};

ka820_mchk(cmcf)
	caddr_t cmcf;
{
	register struct mc8200frame *mcf = (struct mc8200frame *)cmcf;
	register int i, type = mcf->mc82_summary;
	extern int cold;

	/* ignore BI bus errors during configuration */
	if (cold && type == MC8200_BIERR) {
		mtpr(MCESR, 0xf);
		return (MCHK_RECOVERED);
	}

	/*
	 * SOME ERRORS ARE RECOVERABLE
	 * do it later
	 */
	printf("machine check %x: ", type);
	for (i = 0; i < sizeof (mc8200) / sizeof (mc8200[0]); i++)
		if (type & (1 << i))
			printf(" %s,", mc8200[i]);
	printf(" param1 %x\n", mcf->mc82_param1);
	printf(
"\tva %x va' %x ma %x pc %x psl %x\n\tstatus %x errpc %x upc %x\n",
		mcf->mc82_va, mcf->mc82_vap, mcf->mc82_ma,
		mcf->mc82_pc, mcf->mc82_psl,
		mcf->mc82_status, mcf->mc82_epc, mcf->mc82_upc);
	return (MCHK_PANIC);
}

/*
 * Receive a character from logical console.
 */
rxcdintr()
{
	register int c = mfpr(RXCD);

	/* not sure what (if anything) to do with these */
	printf("rxcd node %x c=0x%x\n", (c >> 8) & 0xf, c & 0xff);
}
#endif
