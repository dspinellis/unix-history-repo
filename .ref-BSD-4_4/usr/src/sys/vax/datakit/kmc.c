/*
 * kmc.c from 5.0 (on ihwld) hacked for 4.2
 * Bob Van Valzah  2/7/84
 */

/* @(#)kmc.c	1.3 */
/*
 * KMC11 microprocessor driver
 */

#include "kmc.h"
#if NKMC > 0

#include "sys/param.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/kmcreg.h"
#include "sys/buf.h"
#include "sys/user.h"
#include "sys/syslog.h"
#include "../uba/ubavar.h"
#include "sys/uio.h"

#ifdef	DATAKIT
#include "dkitkmc.h"
#endif
#ifdef	RJE
#include "vpm.h"
#endif

#define ushort u_short

int	kmc_cnt = NKMC;

struct kmc {
	struct clist k_inq;
	short	k_stat;
	char	k_type;
	short	k_arg[3];
	int	(*k_rint)();
	int	(*k_init)();
	int	(*k_reset)();
} kmc[NKMC];

#define	KMC11A	1
#define	KMC11B	2
#define	KASIZE	1024
#define	KBSIZE	4096

#define	RUN	(1<<7)
#define	MCLR	(1<<6)
#define	CWRT	(1<<5)
#define	LUB	(1<<4)
#define	LUA	(1<<3)
#define	ROMO	(1<<2)
#define	ROMI	(1<<1)
#define	STEP	(1<<0)

#define RDYO	0200
#define RDYI	020
#define RQI	0200
#define IEI	01
#define IEO	020

#define	STYPE	017
#define SRUN	020
#define SRINT	040
#define	SOPEN	0100
#define	SLOAD	0200
#define	SINIT	0400
#define	SRESET	01000


struct kmcdevice {
union {
	char	b[8];
	unsigned short	w[4];
} un;
};

#define	bsel0	un.b[0]
#define	bsel1	un.b[1]
#define	bsel2	un.b[2]
#define	bsel3	un.b[3]
#define	bsel4	un.b[4]
#define	bsel5	un.b[5]
#define	bsel6	un.b[6]
#define	bsel7	un.b[7]
#define	sel0	un.w[0]
#define	sel2	un.w[1]
#define	sel4	un.w[2]
#define	sel6	un.w[3]

int rkmcdebug = 0;

int	kmcprobe(), kmcattach(), kmcxint();
struct	uba_device *kmcdinfo[NKMC];

u_short kmcstd[] = { 0 };
struct uba_driver kmcdriver =
	{ kmcprobe, 0, kmcattach, 0, kmcstd, "kmc", kmcdinfo };

kmcprobe(reg)
caddr_t reg;
{	register int br, cvec;	/* don't touch */
	register struct kmcdevice *kp = (struct kmcdevice *)reg;
	register s;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	s = spl7();
	kp->bsel1 = MCLR;
	splx(s);
	kp->bsel1 = ROMI;
	kp->sel4 = 0200;		/* bus request */
	kp->sel6 = 0121111;		/* mov csr4,obr */
	kp->bsel1 = ROMI|STEP;
	DELAY(50);
	kp->bsel1 = 0;
	return(1);
}

kmcattach(ui)
register struct uba_device *ui;
{
	switch(ui->ui_flags & 03) {
#if NVPM>0
		case 0:
			vpminit(ui);
			break;
#endif
#if NDKITKMC>0
		case 1:
			dkkmc_attach(ui);
			break;
#endif
		default:
			log(LOG_ERR, "kmc%d: no protocol %d\n", ui->ui_unit,
			    ui->ui_flags);
			break;
	}
}

/*ARGSUSED*/
kmcopen(dev, flag)
{
	register struct kmcdevice *kp;
	register struct kmc *tp;
	register sav;

	dev = minor(dev);
	if (dev>=kmc_cnt || (tp = &kmc[dev])->k_stat&SOPEN) {
		return (ENXIO);
	}
	tp->k_stat |= SOPEN;
	if (tp->k_type==0) {
		kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
		kp->bsel1 = ROMO;
		kp->sel4 = 0;
		sav = kp->sel6;
		kp->sel6 = ~sav;
		if (kp->sel6 != sav) {
			tp->k_type = KMC11B;
			kp->sel6 = sav;
		} else
			tp->k_type = KMC11A;
		kp->bsel1 = 0;
	}
	return (0);
}

kmcclose(dev)
{
	dev = minor(dev);
	kmc[dev].k_stat &= ~SOPEN;
}

kmcread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct kmcdevice *kp;
	register ad;
	register int error = 0;
	int	dsize;
	ushort	sav;

	dev = minor(dev);
	if (kmc[dev].k_stat&SRUN)
		return (0);
	dsize = (kmc[dev].k_type==KMC11A)?KASIZE:KBSIZE;
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	kp->bsel1 = 0;
	do {
		ad = uio->uio_offset;
		if (ad<dsize*2) {
			if (ad&1) {
				return (ENXIO);
			}
			ad >>= 1;
			kp->bsel1 = ROMO;
			kp->sel4 = ad;
			if ((error=ureadc(kp->bsel6, uio)) < 0)
				break;
			if ((error=ureadc(kp->bsel7, uio)) < 0)
				break;
			kp->bsel1 = 0;
		} else if (ad -= dsize*2, ad<dsize) {
			kp->bsel1 = ROMO;
			kp->sel4 = 0;
			sav = kp->sel6;
			kp->bsel1 = ROMI;
			kp->sel6 = 010000|(ad&0377);	/* mov ad,mar */
			kp->bsel1 = ROMI|STEP;
			kp->bsel1 = ROMI;
			kp->sel6 = 04000|((ad>>8)&0377);	/* mov %ad,%mar */
			kp->bsel1 = ROMI|STEP;
			kp->bsel1 = ROMI;
			kp->sel6 = 055222;	/* mov mem,csr2|mar++ */
			kp->bsel1 = ROMI|STEP;
			if ((error=ureadc(kp->bsel2, uio)) < 0)
				break;
			kp->bsel1 = ROMI;
			kp->sel6 = sav;
			kp->bsel1 = 0;
		} else
			break;
	} while (!error && uio->uio_resid);
	return (error);
}

kmcwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct kmcdevice *kp;
	register ad;
	int	dsize;
	short	ins;
	ushort	sav;

	dev = minor(dev);
	if (kmc[dev].k_stat&SRUN)
		return (0);
	dsize = (kmc[dev].k_type==KMC11A)?KASIZE:KBSIZE;
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	kp->bsel1 = 0;
	while (uio->uio_resid) {
		ad = uio->uio_offset;
		if (ad<dsize*2) {
			if (ad&1) {
				return (ENXIO);
			}
			kp->bsel1 = ROMO;
			kp->sel4 = ad>>1;
			lobyte(ins) = uwritec(uio);
			hibyte(ins) = uwritec(uio);
			kp->sel6 = ins;
			kp->bsel1 |= CWRT;
			kp->bsel1 = 0;
		} else if (ad -= dsize*2, ad<dsize) {
			kp->bsel1 = ROMO;
			kp->sel4 = 0;
			sav = kp->sel6;
			kp->bsel1 = ROMI;
			kp->sel6 = 010000|(ad&0377);	/* mov ad,mar */
			kp->bsel1 = ROMI|STEP;
			kp->bsel1 = ROMI;
			kp->sel6 = 04000|((ad>>8)&0377);	/* mov %ad,%mar */
			kp->bsel1 = ROMI|STEP;
			kp->bsel1 = ROMI;
			kp->bsel2 = uwritec(uio);
			kp->sel6 = 0136440;	/* mov csr2,mem|mar++ */
			kp->bsel1 = ROMI|STEP;
			kp->bsel1 = ROMI;
			kp->sel6 = sav;
			kp->bsel1 = 0;
		} else
			break;
	}
	return (0);
}

/*ARGSUSED*/
kmcioctl(dev, cmd, kk, mode)
	dev_t dev;
	struct kmcntl *kk;
{
	register struct kmcdevice *kp;
	register struct kmc *tp;
	short	csr[4];
	ushort	sav;

	if (rkmcdebug) log(LOG_ERR, "kmcioctl: cmd=%d, kk->kmd=%d, kk->kcsr=0x%x, kk->kval=%d\n",
		cmd, kk->kmd, kk->kcsr, kk->kval);
	dev = minor(dev);
	if (cmd != KCSETA) {
		return (EINVAL);
	}
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	tp = &kmc[dev];
	switch (kk->kmd) {
	case KMCLR:
	case KRESET:
		spl7();
		kp->bsel1 = MCLR;
		spl0();
	case KSTOP:
		tp->k_stat &= ~SRUN;
		kp->bsel1 = 0;
		if (kk->kmd == KRESET) {
			tp->k_stat = 0;
			while(getc(&tp->k_inq) >= 0) ;
			if (tp->k_stat&SINIT)
				(*tp->k_init)(dev);
		}
		return (0);
	case KMS:
		if (tp->k_stat&SRUN)
			break;
		kp->bsel1 = ROMI|ROMO;
		sav = kp->sel6;
		kp->bsel1 = ROMI;
		kp->sel6 = kk->kval;
		kp->bsel1 = ROMI|STEP;
		kp->bsel1 = ROMI;
		kp->sel6 = sav;
		kp->bsel1 = 0;
		goto lcsr;
	case KSTEP:
		if (tp->k_stat&SRUN)
			break;
		kp->bsel1 |= STEP;
		kp->bsel1 = 0;
	case KCSR:
	lcsr:
		csr[0] = kp->sel0;
		csr[1] = kp->sel2;
		csr[2] = kp->sel4;
		csr[3] = kp->sel6;
		if (copyout((caddr_t)csr, (caddr_t)kk->kcsr, sizeof csr))
			return (EFAULT);
		return (0);
	case KWRCR:
		if (tp->k_stat&SRINT)
			break;
		kp->sel6 = kk->kval;
		return (0);
	case KRUN:
		if (tp->k_stat&SRUN)
			break;
		tp->k_stat &= ~STYPE;
		tp->k_stat |= (kk->kval&STYPE)|SRUN;
		kp->bsel1 |= RUN;
		if (tp->k_stat&SRINT) {
			spl5();
			kmcrint(dev);
			spl0();
		}
		if (tp->k_stat&SRESET)
			(*tp->k_reset)(dev);
		return (0);
	case KLU:
		kp->bsel1 = kk->kval&(LUA|LUB);
		return (0);
	}
	if (rkmcdebug) log(LOG_ERR, "kmcioctl: EIO exit, tp->k_stat=0x%x\n", tp->k_stat);
	return (EIO);
}

kmcrint(dev)
{
	register struct kmcdevice *kp;
	register struct kmc *tp;

	dev = minor(dev);
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	tp = &kmc[dev];
	kp->sel0 &= ~IEI;
	while (kp->sel2&RDYI) {
		if ((tp->k_stat&SLOAD) ||
		  q_to_b(&tp->k_inq, (char *)tp->k_arg, sizeof(tp->k_arg)) == sizeof(tp->k_arg)) {
			kp->sel2 = tp->k_arg[0]|RDYI;
			kp->sel4 = tp->k_arg[1];
			kp->sel6 = tp->k_arg[2];
			tp->k_stat &= ~SLOAD;
		} else {
			log(LOG_ERR, "Bad kmc %d load\n", dev);
		}
		if (tp->k_inq.c_cc==0) {
			kp->sel0 &= ~RQI;
			kp->sel2 &= ~RDYI;
			return;
		}
		kp->sel2 &= ~RDYI;
	}
	if ((tp->k_stat&SLOAD) || tp->k_inq.c_cc)
		kp->sel0 |= IEI|RQI;
}

kmcxint(dev)
{
	register struct kmcdevice *kp;
	register struct kmc *tp;
	int p1, p2, p3, p4;

	dev = minor(dev);
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	tp = &kmc[dev];
	kp->sel0 &= ~IEO;
	while(kp->sel2&RDYO) {
		p1 = (dev<<6)|(kp->bsel3&077);
		p2 = kp->bsel2&017;
		p3 = kp->sel4;
		p4 = kp->sel6;
		kp->sel2 &= ~RDYO;
		if (tp->k_stat&SRINT)
			(*tp->k_rint)(p1, p2, p3, p4);
	}
	kp->sel0 |= IEO;
}

kmcload(dev, p1, p2, p3)
{
	register struct kmcdevice *kp;
	register struct kmc *tp;
	register unit;
	register sps;

	dev = minor(dev);
	unit = (dev>>6)&03;
	tp = &kmc[unit];
	if (!(tp->k_stat&SRUN))
		return(-1);
	kp = ((struct kmcdevice *)kmcdinfo[unit]->ui_addr);	/* RAV unit is suspect */
	sps = spl5();
	if (tp->k_stat&SLOAD) {
		b_to_q((char *)tp->k_arg, sizeof(tp->k_arg), &tp->k_inq);
		tp->k_stat &= ~SLOAD;
	}
	kp->sel0 |= RQI;
	tp->k_arg[0] = (p1&017)|((dev&077)<<8);
	tp->k_arg[1] = p2;
	tp->k_arg[2] = p3;
	if (tp->k_inq.c_cc)
		b_to_q((char *)tp->k_arg, sizeof(tp->k_arg), &tp->k_inq);
	else
		tp->k_stat |= SLOAD;
	kmcrint(unit);
	splx(sps);
	return(tp->k_inq.c_cc);
}

kmcset(dev, type, rint)
int (*rint)();
{
	register struct kmcdevice *kp;
	register struct kmc *tp;
	register unit;

	dev = minor(dev);
	unit = (dev>>6)&03;
	kp = ((struct kmcdevice *)kmcdinfo[unit]->ui_addr);	/* RAV unit is suspect */
	tp = &kmc[unit];
	if ((tp->k_stat&(STYPE|SRUN|SOPEN))!=((type&STYPE)|SRUN))
		return (1);
	tp->k_stat |= SRINT;
	tp->k_rint = rint;
	kp->sel0 |= IEO;
	return(0);
}

kmcdclr(dev)
register dev;
{
	register struct kmc *tp;
	register struct kmcdevice *kp;

	dev = minor(dev);
	if (dev < 0 || dev >= kmc_cnt)
		return;
	tp = &kmc[dev];
	while (getc(&tp->k_inq) >= 0) ;
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	kp->sel0 = 0;
	kp->sel2 = 0;
}

kmcreset(dev)
{
	register struct kmc *tp;
	register struct kmcdevice *kp;
	register s;

	dev = minor(dev);
	tp = &kmc[dev];
	kp = ((struct kmcdevice *)kmcdinfo[dev]->ui_addr);
	s = spl7();
	kp->bsel1 = MCLR;
	splx(s);
	kp->bsel1 = 0;
	tp->k_stat = 0;
	while(getc(&tp->k_inq)>=0);
}

kmcifset(dev, init)
int (*init)();
{
	register struct kmc *tp;
	register unit;

	dev = minor(dev);
	unit = (dev>>6)&03;
	if (unit < 0 || unit >= kmc_cnt)
		return;
	tp = &kmc[unit];
	if (init==NULL) {
		tp->k_init = NULL;
		tp->k_stat &= ~SINIT;
	} else {
		tp->k_init = init;
		tp->k_stat |= SINIT;
	}
}

kmcrfset(dev, reset)
int (*reset)();
{
	register struct kmc *tp;
	register unit;

	dev = minor(dev);
	unit = (dev>>6)&03;
	if (unit < 0 || unit >= kmc_cnt)
		return;
	tp = &kmc[unit];
	if (reset==NULL) {
		tp->k_reset = NULL;
		tp->k_stat &= ~SRESET;
	} else {
		tp->k_reset = reset;
		tp->k_stat |= SRESET;
	}
}
#endif
