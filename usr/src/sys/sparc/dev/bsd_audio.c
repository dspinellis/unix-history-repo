/*
 * Copyright (c) 1991, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)bsd_audio.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: bsd_audio.c,v 1.18 93/04/24 16:20:35 leres Exp $ (LBL)
 */
#include "bsdaudio.h"
#if NBSDAUDIO > 0

#include <sys/param.h>
#include <sys/systm.h>

#if BSD < 199103
#ifndef SUNOS
#define SUNOS
#endif
#endif

#include <sys/errno.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/vnode.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#ifndef SUNOS
#include <sys/tty.h>
#endif
#include <sys/uio.h>

#ifdef SUNOS
#include <sundev/mbvar.h>
#include <sun4c/intreg.h>
#else
#include <sys/device.h>
#include <machine/autoconf.h>
#endif
#include <machine/cpu.h>

/*
 * Avoid name clashes with SunOS so we can config either the bsd or sun
 * streams driver in a SunOS kernel.
 */
#ifdef SUNOS
#include <sbusdev/bsd_audioreg.h>
#include <sbusdev/bsd_audiovar.h>
#include <sbusdev/bsd_audioio.h>
struct selinfo {
	struct proc *si_proc;
	int si_coll;
};
#else
#include <sparc/dev/bsd_audioreg.h>
#include <sparc/dev/bsd_audiovar.h>
#include <machine/bsd_audioio.h>
#endif

#ifdef SUNOS
#include "bsd_audiocompat.h"
#endif

/*
 * Initial/default block size is patchable.
 */
int audio_blocksize = DEFBLKSIZE;
int audio_backlog = 400;		/* 50ms in samples */

/*
 * Software state, per AMD79C30 audio chip.
 */
struct audio_softc {
#ifndef SUNOS
	struct	device sc_dev;		/* base device */
	struct	intrhand sc_hwih;	/* hardware interrupt vector */
	struct	intrhand sc_swih;	/* software interrupt vector */
#endif
	int	sc_interrupts;		/* number of interrupts taken */

	int	sc_open;		/* single use device */
	u_long	sc_wseek;		/* timestamp of last frame written */
	u_long	sc_rseek;		/* timestamp of last frame read */
	struct	mapreg sc_map;		/* current contents of map registers */
	struct	selinfo sc_wsel;	/* write selector */
	struct	selinfo sc_rsel;	/* read selector */
	/*
	 * keep track of levels so we don't have to convert back from
	 * MAP gain constants
	 */
	int	sc_rlevel;		/* record level */
	int	sc_plevel;		/* play level */
	int	sc_mlevel;		/* monitor level */

	/* sc_au is special in that the hardware interrupt handler uses it */
	struct	auio sc_au;		/* recv and xmit buffers, etc */

};

/* interrupt interfaces */
#ifndef AUDIO_C_HANDLER
int	audiohwintr __P((void *));
#endif
int	audioswintr __P((void *));

/* forward declarations */
int	audio_sleep __P((struct aucb *, int));
void	audio_setmap __P((volatile struct amd7930 *, struct mapreg *));

static void init_amd();

#if !defined(AUDIO_C_HANDLER) || defined(SUNOS)
struct auio *audio_au;
extern void audio_trap();
#endif

#ifdef SUNOS
struct audio_softc audio_softc;
#define SOFTC(dev) &audio_softc
#define UIOMOVE(cp, len, code, uio) uiomove(cp, len, code, uio)

#define AUDIOOPEN(d, f, i, p)\
	audioopen(d, f, i)\
	dev_t d; int f, i;
#define AUDIOCLOSE(d, f, i, p)\
	audioclose(d, f, i)\
	dev_t d; int f, i;
#define AUDIOREAD(d, u, f) \
	audioread(d, u) dev_t d; struct uio *u;
#define AUDIOWRITE(d, u, f) \
	audiowrite(d, u) dev_t d; struct uio *u;
#define AUDIOIOCTL(d, c, a, f, o)\
	audioioctl(d, c, a, f)\
	dev_t d; int c; caddr_t a; int f;
#define AUDIOSELECT(d, r, p)\
	audio_select(d, r, p)\
	dev_t d; int r; struct proc *p;


#define AUDIO_SET_SWINTR set_intreg(IR_SOFT_INT4, 1)

int
audioselect(dev, rw)
	register dev_t dev;
	int rw;
{
	return (audio_select(dev, rw, u.u_procp));
}

static void
selrecord(p, si)
	struct proc *p;
	struct selinfo *si;
{
	if (si->si_proc != 0)
		si->si_coll = 1;
	else
		si->si_proc = p;
}
#define SELWAKEUP(si) \
{\
	 if ((si)->si_proc != 0) {\
		selwakeup((si)->si_proc, (si)->si_coll); \
		(si)->si_proc = 0;\
		(si)->si_coll = 0;\
	}\
}


static int audioattach();
static int audioidentify();

struct dev_ops bsdaudio_ops = {
	0,
	audioidentify,
	audioattach,
};

static int
audioidentify(cp)
	char *cp;
{
	return (strcmp(cp, "audio") == 0);
}

static int
audioattach(dev)
	struct dev_info *dev;
{
	register struct audio_softc *sc;
	register volatile struct amd7930 *amd;
	struct dev_reg *reg;

	sc = &audio_softc;
	if (dev->devi_nreg != 1 || dev->devi_nintr != 1) {
		printf("audio: bad config\n");
                return (-1);
        }
	reg = dev->devi_reg;
	amd = (struct amd7930 *)map_regs(reg->reg_addr, reg->reg_size,
					 reg->reg_bustype);
	sc->sc_au.au_amd = amd;
	init_amd(amd);

	audio_au = &sc->sc_au;
#ifndef AUDIO_C_HANDLER
	settrap(dev->devi_intr->int_pri, audio_trap);
#else
	/* XXX */
	addintr(dev->devi_intr->int_pri, audiohwintr, dev->devi_name,
		dev->devi_unit);
#endif
	addintr(4, audioswintr, dev->devi_name, dev->devi_unit);
	report_dev(dev);

	return (0);
}
#else
#define AUDIOOPEN(d, f, i, p) audioopen(dev_t d, int f, int i, struct proc *p)
#define AUDIOCLOSE(d, f, i, p) audioclose(dev_t d, int f, int i, \
					  struct proc *p)
#define AUDIOREAD(d, u, f) audioread(dev_t d, struct uio *u, int f)
#define AUDIOWRITE(d, u, f) audiowrite(dev_t d, struct uio *u, int f)
#define AUDIOIOCTL(d, c, a, f, o)\
	audioioctl(dev_t dev, int c, caddr_t a, int f, struct proc *p)
#define AUDIOSELECT(d, r, p) audioselect(dev_t dev, int rw, struct proc *p)
#define SELWAKEUP selwakeup

#define AUDIO_SET_SWINTR ienab_bis(IE_L6)

/* autoconfiguration driver */
void	audioattach(struct device *, struct device *, void *);
struct	cfdriver audiocd =
    { NULL, "audio", matchbyname, audioattach,
      DV_DULL, sizeof(struct audio_softc) };
#define SOFTC(dev) audiocd.cd_devs[minor(dev)]
#define UIOMOVE(cp, len, code, uio) uiomove(cp, len, uio)

/*
 * Audio chip found.
 */
void
audioattach(parent, self, args)
	struct device *parent, *self;
	void *args;
{
	register struct audio_softc *sc = (struct audio_softc *)self;
	register struct romaux *ra = args;
	register volatile struct amd7930 *amd;
	register int pri;

	if (ra->ra_nintr != 1) {
		printf(": expected 1 interrupt, got %d\n", ra->ra_nintr);
		return;
	}
	pri = ra->ra_intr[0].int_pri;
	printf(" pri %d, softpri %d\n", pri, PIL_AUSOFT);
	amd = (volatile struct amd7930 *)(ra->ra_vaddr ?
	    ra->ra_vaddr : mapiodev(ra->ra_paddr, sizeof *amd));
	sc->sc_au.au_amd = amd;

	init_amd(amd);

#ifndef AUDIO_C_HANDLER
	audio_au = &sc->sc_au;
	intr_fasttrap(pri, audio_trap);
#else
	sc->sc_hwih.ih_fun = audiohwintr;
	sc->sc_hwih.ih_arg = &sc->sc_au;
	intr_establish(pri, &sc->sc_hwih);
#endif
	sc->sc_swih.ih_fun = audioswintr;
	sc->sc_swih.ih_arg = sc;
	intr_establish(PIL_AUSOFT, &sc->sc_swih);
}
#endif

static void
init_amd(amd)
	register volatile struct amd7930 *amd;
{
	/* disable interrupts */
	amd->cr = AMDR_INIT;
	amd->dr = AMD_INIT_PMS_ACTIVE | AMD_INIT_INT_DISABLE;

	/*
	 * Initialize the mux unit.  We use MCR3 to route audio (MAP)
	 * through channel Bb.  MCR1 and MCR2 are unused.
	 * Setting the INT enable bit in MCR4 will generate an interrupt
	 * on each converted audio sample.
	 */
	amd->cr = AMDR_MUX_1_4;
 	amd->dr = 0;
	amd->dr = 0;
	amd->dr = (AMD_MCRCHAN_BB << 4) | AMD_MCRCHAN_BA;
	amd->dr = AMD_MCR4_INT_ENABLE;
}

static int audio_default_level = 150;
static void ausetrgain __P((struct audio_softc *, int));
static void ausetpgain __P((struct audio_softc *, int));
static void ausetmgain __P((struct audio_softc *, int));
static int audiosetinfo __P((struct audio_softc *, struct audio_info *));
static int audiogetinfo __P((struct audio_softc *, struct audio_info *));
struct sun_audio_info;
static int sunaudiosetinfo __P((struct audio_softc *,
				struct sun_audio_info *));
static int sunaudiogetinfo __P((struct audio_softc *,
				struct sun_audio_info *));
static void audio_setmmr2 __P((volatile struct amd7930 *, int));

/* ARGSUSED */
int
AUDIOOPEN(dev, flags, ifmt, p)
{
	register struct audio_softc *sc;
	register volatile struct amd7930 *amd;
	int unit = minor(dev);

#ifdef SUNOS
	if (unit > 0)
		return (ENXIO);
	sc = &audio_softc;
#else
	if (unit >= audiocd.cd_ndevs || (sc = audiocd.cd_devs[unit]) == NULL)
		return (ENXIO);
#endif
	if (sc->sc_open)
		return (EBUSY);
	sc->sc_open = 1;

	sc->sc_au.au_lowat = audio_blocksize;
	sc->sc_au.au_hiwat = AUCB_SIZE - sc->sc_au.au_lowat;
	sc->sc_au.au_blksize = audio_blocksize;
	sc->sc_au.au_backlog = audio_backlog;

	/* set up read and write blocks and `dead sound' zero value. */
	AUCB_INIT(&sc->sc_au.au_rb);
	sc->sc_au.au_rb.cb_thresh = AUCB_SIZE;
	AUCB_INIT(&sc->sc_au.au_wb);
	sc->sc_au.au_wb.cb_thresh = -1;

	/* nothing read or written yet */
	sc->sc_rseek = 0;
	sc->sc_wseek = 0;

	bzero((char *)&sc->sc_map, sizeof sc->sc_map);
	/* default to speaker */
	sc->sc_map.mr_mmr2 = AMD_MMR2_AINB | AMD_MMR2_LS;
	
	/* enable interrupts and set parameters established above */
	amd = sc->sc_au.au_amd;
	audio_setmmr2(amd, sc->sc_map.mr_mmr2);
	ausetrgain(sc, audio_default_level);
	ausetpgain(sc, audio_default_level);
	ausetmgain(sc, 0);
	amd->cr = AMDR_INIT;
	amd->dr = AMD_INIT_PMS_ACTIVE;

	return (0);
}

static int
audio_drain(sc)
	register struct audio_softc *sc;
{
	register int error;

	while (!AUCB_EMPTY(&sc->sc_au.au_wb))
		if ((error = audio_sleep(&sc->sc_au.au_wb, 0)) != 0)
			return (error);
	return (0);
}

/*
 * Close an audio chip.
 */
/* ARGSUSED */
int
AUDIOCLOSE(dev, flags, ifmt, p)
{
	register struct audio_softc *sc = SOFTC(dev);
	register volatile struct amd7930 *amd;
	register struct aucb *cb;
	register int s;

	/*
	 * Block until output drains, but allow ^C interrupt.
	 */
	sc->sc_au.au_lowat = 0;	/* avoid excessive wakeups */
	s = splaudio();
	/*
	 * If there is pending output, let it drain (unless
	 * the output is paused).
	 */
	cb = &sc->sc_au.au_wb;
	if (!AUCB_EMPTY(cb) && !cb->cb_pause)
		(void)audio_drain(sc);
	/*
	 * Disable interrupts, clear open flag, and done.
	 */
	amd = sc->sc_au.au_amd;
	amd->cr = AMDR_INIT;
	amd->dr = AMD_INIT_PMS_ACTIVE | AMD_INIT_INT_DISABLE;
	splx(s);
	sc->sc_open = 0;
	return (0);
}

int
audio_sleep(cb, thresh)
	register struct aucb *cb;
	register int thresh;
{
	register int error;
	register int s = splaudio();

	cb->cb_thresh = thresh;
	error = tsleep((caddr_t)cb, (PZERO + 1) | PCATCH, "audio", 0);
	splx(s);
	return (error);
}

/* ARGSUSED */
int
AUDIOREAD(dev, uio, ioflag)
{
	register struct audio_softc *sc = SOFTC(dev);
	register struct aucb *cb;
	register int n, head, taildata, error;
	register int blocksize = sc->sc_au.au_blksize;

	if (uio->uio_resid == 0)
		return (0);
	cb = &sc->sc_au.au_rb;
	error = 0;
	cb->cb_drops = 0;
	sc->sc_rseek = sc->sc_au.au_stamp - AUCB_LEN(cb);
	do {
		while (AUCB_LEN(cb) < blocksize) {
#ifndef SUNOS
			if (ioflag & IO_NDELAY) {
				error = EWOULDBLOCK;
				return (error);
			}
#endif
			if ((error = audio_sleep(cb, blocksize)) != 0)
				return (error);
		}
		/*
		 * The space calculation can only err on the short
		 * side if an interrupt occurs during processing:
		 * only cb_tail is altered in the interrupt code.
		 */
		head = cb->cb_head;
		if ((n = AUCB_LEN(cb)) > uio->uio_resid)
			n = uio->uio_resid;
		taildata = AUCB_SIZE - head;
		if (n > taildata) {
			error = UIOMOVE((caddr_t)cb->cb_data + head,
					taildata, UIO_READ, uio);
			if (error == 0)
				error = UIOMOVE((caddr_t)cb->cb_data,
						n - taildata, UIO_READ, uio);
		} else
			error = UIOMOVE((caddr_t)cb->cb_data + head, n, 
					UIO_READ, uio);
		if (error)
			break;
		head = AUCB_MOD(head + n);
		cb->cb_head = head;
	} while (uio->uio_resid >= blocksize);

	return (error);
}

/* ARGSUSED */
int
AUDIOWRITE(dev, uio, ioflag)
{
	register struct audio_softc *sc = SOFTC(dev);
	register struct aucb *cb = &sc->sc_au.au_wb;
	register int n, tail, tailspace, error, first, watermark;

	error = 0;
	first = 1;
	while (uio->uio_resid > 0) {
		watermark = sc->sc_au.au_hiwat;
		while (AUCB_LEN(cb) > watermark) {
#ifndef SUNOS
			if (ioflag & IO_NDELAY) {
				error = EWOULDBLOCK;
				return (error);
			}
#endif
			if ((error = audio_sleep(cb, watermark)) != 0)
				return (error);
			watermark = sc->sc_au.au_lowat;
		}
		/*
		 * The only value that can change on an interrupt is
		 * cb->cb_head.  We only pull that out once to decide
		 * how much to write into cb_data; if we lose a race
		 * and cb_head changes, we will merely be overly
		 * conservative.  For a legitimate time stamp,
		 * however, we need to synchronize the accesses to
		 * au_stamp and cb_head at a high ipl below.
		 */
		tail = cb->cb_tail;
		if ((n = (AUCB_SIZE - 1) - AUCB_LEN(cb)) > uio->uio_resid) {
			n = uio->uio_resid;
			if (cb->cb_head == tail &&
			    n <= sc->sc_au.au_blksize &&
			    sc->sc_au.au_stamp - sc->sc_wseek > 400) {
				/*
				 * the write is 'small', the buffer is empty
				 * and we have been silent for at least 50ms
				 * so we might be dealing with an application
				 * that writes frames synchronously with
				 * reading them.  If so, we need an output
				 * backlog to cover scheduling delays or
				 * there will be gaps in the sound output.
				 * Also take this opportunity to reset the
				 * buffer pointers in case we ended up on
				 * a bad boundary (odd byte, blksize bytes
				 * from end, etc.).
				 */
				register u_int* ip;
				register int muzero = 0x7f7f7f7f;
				register int i = splaudio();
				cb->cb_head = cb->cb_tail = 0;
				splx(i);
				tail = sc->sc_au.au_backlog;
				ip = (u_int*)cb->cb_data;
				for (i = tail >> 2; --i >= 0; )
					*ip++ = muzero;
			}
		}
		tailspace = AUCB_SIZE - tail;
		if (n > tailspace) {
			/* write first part at tail and rest at head */
			error = UIOMOVE((caddr_t)cb->cb_data + tail,
					tailspace, UIO_WRITE, uio);
			if (error == 0)
				error = UIOMOVE((caddr_t)cb->cb_data,
						n - tailspace, UIO_WRITE, uio);
		} else
			error = UIOMOVE((caddr_t)cb->cb_data + tail, n, 
					UIO_WRITE, uio);
		if (error)
			break;

		tail = AUCB_MOD(tail + n);
		if (first) {
			register int s = splaudio();
			sc->sc_wseek = AUCB_LEN(cb) + sc->sc_au.au_stamp + 1;
			/* 
			 * To guarantee that a write is contiguous in the
			 * sample space, we clear the drop count the first
			 * time through.  If we later get drops, we will
			 * break out of the loop below, before writing
			 * a new frame.
			 */
			cb->cb_drops = 0;
			cb->cb_tail = tail;
			splx(s);
			first = 0;
		} else {
			if (cb->cb_drops != 0)
				break;
			cb->cb_tail = tail;
		}
	}
	return (error);
}

/* Sun audio compatibility */
struct sun_audio_prinfo {
	u_int	sample_rate;
	u_int	channels;
	u_int	precision;
	u_int	encoding;
	u_int	gain;
	u_int	port;
	u_int	reserved0[4];
	u_int	samples;
	u_int	eof;
	u_char	pause;
	u_char	error;
	u_char	waiting;
	u_char	reserved1[3];
	u_char	open;
	u_char	active;
};
struct sun_audio_info {
	struct sun_audio_prinfo play;
	struct sun_audio_prinfo record;
	u_int monitor_gain;
	u_int reserved[4];
};

#ifndef SUNOS
#define SUNAUDIO_GETINFO	_IOR('A', 1, struct sun_audio_info)
#define SUNAUDIO_SETINFO	_IOWR('A', 2, struct sun_audio_info)
#else
#define SUNAUDIO_GETINFO	_IOR(A, 1, struct sun_audio_info)
#define SUNAUDIO_SETINFO	_IOWR(A, 2, struct sun_audio_info)
#endif

/* ARGSUSED */
int
AUDIOIOCTL(dev, cmd, addr, flag, p)
{
	register struct audio_softc *sc = SOFTC(dev);
	int error = 0, s;

	switch (cmd) {

	case AUDIO_GETMAP:
		bcopy((caddr_t)&sc->sc_map, addr, sizeof(sc->sc_map));
		break;

	case AUDIO_SETMAP:
		bcopy(addr, (caddr_t)&sc->sc_map, sizeof(sc->sc_map));
		sc->sc_map.mr_mmr2 &= 0x7f;
		audio_setmap(sc->sc_au.au_amd, &sc->sc_map);
		break;

	case AUDIO_FLUSH:
		s = splaudio();
		AUCB_INIT(&sc->sc_au.au_rb);
		AUCB_INIT(&sc->sc_au.au_wb);
		sc->sc_au.au_stamp = 0;
		splx(s);
		sc->sc_wseek = 0;
		sc->sc_rseek = 0;
		break;

	/*
	 * Number of read samples dropped.  We don't know where or
	 * when they were dropped.
	 */
	case AUDIO_RERROR:
		*(int *)addr = sc->sc_au.au_rb.cb_drops != 0;
		break;

	/*
	 * How many samples will elapse until mike hears the first
	 * sample of what we last wrote?
	 */
	case AUDIO_WSEEK:
		s = splaudio();
		*(u_long *)addr = sc->sc_wseek - sc->sc_au.au_stamp
				  + AUCB_LEN(&sc->sc_au.au_rb);
		splx(s);
		break;

	case AUDIO_SETINFO:
		error = audiosetinfo(sc, (struct audio_info *)addr);
		break;

	case AUDIO_GETINFO:
		error = audiogetinfo(sc, (struct audio_info *)addr);
		break;

	case SUNAUDIO_GETINFO:
		error = sunaudiogetinfo(sc, (struct sun_audio_info *)addr);
		break;

	case SUNAUDIO_SETINFO:
		error = sunaudiosetinfo(sc, (struct sun_audio_info *)addr);
		break;

	case AUDIO_DRAIN:
		error = audio_drain(sc);
		break;

	default:
		error = EINVAL;
		break;
	}
	return (error);
}

/* ARGSUSED */
int
AUDIOSELECT(dev, rw, p)
{
	register struct audio_softc *sc = SOFTC(dev);
	register struct aucb *cb;
	register int s = splaudio();

	switch (rw) {

	case FREAD:
		cb = &sc->sc_au.au_rb;
		if (AUCB_LEN(cb) >= sc->sc_au.au_blksize) {
			splx(s);
			return (1);
		}
		selrecord(p, &sc->sc_rsel);
		cb->cb_thresh = sc->sc_au.au_blksize;
		break;

	case FWRITE:
		cb = &sc->sc_au.au_wb;
		if (AUCB_LEN(cb) <= sc->sc_au.au_lowat) {
			splx(s);
			return (1);
		}
		selrecord(p, &sc->sc_wsel);
		cb->cb_thresh = sc->sc_au.au_lowat;
		break;
	}
	splx(s);
	return (0);
}

#ifdef AUDIO_C_HANDLER
int
audiohwintr(au0)
	void *au0;
{
#ifdef SUNOS
	register struct auio *au = audio_au;
#else
	register struct auio *au = au0;
#endif
	register volatile struct amd7930 *amd = au->au_amd;
	register struct aucb *cb;
	register int h, t, k;

	k = amd->ir;		/* clear interrupt */
	++au->au_stamp;

	/* receive incoming data */
	cb = &au->au_rb;
	h = cb->cb_head;
	t = cb->cb_tail;
	k = AUCB_MOD(t + 1);
	if (h == k)
		cb->cb_drops++;
	else if  (cb->cb_pause != 0)
		cb->cb_pdrops++;
	else {
		cb->cb_data[t] = amd->bbrb;
		cb->cb_tail = t = k;
	}
	if (AUCB_MOD(t - h) >= cb->cb_thresh) {
		cb->cb_thresh = AUCB_SIZE;
		cb->cb_waking = 1;
		AUDIO_SET_SWINTR;
	}
	/* send outgoing data */
	cb = &au->au_wb;
	h = cb->cb_head;
	t = cb->cb_tail;
	if (h == t)
		cb->cb_drops++;
	else if (cb->cb_pause != 0)
		cb->cb_pdrops++;
	else {
		cb->cb_head = h = AUCB_MOD(h + 1);
		amd->bbtb = cb->cb_data[h];
	}
	if (AUCB_MOD(t - h) <= cb->cb_thresh) {
		cb->cb_thresh = -1;
		cb->cb_waking = 1;
		AUDIO_SET_SWINTR;
	}
	return (1);
}
#endif

/* ARGSUSED */
int
audioswintr(sc0)
	void *sc0;
{
	register struct audio_softc *sc;
	register int s, ret = 0;
#ifdef SUNOS
	sc = &audio_softc;
#else
	sc = sc0;
#endif
	s = splaudio();
	if (sc->sc_au.au_rb.cb_waking != 0) {
		sc->sc_au.au_rb.cb_waking = 0;
		splx(s);
		ret = 1;
		wakeup((caddr_t)&sc->sc_au.au_rb);
		SELWAKEUP(&sc->sc_rsel);
	}
	if (sc->sc_au.au_wb.cb_waking != 0) {
		sc->sc_au.au_wb.cb_waking = 0;
		splx(s);
		ret = 1;
		wakeup((caddr_t)&sc->sc_au.au_wb);
		SELWAKEUP(&sc->sc_wsel);
	} else
		splx(s);
	return (ret);
}

/* Write 16 bits of data from variable v to the data port of the audio chip */

#define	WAMD16(amd, v) ((amd)->dr = (v), (amd)->dr = (v) >> 8)

void
audio_setmap(amd, map)
	register volatile struct amd7930 *amd;
	register struct mapreg *map;
{
	register int i, s, v;

	s = splaudio();
	amd->cr = AMDR_MAP_1_10;
	for (i = 0; i < 8; i++) {
		v = map->mr_x[i];
		WAMD16(amd, v);
	}
	for (i = 0; i < 8; ++i) {
		v = map->mr_r[i];
		WAMD16(amd, v);
	}
	v = map->mr_gx; WAMD16(amd, v);
	v = map->mr_gr; WAMD16(amd, v);
	v = map->mr_ger; WAMD16(amd, v);
	v = map->mr_stgr; WAMD16(amd, v);
	v = map->mr_ftgr; WAMD16(amd, v);
	v = map->mr_atgr; WAMD16(amd, v);
	amd->dr = map->mr_mmr1;
	amd->dr = map->mr_mmr2;
	splx(s);
}

/*
 * Set the mmr1 register and one other 16 bit register in the audio chip.
 * The other register is indicated by op and val.
 */
void
audio_setmmr1(amd, mmr1, op, val)
	register volatile struct amd7930 *amd;
	register int mmr1;
	register int op;
	register int val;
{
	register int s = splaudio();

	amd->cr = AMDR_MAP_MMR1;
	amd->dr = mmr1;
	amd->cr = op;
	WAMD16(amd, val);
	splx(s);
}

/*
 * Set the mmr2 register.
 */
static void
audio_setmmr2(amd, mmr2)
	register volatile struct amd7930 *amd;
	register int mmr2;
{
	register int s = splaudio();

	amd->cr = AMDR_MAP_MMR2;
	amd->dr = mmr2;
	splx(s);
}

/*
 * gx, gr & stg gains.  this table must contain 256 elements with
 * the 0th being "infinity" (the magic value 9008).  The remaining
 * elements match sun's gain curve (but with higher resolution):
 * -18 to 0dB in .16dB steps then 0 to 12dB in .08dB steps.
 */
static const u_short gx_coeff[256] = {
	0x9008, 0x8b7c, 0x8b51, 0x8b45, 0x8b42, 0x8b3b, 0x8b36, 0x8b33,
	0x8b32, 0x8b2a, 0x8b2b, 0x8b2c, 0x8b25, 0x8b23, 0x8b22, 0x8b22,
	0x9122, 0x8b1a, 0x8aa3, 0x8aa3, 0x8b1c, 0x8aa6, 0x912d, 0x912b,
	0x8aab, 0x8b12, 0x8aaa, 0x8ab2, 0x9132, 0x8ab4, 0x913c, 0x8abb,
	0x9142, 0x9144, 0x9151, 0x8ad5, 0x8aeb, 0x8a79, 0x8a5a, 0x8a4a,
	0x8b03, 0x91c2, 0x91bb, 0x8a3f, 0x8a33, 0x91b2, 0x9212, 0x9213,
	0x8a2c, 0x921d, 0x8a23, 0x921a, 0x9222, 0x9223, 0x922d, 0x9231,
	0x9234, 0x9242, 0x925b, 0x92dd, 0x92c1, 0x92b3, 0x92ab, 0x92a4,
	0x92a2, 0x932b, 0x9341, 0x93d3, 0x93b2, 0x93a2, 0x943c, 0x94b2,
	0x953a, 0x9653, 0x9782, 0x9e21, 0x9d23, 0x9cd2, 0x9c23, 0x9baa,
	0x9bde, 0x9b33, 0x9b22, 0x9b1d, 0x9ab2, 0xa142, 0xa1e5, 0x9a3b,
	0xa213, 0xa1a2, 0xa231, 0xa2eb, 0xa313, 0xa334, 0xa421, 0xa54b,
	0xada4, 0xac23, 0xab3b, 0xaaab, 0xaa5c, 0xb1a3, 0xb2ca, 0xb3bd,
	0xbe24, 0xbb2b, 0xba33, 0xc32b, 0xcb5a, 0xd2a2, 0xe31d, 0x0808,
	0x72ba, 0x62c2, 0x5c32, 0x52db, 0x513e, 0x4cce, 0x43b2, 0x4243,
	0x41b4, 0x3b12, 0x3bc3, 0x3df2, 0x34bd, 0x3334, 0x32c2, 0x3224,
	0x31aa, 0x2a7b, 0x2aaa, 0x2b23, 0x2bba, 0x2c42, 0x2e23, 0x25bb,
	0x242b, 0x240f, 0x231a, 0x22bb, 0x2241, 0x2223, 0x221f, 0x1a33,
	0x1a4a, 0x1acd, 0x2132, 0x1b1b, 0x1b2c, 0x1b62, 0x1c12, 0x1c32,
	0x1d1b, 0x1e71, 0x16b1, 0x1522, 0x1434, 0x1412, 0x1352, 0x1323,
	0x1315, 0x12bc, 0x127a, 0x1235, 0x1226, 0x11a2, 0x1216, 0x0a2a,
	0x11bc, 0x11d1, 0x1163, 0x0ac2, 0x0ab2, 0x0aab, 0x0b1b, 0x0b23,
	0x0b33, 0x0c0f, 0x0bb3, 0x0c1b, 0x0c3e, 0x0cb1, 0x0d4c, 0x0ec1,
	0x079a, 0x0614, 0x0521, 0x047c, 0x0422, 0x03b1, 0x03e3, 0x0333,
	0x0322, 0x031c, 0x02aa, 0x02ba, 0x02f2, 0x0242, 0x0232, 0x0227,
	0x0222, 0x021b, 0x01ad, 0x0212, 0x01b2, 0x01bb, 0x01cb, 0x01f6,
	0x0152, 0x013a, 0x0133, 0x0131, 0x012c, 0x0123, 0x0122, 0x00a2,
	0x011b, 0x011e, 0x0114, 0x00b1, 0x00aa, 0x00b3, 0x00bd, 0x00ba,
	0x00c5, 0x00d3, 0x00f3, 0x0062, 0x0051, 0x0042, 0x003b, 0x0033,
	0x0032, 0x002a, 0x002c, 0x0025, 0x0023, 0x0022, 0x001a, 0x0021,
	0x001b, 0x001b, 0x001d, 0x0015, 0x0013, 0x0013, 0x0012, 0x0012,
	0x000a, 0x000a, 0x0011, 0x0011, 0x000b, 0x000b, 0x000c, 0x000e,
};

/*
 * second stage play gain.
 */
static const u_short ger_coeff[] = {
	0x431f, /* 5. dB */
	0x331f, /* 5.5 dB */
	0x40dd, /* 6. dB */
	0x11dd, /* 6.5 dB */
	0x440f, /* 7. dB */
	0x411f, /* 7.5 dB */
	0x311f, /* 8. dB */
	0x5520, /* 8.5 dB */
	0x10dd, /* 9. dB */
	0x4211, /* 9.5 dB */
	0x410f, /* 10. dB */
	0x111f, /* 10.5 dB */
	0x600b, /* 11. dB */
	0x00dd, /* 11.5 dB */
	0x4210, /* 12. dB */
	0x110f, /* 13. dB */
	0x7200, /* 14. dB */
	0x2110, /* 15. dB */
	0x2200, /* 15.9 dB */
	0x000b, /* 16.9 dB */
	0x000f  /* 18. dB */
#define NGER (sizeof(ger_coeff) / sizeof(ger_coeff[0]))
};

static void
ausetrgain(sc, level)
	register struct audio_softc *sc;
	register int level;
{
	level &= 0xff;
	sc->sc_rlevel = level;
	sc->sc_map.mr_mmr1 |= AMD_MMR1_GX;
	sc->sc_map.mr_gx = gx_coeff[level];
	audio_setmmr1(sc->sc_au.au_amd, sc->sc_map.mr_mmr1,
		      AMDR_MAP_GX, sc->sc_map.mr_gx);
}

static void
ausetpgain(sc, level)
	register struct audio_softc *sc;
	register int level;
{
	register int gi, s;
	register volatile struct amd7930 *amd;

	level &= 0xff;
	sc->sc_plevel = level;
	sc->sc_map.mr_mmr1 |= AMD_MMR1_GER|AMD_MMR1_GR;
	level *= 256 + NGER;
	level >>= 8;
	if (level >= 256) {
		gi = level - 256;
		level = 255;
	} else
		gi = 0;
	sc->sc_map.mr_ger = ger_coeff[gi];
	sc->sc_map.mr_gr = gx_coeff[level];

	amd = sc->sc_au.au_amd;
	s = splaudio();
	amd->cr = AMDR_MAP_MMR1;
	amd->dr = sc->sc_map.mr_mmr1;
	amd->cr = AMDR_MAP_GR;
	gi =  sc->sc_map.mr_gr;
	WAMD16(amd, gi);
	amd->cr = AMDR_MAP_GER;
	gi =  sc->sc_map.mr_ger;
	WAMD16(amd, gi);
	splx(s);
}

static void
ausetmgain(sc, level)
	register struct audio_softc *sc;
	register int level;
{
	level &= 0xff;
	sc->sc_mlevel = level;
	sc->sc_map.mr_mmr1 |= AMD_MMR1_STG;
	sc->sc_map.mr_stgr = gx_coeff[level];
	audio_setmmr1(sc->sc_au.au_amd, sc->sc_map.mr_mmr1,
		      AMDR_MAP_STG, sc->sc_map.mr_stgr);
}

static int
audiosetinfo(sc, ai)
	struct audio_softc *sc;
	struct audio_info *ai;
{
	struct audio_prinfo *r = &ai->record, *p = &ai->play;
	register int s, bsize;

	if (p->gain != ~0)
		ausetpgain(sc, p->gain);
	if (r->gain != ~0)
		ausetrgain(sc, r->gain);
	if (ai->monitor_gain != ~0)
		ausetmgain(sc, ai->monitor_gain);
	if (p->port == AUDIO_SPEAKER) {
		sc->sc_map.mr_mmr2 |= AMD_MMR2_LS;
		audio_setmmr2(sc->sc_au.au_amd, sc->sc_map.mr_mmr2);
	} else if (p->port == AUDIO_HEADPHONE) {
		sc->sc_map.mr_mmr2 &=~ AMD_MMR2_LS;
		audio_setmmr2(sc->sc_au.au_amd, sc->sc_map.mr_mmr2);
	}
	if (p->pause != (u_char)~0)
		sc->sc_au.au_wb.cb_pause = p->pause;
	if (r->pause != (u_char)~0)
		sc->sc_au.au_rb.cb_pause = r->pause;

	if (ai->blocksize != ~0) {
		if (ai->blocksize == 0)
			bsize = ai->blocksize = DEFBLKSIZE;
		else if (ai->blocksize > MAXBLKSIZE)
			bsize = ai->blocksize = MAXBLKSIZE;
		else
			bsize = ai->blocksize;

		s = splaudio();
		sc->sc_au.au_blksize = bsize;
		/* AUDIO_FLUSH */
		AUCB_INIT(&sc->sc_au.au_rb);
		AUCB_INIT(&sc->sc_au.au_wb);
		splx(s);

	}
	if (ai->hiwat != ~0 && (unsigned)ai->hiwat < AUCB_SIZE)
		sc->sc_au.au_hiwat = ai->hiwat;
	if (ai->lowat != ~0 && ai->lowat < AUCB_SIZE)
		sc->sc_au.au_lowat = ai->lowat;
	if (ai->backlog != ~0 && ai->backlog < (AUCB_SIZE/2))
		sc->sc_au.au_backlog = ai->backlog;

	return (0);
}

static int
sunaudiosetinfo(sc, ai)
	struct audio_softc *sc;
	struct sun_audio_info *ai;
{
	struct sun_audio_prinfo *r = &ai->record, *p = &ai->play;

	if (p->gain != ~0)
		ausetpgain(sc, p->gain);
	if (r->gain != ~0)
		ausetrgain(sc, r->gain);
	if (ai->monitor_gain != ~0)
		ausetmgain(sc, ai->monitor_gain);
	if (p->port == AUDIO_SPEAKER) {
		sc->sc_map.mr_mmr2 |= AMD_MMR2_LS;
		audio_setmmr2(sc->sc_au.au_amd, sc->sc_map.mr_mmr2);
	} else if (p->port == AUDIO_HEADPHONE) {
		sc->sc_map.mr_mmr2 &=~ AMD_MMR2_LS;
		audio_setmmr2(sc->sc_au.au_amd, sc->sc_map.mr_mmr2);
	}
	/*
	 * The bsd driver does not distinguish between paused and active.
	 * (In the sun driver, not active means samples are not ouput
	 * at all, but paused means the last streams buffer is drained
	 * and then output stops.)  If either are 0, then when stop output.
	 * Otherwise, if either are non-zero, we resume.
	 */
	if (p->pause == 0 || p->active == 0)
		sc->sc_au.au_wb.cb_pause = 0;
	else if (p->pause != (u_char)~0 || p->active != (u_char)~0)
		sc->sc_au.au_wb.cb_pause = 1;
	if (r->pause == 0 || r->active == 0)
		sc->sc_au.au_rb.cb_pause = 0;
	else if (r->pause != (u_char)~0 || r->active != (u_char)~0)
		sc->sc_au.au_rb.cb_pause = 1;

	return (0);
}

static int
audiogetinfo(sc, ai)
	struct audio_softc *sc;
	struct audio_info *ai;
{
	struct audio_prinfo *r = &ai->record, *p = &ai->play;

	p->sample_rate = r->sample_rate = 8000;
	p->channels = r->channels = 1;
	p->precision = r->precision = 8;
	p->encoding = r->encoding = AUDIO_ENCODING_ULAW;

	ai->monitor_gain = sc->sc_mlevel;
	r->gain = sc->sc_rlevel;
	p->gain = sc->sc_plevel;
	r->port = 1; p->port = (sc->sc_map.mr_mmr2 & AMD_MMR2_LS) ?
		AUDIO_SPEAKER : AUDIO_HEADPHONE;

	p->pause = sc->sc_au.au_wb.cb_pause;
	r->pause = sc->sc_au.au_rb.cb_pause;
	p->error = sc->sc_au.au_wb.cb_drops != 0;
	r->error = sc->sc_au.au_rb.cb_drops != 0;

	p->open = sc->sc_open;
	r->open = sc->sc_open;

	p->samples = sc->sc_au.au_stamp - sc->sc_au.au_wb.cb_pdrops;
	r->samples = sc->sc_au.au_stamp - sc->sc_au.au_rb.cb_pdrops;

	p->seek = sc->sc_wseek;
	r->seek = sc->sc_rseek;

	ai->blocksize = sc->sc_au.au_blksize;
	ai->hiwat = sc->sc_au.au_hiwat;
	ai->lowat = sc->sc_au.au_lowat;
	ai->backlog = sc->sc_au.au_backlog;

	return (0);
}

static int
sunaudiogetinfo(sc, ai)
	struct audio_softc *sc;
	struct sun_audio_info *ai;
{
	struct sun_audio_prinfo *r = &ai->record, *p = &ai->play;

	p->sample_rate = r->sample_rate = 8000;
	p->channels = r->channels = 1;
	p->precision = r->precision = 8;
	p->encoding = r->encoding = AUDIO_ENCODING_ULAW;

	ai->monitor_gain = sc->sc_mlevel;
	r->gain = sc->sc_rlevel;
	p->gain = sc->sc_plevel;
	r->port = 1; p->port = (sc->sc_map.mr_mmr2 & AMD_MMR2_LS) ?
		AUDIO_SPEAKER : AUDIO_HEADPHONE;

	p->active = p->pause = sc->sc_au.au_wb.cb_pause;
	r->active = r->pause = sc->sc_au.au_rb.cb_pause;
	p->error = sc->sc_au.au_wb.cb_drops != 0;
	r->error = sc->sc_au.au_rb.cb_drops != 0;

	p->waiting = 0;
	r->waiting = 0;
	p->eof = 0;
	r->eof = 0;

	p->open = sc->sc_open;
	r->open = sc->sc_open;

	p->samples = sc->sc_au.au_stamp - sc->sc_au.au_wb.cb_pdrops;
	r->samples = sc->sc_au.au_stamp - sc->sc_au.au_rb.cb_pdrops;

	return (0);
}
#endif
