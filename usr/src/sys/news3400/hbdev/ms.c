/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: ms.c,v 4.300 91/06/09 06:22:04 root Rel41 $ SONY
 *
 *	@(#)ms.c	7.3 (Berkeley) %G%
 */

#include <machine/fix_machine_type.h>

#include "ms.h"
#if NMS > 0
/*
 * mouse 
 */

#include <sys/types.h>
#include <machine/cpu.h>
#include <machine/pte.h>
#include <sys/param.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/buf.h>
#include <sys/malloc.h>
#include <sys/systm.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/file.h>

#include <news3400/iop/mouse.h>
#include <news3400/iop/msreg.h>

#include <news3400/sio/scc.h>
#include <news3400/hbdev/hbvar.h>

#ifndef mips
#define volatile
#endif /* mips */

struct ms_stat	 ms_stat[NMS];

int	msprobe(), msattach();

struct hb_device   *msinfo[NMS];
struct hb_driver   msdriver = {
	msprobe, 0, msattach, 0, 0, "ms", msinfo, "mc", 0, 0
};

#ifndef news700
extern int tty00_is_console;
#endif

#ifdef news3400
#define	splms		spl4
#else /* news3400 */
#ifdef news700
#define	splms		spl4
#else /* news700 */
#define	splms		spl5
#endif /* news700 */
#endif /* news3400 */

/*ARGSUSED*/
msprobe(ii)
	struct hb_device *ii;
{
	return(sizeof(struct ms_stat));
}

/*ARGSUSED*/
msattach(ii)
	struct hb_device *ii;
{

}

/* queue structure operators */

msq_init(unit)
	int unit;
{
	register volatile struct ms_queue *q = ms_stat[unit].mss_queue;

	q->mq_head = q->mq_tail = 0;
}

int
msq_stat(unit)
	int unit;
{
	register volatile struct ms_queue *q = ms_stat[unit].mss_queue;

	while (q->mq_head != q->mq_tail) {
		if (!q->mq_queue[q->mq_head].mse_inval)
			break;
		q->mq_head = ++q->mq_head % MS_MAXREPORT;
	}
	return (q->mq_head != q->mq_tail);
}

struct ms_event *
msq_read(unit)
	int unit;
{
	register volatile struct ms_queue *q = ms_stat[unit].mss_queue;
	register volatile struct ms_event *data;

	while (q->mq_head != q->mq_tail && q->mq_queue[q->mq_head].mse_inval)
		q->mq_head = ++q->mq_head % MS_MAXREPORT;
	if (q->mq_head == q->mq_tail) {
		data = NULL;
	} else {
		data = q->mq_queue + q->mq_head++;
		q->mq_head %= MS_MAXREPORT;
	}
	return (data);
}

struct ms_event *
msq_write(unit)
	int unit;
{
	register volatile struct ms_queue *q = ms_stat[unit].mss_queue;
	register volatile struct ms_event *data = q->mq_queue + q->mq_tail;
	register int new;

	/* if queue is full, newest data is gone away */
	new = (q->mq_tail + 1) % MS_MAXREPORT;
	if (new != q->mq_head)
		q->mq_tail = new;
	return (data);
}

msq_flush(unit, trig)
	int unit;
	char trig;
{
	register volatile struct ms_queue *q = ms_stat[unit].mss_queue;
	register int i;

	i = q->mq_head;
	while (i != q->mq_tail) {
		if (q->mq_queue[i].mse_trig == trig)
			q->mq_queue[i].mse_inval = -1;
		i = ++i % MS_MAXREPORT;
	}
}

/*
 * Mouse open function.
 */
msopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit = MSUNIT(dev);
	register struct ms_stat *ms = &ms_stat[unit];
	register struct hb_device *ii = msinfo[unit];
	static struct ms_coord initxy = { 0, 0 };

	/* check device */
	if (unit < 0 || unit >= NMS || ii == NULL || ii->hi_alive == 0)
		return(ENXIO);

	/* check duplicable open */
	if (ms->mss_stat & MS_ACTIVE)
		return(EBUSY);

	ms->mss_queue = malloc(sizeof(struct ms_queue), M_DEVBUF, M_WAITOK);
	if (ms->mss_queue == NULL)
		return (ENOMEM);
	msq_init(unit);
	ms->mss_mode = flag;
	ms->mss_stat = MS_ACTIVE;

	/* communicate to IOP .. clear event mask, set initial xy. */
	ms->mss_eventmask = 0;
	ms->mss_data.md_sw = 0;			/* XXX */
	ms->mss_data.md_x = 0;
	ms->mss_data.md_y = 0;
	ms->mss_param.mp_delta = 5;
	ms->mss_param.mp_mag = 3;
	ms->mss_range.mr_min.mc_x = 0x80000000;
	ms->mss_range.mr_min.mc_y = 0x80000000;
	ms->mss_range.mr_max.mc_x = 0x7fffffff;
	ms->mss_range.mr_max.mc_y = 0x7fffffff;

	if (curproc->p_pgrp->pg_id == 0) {
		if (ms->mss_pgrp == 0)
			ms->mss_pgrp = curproc->p_pid;
		curproc->p_pgrp->pg_id = ms->mss_pgrp;
	}
#ifdef news700
	scc_open(SCC_MOUSE);
#else
	if (tty00_is_console)
		kbm_open(SCC_KEYBOARD);
	kbm_open(SCC_MOUSE);
#endif

	return (0);
}

/*
 * Mouse close function.
 */

/*ARGSUSED*/
msclose(dev, flag)
	dev_t dev;
	int flag;
{
	int unit = MSUNIT(dev);
	register struct ms_stat	*ms = &ms_stat[unit];
	register struct hb_device *ii = msinfo[unit];

	/* check unit no. */
	if (unit < 0 || unit >= NMS || ii == NULL || ii->hi_alive == 0)
		return ENXIO;

	/* check status */
	if (!(ms->mss_stat & MS_ACTIVE))
		return ENXIO;

	/* clear eventmask and status */
	ms->mss_stat = 0;
	ms->mss_eventmask = 0;

	free(ms->mss_queue, M_DEVBUF);
	ms->mss_pgrp = 0;
#ifndef news700
	if (tty00_is_console)
		kbm_close(SCC_KEYBOARD);
	kbm_close(SCC_MOUSE);
#endif /* news700 */

	return (0);
}

/*
 * Mouse read function.
 */

msread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register int unit = MSUNIT(dev);
	register struct ms_stat *ms = &ms_stat[unit];
	register struct hb_device *ii = msinfo[unit];
	register volatile struct ms_event *data;
	struct ms_data xy;
	int s, error;

	/* check argument */
	if (unit < 0 || unit >= NMS || ii == NULL || ii->hi_alive == 0)
		return ENXIO;

	/* event mode -> waiting */
	if (ms->mss_eventmask & MS_EMEVENT) {
		s = splms();
		if (msq_stat(unit) == 0 && (ms->mss_stat & MS_NBIO)) {
			splx(s);
			return (EWOULDBLOCK);
		}
		while (msq_stat(unit) == 0) {
			ms->mss_stat |= MS_EVWAIT;
			sleep((caddr_t)&ms->mss_queue, MSPRI);
			ms->mss_stat &= ~MS_EVWAIT;
		}
		splx(s);
		if(MSOLDIF(dev)) {
			while ((data = msq_read(unit)) != NULL &&
			    uio->uio_resid >= sizeof(struct ms_data)) {
				error = uiomove((caddr_t)&data->mse_data,
						sizeof(struct ms_data), uio);
				if (error)
					return error;
			}
		} else {
			while ((data = msq_read(unit)) != NULL &&
			    uio->uio_resid >= sizeof(struct ms_event)) {
				error = uiomove((caddr_t)data,
						sizeof(struct ms_event), uio);
				if (error)
					return error;
			}
		}
	} else {
		while (uio->uio_resid >= sizeof(struct ms_data)) {
			s = splms();
			xy = ms->mss_data;
			splx(s);
			error = uiomove((caddr_t)&xy,
					sizeof(struct ms_data), uio);
			if (error)
				return error;
		}
	}
	return 0;
}

/*
 * Mouse write function
 */

mswrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register int unit = MSUNIT(dev);
	register struct ms_stat *ms = &ms_stat[unit];
	register struct hb_device *ii = msinfo[unit];
	struct ms_coord xy;
	register int s, error;

	/* check argument */
	if (unit < 0 || unit >= NMS || ii == NULL || ii->hi_alive == 0)
		return ENXIO;

	while (uio->uio_resid >= sizeof(struct ms_coord)) {
		error = uiomove((caddr_t)&xy, sizeof(xy), uio);
		if (error)
			return error;
		s = splms();
		ms->mss_data.md_x = xy.mc_x;
		ms->mss_data.md_y = xy.mc_y;
		splx(s);
		lock_bitmap();
		updateCursor(&ms->mss_data.md_x, &ms->mss_data.md_y, 1);
		unlock_bitmap();
	}
	return 0;
}


/*
 * Mouse I/O control function
 */

/*ARGSUSED*/
msioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register int unit = MSUNIT(dev);
	register struct ms_stat *ms = &ms_stat[unit];
	register struct hb_device *ii = msinfo[unit];
	register int s;

	if (unit < 0 || unit >= NMS || ii == NULL || ii->hi_alive == 0)
		return EIO;

	s = splms();

	switch (cmd) {
	case MSIOCGETEM:
		(*(int*)data) = ms->mss_eventmask;
		break;
	case MSIOCSETEM:
		ms->mss_eventmask = *(int *)data;
		break;
	case MSIOCSETXY:
		ms->mss_data.md_x = ((struct ms_coord*)data)->mc_x;
		ms->mss_data.md_y = ((struct ms_coord*)data)->mc_y;
		lock_bitmap();
		updateCursor(&ms->mss_data.md_x, &ms->mss_data.md_y, 1);
		unlock_bitmap();
		msq_flush(unit, MSE_MOTION);
		break;
	case MSIOCFLUSH:
		msq_init(unit);
		break;
	case MSIOCSETPARAM:
		ms->mss_param = *(struct ms_param*)data;
		break;
	case MSIOCSETRANGE:
		ms->mss_range = *(struct ms_range*)data;
		break;
	case FIONBIO:
		if (*(int *)data)
			ms->mss_stat |= MS_NBIO;
		else
			ms->mss_stat &= ~MS_NBIO;
		break;
	case FIOASYNC:
		if (*(int *)data)
			ms->mss_stat |= MS_ASYNC;
		else
			ms->mss_stat &= ~MS_ASYNC;
		break;
	case TIOCSPGRP:
		ms->mss_pgrp = *(int *)data;
		break;
	case TIOCGPGRP:
		*(int *)data = ms->mss_pgrp;
		break;
	default:
		splx(s);
		return ENOTTY;
	}
	splx(s);
	return 0;
}

msselect(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit = MSUNIT(dev);
	register struct ms_stat *ms;	
	int s;

	if (unit < 0 || unit >= NMS)
		return(0);

	s = splms();
	ms = &ms_stat[unit];

	switch(flag) {
	case FREAD:
		if (!(ms->mss_eventmask & MS_EMEVENT))
			goto win;
		if(msq_stat(unit))
			goto win;

		if (ms->mss_rsel && ms->mss_rsel->p_wchan == (caddr_t)&selwait)
			ms->mss_stat |= MS_RCOLL;
		else
			ms->mss_rsel = curproc;
		break;

	case FWRITE:
		goto win;
	}
	splx(s);
	return(0);
win:
	splx(s);
	return(1);
}

msselwakeup(ms)
	register struct ms_stat *ms;	
{
	if (ms->mss_rsel) {
		selwakeup(ms->mss_rsel, ms->mss_stat&MS_RCOLL);
		ms->mss_stat &= ~MS_RCOLL;
		ms->mss_rsel = 0;
	}
	if (ms->mss_stat & MS_ASYNC)
		gsignal(ms->mss_pgrp, SIGIO);
}

mssint()
{
	printf("mssint\n");
}

msputevent(unit, trig, dir, code)
	int unit, trig, dir, code;
{
	register struct ms_stat *ms = &ms_stat[unit];
	register volatile struct ms_event *me;
	register int s;

	me = msq_write(unit);

	s = splclock();
	me->mse_time = time;
	me->mse_inval = 0;
	splx(s);

	me->mse_trig = trig;
	me->mse_dir = dir;
	me->mse_code = code;
	me->mse_data = ms->mss_data;

	if (ms->mss_stat & MS_EVWAIT)
		wakeup((caddr_t)&ms->mss_queue);

	msselwakeup(ms);

	return (0);
}

/*
 * for keyboard
 */
mskeytrigger(unit, up, keycode)
	int unit;
	int up;
	int keycode;
{
	register struct ms_stat *ms = &ms_stat[unit];

	if((ms->mss_eventmask & MS_EMEVENT) == 0)
		return 0;
	if((ms->mss_eventmask & MS_EMKEY) == 0)
		return 0;

	(void) msputevent(unit, MSE_KEY, up ? MSE_UP : MSE_DOWN, keycode);

	return 1;
}

/*
 * msconv - convert mouse hardware reports(3 bytes) into internal mouse data
 *		it leaves the old mouse data in ms_data_old and
 *		new mouse data in ms_data.
 */
msconv(unit, rep)
	int unit;
	register char rep[];
{
	register struct ms_stat *ms = &ms_stat[unit];
	register int s_byte;
	register int sw = 0;
	register int dx, dy;
	int adx, ady;

	ms->mss_data_old = ms->mss_data;

	/* switch status */
	s_byte = rep[MS_S_BYTE];
	if (s_byte & MS_S_SW1)
		sw |= MS_BUTNL;
	if (s_byte & MS_S_SW2)
		sw |= MS_BUTNR;
	if (s_byte & MS_S_SW3)
		sw |= MS_BUTNM;
	ms->mss_data.md_sw = sw;

	/* delta x */
	dx = rep[MS_X_BYTE] & MS_X_X06;
	if (s_byte & MS_S_X7)
		dx = (~0&~MS_X_X06)|dx;

	dy = rep[MS_Y_BYTE] & MS_Y_Y06;
	if (s_byte & MS_S_Y7)
		dy = (~0&~MS_Y_Y06)|dy;

#define ABS(x)	((x)>=0 ? (x) : -(x))
	adx = ABS(dx);
	ady = ABS(dy);
#undef ABS

	if (adx > ms->mss_param.mp_delta) {
		adx = ms->mss_param.mp_delta +
		      (adx - ms->mss_param.mp_delta) * ms->mss_param.mp_mag;
		if (dx < 0)
			dx = -adx;
		else
			dx = adx;
	}
	if (ady > ms->mss_param.mp_delta) {
		ady = ms->mss_param.mp_delta +
		      (ady - ms->mss_param.mp_delta) * ms->mss_param.mp_mag;
		if (dy < 0)
			dy = -ady;
		else
			dy = ady;
	}
	ms->mss_data.md_x += dx;
	ms->mss_data.md_y += dy;

	if (dx > 0)
		ms->mss_data.md_x = min(ms->mss_data.md_x,
					ms->mss_range.mr_max.mc_x);
	else
		ms->mss_data.md_x = max(ms->mss_data.md_x,
					ms->mss_range.mr_min.mc_x);
	if (dy > 0)
		ms->mss_data.md_y = min(ms->mss_data.md_y,
					ms->mss_range.mr_max.mc_y);
	else
		ms->mss_data.md_y = max(ms->mss_data.md_y,
					ms->mss_range.mr_min.mc_y);

	if (dx != 0 || dy != 0)
		updateCursor(&ms->mss_data.md_x, &ms->mss_data.md_y, 0);
}

mscheckevent(unit)
	int unit;
{
	register struct ms_stat *ms = &ms_stat[unit];
	register int i;
	register int changebits;
	register int dir;

	if ((ms->mss_eventmask & MS_EMEVENT) == 0)
		return;

	if (ms->mss_data_old.md_sw != ms->mss_data.md_sw) {

		changebits = (ms->mss_data_old.md_sw ^ ms->mss_data.md_sw);
		changebits &= ms->mss_eventmask;

		for(i = 0; i < 3; i++) {
			if(changebits & (1 << i)) {
				if((1 << i) & ms->mss_data.md_sw)
					dir = MSE_DOWN;
				else
					dir = MSE_UP;
				msputevent(unit, MSE_BUTTON, dir, i);
			}
		}
	}

	if ((ms->mss_eventmask & MS_EMMOTION) &&
	    (ms->mss_data_old.md_x != ms->mss_data.md_x ||
	     ms->mss_data_old.md_y != ms->mss_data.md_y)) {
		msputevent(unit, MSE_MOTION, 0, 0);
		return;
	}
}

/*
 * _ms_helper - open the mouse line and read mouse data and
 *		convert them into mouse data (events)
 */
_ms_helper(unit)
	int unit;
{
	register int c;
	static int counter = 0;
	static char buf[MS_DB_SIZE];

#ifdef notyet /* KU:XXX */
	intrcnt[INTR_MOUSE]++;
#endif

#if NBM > 0
	rst_dimmer_cnt();
#endif

	while ((c = xgetc(SCC_MOUSE)) >= 0) {
		if (c & MS_S_MARK)
			counter = 0;
		buf[counter] = c;
		if (++counter == 3) {
			msconv(unit, buf);
			mscheckevent(unit);
			counter = 0;
		}
	}
}
#endif /* NMS > 0 */
