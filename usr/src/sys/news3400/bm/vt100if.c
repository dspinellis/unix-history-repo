/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: vt100if.c,v 4.300 91/06/09 06:15:02 root Rel41 $ SONY
 *
 *	@(#)vt100if.c	7.2 (Berkeley) %G%
 */

#include <machine/fix_machine_type.h>

#ifdef IPC_MRX
#include "types.h"
#include "ioctl.h"
#else /* IPC_MRX */
#include <sys/types.h>
#ifdef CPU_SINGLE
#include <sys/param.h>
#endif /* CPU_SINGLE */
#include <sys/ioctl.h>
#endif /* IPC_MRX */

#if CPU_SINGLE
#include <news3400/sio/scc.h>
#include <news3400/sio/sccparam.h>
#endif /* CPU_SINGLE */

#ifdef IPC_MRX
#include "scc.h"
#include "sccparam.h"
#include "cio.h"
#define SCC_KEYBOARD	0
#undef MAX_CIO
#include "object.h"
#include "process.h"

#include "config.h"
#define kbd_ioctl(chan, cmd, argp)      { \
        if (kb_ioctl) \
                (*kb_ioctl)(chan, cmd, argp); \
}
extern int bitmap_use;
#endif /* IPC_MRX */

#ifdef IPC_MRX
#include "../../iop/kbreg.h"
#include "../../iop/keyboard.h"
#ifdef IPC_3CPU
#include "../../ubdev/msgio.h"
extern int *gcpu_semadr;
#endif /* IPC_3CPU */
#else /* IPC_MRX */
#include <news3400/iop/keyboard.h>
#endif /* IPC_MRX */

#ifdef CPU_SINGLE
#include <machine/cpu.h>
#define PRE_EMPT	need_resched()
#endif

int	tmode;

static int char_mask = 0x7f;

int	bm_todo;

vt100_write(chan, buf, count)
	int chan;
	register char *buf;
	register int count;
{
	bm_todo = count;

	rst_dimmer_cnt();

#ifdef IPC_MRX
#ifdef IPC_3CPU
	*gcpu_semadr = 1;
#endif
	sem_wait(bitmap_use);
#endif

	while (bm_todo-- > 0)
		Putchar(*buf++ & char_mask, 0);
	Putchar(0, 1);

#ifdef IPC_MRX
#ifdef IPC_3CPU
	*gcpu_semadr = 0;
#endif
	sem_signal(bitmap_use);
#endif
#ifdef CPU_SINGLE
	PRE_EMPT;
#endif

	return (count);
}

vt100_read(chan, buf, count)
	int chan;
	char *buf;
	int count;
{
#ifdef IPC_MRX
	if (kb_read) {
		return ((*kb_read)(chan, buf, count));
	} else {
		return (-1);
	}
#else /* IPC_MRX */
	return (kbd_read(chan, buf, count));
#endif /* IPC_MRX */
}

#ifdef CPU_SINGLE
static int param;

bitmap_set_param(c)
int c;
{
	if ((c & CHAR_SIZE) == C7BIT) {
		char_mask = 0x7f;
	} else {
		char_mask = 0xff;
	}
#ifdef KM_JIS
	switch (c & TERM_MODE) {
	case CJIS:
		tmode = KM_JIS;
		break;
	case CSJIS:
		tmode = KM_SJIS;
		break;
	case CEUC:
		tmode = KM_EUC;
		break;
	default:
		tmode = KM_ASCII;
		break;
	}
#endif
	param = c;
	return (0);
}

bitmap_get_param()
{
	return(param);
}

#else /* CPU_SINGLE */

#ifdef IPC_MRX
#define SCC_SETPARAMS	CIO_SETPARAMS
#define SCC_GETPARAMS	CIO_GETPARAMS
#define SCC_LINE_CHG	CIO_LINE_CHG
#define SCC_NREAD	CIO_NREAD
#define SCC_FLUSH	CIO_FLUSH
#define SCC_STOP	CIO_STOP
#define SCC_START	CIO_START
#define SCC_RESET	CIO_RESET
#endif /* IPC_MRX */

vt100_ioctl(chan, cmd, argp)
	int chan, cmd, *argp;
{
	static int param;

	switch (cmd) {
	case SCC_SETPARAMS:
		param = *argp;
		if ((param & CHAR_SIZE) == C7BIT)
			char_mask = 0x7f;
		else
			char_mask = 0xff;
#ifdef KM_JIS
		switch (param & TERM_MODE) {
		case CJIS:
			tmode = KM_JIS;
			break;
		case CSJIS:
			tmode = KM_SJIS;
			break;
		case CEUC:
			tmode = KM_EUC;
			break;
		default:
			tmode = KM_ASCII;
			break;
		}
#endif
		return (0);
	case SCC_GETPARAMS:
		*argp = param;
		return (0);
	case SCC_LINE_CHG:
		return (0);
	case SCC_NREAD:
		kbd_ioctl(SCC_KEYBOARD, KIOCNREAD, argp);
		return (0);
	case SCC_FLUSH:
	case SCC_STOP:
	case SCC_START:
	case SCC_RESET:
		return (0);
	default:
		return (-1);
	}
}
#endif /* CPU_SINGLE */
