/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: kb.c,v 4.300 91/06/09 06:42:44 root Rel41 $ SONY
 *
 *	@(#)kb.c	7.3 (Berkeley) %G%
 */

#include "kb.h"

#if NKB > 0

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/buf.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/uio.h>
#include <sys/kernel.h>

#include <news3400/iop/keyboard.h>
#include <news3400/iop/kbreg.h>

#ifdef CPU_SINGLE
#include <sys/tty.h>
#include <sys/clist.h>
#include <news3400/sio/scc.h>
#include <news3400/hbdev/hbvar.h>
#define	iop_device	hb_device
#define	ii_alive	hi_alive
#else
#include "../iop/iopvar.h"
#endif

int kbprobe(), kbattach();
extern Key_table key_table[];
extern Key_table default_table[];
extern int country;

#ifdef CPU_SINGLE
struct hb_device *kbinfo[NKB];
struct hb_driver kbdriver = 
	{ kbprobe, 0, kbattach, 0, 0, "kb", kbinfo, "mc", 0, 0 };
extern	Key_table *key_table_addr;
#endif

#ifdef CPU_DOUBLE
struct iop_device *kbinfo[NKB];
struct iop_driver kbdriver = 
	{ kbprobe, 0, kbattach, 0, "kb", kbinfo };
#endif

char	kb_busy;

/*ARGSUSED*/
kbprobe(ii)
	struct iop_device *ii;
{

	return (kb_probe(ii));
}

/*ARGSUSED*/
kbattach(ii)
	struct iop_device *ii;
{

	kb_attach(ii);
}

/*ARGSUSED*/
kbopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct iop_device *ii;

	if ((unit = minor(dev)) >= NKB || (ii = kbinfo[unit]) == 0 ||
	    ii->ii_alive == 0)
		return (ENXIO);
	if (kb_busy)
		return (EBUSY);
	kb_busy = 1;
	return (kb_open());
}

/*ARGSUSED*/
kbclose(dev, flag)
	dev_t dev;
	int flag;
{
	kb_close();
	kb_busy = 0;
}

#ifdef KBDEBUG
/*ARGSUSED*/
kbread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int error = 0;

	while (uio->uio_resid > 0)
		if (error = kb_read(uio))
			break;
	return (error);
}
#endif /* KBDEBUG */

/*ARGSUSED*/
kbwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int error = 0;

	while (uio->uio_resid > 0)
		if (error = kb_write(uio))
			break;

	return (error);
}

/*ARGSUSED*/
kbioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	int error = 0;

	switch (cmd) {

	case KBIOCBELL:
		error = kb_ctrl(KIOCBELL, (int *)data);
		break;

	case KBIOCREPT:
		error = kb_ctrl(KIOCREPT, 0);
		break;

	case KBIOCNRPT:
		error = kb_ctrl(KIOCNRPT, 0);
		break;

	case KBIOCSETLOCK:
		error = kb_ctrl(KIOCSETLOCK, (int *)data);
		break;

	case KBIOCSETTBL:
		error = kbchange((Key_tab_info *)data);
		break;

	case KBIOCGETCNUM:
		error = kb_ctrl(KIOCGETCNUM, (int *)data);
		break;

	case KBIOCSETCNUM:
		error = kb_ctrl(KIOCSETCNUM, (int *)data);
		break;

	case KBIOCGETSTAT:
		error = kb_ctrl(KIOCGETSTAT, (int *)data);
		break;

	case KBIOCSETSTAT:
		error = kb_ctrl(KIOCSETSTAT, (int *)data);
		break;

	default:
		return (EIO);
	}
	return (error);
}

/*ARGSUSED*/
kbselect(dev, flag)
	dev_t dev;
	int flag;
{

}

kbchange(argp)
	Key_tab_info *argp;
{
	int keynumber;

	keynumber = ((Key_tab_info *)argp)->key_number;
	if (keynumber < 0 || keynumber > N_KEY)
		return (EINVAL);

	key_table[keynumber] = argp->key_num_table;
	return (kb_ctrl(KIOCCHTBL, (Key_table *)key_table));
}

/*
 * Machine dependent functions
 *
 *	kb_probe()
 *	kb_attach()
 *	kb_open()
 *	kb_close()
 *	kb_write()
 *	kb_ctrl()
 */

#ifdef CPU_SINGLE
extern int tty00_is_console;
extern Key_table *key_table_addr;
#define KBPRI	(PZERO+1)

kb_probe(hi)
	struct hb_device *hi;
{

	return (1);
}

kb_attach(hi)
	struct hb_device *hi;
{

	kbd_init();
	kbd_ioctl(0, KIOCSETCNUM, &country);
}

kb_open()
{

	if (tty00_is_console)
		kbm_open(SCC_KEYBOARD);
	return (0);
}

kb_close()
{

	if (tty00_is_console)
		kbm_close(SCC_KEYBOARD);
	return (0);
}

#ifdef KB_DEBUG
kb_read(uio)
	struct uio *uio;
{
	int n;
	char buf[32];

	return (uiomove((caddr_t)buf, n, UIO_READ, uio));
	n = kbd_read_raw(SCC_KEYBOARD, buf, min(uio->uio_resid, sizeof (buf)));
	if (n == 0)
		return (0);
	return (uiomove((caddr_t)buf, n, UIO_READ, uio));
}
#endif /* KB_DEBUG */

kb_write(uio)
	struct uio *uio;
{
	int n, error;
	char buf[32];

	n = min(sizeof(buf), uio->uio_resid);
	if (error = uiomove((caddr_t)buf, n, UIO_WRITE, uio))
		return (error);
	kbd_write(SCC_KEYBOARD, buf, n);
	return (0);
}

kb_ctrl(func, arg)
	int func;
	int *arg;
{

	return (kbd_ioctl(0, func, arg));
}
#endif /* CPU_SINGLE */

#ifdef IPC_MRX
#include "../ipc/newsipc.h"
#include "../mrx/h/kbms.h"

#ifdef news3800
#define ipc_phys(x)	(caddr_t)(K0_TT0(x))
#endif

int	port_kboutput, port_kboutput_iop, port_kbctrl, port_kbctrl_iop;

kb_probe(ii)
	struct iop_device *ii;
{

	port_kboutput_iop = object_query("kbd_output");
	port_kbctrl_iop = object_query("kbd_io");
	if (port_kboutput_iop <= 0 || port_kbctrl_iop <= 0)
		return (0);
	port_kboutput = port_create("@kboutput", NULL, 0);
	port_kbctrl = port_create("@kbctrl", NULL, 0);
	return (1);
}

kb_attach(ii)
	struct iop_device *ii;
{

	(void) kb_ctrl(KIOCCHTBL, (Key_table *)key_table);
	(void) kb_ctrl(KIOCSETCNUM, &country);
}

kb_open()
{

	return (0);
}

kb_close()
{

	return (0);
}

#ifdef KB_DEBUG
kb_read(uio)
	struct uio *uio;
{
	char *addr;
	int len;
	int error;

	len = uio->uio_resid;
	msg_send(port_kbinput_iop, port_kbinput, &len, sizeof(len), 0);
	msg_recv(port_kbinput, NULL, &addr, &len, 0);
	error = uiomove(addr, len, UIO_READ, uio);
	msg_free(port_kbinput);
	return (error);
}
#endif /* KB_DEBUG */

kb_write(uio)
	struct uio *uio;
{
	int len;
	int error;
	char buf[MAX_CIO];

	len = min(MAX_CIO, uio->uio_resid);
	if (error = uiomove(buf, len, UIO_WRITE, uio))
		return (error);
	msg_send(port_kboutput_iop, port_kboutput, buf, len, 0);
	msg_recv(port_kboutput, NULL, NULL, NULL, 0);
	msg_free(port_kboutput);
	return (0);
}

kb_ctrl(func, arg)
	int func;
	int *arg;
{
	struct kb_ctrl_req req;
	int *reply, result;
	static int tmp;

	if (func == KIOCCHTBL || func == KIOCOYATBL)
		req.kb_arg = (int)ipc_phys(arg);
	else if (arg == NULL)
		req.kb_arg = (int)&tmp;
	else
		req.kb_arg = *arg;
	req.kb_func = func;
	msg_send(port_kbctrl_iop, port_kbctrl, &req, sizeof(req), 0);
	msg_recv(port_kbctrl, NULL, &reply, NULL, 0);
	result = *reply;
	msg_free(port_kbctrl);
	switch (func) {

	case KIOCGETCNUM:
	case KIOCGETSTAT:
		if (arg)
			*(int *)arg = result;
	}
	return (0);
}
#endif /* IPC_MRX */
#endif /* NKB > 0 */
