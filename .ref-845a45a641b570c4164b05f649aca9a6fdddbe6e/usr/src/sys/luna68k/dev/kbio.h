/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: kbio.h,v 1.3 92/06/17 05:35:49 torek Exp $ (LBL)
 *
 * from: sparc/dev/kbio.h	7.2 (Berkeley) 7/21/92
 *
 *	@(#)kbio.h	7.1 (Berkeley) %G%
 */

#ifdef	notyet
/*
 * The following is a minimal emulation of Sun's `kio' structures
 * and related operations necessary to make X11 happy (i.e., make it
 * compile, and make old X11 binaries run).
 */

/*
 * The kiockey structure apparently gets and/or sets keyboard mappings.
 * It seems to be kind of useless, but X11 uses it (according to the
 * comments) to figure out when a Sun 386i has a type-4 keyboard but
 * claims to have a type-3 keyboard.  We need just enough to cause the
 * appropriate ioctl to return the appropriate magic value.
 *
 * KIOCGETKEY fills in kio_entry from kio_station.  Not sure what tablemask
 * is for; X sets it before the call, so it is not an output, but we do not
 * care anyway.  KIOCSDIRECT is supposed to tell the kernel whether to send
 * keys to the console or to X; we just send them to X whenever the keyboard
 * is open at all.  (XXX may need to change this later)
 *
 * Keyboard commands and types are defined in kbd.h as they are actually
 * real hardware commands and type numbers.
 */
struct kiockey {
	int	kio_tablemask;	/* whatever */
	u_char	kio_station;	/* key number */
	u_char	kio_entry;	/* HOLE if not present */
	char	kio_text[10];	/* the silly escape sequences (unsupported) */
};

#define	HOLE	0x302		/* value for kio_entry to say `really type 3' */

#define	KIOCTRANS	_IOW('k', 0, int)	/* set translation mode */
			/* (we only accept TR_UNTRANS_EVENT) */
#define	KIOCGETKEY	_IOWR('k', 2, struct kiockey) /* fill in kio_entry */
#define	KIOCGTRANS	_IOR('k', 5, int)	/* get translation mode */
#define	KIOCCMD		_IOW('k', 8, int)	/* X uses this to ring bell */
#define	KIOCTYPE	_IOR('k', 9, int)	/* get keyboard type */
#define	KIOCSDIRECT	_IOW('k', 10, int)	/* keys to console? */

#define	TR_UNTRANS_EVENT	3
#endif

#define	KIOCMOUSE	_IOW('k', 20, int)	/* enable/disabel to trace mouse motion */
