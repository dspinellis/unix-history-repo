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
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbd.c	7.4 (Berkeley) %G%
 *
 * from: $Header: kbd.c,v 1.16 92/11/26 01:28:44 torek Exp $ (LBL)
 */

/*
 * Keyboard driver (/dev/kbd -- note that we do not have minor numbers
 * [yet?]).  Translates incoming bytes to ASCII or to `firm_events' and
 * passes them up to the appropriate reader.
 */

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/device.h>
#include <sys/ioctl.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/syslog.h>
#include <sys/systm.h>
#include <sys/tty.h>

#include <machine/autoconf.h>

#include <sparc/dev/vuid_event.h>
#include <sparc/dev/event_var.h>
#include <sparc/dev/kbd.h>
#include <sparc/dev/kbio.h>

/*
 * Sun keyboard definitions (from Sprite).
 * These apply to type 2, 3 and 4 keyboards.
 */
#define	KEY_CODE(c)	((c) & KBD_KEYMASK)	/* keyboard code index */
#define	KEY_UP(c)	((c) & KBD_UP)		/* true => key went up */

/*
 * Each KEY_CODE(x) can be translated via the tables below.
 * The result is either a valid ASCII value in [0..0x7f] or is one
 * of the following `magic' values saying something interesting
 * happened.  If LSHIFT or RSHIFT has changed state the next
 * lookup should come from the appropriate table; if ALLUP is
 * sent all keys (including both shifts and the control key) are
 * now up, and the next byte is the keyboard ID code.
 *
 * These tables ignore all function keys (on the theory that if you
 * want these keys, you should use a window system).  Note that
 * `caps lock' is just mapped as `ignore' (so there!). (Only the
 * type 3 and 4 keyboards have a caps lock key anyway.)
 */
#define	KEY_MAGIC	0x80		/* flag => magic value */
#define	KEY_IGNORE	0x80
#define	KEY_L1		KEY_IGNORE
#define	KEY_CAPSLOCK	KEY_IGNORE
#define	KEY_LSHIFT	0x81
#define	KEY_RSHIFT	0x82
#define	KEY_CONTROL	0x83
#define	KEY_ALLUP	0x84		/* all keys are now up; also reset */

/*
 * Decode tables for type 2, 3, and 4 keyboards
 * (stolen from Sprite; see also kbd.h).
 */
static const u_char kbd_unshifted[] = {
/*   0 */	KEY_IGNORE,	KEY_L1,		KEY_IGNORE,	KEY_IGNORE,
/*   4 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*   8 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  12 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  16 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  20 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  24 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  28 */	KEY_IGNORE,	'\033',		'1',		'2',
/*  32 */	'3',		'4',		'5',		'6',
/*  36 */	'7',		'8',		'9',		'0',
/*  40 */	'-',		'=',		'`',		'\b',
/*  44 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  48 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  52 */	KEY_IGNORE,	'\t',		'q',		'w',
/*  56 */	'e',		'r',		't',		'y',
/*  60 */	'u',		'i',		'o',		'p',
/*  64 */	'[',		']',		'\177',		KEY_IGNORE,
/*  68 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  72 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  76 */	KEY_CONTROL,	'a',		's',		'd',
/*  80 */	'f',		'g',		'h',		'j',
/*  84 */	'k',		'l',		';',		'\'',
/*  88 */	'\\',		'\r',		KEY_IGNORE,	KEY_IGNORE,
/*  92 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  96 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_LSHIFT,
/* 100 */	'z',		'x',		'c',		'v',
/* 104 */	'b',		'n',		'm',		',',
/* 108 */	'.',		'/',		KEY_RSHIFT,	'\n',
/* 112 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/* 116 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_CAPSLOCK,
/* 120 */	KEY_IGNORE,	' ',		KEY_IGNORE,	KEY_IGNORE,
/* 124 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_ALLUP,
};

static const u_char kbd_shifted[] = {
/*   0 */	KEY_IGNORE,	KEY_L1,		KEY_IGNORE,	KEY_IGNORE,
/*   4 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*   8 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  12 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  16 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  20 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  24 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  28 */	KEY_IGNORE,	'\033',		'!',		'@',
/*  32 */	'#',		'$',		'%',		'^',
/*  36 */	'&',		'*',		'(',		')',
/*  40 */	'_',		'+',		'~',		'\b',
/*  44 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  48 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  52 */	KEY_IGNORE,	'\t',		'Q',		'W',
/*  56 */	'E',		'R',		'T',		'Y',
/*  60 */	'U',		'I',		'O',		'P',
/*  64 */	'{',		'}',		'\177',		KEY_IGNORE,
/*  68 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  72 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  76 */	KEY_CONTROL,	'A',		'S',		'D',
/*  80 */	'F',		'G',		'H',		'J',
/*  84 */	'K',		'L',		':',		'"',
/*  88 */	'|',		'\r',		KEY_IGNORE,	KEY_IGNORE,
/*  92 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/*  96 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_LSHIFT,
/* 100 */	'Z',		'X',		'C',		'V',
/* 104 */	'B',		'N',		'M',		'<',
/* 108 */	'>',		'?',		KEY_RSHIFT,	'\n',
/* 112 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,
/* 116 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_CAPSLOCK,
/* 120 */	KEY_IGNORE,	' ',		KEY_IGNORE,	KEY_IGNORE,
/* 124 */	KEY_IGNORE,	KEY_IGNORE,	KEY_IGNORE,	KEY_ALLUP,
};

/*
 * We need to remember the state of the keyboard's shift and control
 * keys, and we need a per-type translation table.
 */
struct kbd_state {
	const u_char *kbd_unshifted;	/* unshifted keys */
	const u_char *kbd_shifted;	/* shifted keys */
	const u_char *kbd_cur;	/* current keys (either of the preceding) */
	union {
		char	c[2];	/* left and right shift keys */
		short	s;	/* true => either shift key */
	} kbd_shift;
#define	kbd_lshift	kbd_shift.c[0]
#define	kbd_rshift	kbd_shift.c[1]
#define	kbd_anyshift	kbd_shift.s
	char	kbd_control;	/* true => ctrl down */
	char	kbd_click;	/* true => keyclick enabled */
	char	kbd_takeid;	/* take next byte as ID */
	u_char	kbd_id;		/* a place to store the ID */
};

/*
 * Keyboard driver state.  The ascii and kbd links go up and down and
 * we just sit in the middle doing translation.  Note that it is possible
 * to get just one of the two links, in which case /dev/kbd is unavailable.
 * The downlink supplies us with `internal' open and close routines which
 * will enable dataflow across the downlink.  We promise to call open when
 * we are willing to take keystrokes, and to call close when we are not.
 * If /dev/kbd is not the console tty input source, we do this whenever
 * /dev/kbd is in use; otherwise we just leave it open forever.
 */
struct kbd_softc {
	struct	tty *k_cons;		/* uplink for ASCII data to console */
	struct	tty *k_kbd;		/* downlink for output to keyboard */
	void	(*k_open) __P((struct tty *));	/* enable dataflow */
	void	(*k_close) __P((struct tty *));	/* disable dataflow */
	int	k_evmode;		/* set if we should produce events */
	struct	kbd_state k_state;	/* ASCII decode state */
	struct	evvar k_events;		/* event queue state */
} kbd_softc;

/* Prototypes */
void	kbd_ascii(struct tty *);
void	kbd_serial(struct tty *, void (*)(), void (*)());
static	void kbd_getid(void *);
void	kbd_reset(struct kbd_state *);
static	int kbd_translate(int, struct kbd_state *);
void	kbd_rint(int);
int	kbdopen(dev_t, int, int, struct proc *);
int	kbdclose(dev_t, int, int, struct proc *);
int	kbdread(dev_t, struct uio *, int);
int	kbdwrite(dev_t, struct uio *, int);
int	kbdioctl(dev_t, int, caddr_t, int, struct proc *);
int	kbdselect(dev_t, int, struct proc *);
int	kbd_docmd(int, int);

/*
 * Attach the console keyboard ASCII (up-link) interface.
 * This happens before kbd_serial.
 */
void
kbd_ascii(struct tty *tp)
{

	kbd_softc.k_cons = tp;
}

/*
 * Attach the console keyboard serial (down-link) interface.
 * We pick up the initial keyboard clock state here as well.
 */
void
kbd_serial(struct tty *tp, void (*iopen)(), void (*iclose)())
{
	register struct kbd_softc *k;
	register char *cp;

	k = &kbd_softc;
	k->k_kbd = tp;
	k->k_open = iopen;
	k->k_close = iclose;

	cp = getpropstring(optionsnode, "keyboard-click?");
	if (cp && strcmp(cp, "true") == 0)
		k->k_state.kbd_click = 1;

	if (k->k_cons) {
		/*
		 * We supply keys for /dev/console.  Before we can
		 * do so, we have to ``open'' the line.  We also need
		 * the type, got by sending a RESET down the line ...
		 * but clists are not yet set up, so we use a timeout
		 * to try constantly until we can get the ID.  (gag)
		 */
		(*iopen)(tp);		/* never to be closed */
		kbd_getid(NULL);
	}
}

/*
 * Initial keyboard reset, to obtain ID and thus a translation table.
 * We have to try again and again until the tty subsystem works.
 */
static void
kbd_getid(void *arg)
{
	register struct kbd_softc *k;
	register struct tty *tp;
	register int retry;
	extern int cold;		/* XXX */

	k = &kbd_softc;
	if (k->k_state.kbd_cur != NULL)
		return;
	tp = k->k_kbd;
	if (cold || ttyoutput(KBD_CMD_RESET, tp) >= 0)
		retry = 1;
	else {
		(*tp->t_oproc)(tp);
		retry = 2 * hz;
	}
	timeout(kbd_getid, NULL, retry);
}

void
kbd_reset(register struct kbd_state *ks)
{
	/*
	 * On first identification, wake up anyone waiting for type
	 * and set up the table pointers.
	 */
	if (ks->kbd_unshifted == NULL) {
		wakeup((caddr_t)ks);
		ks->kbd_unshifted = kbd_unshifted;
		ks->kbd_shifted = kbd_shifted;
		ks->kbd_cur = ks->kbd_unshifted;
	}

	/* Restore keyclick, if necessary */
	switch (ks->kbd_id) {

	case KB_SUN2:
		/* Type 2 keyboards don't support keyclick */
		break;

	case KB_SUN3:
		/* Type 3 keyboards come up with keyclick on */
		if (!ks->kbd_click)
			(void) kbd_docmd(KBD_CMD_NOCLICK, 0);
		break;

	case KB_SUN4:
		/* Type 4 keyboards come up with keyclick off */
		if (ks->kbd_click)
			(void) kbd_docmd(KBD_CMD_CLICK, 0);
		break;
	}
}

/*
 * Turn keyboard up/down codes into ASCII.
 */
static int
kbd_translate(register int c, register struct kbd_state *ks)
{
	register int down;

	if (ks->kbd_cur == NULL) {
		/*
		 * Do not know how to translate yet.
		 * We will find out when a RESET comes along.
		 */
		return (-1);
	}
	down = !KEY_UP(c);
	c = ks->kbd_cur[KEY_CODE(c)];
	if (c & KEY_MAGIC) {
		switch (c) {

		case KEY_LSHIFT:
			ks->kbd_lshift = down;
			break;

		case KEY_RSHIFT:
			ks->kbd_rshift = down;
			break;

		case KEY_ALLUP:
			ks->kbd_anyshift = 0;
			ks->kbd_control = 0;
			break;

		case KEY_CONTROL:
			ks->kbd_control = down;
			/* FALLTHROUGH */

		case KEY_IGNORE:
			return (-1);

		default:
			panic("kbd_translate");
		}
		if (ks->kbd_anyshift)
			ks->kbd_cur = ks->kbd_shifted;
		else
			ks->kbd_cur = ks->kbd_unshifted;
		return (-1);
	}
	if (!down)
		return (-1);
	if (ks->kbd_control) {
		/* control space and unshifted control atsign return null */
		if (c == ' ' || c == '2')
			return (0);
		/* unshifted control hat */
		if (c == '6')
			return ('^' & 0x1f);
		/* standard controls */
		if (c >= '@' && c < 0x7f)
			return (c & 0x1f);
	}
	return (c);
}

void
kbd_rint(register int c)
{
	register struct kbd_softc *k = &kbd_softc;
	register struct firm_event *fe;
	register int put;

	/*
	 * Reset keyboard after serial port overrun, so we can resynch.
	 * The printf below should be shortened and/or replaced with a
	 * call to log() after this is tested (and how will we test it?!).
	 */
	if (c & (TTY_FE|TTY_PE)) {
		printf("keyboard input parity or framing error (0x%x)\n", c);
		(void) ttyoutput(KBD_CMD_RESET, k->k_kbd);
		(*k->k_kbd->t_oproc)(k->k_kbd);
		return;
	}

	/* Read the keyboard id if we read a KBD_RESET last time */
	if (k->k_state.kbd_takeid) {
		k->k_state.kbd_takeid = 0;
		k->k_state.kbd_id = c;
		kbd_reset(&k->k_state);
		return;
	}

	/* If we have been reset, setup to grab the keyboard id next time */
	if (c == KBD_RESET) {
		k->k_state.kbd_takeid = 1;
		return;
	}

	/*
	 * If /dev/kbd is not connected in event mode, but we are sending
	 * data to /dev/console, translate and send upstream.  Note that
	 * we will get this while opening /dev/kbd if it is not already
	 * open and we do not know its type.
	 */
	if (!k->k_evmode) {
		c = kbd_translate(c, &k->k_state);
		if (c >= 0 && k->k_cons != NULL)
			ttyinput(c, k->k_cons);
		return;
	}

	/*
	 * IDLEs confuse the MIT X11R4 server badly, so we must drop them.
	 * This is bad as it means the server will not automatically resync
	 * on all-up IDLEs, but I did not drop them before, and the server
	 * goes crazy when it comes time to blank the screen....
	 */
	if (c == KBD_IDLE)
		return;

	/*
	 * Keyboard is generating events.  Turn this keystroke into an
	 * event and put it in the queue.  If the queue is full, the
	 * keystroke is lost (sorry!).
	 */
	put = k->k_events.ev_put;
	fe = &k->k_events.ev_q[put];
	put = (put + 1) % EV_QSIZE;
	if (put == k->k_events.ev_get) {
		log(LOG_WARNING, "keyboard event queue overflow\n"); /* ??? */
		return;
	}
	fe->id = KEY_CODE(c);
	fe->value = KEY_UP(c) ? VKEY_UP : VKEY_DOWN;
	fe->time = time;
	k->k_events.ev_put = put;
	EV_WAKEUP(&k->k_events);
}

int
kbdopen(dev_t dev, int flags, int mode, struct proc *p)
{
	int s, error;

	if (kbd_softc.k_events.ev_io)
		return (EBUSY);
	kbd_softc.k_events.ev_io = p;
	/*
	 * If no console keyboard, tell the device to open up, maybe for
	 * the first time.  Then make sure we know what kind of keyboard
	 * it is.
	 */
	if (kbd_softc.k_cons == NULL)
		(*kbd_softc.k_open)(kbd_softc.k_kbd);
	error = 0;
	s = spltty();
	if (kbd_softc.k_state.kbd_cur == NULL) {
		(void) ttyoutput(KBD_CMD_RESET, kbd_softc.k_kbd);
		error = tsleep((caddr_t)&kbd_softc.k_state, PZERO | PCATCH,
		    devopn, hz);
		if (error == EWOULDBLOCK)	/* no response */
			error = ENXIO;
	}
	splx(s);
	if (error) {
		kbd_softc.k_events.ev_io = NULL;
		return (error);
	}
	ev_init(&kbd_softc.k_events);
	return (0);
}

int
kbdclose(dev_t dev, int flags, int mode, struct proc *p)
{

	/*
	 * Turn off event mode, dump the queue, and close the keyboard
	 * unless it is supplying console input.
	 */
	kbd_softc.k_evmode = 0;
	ev_fini(&kbd_softc.k_events);
	if (kbd_softc.k_cons == NULL)
		(*kbd_softc.k_close)(kbd_softc.k_kbd);
	kbd_softc.k_events.ev_io = NULL;
	return (0);
}

int
kbdread(dev_t dev, struct uio *uio, int flags)
{

	return (ev_read(&kbd_softc.k_events, uio, flags));
}

/* this routine should not exist, but is convenient to write here for now */
int
kbdwrite(dev_t dev, struct uio *uio, int flags)
{

	return (EOPNOTSUPP);
}

int
kbdioctl(dev_t dev, int cmd, register caddr_t data, int flag, struct proc *p)
{
	register struct kbd_softc *k = &kbd_softc;

	switch (cmd) {

	case KIOCTRANS:
		if (*(int *)data == TR_UNTRANS_EVENT)
			return (0);
		break;

	case KIOCGTRANS:
		/*
		 * Get translation mode
		 */
		*(int *)data = TR_UNTRANS_EVENT;
		return (0);

	case KIOCGETKEY:
		if (((struct kiockey *)data)->kio_station == 118) {
			/*
			 * This is X11 asking if a type 3 keyboard is
			 * really a type 3 keyboard.  Say yes.
			 */
			((struct kiockey *)data)->kio_entry = HOLE;
			return (0);
		}
		break;

	case KIOCCMD:
		/*
		 * ``unimplemented commands are ignored'' (blech)
		 * so cannot check return value from kbd_docmd
		 */
#ifdef notyet
		while (kbd_docmd(*(int *)data, 1) == ENOSPC) /*ERESTART?*/
			(void) sleep((caddr_t)&lbolt, TTOPRI);
#else
		(void) kbd_docmd(*(int *)data, 1);
#endif
		return (0);

	case KIOCTYPE:
		*(int *)data = k->k_state.kbd_id;
		return (0);

	case KIOCSDIRECT:
		k->k_evmode = *(int *)data;
		return (0);

	case FIONBIO:		/* we will remove this someday (soon???) */
		return (0);

	case FIOASYNC:
		k->k_events.ev_async = *(int *)data != 0;
		return (0);

	case TIOCSPGRP:
		if (*(int *)data != k->k_events.ev_io->p_pgid)
			return (EPERM);
		return (0);

	default:
		return (ENOTTY);
	}

	/*
	 * We identified the ioctl, but we do not handle it.
	 */
	return (EOPNOTSUPP);		/* misuse, but what the heck */
}

int
kbdselect(dev_t dev, int rw, struct proc *p)
{

	return (ev_select(&kbd_softc.k_events, rw, p));
}

/*
 * Execute a keyboard command; return 0 on success.
 * If `isuser', force a small delay before output if output queue
 * is flooding.  (The keyboard runs at 1200 baud, or 120 cps.)
 */
int
kbd_docmd(int cmd, int isuser)
{
	register struct tty *tp = kbd_softc.k_kbd;
	register struct kbd_softc *k = &kbd_softc;
	int s;

	if (tp == NULL)
		return (ENXIO);		/* ??? */
	switch (cmd) {

	case KBD_CMD_BELL:
	case KBD_CMD_NOBELL:
		/* Supported by type 2, 3, and 4 keyboards */
		break;

	case KBD_CMD_CLICK:
		/* Unsupported by type 2 keyboards */
		if (k->k_state.kbd_id != KB_SUN2) {
			k->k_state.kbd_click = 1;
			break;
		}
		return (EINVAL);

	case KBD_CMD_NOCLICK:
		/* Unsupported by type 2 keyboards */
		if (k->k_state.kbd_id != KB_SUN2) {
			k->k_state.kbd_click = 0;
			break;
		}
		return (EINVAL);

	default:
		return (EINVAL);	/* ENOTTY? EOPNOTSUPP? */
	}

	if (isuser) {
		s = spltty();
		if (tp->t_outq.c_cc > 120)
			(void) tsleep((caddr_t)&lbolt, TTIPRI,
			    ttyout, 0);
		splx(s);
	}
	if (ttyoutput(cmd, tp) >= 0)
		return (ENOSPC);	/* ERESTART? */
	(*tp->t_oproc)(tp);
	return (0);
}
