/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)slc.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "telnetd.h"

#ifdef	LINEMODE
/*
 * local varibles
 */
static unsigned char	*def_slcbuf = (unsigned char *)0;
static int		def_slclen = 0;
static int		slcchange;	/* change to slc is requested */
static unsigned char	*slcptr;	/* pointer into slc buffer */
static unsigned char	slcbuf[NSLC*6];	/* buffer for slc negotiation */

/*
 * send_slc
 *
 * Write out the current special characters to the client.
 */
send_slc()
{
	register int i;

	/*
	 * Send out list of triplets of special characters
	 * to client.  We only send info on the characters
	 * that are currently supported.
	 */
	for (i = 1; i <= NSLC; i++) {
		if ((slctab[i].defset.flag & SLC_LEVELBITS) == SLC_NOSUPPORT)
			continue;
		add_slc((unsigned char)i, slctab[i].current.flag,
							slctab[i].current.val);
	}

}  /* end of send_slc */

/*
 * default_slc
 *
 * Set pty special characters to all the defaults.
 */
default_slc()
{
	register int i;

	for (i = 1; i <= NSLC; i++) {
		slctab[i].current.val = slctab[i].defset.val;
		if (slctab[i].current.val == (cc_t)(_POSIX_VDISABLE))
			slctab[i].current.flag = SLC_NOSUPPORT;
		else
			slctab[i].current.flag = slctab[i].defset.flag;
		if (slctab[i].sptr) {
			*(slctab[i].sptr) = slctab[i].defset.val;
		}
	}
	slcchange = 1;

}  /* end of default_slc */
#endif	LINEMODE

/*
 * get_slc_defaults
 *
 * Initialize the slc mapping table.
 */
get_slc_defaults()
{
	register int i;

	init_termbuf();

	for (i = 1; i <= NSLC; i++) {
		slctab[i].defset.flag = 
			spcset(i, &slctab[i].defset.val, &slctab[i].sptr);
		slctab[i].current.flag = SLC_NOSUPPORT; 
		slctab[i].current.val = 0; 
	}

}  /* end of get_slc_defaults */

#ifdef	LINEMODE
/*
 * add_slc
 *
 * Add an slc triplet to the slc buffer.
 */
add_slc(func, flag, val)
	register unsigned char func, flag;
	cc_t val;
{

	if ((*slcptr++ = func) == 0xff)
		*slcptr++ = 0xff;

	if ((*slcptr++ = flag) == 0xff)
		*slcptr++ = 0xff;

	if ((*slcptr++ = (unsigned char)val) == 0xff)
		*slcptr++ = 0xff;

}  /* end of add_slc */

/*
 * start_slc
 *
 * Get ready to process incoming slc's and respond to them.
 *
 * The parameter getit is non-zero if it is necessary to grab a copy
 * of the terminal control structures.
 */
start_slc(getit)
	register int getit;
{

	slcchange = 0;
	if (getit)
		init_termbuf();
	(void) sprintf(slcbuf, "%c%c%c%c", IAC, SB, TELOPT_LINEMODE, LM_SLC);
	slcptr = slcbuf + 4;

}  /* end of start_slc */

/*
 * end_slc
 *
 * Finish up the slc negotiation.  If something to send, then send it.
 */
end_slc(bufp)
register unsigned char **bufp;
{
	register int len;
	void netflush();

	/*
	 * If a change has occured, store the new terminal control
	 * structures back to the terminal driver.
	 */
	if (slcchange) {
		set_termbuf();
	}

	/*
	 * If the pty state has not yet been fully processed and there is a
	 * deferred slc request from the client, then do not send any
	 * sort of slc negotiation now.  We will respond to the client's
	 * request very soon.
	 */
	if (def_slcbuf && (terminit() == 0)) {
		return;
	}

	if (slcptr > (slcbuf + 4)) {
		if (bufp) {
			*bufp = &slcbuf[4];
			return(slcptr - slcbuf - 4);
		} else {
			(void) sprintf(slcptr, "%c%c", IAC, SE);
			slcptr += 2;
			len = slcptr - slcbuf;
			writenet(slcbuf, len);
			netflush();  /* force it out immediately */
		}
	}

}  /* end of end_slc */

/*
 * process_slc
 *
 * Figure out what to do about the client's slc
 */
process_slc(func, flag, val)
	register unsigned char func, flag;
	cc_t val;
{
	register int hislevel, mylevel, ack;

	/*
	 * Ensure that we know something about this function
	 */
	if (func > NSLC) {
		add_slc(func, SLC_NOSUPPORT, 0);
		return;
	}

	/*
	 * Process the special case requests of 0 SLC_DEFAULT 0
	 * and 0 SLC_VARIABLE 0.  Be a little forgiving here, don't
	 * worry about whether the value is actually 0 or not.
	 */
	if (func == 0) {
		if ((flag = flag & SLC_LEVELBITS) == SLC_DEFAULT) {
			default_slc();
			send_slc(1);
		} else if (flag == SLC_VARIABLE) {
			send_slc(0);
		}
		return;
	}

	/*
	 * Appears to be a function that we know something about.  So
	 * get on with it and see what we know.
	 */

	hislevel = flag & SLC_LEVELBITS;
	mylevel = slctab[func].current.flag & SLC_LEVELBITS;
	ack = flag & SLC_ACK;
	/*
	 * ignore the command if:
	 * the function value and level are the same as what we already have;
	 * or the level is the same and the ack bit is set
	 */
	if (hislevel == mylevel && (val == slctab[func].current.val || ack)) {
		return;
	} else if (ack) {
		/*
		 * If we get here, we got an ack, but the levels don't match.
		 * This shouldn't happen.  If it does, it is probably because
		 * we have sent two requests to set a variable without getting
		 * a response between them, and this is the first response.
		 * So, ignore it, and wait for the next response.
		 */
		return;
	} else {
		change_slc(func, flag, val);
	}

}  /* end of process_slc */

/*
 * change_slc
 *
 * Process a request to change one of our special characters.
 * Compare client's request with what we are capable of supporting.
 */
change_slc(func, flag, val)
	register unsigned char func, flag;
	cc_t val;
{
	register int hislevel, mylevel;
	
	hislevel = flag & SLC_LEVELBITS;
	mylevel = slctab[func].defset.flag & SLC_LEVELBITS;
	/*
	 * If client is setting a function to NOSUPPORT
	 * or DEFAULT, then we can easily and directly
	 * accomodate the request.
	 */
	if (hislevel == SLC_NOSUPPORT) {
		slctab[func].current.flag = flag;
		slctab[func].current.val = (cc_t)_POSIX_VDISABLE;
		flag |= SLC_ACK;
		add_slc(func, flag, val);
		return;
	}
	if (hislevel == SLC_DEFAULT) {
		/*
		 * Special case here.  If client tells us to use
		 * the default on a function we don't support, then
		 * return NOSUPPORT instead of what we may have as a
		 * default level of DEFAULT.
		 */
		if (mylevel == SLC_DEFAULT) {
			slctab[func].current.flag = SLC_NOSUPPORT;
		} else {
			slctab[func].current.flag = slctab[func].defset.flag;
		}
		slctab[func].current.val = slctab[func].defset.val;
		add_slc(func, slctab[func].current.flag,
						slctab[func].current.val);
		return;
	}

	/*
	 * Client wants us to change to a new value or he
	 * is telling us that he can't change to our value.
	 * Some of the slc's we support and can change,
	 * some we do support but can't change,
	 * and others we don't support at all.
	 * If we can change it then we have a pointer to
	 * the place to put the new value, so change it,
	 * otherwise, continue the negotiation.
	 */
	if (slctab[func].sptr) {
		/*
		 * We can change this one.
		 */
		slctab[func].current.val = val;
		*(slctab[func].sptr) = val;
		slctab[func].current.flag = flag;
		flag |= SLC_ACK;
		slcchange = 1;
		add_slc(func, flag, val);
	} else {
		/*
		* It is not possible for us to support this
		* request as he asks.
		*
		* If our level is DEFAULT, then just ack whatever was
		* sent. 
		*
		* If he can't change and we can't change,
		* then degenerate to NOSUPPORT.
		*
		* Otherwise we send our level back to him, (CANTCHANGE
		* or NOSUPPORT) and if CANTCHANGE, send
		* our value as well.
		*/
		if (mylevel == SLC_DEFAULT) {
			slctab[func].current.flag = flag;
			slctab[func].current.val = val;
			flag |= SLC_ACK;
		} else if (hislevel == SLC_CANTCHANGE &&
				    mylevel == SLC_CANTCHANGE) {
			flag &= ~SLC_LEVELBITS;
			flag |= SLC_NOSUPPORT;
			slctab[func].current.flag = flag;
		} else {
			flag &= ~SLC_LEVELBITS;
			flag |= mylevel;
			slctab[func].current.flag = flag;
			if (mylevel == SLC_CANTCHANGE) {
				slctab[func].current.val =
					slctab[func].defset.val;
				val = slctab[func].current.val;
			}
			
		}
		add_slc(func, flag, val);
	}

}  /* end of change_slc */

#if	defined(USE_TERMIO) && (VEOF == VMIN)
cc_t oldeofc = '\004';
#endif

/*
 * check_slc
 *
 * Check the special characters in use and notify the client if any have
 * changed.  Only those characters that are capable of being changed are
 * likely to have changed.  If a local change occurs, kick the support level
 * and flags up to the defaults.
 */
check_slc()
{
	register int i;

	for (i = 1; i <= NSLC; i++) {
#if	defined(USE_TERMIO) && (VEOF == VMIN)
		/*
		 * In a perfect world this would be a neat little
		 * function.  But in this world, we should not notify
		 * client of changes to the VEOF char when
		 * ICANON is off, because it is not representing
		 * a special character.
		 */
		if (i == SLC_EOF) {
			if (!tty_isediting())
				continue;
			else if (slctab[i].sptr)
				oldeofc = *(slctab[i].sptr);
		}
#endif	/* defined(USE_TERMIO) && defined(SYSV_TERMIO) */
		if (slctab[i].sptr &&
				(*(slctab[i].sptr) != slctab[i].current.val)) {
			slctab[i].current.val = *(slctab[i].sptr);
			if (*(slctab[i].sptr) == (cc_t)_POSIX_VDISABLE)
				slctab[i].current.flag = SLC_NOSUPPORT;
			else
				slctab[i].current.flag = slctab[i].defset.flag;
			add_slc((unsigned char)i, slctab[i].current.flag,
						slctab[i].current.val);
		}
	}
			
}  /* check_slc */

/*
 * do_opt_slc
 *
 * Process an slc option buffer.  Defer processing of incoming slc's
 * until after the terminal state has been processed.  Save the first slc
 * request that comes along, but discard all others.
 *
 * ptr points to the beginning of the buffer, len is the length.
 */
do_opt_slc(ptr, len)
register char *ptr;
register int len;
{
	register unsigned char func, flag;
	cc_t val;
	register char *end = (char *)(ptr + len);
	char *malloc();

	if (terminit()) {  /* go ahead */
		while (ptr < end) {
			func = *ptr++;
			if (ptr >= end) break;
			flag = *ptr++;
			if (ptr >= end) break;
			val = (cc_t)*ptr++;

			process_slc(func, flag, val);

		}
	} else {
		/*
		 * save this slc buffer if it is the first, otherwise dump
		 * it.
		 */
		if (def_slcbuf == (unsigned char *)0) {
			def_slclen = len;
			def_slcbuf = (unsigned char *)malloc((unsigned)len);
			if (def_slcbuf == (unsigned char *)0)
				return;  /* too bad */
			bcopy(ptr, def_slcbuf, len);
		}
	}

}  /* end of do_opt_slc */

/*
 * deferslc
 *
 * Do slc stuff that was deferred.
 */
deferslc()
{
	if (def_slcbuf) {
		start_slc(1);
		do_opt_slc(def_slcbuf, def_slclen);
		end_slc(0);
		free(def_slcbuf);
		def_slcbuf = (unsigned char *)0;
		def_slclen = 0;
	}

}  /* end of deferslc */

#endif	/* LINEMODE */
