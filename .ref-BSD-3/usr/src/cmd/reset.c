/*
 * reset - set the teletype mode bits to be sensible
 *
 * Kurt Shoens
 *
 * Very useful after crapping out in raw.
 * Modified by Mark Horton to know about tchars
 * and to not mess with peoples chars unless they are null.
 */
#include <sgtty.h>
#define chk(val, dft) (val==0 ? dft : val)

main()
{
	struct sgttyb buf;
	struct tchars tbuf;

	gtty(2, &buf);
	ioctl(2, TIOCGETC, &tbuf);
	buf.sg_flags &= ~(RAW|CBREAK|VTDELAY|ALLDELAY);
	buf.sg_flags |= XTABS|ECHO|CRMOD|ANYP;
	buf.sg_erase = chk(buf.sg_erase, '\08');	/* ^H */
	buf.sg_kill = chk(buf.sg_kill, '\30');		/* ^X */
	tbuf.t_intrc = chk(tbuf.t_intrc, '\177');	/* ^? */
	tbuf.t_quitc = chk(tbuf.t_quitc, '\34');	/* ^\ */
	tbuf.t_startc = chk(tbuf.t_startc, '\22');	/* ^Q */
	tbuf.t_stopc = chk(tbuf.t_stopc, '\24');	/* ^S */
	tbuf.t_eofc = chk(tbuf.t_eofc, '\4');		/* ^D */
	/* brkc is left alone */
	ioctl(2, TIOCSETN, &buf);
	ioctl(2, TIOCSETC, &tbuf);
}
