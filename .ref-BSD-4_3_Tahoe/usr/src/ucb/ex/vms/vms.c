#include "ex.h"
#include <ssdef.h>
#include <iodef.h>
#include <ttdef.h>
#include <descrip.h>

/*
 * These are hack routines that will only work with
 * ex/vi on vms.  If you use it for anything else,
 * that's your problem.
 */

static $DESCRIPTOR(inpdev, "TT");	/* Terminal to use for input */
static long termset[2] = { 0,0 };	/* No terminator */
static short iosb[4];

static short	ichan = -1;		/* Gets channel for inpdev */
static int	cbreak = 0;		/* Whether we are in cbreak mode */

/* ARGSUSED */
gtty(chan, ttybuf)
int		chan;
struct sgttyb	*ttybuf;
{
	int	errcode;
	long	term_char[2];

	if (ichan < 0)
		set_ichan();
	errcode = sys$qiow(1, ichan, IO$_SENSEMODE, iosb, NULL, 0,
		term_char, 0, 0, 0, 0, 0);
	if (errcode != SS$_NORMAL)
		return -1;
	switch (iosb[1]) {
	  case TT$C_BAUD_300:
		ttybuf->sg_ispeed = ttybuf->sg_ospeed = B300;
		break;
	  case TT$C_BAUD_1200:
		ttybuf->sg_ispeed = ttybuf->sg_ospeed = B1200;
		break;
	  case TT$C_BAUD_9600:
		/*
		 * Effectively the same as 2400
		 */
	  case TT$C_BAUD_2400:
		ttybuf->sg_ispeed = ttybuf->sg_ospeed = B2400;
		break;
	  default:
		lprintf("Defaulting to 2400 baud terminal\n");
		flush();
		ttybuf->sg_ispeed = ttybuf->sg_ospeed = B2400;
		break;
	}
	ttybuf->sg_erase = 0177;
	ttybuf->sg_kill = CTRL(U);
	ttybuf->sg_flags = ECHO | CRMOD;
	if (cbreak)
		ttybuf->sg_flags |= CBREAK;
	return 0;
}

stty(chan, ttybuf)
int		chan;
struct sgttyb	*ttybuf;
{
	if (chan != 1)
		return;
	cbreak = (ttybuf->sg_flags & CBREAK);
	return 0;
}

set_ichan()
{
	if (sys$assign(&inpdev, &ichan, 0, 0) != SS$_NORMAL) {
		syserror("TT");
		ex_exit(1);
	}
}

vms_read(fd, buf, cnt)
int	fd;
char	*buf;
int	cnt;
{
	int	errcode;

	if (fd != 0)
		return read(fd, buf, cnt);
	if (ichan < 0)
		set_ichan();
	errcode = sys$qiow(1, ichan,
		IO$_READLBLK | IO$M_NOECHO | IO$M_NOFILTR,
		NULL, NULL, 0, buf, 1, 0, &termset, NULL, 0);
	if (errcode == SS$_NORMAL)
		return 1;
	else
		return 0;
}

vms_write(fd, buf, cnt)
int	fd;
char	*buf;
int	cnt;
{
	int	errcode;

	if (fd != 1 || !cbreak)
		return write(fd, buf, cnt);
	if (ichan < 0)
		set_ichan();
	errcode = sys$qiow(1, ichan,
		IO$_WRITELBLK | IO$M_NOFORMAT,
		NULL, NULL, 0, buf, cnt, 0, 0, 0, 0);
	if (errcode == SS$_NORMAL)
		return cnt;
	else
		return 0;
}

/*
 * getlog - getenv() look-alike routine for VMS logical names
 *
 * Chris Carlson 1nov84
 */
char *
getlog(log)
	char *log;
{
	struct dsc$descriptor_s log_d = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
	static char trans[128];
	$DESCRIPTOR(trans_d, trans);
	short int translen;

	log_d.dsc$a_pointer = log;
	log_d.dsc$w_length = strlen(log);
	if (lib$sys_trnlog(&log_d, &translen, &trans_d, 0, 0, 0)
	== SS$_NORMAL) {
		do {
			log_d.dsc$a_pointer = trans;
			log_d.dsc$w_length = translen;
		} while (lib$sys_trnlog(&log_d, &translen, &trans_d, 0, 0, 0)
			 == SS$_NORMAL);
		trans[translen] = '\0';
		return(trans);
	}
	return((char *)0);
}

/*
 * unlink:
 *	Removes a file.  This is provided so there is a minimum
 *	of carnage in standard ex sources
 */
unlink(fname)
char	*fname;
{
	return delete(fname);
}

vms_exit(n)
int	n;
{
#undef	_exit
	if (n == 0)
		_exit(SS$_NORMAL);
	else
		_exit(SS$_ABORT);
}

bcopy(from, to, count)
char	*from, *to;
int	count;
{
	lib$movc3(&count, from, to);
}
