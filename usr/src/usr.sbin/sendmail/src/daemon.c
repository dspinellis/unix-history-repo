# include "sendmail.h"
# include <sys/mx.h>

static char	SccsId[] =	"@(#)daemon.c	3.2.1.1	%G%";

/*
**  DAEMON.C -- routines to use when running as a daemon.
*/

static FILE	*MailPort;	/* port that mail comes in on */
/*
**  GETREQUESTS -- open mail IPC port and get requests.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Waits until some interesting activity occurs.  When
**		it does, a child is created to process it, and the
**		parent waits for completion.  Return from this
**		routine is always in the child.
*/

getrequests()
{
	char *portname = "/dev/mailbox";

	/* initsys(); */

	/*
	**  Create "/dev/mailbox"
	*/

	if (Debug)
		portname = "mailbox";
	unlink(portname);
	MailPort = mpx(portname, 0222);
	if (MailPort < 0)
	{
		syserr("cannot create %s", portname);
		exit(EX_OSFILE);
	}
	chmod(portname, 0222);

	/*
	**  Wait for connection.
	*/

	for (;;)
	{
		i = read(MailPort, line, sizeof line);
		if (i < 0)
		{
			if (errno == EINTR)
				continue;
			syserr("mpx read");
			errct++;
			if (errct > 1000)
			{
				syserr("mpx read: too many errors");
				finis();
			}
			sleep(5);
			continue;
		}
		mpxcrack(line, i);
	}
}
/*
**  MPXCRACK -- parse & handle an input line.
**
**	Parameters:
**		rec -- the input record.
**		bc -- the byte count for rec.
**
**	Returns:
**		nothing
**
**	Side Effects:
**		rec is processed.
*/

# define skip(rec, n)	((struct rh *) (((char *) rec) + n))

mpxcrack(rec, bc)
	register struct rh *rec;
	int bc;
{
	struct rh *endrec;

	endrec = skip(rec, bc);

	while (rec < endrec)
	{
		if (rec->count == 0)
		{
			/* control record from mpx file */
			mpxcontrol(rec);
		}
		else
		{
			/* data record -- process message */
			syserr("data record!!");
		}
		rec->count += rec->ccount;
		if (rec->count & 01)
			rec->count++;
		rec = skip(rec, rec->count + sizeof *rec);
	}
}
/*
**  MPXCONTROL -- handle mpx control messages.
**
**	Parameters:
**		rec -- control message.
**
**	Returns:
**		none.
**
**	Side Effects:
**		as necessary for that control message.
*/

short NoIoctl[] = { M_IOANS };

mpxcontrol(rec)
	register struct rh *rec;
{
	register int cmd;
	register short val;
	register short *ap;
# ifdef MPXDEBUG
	char dbbuf[100];
# endif MPXDEBUG

# ifdef DEBUG
	if (Debug > 7)
		printf("%d byte control message\n", rec->ccount);
# endif DEBUG

	ap = (short *) (((char *) rec) + sizeof *rec);
	cmd = *ap++ & 0377;
	val = *ap++;
# ifdef MPXDEBUG
	logmsg(LOG_DEBUG, "mpxctl ch=%x cmd=%d val=%d", rec->index, cmd, val);
# endif MPXDEBUG

	switch (cmd)
	{
	  case M_WATCH:		/* attempted connect; value is uid */
# ifdef DEBUG
		if (Debug > 7)
			printf("WATCH, uid=%d\n", val);
# endif DEBUG
		attach(rec->index, MailPort);
		InChannel = extract(rec->index, MailPort);
		RealUid = val;
		detach(rec->index, MailPort);
		i = fork();
		if (i < 0)
		{
			syserr("daemon: cannot fork");
		}
		else if (i > 0)
		{
			/* parent -- wait for child */
			auto int st;

			(void) wait(&st);
		}
		else
		{
			/* child */
			smtp();
			syserr("smtp returns");
			exit(EX_SOFTWARE);
		}
		break;

	  case M_CLOSE:		/* close channel; value is unused */
# ifdef DEBUG
		if (Debug > 7)
			printf("CLOSE, val=%d\n", val);
# endif DEBUG
		detach(rec->index, MailPort);
		break;

	  case M_IOCTL:
# ifdef DEBUG
		if (Debug > 7)
			printf("IOCTL, val=%d\n", val);
# endif DEBUG
		wmpxctl(rec->index, NoIoctl, sizeof NoIoctl);
		break;

	  default:
		syserr("unknown mpx cmd %d, val=%d\n", cmd, val);
		break;
	}
}
/*
**  WMPXCTL -- write mpx control message
**
**	Parameters:
**		index -- index to write to.
**		buf -- place to write.
**		len -- length to write.
**
**	Returns:
**		none.
**
**	Side Effects:
**		writes to MailPort.
*/

wmpxctl(index, buf, cnt)
	int index;
	char *buf;
	int cnt;
{
	struct wh wbuf;

	wbuf.index = index;
	wbuf.count = 0;
	wbuf.ccount = cnt;
	wbuf.data = buf;
	write(MailPort, &wbuf, sizeof wbuf);
}
