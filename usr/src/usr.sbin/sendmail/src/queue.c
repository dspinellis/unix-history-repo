# include "sendmail.h"
# include <sys/stat.h>
# include <sys/dir.h>
# include <signal.h>
# include <errno.h>

# ifndef QUEUE
SCCSID(@(#)queue.c	3.11		%G%	(no queueing));
# else QUEUE

SCCSID(@(#)queue.c	3.11		%G%);

/*
**  QUEUEUP -- queue a message up for future transmission.
**
**	The queued message should already be in the correct place.
**	This routine just outputs the control file as appropriate.
**
**	Parameters:
**		df -- location of the data file.  The name will
**			be transformed into a control file name.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The current request (only unsatisfied addresses)
**			are saved in a control file.
*/

queueup(df)
	char *df;
{
	char cf[MAXNAME];
	register FILE *f;
	register HDR *h;
	register ADDRESS *q;
	extern char *mktemp();
	register int i;

	/*
	**  Create control file.
	*/

	strcpy(cf, QueueDir);
	strcat(cf, "/cfXXXXXX");
	(void) mktemp(cf);
	f = fopen(cf, "w");
	if (f == NULL)
	{
		syserr("queueup: cannot create control file %s", cf);
		return;
	}

# ifdef DEBUG
	if (Debug)
		printf("queued in %s\n", cf);
# endif DEBUG

	/*
	**  Output future work requests.
	*/

	/* output name of data file */
	fprintf(f, "D%s\n", df);

	/* output name of sender */
	fprintf(f, "S%s\n", From.q_paddr);

	/* output timeout */
	fprintf(f, "T%ld\n", TimeOut);

	/* output message priority */
	fprintf(f, "P%ld\n", MsgPriority);

	/* output macro definitions */
	for (i = 0; i < 128; i++)
	{
		extern char *Macro[128];
		register char *p = Macro[i];

		if (p != NULL && i != (int) 'b')
			fprintf(f, "M%c%s\n", i, p);
	}

	/* output list of recipient addresses */
	for (q = SendQueue; q != NULL; q = q->q_next)
	{
# ifdef DEBUG
		if (Debug > 0)
		{
			printf("queueing ");
			printaddr(q, FALSE);
		}
# endif DEBUG
		if (bitset(QQUEUEUP, q->q_flags))
			fprintf(f, "R%s\n", q->q_paddr);
	}

	/* output headers for this message */
	for (h = Header; h != NULL; h = h->h_link)
	{
		if (h->h_value == NULL || h->h_value[0] == '\0')
			continue;
		fprintf(f, "H");
		if (h->h_mflags != 0 && bitset(H_CHECK|H_ACHECK, h->h_flags))
			mfdecode(h->h_mflags, f);
		fprintf(f, "%s: %s\n", h->h_field, h->h_value);
	}

	/*
	**  Clean up.
	*/

	(void) fclose(f);
}
/*
**  RUNQUEUE -- run the jobs in the queue.
**
**	Gets the stuff out of the queue in some presumably logical
**	order and processes them.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		runs things in the mail queue.
*/

bool	ReorderQueue;		/* if set, reorder the send queue */
int	QueuePid;		/* pid of child running queue */

runqueue(forkflag)
	bool forkflag;
{
	extern reordersig();

	if (QueueIntvl != 0)
	{
		(void) signal(SIGALRM, reordersig);
		(void) alarm((unsigned) QueueIntvl);
	}

	if (forkflag)
	{
		QueuePid = dofork();
		if (QueuePid > 0)
		{
			/* parent */
			return;
		}
		else
			(void) alarm((unsigned) 0);
	}

	for (;;)
	{
		/*
		**  Order the existing work requests.
		*/

		orderq();

		if (WorkQ == NULL)
		{
			/* no work?  well, maybe later */
			if (QueueIntvl == 0)
				break;
			pause();
			continue;
		}

		ReorderQueue = FALSE;

		/*
		**  Process them once at a time.
		**	The queue could be reordered while we do this to take
		**	new requests into account.  If so, the existing job
		**	will be finished but the next thing taken off WorkQ
		**	may be something else.
		*/

		while (WorkQ != NULL)
		{
			WORK *w = WorkQ;

			WorkQ = WorkQ->w_next;
			dowork(w);
			free(w->w_name);
			free((char *) w);
			if (ReorderQueue)
				break;
		}

		if (QueueIntvl == 0)
			break;
	}

	/* no work to do -- just exit */
	finis();
}
/*
**  REORDERSIG -- catch the alarm signal and tell sendmail to reorder queue.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sets the "reorder work queue" flag.
*/

reordersig()
{
	if (QueuePid == 0)
	{
		/* we are in a child doing queueing */
		ReorderQueue = TRUE;
	}
	else
	{
		/* we are in a parent -- poke child or start new one */
		if (kill(QueuePid, SIGALRM) < 0)
		{
			/* no child -- get zombie & start new one */
			static int st;

			(void) wait(&st);
			QueuePid = dofork();
			if (QueuePid == 0)
			{
				/* new child; run queue */
				runqueue(FALSE);
				finis();
			}
		}
	}

	/*
	**  Arrange to get this signal again.
	*/

	(void) signal(SIGALRM, reordersig);
	(void) alarm((unsigned) QueueIntvl);
}
/*
**  ORDERQ -- order the work queue.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets WorkQ to the queue of available work, in order.
*/

# define WLSIZE		120	/* max size of worklist per sort */

orderq()
{
	struct direct d;
	register WORK *w;
	register WORK **wp;		/* parent of w */
	register FILE *f;
	register int i;
	WORK wlist[WLSIZE];
	int wn = 0;
	extern workcmpf();
	extern char *QueueDir;

	/* clear out old WorkQ */
	for (w = WorkQ; w != NULL; )
	{
		register WORK *nw = w->w_next;

		WorkQ = nw;
		free(w->w_name);
		free((char *) w);
		w = nw;
	}

	/* open the queue directory */
	f = fopen(QueueDir, "r");
	if (f == NULL)
	{
		syserr("orderq: cannot open %s", QueueDir);
		return;
	}

	/*
	**  Read the work directory.
	*/

	while (wn < WLSIZE && fread((char *) &d, sizeof d, 1, f) == 1)
	{
		char cbuf[MAXNAME];
		char lbuf[MAXNAME];
		FILE *cf;
		register char *p;

		/* is this an interesting entry? */
		if (d.d_ino == 0 || d.d_name[0] != 'c')
			continue;

		/* yes -- find the control file location */
		strcpy(cbuf, QueueDir);
		strcat(cbuf, "/");
		p = &cbuf[strlen(cbuf)];
		strncpy(p, d.d_name, DIRSIZ);
		p[DIRSIZ] = '\0';

		/* open control file */
		cf = fopen(cbuf, "r");
		if (cf == NULL)
		{
			syserr("orderq: cannot open %s", cbuf);
			continue;
		}
		wlist[wn].w_name = newstr(cbuf);

		/* extract useful information */
		while (fgets(lbuf, sizeof lbuf, cf) != NULL)
		{
			fixcrlf(lbuf, TRUE);

			switch (lbuf[0])
			{
			  case 'P':		/* message priority */
				(void) sscanf(&lbuf[1], "%ld", &wlist[wn].w_pri);
				break;
			}
		}
		wn++;
		(void) fclose(cf);
	}
	(void) fclose(f);

	/*
	**  Sort the work directory.
	*/

	qsort(wlist, wn, sizeof *wlist, workcmpf);

	/*
	**  Convert the work list into canonical form.
	*/

	wp = &WorkQ;
	for (i = 0; i < wn; i++)
	{
		w = (WORK *) xalloc(sizeof *w);
		w->w_name = wlist[i].w_name;
		w->w_pri = wlist[i].w_pri;
		w->w_next = NULL;
		*wp = w;
		wp = &w->w_next;
	}

# ifdef DEBUG
	if (Debug)
	{
		for (w = WorkQ; w != NULL; w = w->w_next)
			printf("%32s: pri=%ld\n", w->w_name, w->w_pri);
	}
# endif DEBUG
}
/*
**	WORKCMPF -- compare function for ordering work.
**
**	Parameters:
**		a -- the first argument.
**		b -- the second argument.
**
**	Returns:
**		-1 if a < b
**		0 if a == b
**		1 if a > b
**
**	Side Effects:
**		none.
*/

# define PRIFACT	1800		/* bytes each priority point is worth */

workcmpf(a, b)
	register WORK *a;
	register WORK *b;
{
	if (a->w_pri == b->w_pri)
		return (0);
	else if (a->w_pri > b->w_pri)
		return (1);
	else
		return (-1);
}
/*
**  DOWORK -- do a work request.
**
**	Parameters:
**		w -- the work request to be satisfied.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The work request is satisfied if possible.
*/

dowork(w)
	register WORK *w;
{
	register int i;
	auto int xstat;

# ifdef DEBUG
	if (Debug)
		printf("dowork: %s pri %ld\n", w->w_name, w->w_pri);
# endif DEBUG

	/*
	**  Fork for work.
	*/

	i = fork();
	if (i < 0)
	{
		syserr("dowork: cannot fork");
		return;
	}

	if (i == 0)
	{
		/*
		**  CHILD
		*/

		QueueRun = TRUE;
		openxscrpt();
		initsys();
		readqf(w->w_name);
		sendall(FALSE);
# ifdef DEBUG
		if (Debug > 2)
			printf("CurTime=%ld, TimeOut=%ld\n", CurTime, TimeOut);
# endif DEBUG
		if (QueueUp && CurTime > TimeOut)
			timeout(w);
		(void) unlink(w->w_name);
		finis();
	}

	/*
	**  Parent -- pick up results.
	*/

	errno = 0;
	while ((i = wait(&xstat)) > 0 && errno != EINTR)
	{
		if (errno == EINTR)
		{
			errno = 0;
		}
	}
}
/*
**  READQF -- read queue file and set up environment.
**
**	Parameters:
**		cf -- name of queue control file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		cf is read and created as the current job, as though
**		we had been invoked by argument.
*/

readqf(cf)
	char *cf;
{
	register FILE *f;
	char buf[MAXLINE];
	extern ADDRESS *sendto();

	/*
	**  Open the file created by queueup.
	*/

	f = fopen(cf, "r");
	if (f == NULL)
	{
		syserr("readqf: no cf file %s", cf);
		return;
	}

	/*
	**  Read and process the file.
	*/

	if (Verbose)
		message(Arpa_Info, "Running %s (from %s)", cf, From.q_paddr);

	while (fgets(buf, sizeof buf, f) != NULL)
	{
		fixcrlf(buf, TRUE);

		switch (buf[0])
		{
		  case 'R':		/* specify recipient */
			(void) sendto(&buf[1], 1, (ADDRESS *) NULL, 0);
			break;

		  case 'H':		/* header */
			(void) chompheader(&buf[1], FALSE);
			break;

		  case 'S':		/* sender */
			setsender(newstr(&buf[1]));
			break;

		  case 'D':		/* data file name */
			InFileName = newstr(&buf[1]);
			TempFile = fopen(InFileName, "r");
			if (TempFile == NULL)
				syserr("readqf: cannot open %s", InFileName);
			break;

		  case 'T':		/* timeout */
			(void) sscanf(&buf[1], "%ld", &TimeOut);
			break;

		  case 'P':		/* message priority */
			(void) sscanf(&buf[1], "%ld", &MsgPriority);

			/* make sure that big things get sent eventually */
			MsgPriority -= WKTIMEFACT;
			break;

		  case 'M':		/* define macro */
			define(buf[1], newstr(&buf[2]));
			break;

		  default:
			syserr("readqf(%s): bad line \"%s\"", cf, buf);
			break;
		}
	}
}
/*
**  TIMEOUT -- process timeout on queue file.
**
**	Parameters:
**		w -- pointer to work request that timed out.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Returns a message to the sender saying that this
**		message has timed out.
*/

timeout(w)
	register WORK *w;
{
# ifdef DEBUG
	if (Debug > 0)
		printf("timeout(%s)\n", w->w_name);
# endif DEBUG

	/* return message to sender */
	(void) returntosender("Cannot send mail for three days");

	/* arrange to remove files from queue */
	QueueUp = FALSE;
}

# endif QUEUE
