# include "sendmail.h"
# include <sys/stat.h>
# include <dir.h>
# include <signal.h>
# include <errno.h>

# ifndef QUEUE
SCCSID(@(#)queue.c	3.48		%G%	(no queueing));
# else QUEUE

SCCSID(@(#)queue.c	3.48		%G%);

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
	char *tf;
	char *qf;
	register FILE *f;
	register HDR *h;
	register ADDRESS *q;
	register int i;

	/*
	**  Create control file.
	*/

	{
		syserr("queueup: cannot create temp file %s", tf);
		return;
	}
	(void) chmod(tf, FileMode);

# ifdef DEBUG
	if (tTd(40, 1))
		printf("queued in %s\n", cf);
# endif DEBUG

	/*
	**  Output future work requests.
	*/

	/* output name of data file */
	fprintf(f, "D%s\n", df);

	/* output name of sender */
	fprintf(f, "S%s\n", CurEnv->e_from.q_paddr);


	/* output message priority */
	fprintf(f, "P%ld\n", CurEnv->e_msgpriority);

	/* output macro definitions */
	/* I don't think this is needed any more.....
	for (i = 0; i < 128; i++)
	{
		extern char *Macro[128];
		register char *p = Macro[i];

		if (p != NULL && i != (int) 'b')
			fprintf(f, "M%c%s\n", i, p);
	}
	.....  */

	/* output list of recipient addresses */
	for (q = CurEnv->e_sendqueue; q != NULL; q = q->q_next)
	{
# ifdef DEBUG
		if (tTd(40, 1))
		{
			printf("queueing ");
			printaddr(q, FALSE);
		}
# endif DEBUG
		if (queueall ? !bitset(QDONTSEND, q->q_flags) :
			       bitset(QQUEUEUP, q->q_flags))
		{
			fprintf(f, "R%s\n", q->q_paddr);
	}

	/* output headers for this message */
	define('g', "$f");
	for (h = CurEnv->e_header; h != NULL; h = h->h_link)
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

runqueue(forkflag)
	bool forkflag;
{
	register int i;

	/*
	**  See if we want to go off and do other useful work.
	*/

	if (forkflag)
	{
		int pid;

		pid = dofork();
		if (pid != 0)
		{
			/* parent -- pick up intermediate zombie */
			do
			{
				auto int stat;

				i = wait(&stat);
			} while (i >= 0 && i != pid);
			if (QueueIntvl != 0)
				setevent(QueueIntvl, runqueue, TRUE);
			return;
		}
		/* child -- double fork */
		if (fork() != 0)
			exit(EX_OK);
	}
# ifdef LOG
	if (LogLevel > 11)
		syslog(LOG_DEBUG, "runqueue %s, pid=%d", QueueDir, getpid());
# endif LOG

	/*
	**  Start making passes through the queue.
	**	First, read and sort the entire queue.
	**	Then, process the work in that order.
	**		But if you take too long, start over.
	*/

	/* order the existing work requests */
	orderq();

	/* process them once at a time */
	while (WorkQ != NULL)
	{
		WORK *w = WorkQ;

		WorkQ = WorkQ->w_next;
		dowork(w);
		free(w->w_name);
		free((char *) w);
	}
	finis();
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

# ifndef DIR
# define DIR		FILE
# define direct		dir
# define opendir(d)	fopen(d, "r")
# define readdir(f)	(fread(dbuf, sizeof dbuf, 1, f) > 0 ? &dbuf : 0)
static struct dir	dbuf;
# define closedir(f)	fclose(f)
# endif DIR

orderq()
{
	register struct direct *d;
	register WORK *w;
	register WORK **wp;		/* parent of w */
	DIR *f;
	register int i;
	WORK wlist[WLSIZE];
	int wn = 0;
	extern workcmpf();

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
	f = opendir(".");
	if (f == NULL)
	{
		syserr("orderq: cannot open \"%s\" as \".\"", QueueDir);
		return;
	}

	/*
	**  Read the work directory.
	*/

	while (wn < WLSIZE && (d = readdir(f)) != NULL)
	{
		char lbuf[MAXNAME];
		FILE *cf;

		/* is this an interesting entry? */
		if (d->d_name[0] != 'q' || d->d_name[1] != 'f')
			continue;

		/* yes -- open control file */
		cf = fopen(d->d_name, "r");
		if (cf == NULL)
		{
			/* this may be some random person sending hir msgs */
			/* syserr("orderq: cannot open %s", cbuf); */
			errno = 0;
			continue;
		}
		wlist[wn].w_name = newstr(d->d_name);

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
	(void) closedir(f);

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
	if (tTd(40, 1))
	{
		for (w = WorkQ; w != NULL; w = w->w_next)
			printf("%32s: pri=%ld\n", w->w_name, w->w_pri);
	}
# endif DEBUG
}
/*
**  WORKCMPF -- compare function for ordering work.
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
	if (tTd(40, 1))
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

		/* set basic modes, etc. */
		(void) alarm(0);
		FatalErrors = FALSE;
		QueueRun = TRUE;
		MailBack = TRUE;
		CurEnv->e_qf = w->w_name;
		CurEnv->e_id = &w->w_name[2];
# ifdef LOG
		if (LogLevel > 11)
			syslog(LOG_DEBUG, "%s: dowork, pid=%d", CurEnv->e_id,
			       getpid());
# endif LOG

		/* don't use the headers from sendmail.cf... */
		CurEnv->e_header = NULL;
		chompheader("from: $q", TRUE);

		/* create the link to the control file during processing */
		openxscrpt();
		initsys();
		readqf(w->w_name);
		if (!FatalErrors)
			sendall(CurEnv, FALSE);
# ifdef DEBUG
		if (tTd(40, 3))
			printf("curtime=%ld, TimeOut=%ld\n", curtime(),
					     CurEnv->e_ctime + TimeOut);
# endif DEBUG
		if (CurEnv->e_queueup && curtime() > CurEnv->e_ctime + TimeOut)
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
	char buf[MAXFIELD];
	register char *p;
	register int i;

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
		printf("\nRunning %s\n", cf);
	while (fgetfolded(buf, sizeof buf, f) != NULL)
	{
		switch (buf[0])
		{
		  case 'R':		/* specify recipient */
			sendto(&buf[1], (ADDRESS *) NULL, &CurEnv->e_sendqueue);
			break;

		  case 'H':		/* header */
			(void) chompheader(&buf[1], FALSE);
			break;

		  case 'S':		/* sender */
			if (Verbose)
				message(Arpa_Info, "Sender: %s", &buf[1]);
			setsender(newstr(&buf[1]));
			break;

		  case 'D':		/* data file name */
			CurEnv->e_df = newstr(&buf[1]);
			TempFile = fopen(CurEnv->e_df, "r");
			if (TempFile == NULL)
				syserr("readqf: cannot open %s", CurEnv->e_df);
			break;

		  case 'T':		/* init time */
			(void) sscanf(&buf[1], "%ld", &CurEnv->e_ctime);
			break;

		  case 'P':		/* message priority */
			(void) sscanf(&buf[1], "%ld", &CurEnv->e_msgpriority);

			/* make sure that big things get sent eventually */
			CurEnv->e_msgpriority -= WKTIMEFACT;
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
	char buf[MAXLINE];
	extern char *pintvl();

# ifdef DEBUG
	if (tTd(40, 3))
		printf("timeout(%s)\n", w->w_name);
# endif DEBUG
	message(Arpa_Info, "Message has timed out");

	/* return message to sender */
	(void) returntosender("Cannot send mail for three days");

	/* arrange to remove files from queue */
	CurEnv->e_dontqueue = TRUE;
}

# endif QUEUE
