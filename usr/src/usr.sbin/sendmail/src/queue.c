# include "sendmail.h"
# include <sys/stat.h>
# include <ndir.h>
# include <signal.h>
# include <errno.h>

# ifndef QUEUE
SCCSID(@(#)queue.c	3.41		%G%	(no queueing));
# else QUEUE

SCCSID(@(#)queue.c	3.41		%G%);

/*
**  QUEUEUP -- queue a message up for future transmission.
**
**	The queued message should already be in the correct place.
**	This routine just outputs the control file as appropriate.
**
**	Parameters:
**		e -- the envelope to queue up.
**		queueall -- if TRUE, queue all addresses, rather than
**			just those with the QQUEUEUP flag set.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The current request (only unsatisfied addresses)
**			are saved in a control file.
*/

queueup(e, queueall)
	register ENVELOPE *e;
	bool queueall;
{
	char *tf;
	char *qf;
	char buf[MAXLINE];
	register FILE *tfp;
	register HDR *h;
	register ADDRESS *q;
	register int i;

	/*
	**  Create control file.
	*/

	tf = newstr(queuename(e, 't'));
	tfp = fopen(tf, "w");
	if (tfp == NULL)
	{
		syserr("queueup: cannot create temp file %s", tf);
		return;
	}
	(void) chmod(tf, 0600);

# ifdef DEBUG
	if (tTd(40, 1))
		printf("queueing in %s\n", tf);
# endif DEBUG

	/*
	**  If there is no data file yet, create one.
	*/

	if (e->e_df == NULL)
	{
		register FILE *dfp;

		e->e_df = newstr(queuename(e, 'd'));
		dfp = fopen(e->e_df, "w");
		if (dfp == NULL)
		{
			syserr("queueup: cannot create %s", e->e_df);
			(void) fclose(tfp);
			return;
		}
		(void) chmod(e->e_df, 0600);
		(*e->e_putbody)(dfp, Mailer[1], FALSE);
		(void) fclose(dfp);
	}

	/*
	**  Output future work requests.
	*/

	/* output name of data file */
	fprintf(tfp, "D%s\n", e->e_df);

	/* output name of sender */
	fprintf(tfp, "S%s\n", e->e_from.q_paddr);

	/* output creation time */
	fprintf(tfp, "T%ld\n", e->e_ctime);

	/* output message priority */
	fprintf(tfp, "P%ld\n", e->e_msgpriority);

	/* output message class */
	fprintf(tfp, "C%d\n", e->e_class);

	/* output macro definitions */
	/* I don't think this is needed any more.....
	for (i = 0; i < 128; i++)
	{
		register char *p = e->e_macro[i];

		if (p != NULL && i != (int) 'b')
			fprintf(tfp, "M%c%s\n", i, p);
	}
	.....  */

	/* output list of recipient addresses */
	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
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
			fprintf(tfp, "R%s\n", q->q_paddr);
	}

	/* output headers for this message */
	define('g', "$f");
	for (h = e->e_header; h != NULL; h = h->h_link)
	{
		if (h->h_value == NULL || h->h_value[0] == '\0')
			continue;
		fprintf(tfp, "H");
		if (h->h_mflags != 0 && bitset(H_CHECK|H_ACHECK, h->h_flags))
			mfdecode(h->h_mflags, tfp);
		fprintf(tfp, "%s: ", h->h_field);
		if (bitset(H_DEFAULT, h->h_flags))
		{
			(void) expand(h->h_value, buf, &buf[sizeof buf], e);
			fprintf(tfp, "%s\n", buf);
		}
		else
			fprintf(tfp, "%s\n", h->h_value);
	}

	/*
	**  Clean up.
	*/

	(void) fclose(tfp);
	qf = queuename(e, 'q');
	(void) unlink(qf);
	if (link(tf, qf) < 0)
		syserr("cannot link(%s, %s), df=%s", tf, qf, e->e_df);
	else
		(void) unlink(tf);
	e->e_qf = NULL;

# ifdef LOG
	/* save log info */
	if (LogLevel > 15)
		syslog(LOG_DEBUG, "%s: queueup, qf=%s, df=%s\n", e->e_id, qf, e->e_df);
# endif LOG

	/* disconnect this temp file from the job */
	e->e_df = NULL;
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
	f = opendir(QueueDir);
	if (f == NULL)
	{
		syserr("orderq: cannot open %s", QueueDir);
		return;
	}

	/*
	**  Read the work directory.
	*/

	while (wn < WLSIZE && (d = readdir(f)) != NULL)
	{
		char cbuf[MAXNAME];
		char lbuf[MAXNAME];
		FILE *cf;
		register char *p;

		/* is this an interesting entry? */
		if (d->d_name[0] != 'q' || d->d_name[1] != 'f')
			continue;

		/* yes -- find the control file location */
		(void) strcpy(cbuf, QueueDir);
		(void) strcat(cbuf, "/");
		p = &cbuf[strlen(cbuf)];
		(void) strcpy(p, d->d_name);

		/* open control file */
		cf = fopen(cbuf, "r");
		if (cf == NULL)
		{
			/* this may be some random person sending hir msgs */
			/* syserr("orderq: cannot open %s", cbuf); */
			errno = 0;
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
		char buf[MAXNAME];

		/*
		**  CHILD
		**	Change the name of the control file to avoid
		**		duplicate deliveries.   Then run the file
		**		as though we had just read it.
		**	We save an idea of the temporary name so we
		**		can recover on interrupt.
		*/

		/* set basic modes, etc. */
		(void) alarm(0);
		FatalErrors = FALSE;
		QueueRun = TRUE;
		MailBack = TRUE;
		CurEnv->e_qf = w->w_name;
		CurEnv->e_id = &w->w_name[strlen(QueueDir) + 3];
# ifdef LOG
		if (LogLevel > 11)
			syslog(LOG_DEBUG, "%s: dowork, pid=%d", CurEnv->e_id,
			       getpid());
# endif LOG

		/* don't use the headers from sendmail.cf... */
		CurEnv->e_header = NULL;
		chompheader("from: $q", TRUE);

		/* create the link to the control file during processing */
		if (link(w->w_name, queuename(CurEnv, 'l')) < 0)
		{
			/* being processed by another queuer */
# ifdef LOG
			if (LogLevel > 4)
				syslog(LOG_DEBUG, "%s: locked", CurEnv->e_id);
# endif LOG
			exit(EX_OK);
		}

		/* create ourselves a transcript file */
		openxscrpt();

		/* do basic system initialization */
		initsys();

		/* read the queue control file */
		readqf(CurEnv->e_qf);
		eatheader();

		/* do the delivery */
		if (!FatalErrors)
			sendall(CurEnv, FALSE);

		/* if still not sent, perhaps we should time out.... */
# ifdef DEBUG
		if (tTd(40, 3))
			printf("curtime=%ld, TimeOut=%ld\n", curtime(),
					     CurEnv->e_ctime + TimeOut);
# endif DEBUG
		if (CurEnv->e_queueup && curtime() > CurEnv->e_ctime + TimeOut)
			timeout(w);

		/* finish up and exit */
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
		printf("\nRunning %s\n", cf);
	while (fgetfolded(buf, sizeof buf, f) != NULL)
	{
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

		  case 'C':		/* message class */
			(void) sscanf(&buf[1], "%hd", &CurEnv->e_class);
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
	(void) sprintf(buf, "Cannot send mail for %s", pintvl(TimeOut, FALSE));
	(void) returntosender(buf, &CurEnv->e_from, TRUE);

	/* arrange to remove files from queue */
	CurEnv->e_dontqueue = TRUE;
}

# endif QUEUE
