# include "sendmail.h"
# include <sys/stat.h>
# include <sys/dir.h>
# include <signal.h>
# include <errno.h>

# ifndef QUEUE
SCCSID(@(#)queue.c	4.1		%G%	(no queueing));
# else QUEUE

SCCSID(@(#)queue.c	4.1		%G%);

/*
**  Work queue.
*/

struct work
{
	char		*w_name;	/* name of control file */
	long		w_pri;		/* priority of message, see below */
	struct work	*w_next;	/* next in queue */
};

typedef struct work	WORK;

WORK	*WorkQ;			/* queue of things to be done */
/*
**  QUEUEUP -- queue a message up for future transmission.
**
**	Parameters:
**		df -- location of the data file.  The name will
**			be transformed into a control file name.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The current request are saved in a control file.
*/

queueup(df)
	char *df;
{
	char *tf;
	char *qf;
	register FILE *f;
	register HDR *h;
	register ADDRESS *q;
	MAILER nullmailer;

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
	**	Priority should be first, since it is read by orderq.
	*/

	/* output message priority */
	fprintf(tfp, "P%ld\n", e->e_msgpriority);

	/* output creation time */
	fprintf(tfp, "T%ld\n", e->e_ctime);

	/* output name of data file */
	fprintf(f, "D%s\n", df);

	/* message from envelope, if it exists */
	if (e->e_message != NULL)
		fprintf(tfp, "M%s\n", e->e_message);

	/* output name of sender */
	fprintf(f, "S%s\n", CurEnv->e_from.q_paddr);

	/* output list of recipient addresses */
	for (q = CurEnv->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (queueall ? !bitset(QDONTSEND, q->q_flags) :
			       bitset(QQUEUEUP, q->q_flags))
		{
			fprintf(f, "R%s\n", q->q_paddr);
	}

	/*
	**  Output headers for this message.
	**	Expand macros completely here.  Queue run will deal with
	**	everything as absolute headers.
	**		All headers that must be relative to the recipient
	**		can be cracked later.
	**	We set up a "null mailer" -- i.e., a mailer that will have
	**	no effect on the addresses as they are output.
	*/

	bzero((char *) &nullmailer, sizeof nullmailer);
	nullmailer.m_r_rwset = nullmailer.m_s_rwset = -1;
	nullmailer.m_eol = "\n";

	define('g', "$f", e);
	for (h = CurEnv->e_header; h != NULL; h = h->h_link)
	{
		extern bool bitzerop();

		/* don't output null headers */
		if (h->h_value == NULL || h->h_value[0] == '\0')
			continue;

		/* don't output resent headers on non-resent messages */
		if (bitset(H_RESENT, h->h_flags) && !bitset(EF_RESENT, e->e_flags))
			continue;

		/* output this header */
		fprintf(f, "H");
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
			(void) waitfor(pid);
			if (QueueIntvl != 0)
				(void) setevent(QueueIntvl, runqueue, TRUE);
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
	**  Release any resources used by the daemon code.
	*/

# ifdef DAEMON
	clrdaemon();
# endif DAEMON

	/*
	**  Start making passes through the queue.
	**	First, read and sort the entire queue.
	**	Then, process the work in that order.
	**		But if you take too long, start over.
	*/

	/* order the existing work requests */
	(void) orderq();

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
**		The number of request in the queue (not necessarily
**		the number of requests in WorkQ however).
**
**	Side Effects:
**		Sets WorkQ to the queue of available work, in order.
*/

# define WLSIZE		120	/* max size of worklist per sort */

# ifndef DIR
# define DIR		FILE
# define direct		dir
# define opendir(d)	fopen(d, "r")
# define readdir(f)	((fread(&dbuf, sizeof dbuf, 1, f) > 0) ? &dbuf : 0)
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
	WORK wlist[WLSIZE+1];
	int wn = -1;
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
		return (0);
	}

	/*
	**  Read the work directory.
	*/

	while ((d = readdir(f)) != NULL)
	{
		FILE *cf;
		char lbuf[MAXNAME];

		/* is this an interesting entry? */
		if (d->d_ino == 0)
			continue;
# ifdef DEBUG
		if (tTd(40, 10))
			printf("orderq: %12s\n", d->d_name);
# endif DEBUG
		if (d->d_name[0] != 'q' || d->d_name[1] != 'f')
			continue;

		/* yes -- open control file (if not too many files) */
		if (++wn >= WLSIZE)
			continue;
		cf = fopen(d->d_name, "r");
		if (cf == NULL)
		{
			/* this may be some random person sending hir msgs */
			/* syserr("orderq: cannot open %s", cbuf); */
#ifdef DEBUG
			if (tTd(41, 2))
				printf("orderq: cannot open %s (%d)\n",
					d->d_name, errno);
#endif DEBUG
			errno = 0;
			wn--;
			continue;
		}
		wlist[wn].w_name = newstr(d->d_name);

		/* extract useful information */
		while (fgets(lbuf, sizeof lbuf, cf) != NULL)
		{
			if (lbuf[0] == 'P')
			{
				(void) sscanf(&lbuf[1], "%ld", &wlist[wn].w_pri);
				break;
			}
		}
		(void) fclose(cf);
	}
	(void) closedir(f);
	wn++;

	/*
	**  Sort the work directory.
	*/

	qsort(wlist, min(wn, WLSIZE), sizeof *wlist, workcmpf);

	/*
	**  Convert the work list into canonical form.
	**	Should be turning it into a list of envelopes here perhaps.
	*/

	wp = &WorkQ;
	for (i = min(wn, WLSIZE); --i >= 0; )
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

	return (wn);
}
/*
**  WORKCMPF -- compare function for ordering work.
**
**	Parameters:
**		a -- the first argument.
**		b -- the second argument.
**
**	Returns:
**		1 if a < b
**		0 if a == b
**		-1 if a > b
**
**	Side Effects:
**		none.
*/

workcmpf(a, b)
	register WORK *a;
	register WORK *b;
{
	if (a->w_pri == b->w_pri)
		return (0);
	else if (a->w_pri > b->w_pri)
		return (-1);
	else
		return (1);
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
		closexscript(CurEnv);
		CurEnv->e_flags &= ~EF_FATALERRS;
		QueueRun = TRUE;
		ErrorMode = EM_MAIL;
		CurEnv->e_id = &w->w_name[2];
# ifdef LOG
		if (LogLevel > 11)
			syslog(LOG_DEBUG, "%s: dowork, pid=%d", CurEnv->e_id,
			       getpid());
# endif LOG

		/* don't use the headers from sendmail.cf... */
		CurEnv->e_header = NULL;

		/* create the link to the control file during processing */
		initsys();
		readqf(w->w_name);
		if (!bitset(EF_FATALERRS, CurEnv->e_flags))
			sendall(CurEnv, SM_DELIVER);
		finis();
	}

	/*
	**  Parent -- pick up results.
	*/

	errno = 0;
	(void) waitfor(i);
}
/*
**  READQF -- read queue file and set up environment.
**
**	Parameters:
**		e -- the envelope of the job to run.
**		full -- if set, read in all information.  Otherwise just
**			read in info needed for a queue print.
**
**	Returns:
**		none.
**
**	Side Effects:
**		cf is read and created as the current job, as though
**		we had been invoked by argument.
*/

readqf(e, full)
	register ENVELOPE *e;
	bool full;
{
	register FILE *f;
	char buf[MAXFIELD];
	extern char *fgetfolded();
	register char *p;

	/*
	**  Open the file created by queueup.
	*/

	p = queuename(e, 'q');
	f = fopen(p, "r");
	if (f == NULL)
	{
		syserr("readqf: no control file %s", p);
		return;
	}
	FileName = p;
	LineNumber = 0;

	/*
	**  Read and process the file.
	*/

	if (Verbose && full)
		printf("\nRunning %s\n", e->e_id);
	while (fgetfolded(buf, sizeof buf, f) != NULL)
	{
		switch (buf[0])
		{
		  case 'R':		/* specify recipient */
			sendtolist(&buf[1], (ADDRESS *) NULL, &e->e_sendqueue);
			break;

		  case 'H':		/* header */
			if (full)
				(void) chompheader(&buf[1], FALSE);
			break;

		  case 'M':		/* message */
			e->e_message = newstr(&buf[1]);
			break;

		  case 'S':		/* sender */
			if (Verbose)
				message(Arpa_Info, "Sender: %s", &buf[1]);
			setsender(newstr(&buf[1]));
			break;

		  case 'D':		/* data file name */
			if (!full)
				break;
			e->e_df = newstr(&buf[1]);
			e->e_dfp = fopen(e->e_df, "r");
			if (e->e_dfp == NULL)
				syserr("readqf: cannot open %s", e->e_df);
			break;

		  case 'T':		/* init time */
			(void) sscanf(&buf[1], "%ld", &e->e_ctime);
			break;

		  case 'P':		/* message priority */
			(void) sscanf(&buf[1], "%ld", &e->e_msgpriority);

			/* make sure that big things get sent eventually */
			e->e_msgpriority -= WKTIMEFACT;
			break;

		  default:
			syserr("readqf(%s): bad line \"%s\"", e->e_id, buf);
			break;
		}
	}

	FileName = NULL;
}
/*
**  PRINTQUEUE -- print out a representation of the mail queue
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Prints a listing of the mail queue on the standard output.
*/

printqueue()
{
	register WORK *w;
	FILE *f;
	int nrequests;
	char buf[MAXLINE];

	/*
	**  Read and order the queue.
	*/

	nrequests = orderq();

	/*
	**  Print the work list that we have read.
	*/

	/* first see if there is anything */
	if (nrequests <= 0)
	{
		printf("Mail queue is empty\n");
		return;
	}

	printf("\t\tMail Queue (%d request%s", nrequests, nrequests == 1 ? "" : "s");
	if (nrequests > WLSIZE)
		printf(", only %d printed", WLSIZE);
	printf(")\n--QID-- --Size-- -----Q-Time----- ------------Sender/Recipient------------\n");
	for (w = WorkQ; w != NULL; w = w->w_next)
	{
		struct stat st;
		auto time_t submittime = 0;
		long dfsize = -1;
		char lf[20];
		char message[MAXLINE];

		printf("%7s", w->w_name + 2);
		strcpy(lf, w->w_name);
		lf[0] = 'l';
		if (stat(lf, &st) >= 0)
			printf("*");
		else
			printf(" ");
		errno = 0;
		f = fopen(w->w_name, "r");
		if (f == NULL)
		{
			printf(" (finished)\n");
			errno = 0;
			continue;
		}
		message[0] = '\0';
		while (fgets(buf, sizeof buf, f) != NULL)
		{
			fixcrlf(buf, TRUE);
			switch (buf[0])
			{
			  case 'M':	/* error message */
				strcpy(message, &buf[1]);
				break;

			  case 'S':	/* sender name */
				printf("%8ld %.16s %.45s", dfsize,
					ctime(&submittime), &buf[1]);
				if (message[0] != '\0')
					printf("\n\t\t\t\t  (%.43s)", message);
				break;

			  case 'R':	/* recipient name */
				printf("\n\t\t\t\t  %.45s", &buf[1]);
				break;

			  case 'T':	/* creation time */
				sscanf(&buf[1], "%ld", &submittime);
				break;

			  case 'D':	/* data file name */
				if (stat(&buf[1], &st) >= 0)
					dfsize = st.st_size;
				break;
			}
		}
		if (submittime == (time_t) 0)
			printf(" (no control file)");
		printf("\n");
		fclose(f);
	}
}

# endif QUEUE
