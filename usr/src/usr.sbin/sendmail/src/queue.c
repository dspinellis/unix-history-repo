# include "sendmail.h"
# include <sys/stat.h>
# include <sys/dir.h>
# include <errno.h>

static char	SccsId[] =	"@(#)queue.c	3.1	%G%";

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
	register int i;
	register HDR *h;
	register char *p;

	/* create control file name from data file name */
	strcpy(cf, df);
	p = rindex(cf, '/');
	if (p == NULL || *++p != 'd')
	{
		syserr("queueup: bad df name %s", df);
		return;
	}
	*p = 'c';

	/* create control file */
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

	/* output list of recipient addresses */
	for (i = 0; Mailer[i] != NULL; i++)
	{
		register ADDRESS *q;

		for (q = Mailer[i]->m_sendq; q != NULL; q = q->q_next)
		{
			if (!bitset(QQUEUEUP, q->q_flags))
				continue;
			fprintf(f, "R%s\n", q->q_paddr);
		}
	}

	/* output headers for this message */
	for (h = Header; h != NULL; h = h->h_link)
	{
		if (h->h_value == NULL || h->h_value[0] == '\0')
			continue;
		fprintf(f, "H");
		if (h->h_mflags != 0 && bitset(H_CHECK|H_ACHECK, h->h_flags))
			mfdecode(h->h_mflags, f);
		fprintf(f, "%s: ", h->h_field);
		if (bitset(H_DEFAULT, h->h_flags))
		{
			char buf[MAXLINE];

			(void) expand(h->h_value, buf, &buf[sizeof buf]);
			fprintf(f, "%s\n", buf);
		}
		else
			fprintf(f, "%s\n", h->h_value);
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

runqueue()
{
	/*
	**  Order the existing work requests.
	*/

	orderq();

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
	}
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
	struct stat st;
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

		/* extract useful information */
		wlist[wn].w_pri = PRI_NORMAL;
		while (fgets(lbuf, sizeof lbuf, cf) != NULL)
		{
			fixcrlf(lbuf, TRUE);

			switch (lbuf[0])
			{
			  case 'D':		/* data file name */
				if (stat(&lbuf[1], &st) < 0)
				{
					syserr("orderq: cannot stat %s", &lbuf[1]);
					(void) fclose(cf);
					(void) unlink(cbuf);
					wn--;
					continue;
				}
				wlist[wn].w_name = newstr(cbuf);
				wlist[wn].w_size = st.st_size;
				break;

			  case 'P':		/* message priority */
				wlist[wn].w_pri = atoi(&lbuf[1]);
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
		w->w_size = wlist[i].w_size;
		w->w_pri = wlist[i].w_pri;
		w->w_next = NULL;
		*wp = w;
		wp = &w->w_next;
	}

# ifdef DEBUG
	if (Debug)
	{
		for (w = WorkQ; w != NULL; w = w->w_next)
			printf("%32s: sz=%ld\n", w->w_name, w->w_size);
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
	WORK *a;
	WORK *b;
{
	register long aval;
	register long bval;

	aval = a->w_size - PRIFACT * a->w_pri;
	bval = b->w_size - PRIFACT * b->w_pri;

	if (aval == bval)
		return (0);
	else if (aval > bval)
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
		printf("dowork: %s size %ld pri %d\n", w->w_name,
		    w->w_size, w->w_pri);
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
		initsys();
		readqf(w->w_name);
		sendall(FALSE);
		if (!QueueUp)
			(void) unlink(w->w_name);
		else if (CurTime > TimeOut)
			timeout(w);
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

	while (fgets(buf, sizeof buf, f) != NULL)
	{
		fixcrlf(buf, TRUE);

		switch (buf[0])
		{
		  case 'R':		/* specify recipient */
			sendto(&buf[1], 1, (ADDRESS *) NULL);
			break;

		  case 'H':		/* header */
			(void) chompheader(&buf[1], FALSE);
			break;

		  case 'S':		/* sender */
			setsender(&buf[1]);
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
	printf("timeout(%s)\n", w->w_name);
}
