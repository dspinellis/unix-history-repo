#ifndef lint
static char sccsid[] = "@(#)cmds.c	4.8 (Berkeley) 7/27/83";
#endif

/*
 * lpc -- line printer control program
 */

#include "lp.h"

/*
 * kill an existing daemon and disable printing.
 */
abort(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: abort {all | printer ...}\n");
		return;
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			abortpr();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		abortpr();
	}
}

abortpr()
{
	register FILE *fp;
	struct stat stbuf;
	int pid, fd;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	(void) sprintf(line, "%s/%s", SD, LO);
	printf("%s:\n", printer);

	/*
	 * Turn on the owner execute bit of the lock file to disable printing.
	 */
	if (stat(line, &stbuf) >= 0) {
		if (chmod(line, (stbuf.st_mode & 0777) | 0100) < 0)
			printf("\tcannot disable printing\n");
		else
			printf("\tprinting disabled\n");
	} else if (errno == ENOENT) {
		if ((fd = open(line, O_WRONLY|O_CREAT, 0760)) < 0)
			printf("\tcannot create lock file\n");
		else {
			(void) close(fd);
			printf("\tprinting disabled\n");
			printf("\tno daemon to abort\n");
		}
		return;
	} else {
		printf("\tcannot stat lock file\n");
		return;
	}
	/*
	 * Kill the current daemon to stop printing now.
	 */
	if ((fp = fopen(line, "r")) == NULL) {
		printf("\tcannot open lock file\n");
		return;
	}
	if (!getline(fp) || flock(fileno(fp), LOCK_SH|LOCK_NB) == 0) {
		(void) fclose(fp);	/* unlocks as well */
		printf("\tno daemon to abort\n");
		return;
	}
	(void) fclose(fp);
	if (kill(pid = atoi(line), SIGINT) < 0)
		printf("\tWarning: daemon (pid %d) not killed\n", pid);
	else
		printf("\tdaemon (pid %d) killed\n", pid);
}

/*
 * Remove all spool files and temporaries from the spooling area.
 */
clean(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: clean {all | printer ...}\n");
		return;
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			cleanpr();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		cleanpr();
	}
}

cleanpr()
{
	register int c;
	register DIR *dirp;
	register struct direct *dp;
	char *cp, *cp1;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	for (cp = line, cp1 = SD; *cp++ = *cp1++; );
	cp[-1] = '/';
	printf("%s:\n", printer);

	if ((dirp = opendir(SD)) == NULL) {
		printf("\tcannot examine spool directory\n");
		return;
	}
	while ((dp = readdir(dirp)) != NULL) {
		c = dp->d_name[0];
		if ((c == 'c' || c == 't' || c == 'd') && dp->d_name[1]=='f') {
			strcpy(cp, dp->d_name);
			if (unlink(line) < 0)
				printf("\tcannot remove %s\n", line);
			else
				printf("\tremoved %s\n", line);
		}
	}
	closedir(dirp);
}

/*
 * Enable queuing to the printer (allow lpr's).
 */
enable(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: enable {all | printer ...}\n");
		return;
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			enablepr();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		enablepr();
	}
}

enablepr()
{
	struct stat stbuf;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	(void) sprintf(line, "%s/%s", SD, LO);
	printf("%s:\n", printer);

	/*
	 * Turn off the group execute bit of the lock file to enable queuing.
	 */
	if (stat(line, &stbuf) >= 0) {
		if (chmod(line, stbuf.st_mode & 0767) < 0)
			printf("\tcannot enable queuing\n");
		else
			printf("\tqueuing enabled\n");
	}
}

/*
 * Disable queuing.
 */
disable(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: disable {all | printer ...}\n");
		return;
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			disablepr();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		disablepr();
	}
}

disablepr()
{
	register int fd;
	struct stat stbuf;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	(void) sprintf(line, "%s/%s", SD, LO);
	printf("%s:\n", printer);
	/*
	 * Turn on the group execute bit of the lock file to disable queuing.
	 */
	if (stat(line, &stbuf) >= 0) {
		if (chmod(line, (stbuf.st_mode & 0777) | 010) < 0)
			printf("\tcannot disable queuing\n");
		else
			printf("\tqueuing disabled\n");
	} else if (errno == ENOENT) {
		if ((fd = open(line, O_WRONLY|O_CREAT, 0670)) < 0)
			printf("\tcannot create lock file\n");
		else {
			(void) close(fd);
			printf("\tqueuing disabled\n");
		}
		return;
	} else
		printf("\tcannot stat lock file\n");
}

/*
 * Exit lpc
 */
quit(argc, argv)
	char *argv[];
{
	exit(0);
}

/*
 * Startup the daemon.
 */
restart(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: restart {all | printer ...}\n");
		return;
	}
	gethostname(host, sizeof(host));
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			startpr(0);
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		startpr(0);
	}
}

/*
 * Enable printing on the specified printer and startup the daemon.
 */
start(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: start {all | printer ...}\n");
		return;
	}
	gethostname(host, sizeof(host));
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			startpr(1);
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		startpr(1);
	}
}

startpr(enable)
{
	struct stat stbuf;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	(void) sprintf(line, "%s/%s", SD, LO);
	printf("%s:\n", printer);

	/*
	 * Turn off the owner execute bit of the lock file to enable printing.
	 */
	if (enable && stat(line, &stbuf) >= 0) {
		if (chmod(line, stbuf.st_mode & 0677) < 0)
			printf("\tcannot enable printing\n");
		else
			printf("\tprinting enabled\n");
	}
	if (!startdaemon(printer))
		printf("\tcouldn't start daemon\n");
	else
		printf("\tdaemon started\n");
}

/*
 * Print the status of each queue listed or all the queues.
 */
status(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			prstat();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		prstat();
	}
}

/*
 * Print the status of the printer queue.
 */
prstat()
{
	struct stat stbuf;
	register int fd, i;
	register struct direct *dp;
	DIR *dirp;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	if ((ST = pgetstr("st", &bp)) == NULL)
		ST = DEFSTAT;
	printf("%s:\n", printer);
	(void) sprintf(line, "%s/%s", SD, LO);
	if (stat(line, &stbuf) >= 0) {
		printf("\tqueuing is %s\n",
			(stbuf.st_mode & 010) ? "disabled" : "enabled");
		printf("\tprinting is %s\n",
			(stbuf.st_mode & 0100) ? "disabled" : "enabled");
	} else {
		printf("\tqueuing is enabled\n");
		printf("\tprinting is enabled\n");
	}
	if ((dirp = opendir(SD)) == NULL) {
		printf("\tcannot examine spool directory\n");
		return;
	}
	i = 0;
	while ((dp = readdir(dirp)) != NULL) {
		if (*dp->d_name == 'c' && dp->d_name[1] == 'f')
			i++;
	}
	closedir(dirp);
	if (i == 0)
		printf("\tno entries\n");
	else if (i == 1)
		printf("\t1 entry in spool area\n");
	else
		printf("\t%d entries in spool area\n", i);
	fd = open(line, O_RDONLY);
	if (fd < 0 || flock(fd, LOCK_SH|LOCK_NB) == 0) {
		(void) close(fd);	/* unlocks as well */
		printf("\tno daemon present\n");
		return;
	}
	(void) close(fd);
	putchar('\t');
	(void) sprintf(line, "%s/%s", SD, ST);
	fd = open(line, O_RDONLY);
	if (fd >= 0) {
		(void) flock(fd, LOCK_SH);
		while ((i = read(fd, line, sizeof(line))) > 0)
			(void) fwrite(line, 1, i, stdout);
		(void) close(fd);	/* unlocks as well */
	}
}

/*
 * Stop the specified daemon after completing the current job and disable
 * printing.
 */
stop(argc, argv)
	char *argv[];
{
	register int c, status;
	register char *cp1, *cp2;
	char prbuf[100];

	if (argc == 1) {
		printf("Usage: stop {all | printer ...}\n");
		return;
	}
	if (argc == 2 && !strcmp(argv[1], "all")) {
		printer = prbuf;
		while (getprent(line) > 0) {
			cp1 = prbuf;
			cp2 = line;
			while ((c = *cp2++) && c != '|' && c != ':')
				*cp1++ = c;
			*cp1 = '\0';
			stoppr();
		}
		return;
	}
	while (--argc) {
		printer = *++argv;
		if ((status = pgetent(line, printer)) < 0) {
			printf("cannot open printer description file\n");
			continue;
		} else if (status == 0) {
			printf("unknown printer %s\n", printer);
			continue;
		}
		stoppr();
	}
}

stoppr()
{
	register int fd;
	struct stat stbuf;

	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	(void) sprintf(line, "%s/%s", SD, LO);
	printf("%s:\n", printer);

	/*
	 * Turn on the owner execute bit of the lock file to disable printing.
	 */
	if (stat(line, &stbuf) >= 0) {
		if (chmod(line, (stbuf.st_mode & 0777) | 0100) < 0)
			printf("\tcannot disable printing\n");
		else
			printf("\tprinting disabled\n");
	} else if (errno == ENOENT) {
		if ((fd = open(line, O_WRONLY|O_CREAT, 0760)) < 0)
			printf("\tcannot create lock file\n");
		else {
			(void) close(fd);
			printf("\tprinting disabled\n");
		}
	} else
		printf("\tcannot stat lock file\n");
}

/*
 * Put the specified jobs at the top of printer queue.
 */
topq(argc, argv)
	char *argv[];
{
	register int status, nitems, n;
	struct stat stbuf;
	register char *cfname;
	struct queue **queue;
	int changed = 0;

	if (argc == 1) {
		printf("Usage: topq printer [jobnum ...] [user ...]\n");
		return;
	}

	--argc;
	printer = *++argv;
	status = pgetent(line, printer);
	if (status < 0) {
		printf("cannot open printer description file\n");
		return;
	} else if (status == 0) {
		printf("%s: unknown printer\n", printer);
		return;
	}
	bp = pbuf;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	printf("%s:\n", printer);

	if (chdir(SD) < 0) {
		printf("\tcannot chdir to %s\n", SD);
		return;
	}
	nitems = getq(&queue);
	while (--argc) {
		if ((n = inqueue(*++argv, queue, nitems)) < 0) {
			printf("\tjob %s is not in the queue\n", *argv);
			continue;
		}
		/*
		 * Reposition the job by changing the modification time of
		 * the control file.
		 */
		if (touch(queue[n]->q_name)) {
			free(queue[n]);
			queue[n] = NULL;
			changed++;
		}
	}
	/*
	 * Put the remaining jobs at the end of the queue.
	 */
	for (n = 0; n < nitems; n++) {
		if (queue[n] == NULL)
			continue;
		cfname = queue[n]->q_name;
		if (changed)
			touch(cfname);
		free(cfname);
	}
	free(queue);
	printf("\tqueue order %s\n", changed ? "changed" : "unchanged");
	/*
	 * Turn on the public execute bit of the lock file to
	 * get lpd to rebuild the queue after the current job.
	 */
	if (changed && stat(LO, &stbuf) >= 0)
		(void) chmod(LO, (stbuf.st_mode & 0777) | 01);
} 

/* 
 * Change the modification time of the file.
 *	Returns boolean if successful.  
 */
touch(cfname)
	char *cfname;
{
	register int fd;

	fd = open(cfname, O_RDWR);
	if (fd < 0) {
		printf("\tcannot open %s\n", cfname);
		return(0); 
	}
	(void) read(fd, line, 1);
	(void) lseek(fd, 0L, 0); 	/* set pointer back to top of file */
	(void) write(fd, line, 1);
	(void) close(fd);
	sleep(1);			/* so times will be different */
	return(1);
}

/*
 * Checks if specified job name is in the printer's queue.
 * Returns:  negative (-1) if argument name is not in the queue.
 *     0 to n:  array index of pointer to argument name.
 */
inqueue(job, queue, nitems)
	char *job;
	struct queue *queue[];
	int nitems;
{
	register struct queue *q;
	register int n, jobnum;
	register char *cp;
	FILE *fp;

	jobnum = -1;
	if (isdigit(*job)) {
		jobnum = 0;
		do
			jobnum = jobnum * 10 + (*job++ - '0');
		while (isdigit(*job));
	}

	while (--nitems >= 0) {
		if ((q = queue[nitems]) == NULL)
			continue;
		/* this needs to be fixed since the same number can be used
		   by different machines (i.e. jobnum & machine) */
		if (jobnum >= 0) {
			n = 0;
			for (cp = q->q_name+3; isdigit(*cp); )
				n = n * 10 + (*cp++ - '0');
			if (jobnum == n)
				return(nitems);
			continue;
		}
		/*
		 * Read cf file for owner's name
		 */
		if ((fp = fopen(q->q_name, "r")) == NULL)
			continue;
		while (getline(fp) > 0) {
			if (line[0] == 'P' && !strcmp(job, line+1)) {
				(void) fclose(fp);
				return(nitems);
			}
		}
		(void) fclose(fp);
	}
	return(-1);
}
