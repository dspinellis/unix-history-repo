
#ifndef	lint
char sccsid[] = "@(#)configttys.c	4.4 (Berkeley) 6/19/83";
#endif

/*
 * configttys - configure "tty" ports
 *
 * David L. Wasley
 * U.C.Berkeley
 */

#include <stdio.h>
#include <getty.h>
#include <signal.h>

#define	exists(file)	(access(file, 0) == 0)

char	*etc_ttys	= "/etc/ttys";		/* active port speed table */
char	*etc_ttytype	= "/etc/ttytype";	/* terminal type table */
char	*etc_conf	= "/etc/ttyconf";	/* master config file */
char	*lockfile	= "/etc/ttyconf.lock";	/* interlock file */

struct ttys {
	char	ty_active;	/* active login port */
	char	ty_speed;	/* speed table character */
	char	ty_port[32];	/* port name */
} ttys[256];

struct ttytype {
	char	tp_term[64];	/* terminal type name */
	char	tp_port[32];	/* port name */
} ttytype[256];

char	conformat[]	= "%s\t%s\t%s\t%s\n";

int	error	= 0;
int	renamed = 0;
int	debug	= 0;			/* debug mode */
int	backup	= 0;			/* create backup copies of old data */
int	interactive = 1;		/* interactive mode */

char	*speedname();			/* port speed code name */
char	speedchar();			/* getty table name */
char	*termname();			/* name of terminal on this port */
char	*rindex();
struct ttytype	*type();		/* find ttytype for port */
FILE	*fopen();

main (argc, argv)
	int argc;
	char **argv;
{
	int		lineno;
	int		child;
	int		status;
	char		c;
	struct ttys	*ty;
	struct ttytype	*tp;
	char		port[32];
	char		active[16];
	char		speed[32];
	char		term[64];
	FILE		*tyf, *tpf, *conf;
	char		buf[1024];
	char		ans[32];

	while (--argc > 0)
	{
		if (**++argv == '-') switch (*++*argv)
		{
			case 'd':
				debug = 1;
				break;

			case 'n':		/* non-interactive */
				interactive = 0;
				break;

			case 'b':		/* backup old databases */
				backup = 1;
				break;

			default:
				fprintf(stderr, "unknown option %c\n", **argv);
				exit(1);
		}
	}

	if (debug)
	{
		etc_ttys = rindex(etc_ttys, '/') + 1;
		etc_ttytype = rindex(etc_ttytype, '/') + 1;
		etc_conf = rindex(etc_conf, '/') + 1;
		lockfile = rindex(lockfile, '/') + 1;
	}

	/*
	 * create backup copies of the databases?
	 */
	if (backup)
	{
		if (exists(etc_ttys))
		{
			sprintf(buf, "/bin/cp %s %s.bak", etc_ttys, etc_ttys);
			system(buf);
		}
		if (exists(etc_ttys))
		{
			sprintf(buf, "/bin/cp %s %s.bak", etc_ttytype, etc_ttytype);
			system(buf);
		}
		if (exists(etc_conf))
		{
			sprintf(buf, "/bin/cp %s %s.bak", etc_conf, etc_conf);
			system(buf);
		}
	}

	/*
	 * create interlock file
	 */
	getlockfile(lockfile);

	/*
	 * always read ttys file for comparison
	 * It is afterall what really counts!
	 */
	if (readttys() != 0)
		quit(1);

	/*
	 * read old ttytypes if necessary
	 */
	if (! exists(etc_conf))
	{
		/*
		 * open old ttytype file
		 */
		if ((tpf = fopen(etc_ttytype, "r")) == NULL)
		{
			perror(etc_ttytype);
			quit(1);
		}

		/*
		 * read ttytype file
		 */
		lineno = 0;
		tp = ttytype;
		while (fgets(buf, sizeof buf, tpf))
		{
			lineno++;
			if (sscanf(buf, "%s %s", tp->tp_term, tp->tp_port) == 2)
				tp++;
			else
			{
				error++;
				fprintf(stderr, "bad line %d in %s: %s",
					lineno, etc_ttytype, buf);
			}
		}
		fclose(tpf);
		tp->tp_term[0] = '\0';

		if (error > 0)
			quit(1);

		/*
		 * create master config file
		 */
		if ((conf = fopen(etc_conf, "w")) == NULL)
		{
			perror(etc_conf);
			quit(1);
		}

		fprintf(conf, conformat, "port", "login", "speed\t", "terminal type");
		fprintf(conf, conformat, "----", "-----", "-----\t", "-------------");
		for (ty = ttys; ty->ty_active; ty++)
		{
			fprintf(conf, conformat, ty->ty_port,
				ty->ty_active == '1'? "active":"-",
				speedname(ty->ty_speed),
				termname(ty->ty_port));
		}
		fclose(conf);
	}

	/*
	 * open master config file
	 */
	if ((conf = fopen(etc_conf, "r")) == NULL)
	{
		perror(etc_conf);
		quit(1);
	}

	if (interactive)
		edit();

	/*
	 * read conf file
	 */
re_read:
	rewind(conf);
	ty = ttys;
	renamed = 0;
	error = 0;
	lineno = 0;

	while (fgets(buf, sizeof buf, conf))	/* skip heading */
	{
		lineno++;
		if (buf[0] == '-')
			break;
	}

	while (fgets(buf, sizeof buf, conf))
	{
		lineno++;
		if (sscanf(buf, "%s %s %s %s", port, active, speed, term) < 4)
		{
			fprintf(stderr, "line %d: field(s) missing: %s",
				lineno, buf);
			error++;
			break;
		}

		if (strcmp(port, ty->ty_port) != 0)
		{
			if (! ty->ty_active || renamed)
				strcpy(ty->ty_port, port);
			else
			{
				fprintf(stderr, "line %d: port name changed! %s -> %s\n",
					lineno, ty->ty_port, port);
				fprintf(stderr, "Are you sure this is OK? ");
				gets(ans);
				if (ans[0] != 'y')
				{
					edit();
					goto re_read;
				}
				renamed++;
				strcpy(ty->ty_port, port);
			}
		}

		if (strcmp(active, "active") == 0)
			ty->ty_active = '1';
		else
			ty->ty_active = '0';

		if (c = speedchar(speed))
			ty->ty_speed = c;
		else
		{
			fprintf(stderr, "line %d: speed name not known: %s\n",
				lineno, speed);
			error++;
		}
		
		if (tp = type(port))
			strcpy(tp->tp_term, term);
		/* else ?? */

		ty++;
	}

	if (ty == ttys)
	{
		fprintf(stderr, "%s empty??\n", etc_conf);
		error++;
	}

	if (error)
	{
		if (interactive)
		{
			fprintf(stderr, "re-edit? ");
			gets(ans);
			if (ans[0] == 'y')
			{
				edit();
				goto re_read;
			}
		}
		fprintf(stderr, "Files not modified.\n");
		quit(1);
	}

	writettys();
	quit(0);
}

/*
 * read ttys file
 */
readttys()
{
	FILE			*tyf;
	register struct ttys	*ty;
	char			buf[1024];
	int			lineno;
	int			error	= 0;

	if ((tyf = fopen(etc_ttys, "r")) == NULL)
	{
		if (exists(etc_conf))
			return (0);	/* hope user has it together! */
		perror(etc_ttys);
		quit(1);
	}

	lineno = 0;
	ty = ttys;
	while (fgets(buf, sizeof buf, tyf))
	{
		lineno++;
		if (sscanf(buf, "%c%c%s",
			&ty->ty_active, &ty->ty_speed, ty->ty_port) == 3)
			ty++;
		else
		{
			error++;
			fprintf(stderr, "bad line %d in %s: %s",
				lineno, etc_ttys, buf);
		}
	}
	fclose(tyf);
	ty->ty_active = '\0';
	return(error);
}

writettys()
{
	int	rtn = 0;
	char	temp[1024];
	FILE	*tyf, *tpf;
	register struct ttys	*ty;

	sprintf(temp, "%s.tmp", etc_ttys);
	if ((tyf = fopen(temp, "w")) == NULL)
	{
		perror(temp);
		quit(1);
	}

	for (ty = ttys; ty->ty_active; ty++)
		fprintf(tyf, "%c%c%s\n",
			ty->ty_active, ty->ty_speed, ty->ty_port);
	fclose(tyf);

	if (rename(temp, etc_ttys) != 0)
	{
		fprintf(stderr, "Can't rename %s\n", temp);
		rtn = 1;
	}

	sprintf(temp, "%s.tmp", etc_ttytype);
	if ((tpf = fopen(temp, "w")) == NULL)
	{
		perror(temp);
		quit(1);
	}

	for (ty = ttys; ty->ty_active; ty++)	/* same ports! */
		fprintf(tpf, "%s %s\n",
			type(ty->ty_port)->tp_term, ty->ty_port);
	fclose(tpf);

	if (rename(temp, etc_ttytype) != 0)
	{
		fprintf(stderr, "Can't rename %s\n", temp);
		rtn = 1;
	}

	return (rtn);
}

/*
 * invoke editor
 */
edit()
{
	int	child;
	int	status;

	if ((child = fork()) == 0)
	{
		execl("/usr/ucb/vi", "vi", etc_conf, 0);
		execl("/bin/ed", "ed", etc_conf, 0);
		exit(1);
	}

	if (child < 0)
	{
		perror("can't fork editor");
		quit(1);
	}

	/*
	 * wait for editor
	 */
	while (wait(&status) >= 0)
		;

	return (status);
}

quit (n)
int	n;
{
	unlink (lockfile);
	if (n > 1)
	{
		signal (n, SIG_DFL);
		kill (getpid(), n);
	}
	exit (n);
}

getlockfile ()
{
	char	*p;
	char	locktmp[64];
	int	fd;

	strcpy(locktmp, lockfile);
	if (p = rindex(locktmp, '/'))
		p++;
	else
		p = locktmp;
	strcpy(p, "confttysXXXXXX");
	mktemp(locktmp);

	if ((fd = creat(locktmp, 0600)) < 0)
	{
		perror(locktmp);
		exit(1);
	}

	if (link(locktmp, lockfile) < 0)
	{
		perror(lockfile);
		unlink(locktmp);
		exit(1);
	}

	signal(SIGINT,	quit);
	signal(SIGQUIT,	quit);

	unlink(locktmp);
	return(0);
}

struct speeds {
	char	*sp_name;	/* human readable name */
	char	sp_table;	/* getty table name */
} speeds[] = {
	{ "dialup",	GT_DIALUP },	/* normal dialup rotation */
	{ "selector",	GT_SELECTOR },	/* port selector pseudo-table autobaud*/
	{ "b110",	GT_B110 },	/* 110 baud */
	{ "b134",	GT_B134 },	/* 134.5 baud selectric */
	{ "b150",	GT_B150 },	/* 150 baud */
	{ "b300",	GT_B300 },	/* 300 baud */
	{ "b600",	GT_B600 },	/* 600 baud */
	{ "b1200",	GT_B1200 },	/* 1200 baud */
	{ "b2400",	GT_B2400 },	/* 2400 baud */
	{ "b4800",	GT_B4800 },	/* 4800 baud */
	{ "b9600",	GT_B9600 },	/* 9600 baud */
	{ "dw2console",	GT_DW2CONSOLE },/* Decwriter Console - 300 baud */
	{ "fastdialup",	GT_FASTDIALUP },/* 1200-300 baud rotation for dialup */
	{ "fastdialup1",GT_FASTDIALUP1},/* 300-1200  "     "       "     "    */
	{ "crt",	GT_CRT_HCPY },	/* 9600-300 CRT + hardcopy rotation */
	{ "hardcopy",	GT_HCPY_CRT },	/* 300-9600  "      "        "      */
	{ "plugboard",	GT_PLUGBOARD },	/* 9600-300-1200 rotation */
	{ "plugboard1",	GT_PLUGBOARD2 },/* 300-1200-9600 rotation */
	{ "plugboard2",	GT_PLUGBOARD2 },/* 1200-9600-300 rotation */
	{ "interdata",	GT_INTERDATA },	/* Interdata Console */
	{ "chess",	GT_CHESS },	/* LSI Chess Terminal */
	{ "tty33",	GT_TTY33 },	/* 110 baud Model 33 TTY */
	{ "network",	GT_NETWORK },	/* network port */
	{ "", 0 }
};

char *
speedname (c)
	char	c;
{
	struct speeds	*sp;
	static char	sbuf[32];

	for (sp = speeds; sp->sp_table; sp++)
		if (sp->sp_table == c)
			break;

	if (sp->sp_table)
		strcpy(sbuf, sp->sp_name);
	else
		strcpy(sbuf, "-");

	if (strlen(sbuf) < 8)
		strcat(sbuf, "\t");

	return (sbuf);
}

char *
termname (port)
	char	*port;
{
	register struct ttytype	*tp;

	for (tp = ttytype; tp->tp_term[0]; tp++)
		if (strcmp(port, tp->tp_port) == 0)
			return (tp->tp_term);

	if (tp < &ttytype[(sizeof ttytype / sizeof (struct ttytype)) -1])
	{
		strcpy(tp->tp_port, port);
		strcpy(tp->tp_term, "unknown");
		(++tp)->tp_term[0] = '\0';
	}

	return ("unknown");
}

char
speedchar (speed)
	char	*speed;
{
	register struct speeds	*sp;

	for (sp = speeds; sp->sp_table; sp++)
		if (strcmp(sp->sp_name, speed) == 0)
			return (sp->sp_table);
	return ('\0');
}

struct ttytype *
type (port)
	char	*port;
{
	register struct ttytype	*tp;

	for (tp = ttytype; tp->tp_term[0]; tp++)
		if (strcmp(tp->tp_port, port) == 0)
			return (tp);

	if (tp < &ttytype[(sizeof ttytype / sizeof (struct ttytype)) -1])
	{
		strcpy(tp->tp_port, port);
		strcpy(tp->tp_term, "unknown");
		return(tp);
	}

	return((struct ttytype *)0);
}
