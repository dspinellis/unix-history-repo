#include <signal.h>
/*
 * Line-printer daemon for Versatek
 *
 */

#define	TIMEOUT	100
#define	DAEMUID	1

struct {
	int	ino;
	char	name[14];
} dbuf;

char	line[128];
char	banbuf[64];
int	linel;
int	dfb[259];
char	dfname[26] = "/usr/lpd/";
int	waittm	= 60;

main(argc, argv)
{
	register char *p1, *p2;
	register df;

	setuid(DAEMUID);
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
/*
 * Close all files, open root as 0, 1, 2
 * to assure standard environment
 */
	for (df=0; df<=15; df++)
		close(df);
	open("/", 0);
	dup(0);
	dup(0);
	if ((df=creat("/usr/lpd/lock", 0)) < 0)
		exit(0);
	close(df);
	if (fork())
		exit(0);
again:
	df = open("/usr/lpd", 0);
	do {
		if (read(df, &dbuf, sizeof dbuf) < sizeof dbuf) {
			unlink("/usr/lpd/lock");
			exit(0);
		}
	} while (dbuf.ino==0 || dbuf.name[0]!='d' || dbuf.name[1]!='f');
	close(df);
	p1 = dbuf.name;
	p2 = &dfname[9];
	while (p1 < &dbuf.name[14])
		*p2++ = *p1++;
	if (trysend(dfname) == 0)
		goto again;
	sleep(waittm);
	goto again;
}

trysend(file)
{
	register char *p1, *p2;
	register i;
	extern int badexit();

	if (fopen(file, dfb) < 0)
		return(0);
	banbuf[0] = 0;
	while (getline()) switch (line[0]) {
	case 'L':
		p1 = line+1;
		p2 = banbuf;
		while (*p2++ = *p1++);
		continue;

	case 'F':
		if (send())
			return(1);
		continue;

	case 'U':
		continue;

	case 'M':
		continue;
	}
/*
 * Second pass.
 * Unlink files and send mail.
 */
	lseek(dfb[0], 0L, 0);
	dfb[1] = 0;
	while (getline()) switch (line[0]) {

	default:
		continue;

	case 'U':
		unlink(&line[1]);
		continue;

	case 'M':
		sendmail();
		continue;
	}
	close(dfb[0]);
	unlink(file);
}

sendmail()
{
	static int p[2];
	register i;
	int stat;

	pipe(p);
	if (fork()==0) {
		alarm(0);
		close(0);
		dup(p[0]);
		for (i=3; i<=15; i++)
			close(i);
		execl("/bin/mail", "mail", &line[1], 0);
		exit(0);
	}
	close(1);
	dup(p[1]);
	printf("Your printer job is done\n");
	close(1);
	close(p[0]);
	close(p[1]);
	open("/", 0);
	wait(&stat);
}

getline()
{
	register char *lp;
	register c;

	lp = line;
	linel = 0;
	while ((c = getc(dfb)) != '\n') {
		if (c<0)
			return(0);
		if (c=='\t') {
			do {
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = 0;
	return(1);
}

send()
{
	int p;

	if (p = fork()) {
		if (p == -1)
			return(1);
		wait(&p);
		return(p);
	}
	if (banbuf[0]) {
		execl("/usr/bin/vpr", "vpr", "-b", banbuf, line+1, 0);
		return(1);
	}
	execl("/usr/bin/vpr", "vpr", line, 0);
	return(1);
}
