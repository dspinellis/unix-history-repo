/*
 * Line-printer daemon
 */

#include <sys/types.h>
#include <stdio.h>
#include <dir.h>
#include <signal.h>
#include <stat.h>
#include <sgtty.h>

char	line[128];
char	banbuf[64];
int	linel;
FILE	*dfb;
char	dfname[26] = "/usr/spool/lpd/";
int	waittm	= 60;
struct	dir dbuf;
int	onalrm();

main(argc, argv)
{
	register char *p1, *p2;
	register int df;
	register FILE *dp;
	struct stat stb;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
/*
 * Close all files, open root as 0, 1, 2
 * to assure standard environment
 */
	for (df=0; df<=15; df++)
		close(df);
	open("/", 0);
	dup(0);
	dup(0);
	if (stat("/usr/spool/lpd/lock", &stb) >= 0)
		exit(0);
	if ((df=creat("/usr/spool/lpd/lock", 0)) < 0)
		exit(0);
	close(df);
	if (fork())
		exit(0);
again:
	dp = fopen("/usr/spool/lpd", "r");
	do {
		if (fread(&dbuf, sizeof dbuf, 1, dp) != 1) {
			feedpage();
			unlink("/usr/spool/lpd/lock");
			exit(0);
		}
	} while (dbuf.d_ino==0 || dbuf.d_name[0]!='d' || dbuf.d_name[1]!='f');
	fclose(dp);
	strcpy(dfname, "/usr/spool/lpd/");
	strcatn(dfname, dbuf.d_name, DIRSIZ);
	if (trysend(dfname) == 0)
		goto again;
	sleep(waittm);
	goto again;
}

trysend(file)
	char *file;
{
	register char *p1, *p2;
	register int i;
	extern int badexit();

	dfb = fopen(file, "r");
	if (dfb == NULL)
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
	fseek(dfb, 0L, 0);
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
	fclose(dfb);
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
		if (p[0] != 0) {
			close(0);
			dup(p[0]);
			close(p[0]);
		}
		close(p[1]);
		for (i=3; i<=15; i++)
			close(i);
		execl("/bin/mail", "mail", &line[1], 0);
		exit(0);
	}
	write(p[1], "Your printer job is done\n", 25);
	close(p[0]);
	close(p[1]);
	wait(&stat);
}

getline()
{
	register char *lp;
	register int c;

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

int	pid;

send()
{
	int p;

	if (pid = fork()) {
		if (pid == -1)
			return(1);
		setexit();
		signal(SIGALRM, onalrm);
		alarm(30);
		wait(&p);
		alarm(0);
		return(p);
	}
	if (banbuf[0]) {
		execl("/usr/lib/lpf", "lpf", "-b", banbuf, line+1, 0);
		return(1);
	}
	execl("/usr/lib/lpf", "lpf", line, 0);
	return(1);
}

onalrm()
{
	struct stat stb;

	signal(SIGALRM, onalrm);
	if (stat(dfname, &stb) < 0)
		kill(pid, SIGEMT);
	reset();
}

struct	sgttyb ttyb = {
	B9600, B9600,
	0, 0,
	XTABS|ANYP|ECHO
};

feedpage()
{
	register int i = 66;
	FILE *lp;

	lp = fopen("/dev/lp", "w");
	if (lp == NULL)
		return;
	stty(fileno(lp), &ttyb);
	while (i > 0)
		fprintf(lp, "\n"), i--;
	fclose(lp);
}
