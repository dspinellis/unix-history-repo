/*
 * Run programs submitted by at.
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <time.h>
#include <sys/stat.h>

# define DIR "/usr/spool/at"
# define PDIR	"past"
# define LASTF "/usr/spool/at/lasttimedone"

int	nowtime;
int	nowdate;
int	nowyear;

main(argc, argv)
char **argv;
{
	int tt, day, year, uniq;
	struct direct dirent;
	FILE *dirf;

	setuid(0);
	setgid(0);
	chdir(DIR);
	makenowtime();
	if ((dirf = fopen(".", "r")) == NULL) {
		fprintf(stderr, "Cannot read at directory\n");
		exit(1);
	}
	while (fread((char *)&dirent, sizeof(dirent), 1, dirf) == 1) {
		if (dirent.d_ino==0)
			continue;
		if (sscanf(dirent.d_name, "%2d.%3d.%4d.%2d", &year, &day, &tt, &uniq) != 4)
			continue;
		if (nowyear < year)
			continue;
		if (nowyear==year && nowdate < day)
			continue;
		if (nowyear==year && nowdate==day && nowtime < tt)
			continue;
		run(dirent.d_name);
	}
	fclose(dirf);
	updatetime(nowtime);
	exit(0);
}

makenowtime()
{
	long t;
	struct tm *localtime();
	register struct tm *tp;

	time(&t);
	tp = localtime(&t);
	nowtime = tp->tm_hour*100 + tp->tm_min;
	nowdate = tp->tm_yday;
	nowyear = tp->tm_year;
}

updatetime(t)
{
	FILE *tfile;

	tfile = fopen(LASTF, "w");
	if (tfile == NULL) {
		fprintf(stderr, "can't write lastfile\n");
		exit(1);
	}
	fprintf(tfile, "%04d\n", t);
}

run(file)
char *file;
{
	struct stat stbuf;
	register pid, i;
	char sbuf[64];

	/* printf("running %s\n", file); */
	if (fork()!=0)
		return;
	for (i=0; i<15; i++)
		close(i);
	dup(dup(open("/dev/null", 0)));
	sprintf(sbuf, "/bin/mv %.14s %s", file, PDIR);
	system(sbuf);
	chdir(PDIR);
	if (stat(file, &stbuf) == -1)
		exit(1);
	if (pid = fork()) {
		if (pid == -1)
			exit(1);
		wait((int *)0);
		unlink(file);
		exit(0);
	}
	setgid(stbuf.st_gid);
	setuid(stbuf.st_uid);
	nice(3);
	execl("/bin/sh", "sh", file, 0);
	execl("/usr/bin/sh", "sh", file, 0);
	fprintf(stderr, "Can't execl shell\n");
	exit(1);
}
