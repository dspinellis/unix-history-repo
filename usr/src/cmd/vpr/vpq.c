/*
 * Varian and Versatec queue
 */

#include <sys/types.h>
#include <dir.h>
#include <stat.h>
#include <stdio.h>
#include <errno.h>
#define	MAXJOBS 100

struct	dir dirent;
struct	stat stbuf;
int	nextflag;
int	linecnt;
FILE	*df;
FILE	*jf;
char	line[100];
char	username[10];
int	cnt;
extern	int errno;
extern	char _sobuf[];

main(argc, argv)
int argc;
char **argv;
{
	int varian = 1;
	int versatec = 1;

	setbuf(stdout, _sobuf);

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch (argv[0][1]) {

		case 'W':		/* Wide: the versatec. */
			varian = 0;
			versatec++;
			break;

		case 'b':
			varian++;
			versatec++;
			break;

		default:
			fprintf(stderr, "usage: vpq [ -W ] [ -b ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (varian)
		queue("/dev/va0", "Varian", "/usr/spool/vad", "/usr/lib/vad");
	if (versatec)
		queue("/dev/vp0", "Versatec", "/usr/spool/vpd", "/usr/lib/vpd");
	exit(0);
}


queue(device, devname, spooldir, daemon)
char *device, *devname, *spooldir, *daemon;
{
	FILE *vc;

	printf("%s: ", devname);
	vc = fopen(device, "w");
	if (vc == NULL) {
		if (errno == EIO)
			printf("offline\n");
		else if (errno == ENXIO)
			printf("in use\n");
		else
			printf("not available\n");
	} else {
		printf("ready and idle.\n");
		fclose(vc);
	}
	if (access(daemon, 1))
		printf("Daemon is disabled.\n");
	if (chdir(spooldir) < 0) {
		perror(spooldir);
		return;
	}
oloop:
	df = fopen(".", "r");
	if (df == NULL) {
		perror(spooldir);
		return;
	}
loop:
	fseek(df, 0l, 0);
	linecnt = 0;
	cnt = 0;
	while (fread(&dirent, sizeof dirent, 1, df) == 1) {
		if (dirent.d_ino == 0)
			continue;
		if (dirent.d_name[0] != 'd')
			continue;
		if (dirent.d_name[1] != 'f')
			continue;
		if (stat(dirent.d_name, &stbuf) < 0)
			continue;
		if (cnt == 0)
			printf("Owner\t  Id      Chars  Filename\n");
		cnt++;
		process();
	}
	if (cnt == 0)
		printf("Queue is empty.\n");
	printf("\n");
}

process()
{

	jf = fopen(dirent.d_name, "r");
	if (jf == NULL)
		return;
	while (getline()) {
		switch (line[0]) {

		case 'L':
			strcpy(username, line+1);
			break;

		case 'B':
		case 'F':
		case 'G':
		case 'P':
		case 'T':
			if (stat(line+1, &stbuf) < 0)
				stbuf.st_size = 0;
			printf("%-10s%5s%8d  %s\n", username, dirent.d_name+3,
			    stbuf.st_size, line+1);
			break;
		}
	}
	fclose(jf);
}

getline()
{
	register int i, c;

	i = 0;
	while ((c = getc(jf)) != '\n') {
		if (c <= 0)
			return(0);
		if (i < 100)
			line[i++] = c;
	}
	line[i++] = 0;
	return (1);
}
