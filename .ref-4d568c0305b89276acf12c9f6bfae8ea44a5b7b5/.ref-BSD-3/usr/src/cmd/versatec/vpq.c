/*
 * Versatec queue
 */

#include <sys/types.h>
#include <dir.h>
#include <stat.h>
#include <stdio.h>
#include <errno.h>
#define	MAXJOBS 100

struct	dir dirent;
struct	stat stbuf;
char	vpddir[] =	"/usr/vpd";
int	nextflag;
int	linecnt;
FILE	*df;
FILE	*jf;
char	line[100];
char	username[10];
int	cnt;
extern	int errno;

main()
{
	FILE *vc;

	vc = fopen("/dev/vp0", "w");
	if (vc == NULL) {
		if (errno == EIO)
			printf("Versatec is OFFLINE.\n");
		else if (errno == ENXIO)
			/* printf("Versatec in use.\n") */;
		else
			printf("Versatec not available.\n");
	} else {
		printf("Versatec is ready and idle.\n");
		fclose(vc);
	}
	if (access("/usr/ucb/vpr", 1))
		printf("Versatec daemon is disabled.\n");
	if (chdir(vpddir) < 0) {
		perror(vpddir);
		exit(1);
	}
oloop:
	df = fopen(".", "r");
	if (df == NULL) {
		perror(vpddir);
		exit(1);
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
		printf("Versatec queue is empty.\n");
	exit(0);
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
		case 'P':
		case 'T':
			if (stat(line+1, &stbuf) < 0)
				stbuf.st_size = 0;
			printf("%-10s%5s%8d  %s\n", username, dirent.d_name+3,
			    stbuf.st_size, line+1);
			break;
		}
	}
	close(jf);
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
