#
/* flag version 6 a.outs by putting a 1 in word 6 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
struct stat stbuf;
int	fd;
short	hbuf[8];
time_t	timep[2];
main(argc, argv) char *argv[]; {
	if(stat(argv[1], &stbuf) < 0) exit(0);
	if(!(stbuf.st_mode & S_IEXEC)) exit(0);
	if((fd = open(argv[1], 2)) < 0) exit(0);
	if(read(fd, hbuf, sizeof hbuf) != sizeof hbuf) exit(0);
	lseek(fd, 0L, 0);
	if(hbuf[0]!=0407&&hbuf[0]!=0410&&hbuf[0]!=0411&&hbuf[0]!=0405) exit(0);
	if(hbuf[6] != 0) exit(0);
	hbuf[6] = 01;
	if(write(fd, hbuf, sizeof hbuf) != sizeof hbuf) exit(0);
	close(fd);
	timep[0] = stbuf.st_atime;
	timep[1] = stbuf.st_mtime;
	utime(argv[1], timep);
	printf("%s fixed\n",argv[1]);
}
