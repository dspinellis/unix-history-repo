static	char sccsid[] = "@(#)arcv.c 4.2 %G%";
/*
 * arcv - convert old to new archive format
 */

#include <signal.h>
#include <ar.h>
#define	OARMAG	0177545
struct	oar_hdr {
	char	oar_name[14];
	long	oar_date;
	char	oar_uid;
	char	oar_gid;
	int	oar_mode;
	long	oar_size;
};

struct	ar_hdr nh;
struct	oar_hdr oh;
char	tmp[] = "/usr/tmp/arcXXXXXX";
char	*mktemp();
int	f;
char	buf[512];
int	tf;
main(argc, argv)
char *argv[];
{
	register i;

	if (argc>1 && strcmp(argv[1], "-t")==0) {
		argc--;
		argv++;
	} else {
		strcpy(tmp, "/tmp/arcXXXXXX");
	}
	mktemp(tmp);
	for(i=1; i<4; i++)
		signal(i, SIG_IGN);
	for(i=1; i<argc; i++) {
		if (argc>1)
			printf("%s:\n", argv[i]);
		conv(argv[i]);
	}
	unlink(tmp);
	return(0);
}

conv(fil)
char *fil;
{
	int oldmagic;
	long n;
	unsigned i;

	f = open(fil, 2);
	if(f < 0) {
		printf("arcv: cannot open %s\n", fil);
		return;
	}
	close(creat(tmp, 0600));
	tf = open(tmp, 2);
	if(tf < 0) {
		printf("arcv: cannot open temp\n");
		close(f);
		return;
	}
	oldmagic = 0;
	read(f, (char *)&oldmagic, sizeof(oldmagic));
	if(oldmagic != 0177545) {
		printf("arcv: %s not old archive format\n", fil);
		close(tf);
		close(f);
		return;
	}
	chkwrite(tf, ARMAG, SARMAG);
loop:
	i = read(f, (char *)&oh, sizeof(oh));
	if(i != sizeof(oh))
		goto out;

	sprintf(buf, "%-16.14s%-12ld%-6u%-6u%-8o%-10ld%-2s",
	   oh.oar_name,
	   oh.oar_date,
	   oh.oar_uid,
	   oh.oar_gid,
	   (unsigned short)oh.oar_mode,
	   oh.oar_size,
	   ARFMAG);
	strncpy((char *)&nh, buf, sizeof(nh));
	n = oh.oar_size;
	chkwrite(tf, (char *)&nh, sizeof(nh));
	while(n > 0) {
		i = 512;
		if (n<i)
			i = n;
		read(f, buf, i&01? i+1:i);
		if (i&01) {
			buf[i] = '\n';
			i++;
		}
		chkwrite(tf, buf, i);
		n -= i;
	}
	goto loop;
out:
	lseek(f, 0L, 0);
	lseek(tf, 0L, 0);
	while((i=read(tf, buf, 512)) > 0)
		chkwrite(f, buf, i);
	close(f);
	close(tf);
}

chkwrite(f, b, n)
char *b;
{
	if (write(f, b, n) != n) {
		printf("arcv: write error\n");
		unlink(tmp);
		exit(1);
	}
}
