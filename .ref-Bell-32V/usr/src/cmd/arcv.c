/*
 * Convert old to new archive format
*/

#include <signal.h>
#include <ar.h>

#define	omag	0177555

struct	ar_hdr nh;
struct
{
	char	oname[8];
	long	odate;
	char	ouid;
	char	omode;
	unsigned siz;
} oh;

char	*tmp;
char	*mktemp();
int	f;
int	tf;
union {
	char	buf[512];
	int	magic;
} b;

main(argc, argv)
char *argv[];
{
	register i;

	tmp = mktemp("/tmp/arcXXXXX");
	for(i=1; i<4; i++)
		signal(i, SIG_IGN);
	for(i=1; i<argc; i++)
		conv(argv[i]);
	unlink(tmp);
}

conv(fil)
char *fil;
{
	register unsigned i, n;

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
	b.magic = 0;
	read(f, (char *)&b.magic, sizeof(b.magic));
	if(b.magic != omag) {
		printf("arcv: %s not archive format\n", fil);
		close(tf);
		close(f);
		return;
	}
	b.magic = ARMAG;
	write(tf, (char *)&b.magic, sizeof(b.magic));
loop:
	i = read(f, (char *)&oh, sizeof(oh));
	if(i != sizeof(oh))
		goto out;
	for(i=0; i<8; i++)
		nh.ar_name[i] = oh.oname[i];
	nh.ar_size = oh.siz;
	nh.ar_uid = oh.ouid;
	nh.ar_gid = 1;
	nh.ar_mode = 0666;
	nh.ar_date = oh.odate;
	n = (oh.siz+1) & ~01;
	write(tf, (char *)&nh, sizeof(nh));
	while(n > 0) {
		i = 512;
		if(n < i)
			i = n;
		read(f, b.buf, i);
		write(tf, b.buf, i);
		n -= i;
	}
	goto loop;
out:
	lseek(f, 0L, 0);
	lseek(tf, 0L, 0);
	while((i=read(tf, b.buf, 512)) > 0)
		write(f, b.buf, i);
	close(f);
	close(tf);
}
