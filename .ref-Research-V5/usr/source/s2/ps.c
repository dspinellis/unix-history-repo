#

#include "/usr/sys/param.h"
#include "/usr/sys/proc.h"
#include "/usr/sys/tty.h"
#include "/usr/sys/user.h"
#define NDC11 10
#define NKL11 4
#define NDH11 16

struct {
	char name[8];
	int  type;
	char  *value;
} nl[5];

struct proc proc[1];
struct user u;
int	lflg;
int	kflg;
int	xflg;
int	tflg;
int	aflg;
int	mem;
int	swap;

int	stbuf[257];
char	*coref;

main(argc, argv)
char **argv;
{
	struct proc *p;
	int n, b;
	int i, c, mtty;
	char *ap;
	int hmem;
	int uid, puid;

	if (argc>1) {
		ap = argv[1];
		while (*ap) switch (*ap++) {
		case 'a':
			aflg++;
			break;

		case 't':
			tflg++;
			break;

		case 'x':
			xflg++;
			break;

		case 'l':
			lflg++;
			break;

		case 'k':
			kflg++;
			break;

		}
	}
	setup(&nl[0], "_proc");
	setup(&nl[1], "_dc11");
	setup(&nl[2], "_kl11");
	setup(&nl[3], "_dh11");
	nlist("/unix", nl);
	if (nl[0].type==0) {
		printf("No namelist\n");
		return;
	}
	coref = "/dev/mem";
	if(kflg)
		coref = "/usr/sys/core";
	if ((hmem = open(coref, 0)) < 0) {
		printf("No mem\n");
		return;
	}
	mem = open(coref, 0);
	swap = open("/dev/swap", 0);
	if (swap < 0)
		printf("No swap\n");
	n = &proc[1];
	b = &proc;
	n =- b;
	seek(hmem, nl[0].value, 0);
	mtty = ttyn(0);
	uid = getuid() & 0377;
	for (i=0; i<NPROC; i++) {
		read(hmem, proc, n);
		if (proc[0].p_stat==0)
			continue;
		if ((c=ttyc(proc[0].p_ttyp))=='?' && xflg==0)
			continue;
		puid = proc[0].p_uid & 0377;
		if (uid != puid && aflg==0)
			continue;
		if (lflg || c!=mtty)
			printf("%c:", c);
		else
			printf("  ");
		if (lflg) {
			printf("%3o%4d%5d%5d",
				proc[0].p_flag<<3|proc[0].p_stat, puid,
				proc[0].p_pri,
				proc[0].p_time);
		}
		printf("%6d", proc[0].p_pid);
		if (lflg) {
			printf("%4d", (proc[0].p_size+7)>>3);
			if (proc[0].p_wchan)
				printf("%8o", proc[0].p_wchan);
			else
				printf("        ");
		}
		if (proc[0].p_stat==5)
			printf(" <defunct>");
		else
			prcom();
		printf("\n");
	}
}

ttyc(p)
struct tty *p;
{
	int n;
	struct { struct tty xdc11[]; };

	n = p - nl[1].value->xdc11;
	if (n>=0 && n<NDC11) {
		if (n<8)
			return(n+'0');
		return(n+'a'-8);
	}
	n = p - nl[2].value->xdc11;
	if(n>=0 && n<NKL11)
		return('8'+n);
	n = p - nl[3].value->xdc11;
	if(n>=0 && n<NDH11)
		return('f'+n);
	return('?');
}

setup(p, s)
char *p, *s;
{
	while (*p++ = *s++);
}

prcom()
{
	int baddr, laddr, mf;
	int *ip;
	char *cp, *cp1;
	int c;

	baddr = 0;
	laddr = 0;
	if (proc[0].p_flag&SLOAD) {
		laddr = proc[0].p_addr;
		mf = mem;
	} else {
		baddr = proc[0].p_addr;
		mf = swap;
	}
	laddr =+ proc[0].p_size - 8;
	baddr =+ laddr>>3;
	laddr = (laddr&07)<<6;
	seek(mf, baddr, 3);
	seek(mf, laddr, 1);
	if (read(mf, stbuf, 512) != 512)
		return(0);
	for (ip = &stbuf[256]; ip > &stbuf[0];) {
		if (*--ip == -1) {
			cp = ip+1;
			if (*cp==0)
				cp++;
			for (cp1 = cp; cp1 < &stbuf[256]; cp1++) {
				c = *cp1;
				if (c==0)
					*cp1 = ' ';
				else if (c < ' ' || c > 0176)
					*cp1 = '?';
			}
			printf(" %.32s ", cp);
			return(1);
		}
	}
	return(0);
}
