#
/*
 * procp - print out many statistics about a process 
 *
 * Jeff Schriebman UC Berkeley June 1977
 *
 * Data is output from the proc table, u dot area,
 * file table, and inode table.
 */

#include "/usr/sys/param.h"
#include "/usr/sys/conf.h"
#include "/usr/sys/proc.h"
#include "/usr/sys/inode.h"
#include "/usr/sys/file.h"
#include "/usr/sys/tty.h"
#include "/usr/sys/user.h"

struct {
	char name[8];
	int  type;
	char  *value;
} nl[3];

struct proc proc[1];
int ua[256];
int	mem;
int	swmem;
int	swap;
int	pid;
int	 baddr, laddr, mf;
char	*coref;
struct ibuf {
	char	idevmin, idevmaj;
	int	inum;
	int	iflags;
	char	inl;
	char	iuid;
	char	igid;
	char	isize0;
	int	isize;
	int	iaddr[8];
	char	*ictime[2];
	char	*imtime[2];
	int	fill;
};


main(argc, argv)
char **argv;
{
	register *p, i;

	pid = getpid();
	if (argc > 1)
		pid = atoi(argv[1]);
	printf("PID = %d\n", pid);
	if(chdir("/dev") < 0) {
		printf("cannot change to /dev\n");
		done();
	}
	setup(&nl[0], "_proc");
	setup(&nl[1], "_swapdev");
	nlist("/unix", &nl);
	if (nl[0].type==0) {
		printf("No namelist\n");
		done();
	}
	coref = "/dev/mem";
	if ((mem = open(coref, 0)) < 0) {
		printf("No mem\n");
		done();
	}
	swmem = open(coref, 0);
	/*
	 * Locate proc table
	 */
	if (getproc(pid)) {
		printf("No proc\n");
		done();
	}
	/*
	 * get u area
	 */
	getdev();
	if (setcom()) {
		printf("No U. area\n");
		done();
	}
	pproc();
	pudot();
	pfile();
	psig();
	done();
}

/*
 * print process table information
 */
pproc()
{
	register *p;

	p = proc;
	printf("stat nice   sig  uid  gid  pgrp  ppid  wchan clock\n");
	printf("%4d %4d %5o %4d %4d %5d %5d %6o %5d\n",
	p->p_stat, p->p_nice, p->p_sig, p->p_uid&0377, (p->p_uid>>8)&0377,
		p->p_pgrp, p->p_ppid, p->p_wchan, p->p_clktim);
}

pudot()
{
	register *p, *n;
	int j, k, jj, kk;

	p = ua;
	n = inode;
	getino(p->u_cdir);
	j = p->u_ttyd.d_major & 0377;
	k = p->u_ttyd.d_minor & 0377;
	jj = n->i_dev.d_major & 0377;
	kk = n->i_dev.d_minor & 0377;
	printf("ruid rgid         direct  inode    tty   dev   cdir\n");
	printf("%4d %4d %14s %6d   %2d/%-2d %2d/%-2d %5d\n",
	p->u_ruid&0377, p->u_rgid&0377, p->u_dent.u_name, p->u_dent.u_ino,
	j, k, jj, kk, n->i_number);
}

pfile()
{
	register *p, *n, i;
	int j, k;
	int t1, t2, t3;

	p = ua[0].u_ofile;
	n = &inode[0];
	printf("     files           inodes\n");
	printf("num type count count dev  inum  uid  gid\n");
	for (i=0; i < NOFILE; i++) {
		if (*p != 0) {
			seek(mem, *p, 0);
			read(mem, file, sizeof file[0]);
			getino(file[0].f_inode);
			t1 = t2 = t3 = ' ';
			if (file[0].f_flag & FREAD)
				t1 = 'R';
			if (file[0].f_flag & FWRITE)
				t2 = 'W';
			if (file[0].f_flag & FPIPE)
				t3 = 'P';
			j = n->i_dev.d_minor;
			k = n->i_dev.d_major;
	printf("%3d  %c%c%c %5d %4d %2d/%-2d %4d %4d %4d\n",
	i, t1, t2, t3, file[0].f_count,
	n->i_count, j, k, n->i_number, n->i_uid&0377, n->i_gid&0377);
		}
		p++;
	}
}

psig()
{
	register *p, i, s;

	p = ua;
	printf("	signals\n");
	for (i=0; i< NSIG; i++) {
		if ((s=p->u_signal[i]) == 0)
			continue;
		if (s & 01)
			printf("%3d %5d ignored\n", i, s);
		else
			printf("%3d %5d caught\n", i, s);
	}
}

getproc(a)
{
	register *p, i;

	seek(mem, nl[0].value, 0);
	p = proc;
	for (i=0; i<NPROC; i++) {
		read(mem, proc, sizeof proc);
		if (p->p_pid==a)
			return(0);
	}
	return(1);
}

getino(a)
{
	seek(mem, a, 0);
	read(mem, inode, sizeof inode[0]);
}


getdev()
{
	register struct { int dir_ino; char dir_n[14]; } *p;
	register i, c;
	int f;
	char dbuf[512];
	int sbuf[20];

	seek(mem, nl[1].value, 0);
	read(mem, &nl[1].value, 2);
	f = open("/dev", 0);
	if(f < 0) {
		printf("cannot open /dev\n");
		done();
	}
	swap = -1;
	c = 0;

loop:
	i = read(f, dbuf, 512);
	if(i <= 0) {
		close(f);
		if(swap < 0) {
			printf("no swap device\n");
			done();
		}
		return;
	}
	while(i < 512)
		dbuf[i++] = 0;
	for(p = dbuf; p < dbuf+512; p++) {
		if(p->dir_ino == 0)
			continue;
		if(swap >= 0)
			continue;
		if(stat(p->dir_n, sbuf) < 0)
			continue;
		if((sbuf->iflags & 060000) != 060000)
			continue;
		if(sbuf->iaddr[0] == nl[1].value)
			swap = open(p->dir_n, 0);
	}
	goto loop;
}

setup(p, s)
char *p, *s;
{
	while (*p++ = *s++);
}

setcom()
{
	baddr = 0;
	laddr = 0;
	if (proc[0].p_flag&SLOAD) {
		laddr = proc[0].p_addr;
		mf = swmem;
	} else {
		baddr = proc[0].p_addr;
		mf = swap;
	}
	baddr =+ laddr>>3;
	laddr = (laddr&07)<<6;
	seek(mf, baddr, 3);
	seek(mf, laddr, 1);
	if (read(mf, &ua[0], 512) != 512)
		return(1);
	return(0);
}

done()
{
	exit();
}
