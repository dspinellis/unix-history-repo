/*
 * Print system stuff
 */

#define mask(x) (x&0377)
#include <sys/param.h>
#include <sys/conf.h>
#include <sys/tty.h>

char	*fcore	= "/dev/mem";
char	*fnlist	= "/unix";
int	fc;

struct setup {
	char	name[8];
	int	type;
	unsigned	value;
} setup[] = {
#define	SINODE	0
	"_inode", 0, 0,
#define	STEXT	1
	"_text", 0, 0,
#define	SPROC	2
	"_proc", 0, 0,
#define	SDH	3
	"_dh11", 0, 0,
#define	SNDH	4
	"_ndh11", 0, 0,
#define	SKL	5
	"_kl11", 0, 0,
#define	SFIL	6
	"_file", 0, 0,
	0,
};

int	inof;
int	txtf;
int	prcf;
int	ttyf;
int	usrf;
long	ubase;
int	filf;
int	allflg;

main(argc, argv)
char **argv;
{

	while (--argc && **++argv == '-') {
		while (*++*argv)
		switch (**argv) {

		case 'a':
			allflg++;
			break;

		case 'i':
			inof++;
			break;

		case 'x':
			txtf++;
			break;
		case 'p':
			prcf++;
			break;

		case 't':
			ttyf++;
			break;

		case 'u':
			if (--argc == 0)
				break;
			usrf++;
			ubase = oatoi(*++argv);
			break;

		case 'f':
			filf++;
			break;
		}
	}
	if (argc>0)
		fcore = argv[0];
	if ((fc = open(fcore, 0)) < 0) {
		printf("Can't find %s\n", fcore);
		exit(1);
	}
	if (argc>1)
		fnlist = argv[1];
	nlist(fnlist, setup);
	if (setup[SINODE].type == -1) {
		printf("no namelist\n");
		exit(1);
	}
	if (inof)
		doinode();
	if (txtf)
		dotext();
	if (ttyf)
		dotty();
	if (prcf)
		doproc();
	if (usrf)
		dousr();
	if (filf)
		dofil();
}

doinode()
{
#include <sys/inode.h>
	register struct inode *ip;
	struct inode xinode[NINODE];
	register int nin, loc;

	nin = 0;
	lseek(fc, (long)setup[SINODE].value, 0);
	read(fc, (char *)xinode, sizeof(xinode));
	for (ip = xinode; ip < &xinode[NINODE]; ip++)
		if (ip->i_count)
			nin++;
	printf("%d active inodes\n", nin);
	printf("   LOC  FLAGS  CNT DEVICE   INO   MODE NLK UID  SIZE/DEV\n");
	loc = setup[SINODE].value;
	for (ip = xinode; ip < &xinode[NINODE]; ip++, loc += sizeof(xinode[0])) {
		if (ip->i_count == 0)
			continue;
		printf("%7.1o ", loc);
		putf(ip->i_flag&ILOCK, 'L');
		putf(ip->i_flag&IUPD, 'U');
		putf(ip->i_flag&IACC, 'A');
		putf(ip->i_flag&IMOUNT, 'M');
		putf(ip->i_flag&IWANT, 'W');
		putf(ip->i_flag&ITEXT, 'T');
		printf("%4d", ip->i_count&0377);
		printf("%3d,%3d", major(ip->i_dev), minor(ip->i_dev));
		printf("%6l", ip->i_number);
		printf("%7o", ip->i_mode);
		printf("%4d", ip->i_nlink);
		printf("%4d", ip->i_uid);
		if ((ip->i_mode&IFMT)==IFBLK || (ip->i_mode&IFMT)==IFCHR)
			printf("%6d,%3d", major(ip->i_un.i_rdev), minor(ip->i_un.i_rdev));
		else
			printf("%10ld", ip->i_size);
		printf("\n");
	}
}

putf(v, n)
{
	if (v)
		printf("%c", n);
	else
		printf(" ");
}

dotext()
{
#include <sys/text.h>
	register struct text *xp;
	struct text xtext[NTEXT];
	register loc;
	int ntx;

	ntx = 0;
	lseek(fc, (long)setup[STEXT].value, 0);
	read(fc, (char *)xtext, sizeof(xtext));
	for (xp = xtext; xp < &xtext[NTEXT]; xp++)
		if (xp->x_iptr!=NULL)
			ntx++;
	printf("%d text segments\n", ntx);
	printf("   LOC FLAGS DADDR  CADDR SIZE   IPTR  CNT CCNT\n");
	loc = setup[STEXT].value;
	for (xp = xtext; xp < &xtext[NTEXT]; xp++, loc+=sizeof(xtext[0])) {
		if (xp->x_iptr == NULL)
			continue;
		printf("%7.1o", loc);
		printf(" ");
		putf(xp->x_flag&XTRC, 'T');
		putf(xp->x_flag&XWRIT, 'W');
		putf(xp->x_flag&XLOAD, 'L');
		putf(xp->x_flag&XLOCK, 'K');
		putf(xp->x_flag&XWANT, 'w');
		printf("%5u", xp->x_daddr);
		printf("%7.1o", xp->x_caddr);
		printf("%5d", xp->x_size);
		printf("%8.1o", xp->x_iptr);
		printf("%4d", xp->x_count&0377);
		printf("%4d", xp->x_ccount);
		printf("\n");
	}
}

doproc()
{
#include <sys/proc.h>
	struct proc xproc[NPROC];
	register struct proc *pp;
	register loc, np;

	lseek(fc, (long)setup[SPROC].value, 0);
	read(fc, (char *)xproc, sizeof(xproc));
	np = 0;
	for (pp=xproc; pp < &xproc[NPROC]; pp++)
		if (pp->p_stat)
			np++;
	printf("%d processes\n", np);
	printf("   LOC S  F  PRI SIGNAL UID TIM CPU NI  PGRP   PID  PPID ADDR SIZE  WCHAN   LINK  TEXTP  CLKT\n");
	for (loc=setup[SPROC].value,pp=xproc; pp<&xproc[NPROC]; pp++,loc+=sizeof(xproc[0])) {
		if (pp->p_stat==0 && allflg==0)
			continue;
		printf("%6o", loc);
		printf("%2d", pp->p_stat);
		printf("%3o", pp->p_flag);
		printf("%5d", pp->p_pri);
		printf("%7o", pp->p_sig);
		printf("%4d", pp->p_uid&0377);
		printf("%4d", pp->p_time&0377);
		printf("%4d", pp->p_cpu&0377);
		printf("%3d", pp->p_nice);
		printf("%6d", pp->p_pgrp);
		printf("%6d", pp->p_pid);
		printf("%6d", pp->p_ppid);
		printf("%5o", pp->p_addr);
		printf("%5o", pp->p_size);
		printf("%7o", pp->p_wchan);
		printf("%7o", pp->p_link);
		printf("%7o", pp->p_textp);
		printf(" %u", pp->p_clktim);
		printf("\n");
	}
}

dotty()
{
	struct tty dh11[48];
	int ndh;
	register struct tty *tp;
	register char *mesg;

	printf("1 kl11\n");
	lseek(fc, (long)setup[SKL].value, 0);
	read(fc, (char *)dh11, sizeof(dh11[0]));
	mesg = " # RAW CAN OUT   MODE   ADDR   DEL COL  STATE   PGRP\n";
	printf(mesg);
	ttyprt(0, &dh11[0]);
	if (setup[SNDH].type == -1)
		return;
	lseek(fc, (long)setup[SNDH].value, 0);
	read(fc, (char *)&ndh, sizeof(ndh));
	printf("%d dh lines\n", ndh);
	lseek(fc, (long)setup[SDH].value, 0);
	read(fc, (char *)dh11, sizeof(dh11));
	for (tp = dh11; tp < &dh11[ndh]; tp++)
		ttyprt(tp-dh11, tp);
}

ttyprt(n, atp)
struct tty *atp;
{
	register struct tty *tp;

	tp = atp;
	printf("%2d", n);
	printf("%4d", tp->t_rawq.c_cc);
	printf("%4d", tp->t_canq.c_cc);
	printf("%4d", tp->t_outq.c_cc);
	printf("%8.1o", tp->t_flags);
	printf("%8.1o", tp->t_addr);
	printf("%3d", tp->t_delct);
	printf("%4d ", tp->t_col);
	putf(tp->t_state&TIMEOUT, 'T');
	putf(tp->t_state&WOPEN, 'W');
	putf(tp->t_state&ISOPEN, 'O');
	putf(tp->t_state&CARR_ON, 'C');
	putf(tp->t_state&BUSY, 'B');
	putf(tp->t_state&ASLEEP, 'A');
	putf(tp->t_state&XCLUDE, 'X');
	putf(tp->t_state&HUPCLS, 'H');
	printf("%6d", tp->t_pgrp);
	printf("\n");
}

dousr()
{
#include <sys/dir.h>
#include <sys/user.h>
	union {
		struct	user rxu;
		char	fxu[ctob(USIZE)];
	} xu;
	register struct user *up;
	register i;

	lseek(fc, ubase<<6, 0);
	read(fc, (char *)&xu, sizeof(xu));
	up = &xu.rxu;
	printf("rsav %.1o %.1o\n", up->u_rsav[0], up->u_rsav[1]);
	printf("segflg, error %d, %d\n", up->u_segflg, up->u_error);
	printf("uids %d,%d,%d,%d\n", up->u_uid,up->u_gid,up->u_ruid,up->u_rgid);
	printf("procp %.1o\n", up->u_procp);
	printf("base, count, offset %.1o %.1o %ld\n", up->u_base,
		up->u_count, up->u_offset);
	printf("cdir %.1o\n", up->u_cdir);
	printf("dbuf %.14s\n", up->u_dbuf);
	printf("dirp %.1o\n", up->u_dirp);
	printf("dent %d %.14s\n", up->u_dent.d_ino, up->u_dent.d_name);
	printf("pdir %.1o\n", up->u_pdir);
	printf("dseg");
	for (i=0; i<8; i++)
		printf("%8.1o", up->u_uisa[i]);
	printf("\n    ");
	for (i=0; i<8; i++)
		printf("%8.1o", up->u_uisd[i]);
	if (up->u_sep) {
		printf("\ntseg");
		for (i=8; i<16; i++)
			printf("%8.1o", up->u_uisa[i]);
		printf("\n    ");
		for (i=8; i<16; i++)
			printf("%8.1o", up->u_uisd[i]);
	}
	printf("\nfile");
	for (i=0; i<10; i++)
		printf("%8.1o", up->u_ofile[i]);
	printf("\n    ");
	for (i=10; i<NOFILE; i++)
		printf("%8.1o", up->u_ofile[i]);
	printf("\nargs");
	for (i=0; i<5; i++)
		printf(" %.1o", up->u_arg[i]);
	printf("\nsizes %.1o %.1o %.1o\n", up->u_tsize, up->u_dsize, up->u_ssize);
	printf("sep %d\n", up->u_sep);
	printf("qsav %.1o %.1o\n", up->u_qsav[0], up->u_qsav[1]);
	printf("ssav %.1o %.1o\n", up->u_ssav[0], up->u_ssav[1]);
	printf("sigs");
	for (i=0; i<NSIG; i++)
		printf(" %.1o", up->u_signal[i]);
	printf("\ntimes %ld %ld\n", up->u_utime/60, up->u_stime/60);
	printf("ctimes %ld %ld\n", up->u_cutime/60, up->u_cstime/60);
	printf("ar0 %.1o\n", up->u_ar0);
/*
	printf("prof");
	for (i=0; i<4; i++)
		printf(" %.1o", up->u_prof[i]);
*/
	printf("\nintflg %d\n", up->u_intflg);
	printf("ttyp %.1o\n", up->u_ttyp);
	printf("ttydev %d,%d\n", major(up->u_ttyd), minor(up->u_ttyd));
	printf("comm %.14s\n", up->u_comm);
}

oatoi(s)
char *s;
{
	register v;

	v = 0;
	while (*s)
		v = (v<<3) + *s++ - '0';
	return(v);
}

dofil()
{
#include <sys/file.h>
	struct file xfile[NFILE];
	register struct file *fp;
	register nf;
	int loc;

	nf = 0;
	lseek(fc, (long)setup[SFIL].value, 0);
	read(fc, (char *)xfile, sizeof(xfile));
	for (fp=xfile; fp < &xfile[NFILE]; fp++)
		if (fp->f_count)
			nf++;
	printf("%d open files\n", nf);
	printf("  LOC   FLG CNT   INO    OFFS\n");
	for (fp=xfile,loc=setup[SFIL].value; fp < &xfile[NFILE]; fp++,loc+=sizeof(xfile[0])) {
		if (fp->f_count==0)
			continue;
		printf("%7.1o ", loc);
		putf(fp->f_flag&FREAD, 'R');
		putf(fp->f_flag&FWRITE, 'W');
		putf(fp->f_flag&FPIPE, 'P');
		printf("%4d", mask(fp->f_count));
		printf("%8.1o", fp->f_inode);
		printf(" %ld\n", fp->f_un.f_offset);
	}
}
