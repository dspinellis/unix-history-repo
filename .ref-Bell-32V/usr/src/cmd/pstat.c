/*
 * Print system stuff
 */

#define mask(x) (x&0377)
#include <sys/param.h>
#include <sys/conf.h>
#include <sys/tty.h>

char	*fcore	= "/dev/kmem";
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
#define	SDZ	3
	"_dz_tty", 0, 0,
#define	SNDZ	4
	"_dz_cnt", 0, 0,
#define	SKL	5
	"_cons", 0, 0,
#define	SFIL	6
	"_file", 0, 0,
#define	SMPXC	7
	"_mpx_chan", 0, 0,
#define	SMPXM	8
	"_mpx_mach", 0, 0,
#define	SMPXB1	9
	"_mptbc", 0, 0,
#define	SMPXB2	10
	"_mptbuf", 0, 0,
#define	SMPSM	11
	"_mpsm", 0, 0,
	0,
};

int	inof;
int	txtf;
int	prcf;
int	ttyf;
int	mpxf;
int	usrf;
long	ubase;
int	filf;
char	partab[1];
struct	cdevsw	cdevsw[1];
struct	bdevsw	bdevsw[1];
int	allflg;
int	kflg;

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

		case 'k':
			kflg++;
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

		case 'm':
			mpxf++;
			break;

		case 'u':
			printf("pstat: -u not implemented\n");
			if (--argc == 0)
				break;
			usrf++;
			sscanf( *++argv, "%x", &ubase);
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
	if (kflg) {
		register struct setup *sp;

		for (sp=setup; sp->value; sp++)
			sp->value &= 0x7fffffff;
	}
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
/*
	if (usrf)
		dousr();
 */
	if (filf)
		dofil();
/*
	if(mpxf)
		dompx();
*/
}

doinode()
{
#include <sys/inode.h>
	register struct inode *ip;
	struct inode xinode[NINODE];
	register int nin, loc;

	nin = 0;
	lseek(fc, (long)setup[SINODE].value, 0);
	read(fc, xinode, sizeof(xinode));
	for (ip = xinode; ip < &xinode[NINODE]; ip++)
		if (ip->i_count)
			nin++;
	printf("%d active xinodes\n", nin);
	printf("   LOC    FLAGS  CNT DEVICE  INO  MODE  NLK UID   SIZE/DEV\n");
	loc = setup[SINODE].value;
	for (ip = xinode; ip < &xinode[NINODE]; ip++, loc += sizeof(xinode[0])) {
		if (ip->i_count == 0)
			continue;
		printf("%8.1x ", loc);
		putf(ip->i_flag&ILOCK, 'L');
		putf(ip->i_flag&IUPD, 'U');
		putf(ip->i_flag&IACC, 'A');
		putf(ip->i_flag&IMOUNT, 'M');
		putf(ip->i_flag&IWANT, 'W');
		putf(ip->i_flag&ITEXT, 'T');
		printf("%4d", ip->i_count&0377);
		printf("%4d,%3d", major(ip->i_dev), minor(ip->i_dev));
		printf("%5l", ip->i_number);
		printf("%6x", ip->i_mode & 0xffff);
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
	struct text xtext[NTEXT], *xp;
	register loc;
	int ntx;

	ntx = 0;
	lseek(fc, (long)setup[STEXT].value, 0);
	read(fc, xtext, sizeof(xtext));
	for (xp = xtext; xp < &xtext[NTEXT]; xp++)
		if (xp->x_iptr!=NULL)
			ntx++;
	printf("%d text segments\n", ntx);
	printf("   LOC   FLAGS  DADDR   CADDR   SIZE   ITPR    CNT CCNT\n");
	loc = setup[STEXT].value;
	for (xp = xtext; xp < &xtext[NTEXT]; xp++, loc+=sizeof(xtext[0])) {
		if (xp->x_iptr == NULL)
			continue;
		printf("%8.1x", loc);
		printf(" ");
		putf(xp->x_flag&XTRC, 'T');
		putf(xp->x_flag&XWRIT, 'W');
		putf(xp->x_flag&XLOAD, 'L');
		putf(xp->x_flag&XLOCK, 'K');
		putf(xp->x_flag&XWANT, 'w');
		printf("%5u", xp->x_daddr);
		printf("%11x", xp->x_caddr);
		printf("%5d", xp->x_size);
		printf("%10.1x", xp->x_iptr);
		printf("%5d", xp->x_count&0377);
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
	read(fc, xproc, sizeof(xproc));
	np = 0;
	for (pp=xproc; pp < &xproc[NPROC]; pp++)
		if (pp->p_stat)
			np++;
	printf("%d processes\n", np);
	printf("   LOC   S  F   PRI SIGNAL UID TIM CPU NI   PGRP  PID  PPID   ADDR  SIZE  WCHAN      LINK    TEXTP  CLKT\n");
	for (loc=setup[SPROC].value,pp=xproc; pp<&xproc[NPROC]; pp++,loc+=sizeof(xproc[0])) {
		if (pp->p_stat==0 && allflg==0)
			continue;
		printf("%8x", loc);
		printf("%2d", pp->p_stat);
		printf("%3o", pp->p_flag&0377);
		printf("%5d", pp->p_pri);
		printf("%7o", pp->p_sig);
		printf("%4d", pp->p_uid&0377);
		printf("%5d", pp->p_time&0377);
		printf("%4d", pp->p_cpu&0377);
		printf("%3d", pp->p_nice);
		printf("%6d", pp->p_pgrp);
		printf("%6d", pp->p_pid);
		printf("%6d", pp->p_ppid);
		printf("%8x", pp->p_addr[0]);
		printf("%5x", pp->p_size);
		printf("%9x", pp->p_wchan);
		printf("%9x", pp->p_link);
		printf("%9x", pp->p_textp);
		printf("   %u", pp->p_clktim);
		printf("\n");
	}
}

dotty()
{
	struct tty dz_tty[32];
	int ndz;
	register struct tty *tp;
	register char *mesg;

	printf("1 cons\n");
	lseek(fc, (long)setup[SKL].value, 0);
	read(fc, dz_tty, sizeof(dz_tty[0]));
	mesg = " RAW CAN OUT   MODE    ADDR   DEL COL  STATE   PGRP\n";
	printf(mesg);
	ttyprt(&dz_tty[0]);
	if (setup[SNDZ].type == -1)
		return;
	lseek(fc, (long)setup[SNDZ].value, 0);
	read(fc, &ndz, sizeof(ndz));
	printf("%d dz lines\n", ndz);
	lseek(fc, (long)setup[SDZ].value, 0);
	read(fc, dz_tty, sizeof(dz_tty));
	for (tp = dz_tty; tp < &dz_tty[ndz]; tp++)
		ttyprt(tp);
}

ttyprt(atp)
struct tty *atp;
{
	register struct tty *tp;

	tp = atp;
	printf("%4d", tp->t_rawq.c_cc);
	printf("%4d", tp->t_canq.c_cc);
	printf("%4d", tp->t_outq.c_cc);
	printf("%8.1o", tp->t_flags);
	printf(" %8.1x", tp->t_addr);
	printf("%3d", tp->t_delct);
	printf("%4d ", tp->t_col);
	putf(tp->t_state&TIMEOUT, 'T');
	putf(tp->t_state&WOPEN, 'W');
	putf(tp->t_state&ISOPEN, 'O');
	putf(tp->t_state&CARR_ON, 'C');
	putf(tp->t_state&BUSY, 'B');
	putf(tp->t_state&ASLEEP, 'A');
	putf(tp->t_state&XCLUDE, 'X');
/*
	putf(tp->t_state&HUPCLS, 'H');
 */
	printf("%6d", tp->t_pgrp);
	printf("\n");
}

dousr()
{
#include <sys/dir.h>
#include <sys/user.h>
	struct user U;
	register i, j, *ip;

	lseek(fc, ubase, 0);
	read(fc, &U, sizeof(U));
/*
	printf("rsav %.1o %.1o\n", U.u_rsav[0], U.u_rsav[1]);
 */
	printf("segflg, error %d, %d\n", U.u_segflg, U.u_error);
	printf("uids %d,%d,%d,%d\n", U.u_uid,U.u_gid,U.u_ruid,U.u_rgid);
	printf("procp %.1x\n", U.u_procp);
	printf("base, count, offset %.1x %.1x %ld\n", U.u_base,
		U.u_count, U.u_offset);
	printf("cdir %.1x\n", U.u_cdir);
	printf("dbuf %.14s\n", U.u_dbuf);
	printf("dirp %.1x\n", U.u_dirp);
	printf("dent %d %.14s\n", U.u_dent.d_ino, U.u_dent.d_name);
	printf("pdir %.1o\n", U.u_pdir);
/*
	printf("dseg");
	for (i=0; i<8; i++)
		printf("%8.1o", U.u_uisa[i]);
	printf("\n    ");
	for (i=0; i<8; i++)
		printf("%8.1o", U.u_uisd[i]);
	if (U.u_sep) {
		printf("\ntseg");
		for (i=8; i<16; i++)
			printf("%8.1o", U.u_uisa[i]);
		printf("\n    ");
		for (i=8; i<16; i++)
			printf("%8.1o", U.u_uisd[i]);
	}
 */
	printf("\nfile");
	for (i=0; i<10; i++)
		printf("%9.1x", U.u_ofile[i]);
	printf("\n    ");
	for (i=10; i<NOFILE; i++)
		printf("%9.1x", U.u_ofile[i]);
	printf("\nargs");
	for (i=0; i<5; i++)
		printf(" %.1x", U.u_arg[i]);
	printf("\nsizes %.1x %.1x %.1x\n", U.u_tsize, U.u_dsize, U.u_ssize);
	printf("sep %d\n", U.u_sep);
	printf("qsav %.1x %.1x\n", U.u_qsav[0], U.u_qsav[1]);
	printf("ssav %.1x %.1x\n", U.u_ssav[0], U.u_ssav[1]);
	printf("sigs");
	for (i=0; i<NSIG; i++)
		printf(" %.1x", U.u_signal[i]);
	printf("\ntimes %ld %ld\n", U.u_utime/60, U.u_stime/60);
	printf("ctimes %ld %ld\n", U.u_cutime/60, U.u_cstime/60);
	printf("ar0 %.1x\n", U.u_ar0);
/*
	printf("prof");
	for (i=0; i<4; i++)
		printf(" %.1o", U.u_prof[i]);
*/
	printf("\nintflg %d\n", U.u_intflg);
	printf("ttyp %.1x\n", U.u_ttyp);
	printf("ttydev %d,%d\n", major(U.u_ttyd), minor(U.u_ttyd));
	printf("comm %.14s\n", U.u_comm);
/*
	i =  U.u_stack - &U;
	while (U[++i] == 0);
	i &= ~07;
	while (i < 512) {
		printf("%x ", 0140000+2*i);
		for (j=0; j<8; j++)
			printf("%9x", U[i++]);
		printf("\n");
	}
*/
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
	read(fc, xfile, sizeof(xfile));
	for (fp=xfile; fp < &xfile[NFILE]; fp++)
		if (fp->f_count)
			nf++;
	printf("%d open files\n", nf);
	printf("   LOC   FLG  CNT   INO    OFFS\n");
	for (fp=xfile,loc=setup[SFIL].value; fp < &xfile[NFILE]; fp++,loc+=sizeof(xfile[0])) {
		if (fp->f_count==0)
			continue;
		printf("%8x ", loc);
		putf(fp->f_flag&FREAD, 'R');
		putf(fp->f_flag&FWRITE, 'W');
		putf(fp->f_flag&FPIPE, 'P');
		printf("%4d", mask(fp->f_count));
		printf("%9.1x", fp->f_inode);
		printf("  %ld\n", fp->f_un.f_offset);
	}
}

/*********
dompx()
{
#include <sys/mpx.h>
	struct chan chan[C];
	struct mach mach[M];
	struct line line[M-1];
	int mptbc;
	char mptbuf[TBSIZ];
	register struct chan *cp;
	register struct mach *mp;
	register struct line *lp;
	int loc, nc;

	lseek(fc, (long)setup[SMPXC].value, 0);
	read(fc, chan, sizeof(chan));
	lseek(fc, (long)setup[SMPXM].value, 0);
	read(fc, mach, sizeof(mach));
	lseek(fc, (long)setup[SMPXB1].value, 0);
	read(fc, &mptbc, sizeof(mptbc));
	lseek(fc, (long)setup[SMPXB2].value, 0);
	read(fc, mptbuf, sizeof(mptbuf));
	lseek(fc, (long)setup[SMPSM].value, 0);
	read(fc, line, sizeof(line));
	nc = 0;
	for(cp=chan; cp < &chan[C]; cp++)
		if(cp->cflag&ALLOC)
			nc++;
	printf("%d mpx channels\n", nc);
	printf("   LOC      FLG M   C    DEST\n");
	for(cp=chan,loc=setup[SMPXC].value; cp < &chan[C]; cp++,loc=+sizeof(chan[0])) {
		if((cp->cflag&ALLOC) == 0)
			continue;
		printf("%7.1o ", loc);
		putf(cp->cflag&BLOCK, 'B');
		putf(cp->cflag&WWAIT, 'B');
		putf(cp->cflag&CRUN, 'R');
		putf(cp->cflag&RWAIT, 'W');
		putf(cp->cflag&ALLOC, 'A');
		putf(cp->cflag&DIS, 'D');
		putf(cp->cflag&DLY, 'D');
		printf(" %1d %3d ", mask(cp->m), mask(cp->c));
		printf("%7.1o ", cp->dest);
		printf("\n");
	}

	printf("%d mpx machines\n", M);
	printf("   LOC  FLG RCH RCN XCH XCN\n");
	for(mp=mach,loc=setup[SMPXM].value; mp < &mach[M]; mp++,loc=+sizeof(mach[0])) {
		printf("%7.1o ", loc);
		putf(mp->mflag&RNEXT, 'N');
		putf(mp->mflag&MRUN, 'R');
		putf(mp->mflag&XNEXT, 'N');
		printf(" %3d", mask(mp->rchan));
		printf(" %3d", mask(mp->rcount));
		printf(" %3d", mask(mp->xchan));
		printf(" %3d", mask(mp->xcount));
		for(nc=0; nc<128; nc++) {
			cp = mp->chanp[nc];
			if(cp == 0)
				continue;
			printf(" %d-%o", nc, cp);
		}
		printf("\n");
	}
	printf("%d mpx lines\n", M-1);
	printf("   LOC  RSQ XSQ AKF XMF STE TIM SUM\n");
	for(lp=line,loc=setup[SMPSM].value; lp < &line[M-1]; lp++, loc =+ sizeof(line[0])) {
		printf("%7.1o ", loc);
		printf("%3o ", lp->rseq);
		printf("%3o ", lp->xseq);
		printf("%3o ", lp->ackf);
		printf("%3o ", lp->xflag);
		printf("%3d ", lp->state);
		printf("%3d ", lp->time);
		printf("%7o\n", lp->sum);
	}
	printf("last characters recieved\n");
	nc = -1;
	loc = mptbc;
	for(;;) {
		if(nc != mptbuf[loc]) {
			if(nc >= 0)
				printf(")\n");
			nc = mptbuf[loc];
			printf("%d(", nc);
		} else
			printf(",");
		loc++;
		if(loc >= TBSIZ)
			loc = 0;
		if(loc == mptbc)
			break;
		printf("%o", mask(mptbuf[loc]));
		loc++;
		if(loc >= TBSIZ)
			loc = 0;
		if(loc == mptbc)
			break;
	}
	printf(")\n");
}
*********/
