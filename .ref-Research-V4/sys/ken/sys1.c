#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/text.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/inode.h"

char regloc[8];

exec()
{
	int ap, c, na, nc, *bp, *ip;
	int ts, ds;
	register char *cp;
	extern uchar;

	/*
	 * pick up file names
	 * and check various modes
	 * for execute permission
	 */

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	bp = getblk(NODEV);
	if(access(ip, IEXEC))
		goto bad;

	/*
	 * pack up arguments into
	 * allocated disk buffer
	 */

	cp = bp->b_addr;
	na = 0;
	nc = 0;
	while(ap = fuword(u.u_arg[1])) {
		na++;
		if(ap == -1)
			goto bad;
		u.u_arg[1] =+ 2;
		for(;;) {
			c = fubyte(ap++);
			if(c == -1)
				goto bad;
			*cp++ = c;
			nc++;
			if(nc > 510) {
				u.u_error = E2BIG;
				goto bad;
			}
			if(c == 0)
				break;
		}
	}
	if((nc&1) != 0) {
		*cp++ = 0;
		nc++;
	}

	/*
	 * read in first 8 bytes
	 * of file for segment
	 * sizes:
	 * w0 = 407/410 (410 implies RO text)
	 * w1 = text size
	 * w2 = data size
	 * w3 = bss size
	 */

	u.u_base = &u.u_arg[0];
	u.u_count = 8;
	u.u_offset[1] = 0;
	u.u_offset[0] = 0;
	u.u_segflg = 1;
	readi(ip);
	u.u_segflg = 0;
	if(u.u_error)
		goto bad;
	if(u.u_arg[0] == 0407) {
		u.u_arg[2] =+ u.u_arg[1];
		u.u_arg[1] = 0;
	} else
	if(u.u_arg[0] != 0410) {
		u.u_error = ENOEXEC;
		goto bad;
	}
	cp = ip;
	if(u.u_arg[1]!=0 && (cp->i_flag&ITEXT)==0 && cp->i_count!=1) {
		u.u_error = ETXTBSY;
		goto bad;
	}

	/*
	 * find text and data sizes
	 * try them out for possible
	 * exceed of max sizes
	 */

	ts = ((u.u_arg[1]+63)>>6) & 01777;
	ds = ((u.u_arg[2]+u.u_arg[3]+63)>>6) & 01777;
	if(estabur(ts, ds, SSIZE))
		goto bad;

	/*
	 * allocate and clear core
	 * at this point, committed
	 * to the new image
	 */

	xfree();
	xalloc(ip);
	c = USIZE+ds+SSIZE;
	expand(USIZE);
	expand(c);
	while(--c >= USIZE)
		clearseg(u.u_procp->p_addr+c);

	/*
	 * read in data segment
	 */

	estabur(0, ds, 0);
	u.u_base = 0;
	u.u_offset[1] = 020+u.u_arg[1];
	u.u_count = u.u_arg[2];
	readi(ip);

	/*
	 * initialize stack segment
	 */

	u.u_tsize = ts;
	u.u_dsize = ds;
	u.u_ssize = SSIZE;
	estabur(u.u_tsize, u.u_dsize, u.u_ssize);
	cp = bp->b_addr;
	ap = -nc - na*2 - 4;
	u.u_ar0[R6] = ap;
	suword(ap, na);
	c = -nc;
	while(na--) {
		suword(ap=+2, c);
		do
			subyte(c++, *cp);
		while(*cp++);
	}
	suword(ap+2, -1);

	/*
	 * set SUID/SGID protections
	 */

	cp = ip;
	if(cp->i_mode&ISUID)
		if(u.u_uid != 0)
			u.u_uid = cp->i_uid;
	if(cp->i_mode&ISGID)
		u.u_gid = cp->i_gid;

	/*
	 * clear sigs, regs and return
	 */

	for(c=0; c<NSIG; c++)
		if((u.u_signal[c]&1) == 0)
			u.u_signal[c] = 0;
	for(c=0; c<6; c++)
		u.u_ar0[regloc[c]] = 0;
	u.u_ar0[R7] = 0;
	for(c=0; c<25; c++)
		u.u_fsav[c] = 0;

bad:
	iput(ip);
	brelse(bp);
}

rexit()
{

	u.u_arg[0] = u.u_ar0[R0] << 8;
	exit();
}

exit()
{
	int i, a, *p;
	register int *q1, *q2;

	for(i=0; i<NSIG; i++)
		u.u_signal[i] = 1;
	for(i=0; i<NOFILE; i++)
		if(p = u.u_ofile[i]) {
			closef(p);
			u.u_ofile[i] = NULL;
		}
	iput(u.u_cdir);
	xfree();
	a = malloc(swapmap, 8);
	p = getblk(SWAPDEV, a);
	q1 = p->b_addr;
	q2 = &u;
	for(i=0; i<256; i++)
		*q1++ = *q2++;
	bwrite(p);
	p = u.u_procp;
	mfree(coremap, p->p_size, p->p_addr);
	p->p_addr = a;
	p->p_stat = SZOMB;

loop:
	for(i=0; i<NPROC; i++)
	if(p->p_ppid == proc[i].p_pid) {
		wakeup(&proc[1]);
		wakeup(&proc[i]);
		for(i=0; i<NPROC; i++)
		if(p->p_pid == proc[i].p_ppid)
			proc[i].p_ppid  = 1;
		swtch();
		/* no return */
	}
	if(p->p_ppid == 1)
		panic("no init proc");
	p->p_ppid = 1;
	goto loop;
}

wait()
{
	int i, f, *bp;
	register struct proc *p;

	f = 0;

loop:
	p = &proc[0];
	for(i=0; i<NPROC; i++) {
		if(p->p_ppid == u.u_procp->p_pid) {
			f++;
			if(p->p_stat == SZOMB) {
				u.u_ar0[R0] = p->p_pid;
				p->p_stat = NULL;
				p->p_pid = 0;
				p->p_ppid = 0;
				p->p_sig = 0;
				p->p_ttyp = 0;
				p->p_flag = 0;
				bp = bread(SWAPDEV, i=p->p_addr);
				mfree(swapmap, 8, i);
				p = bp->b_addr;
				u.u_cstime[0] =+ p->u_cstime[0];
				dpadd(u.u_cstime, p->u_cstime[1]);
				dpadd(u.u_cstime, p->u_stime);
				u.u_cutime[0] =+ p->u_cutime[0];
				dpadd(u.u_cutime, p->u_cutime[1]);
				dpadd(u.u_cutime, p->u_utime);
				u.u_ar0[R1] = p->u_arg[0];
				brelse(bp);
				return;
			}
		}
		p++;
	}
	if(f) {
		sleep(u.u_procp, PWAIT);
		goto loop;
	}
	u.u_error = ECHILD;
}

fork()
{
	int i;
	struct proc *p1, *p2;

	p1 = u.u_procp;
	p2 = &proc[0];
	for(i=0; i<NPROC; i++) {
		if(p2->p_stat == NULL)
			goto found;
		p2++;
	}
	u.u_error = EAGAIN;
	goto out;

found:
	if(newproc()) {
		u.u_ar0[R0] = p1->p_pid;
		u.u_cstime[0] = 0;
		u.u_cstime[1] = 0;
		u.u_stime = 0;
		u.u_cutime[0] = 0;
		u.u_cutime[1] = 0;
		u.u_utime = 0;
		return;
	}
	u.u_ar0[R0] = p2->p_pid;

out:
	u.u_ar0[R7] =+ 2;
}

sbreak()
{
	int a, i, n, d;

	/*
	 * set n to new data size
	 * set d to new-old
	 * set n to new total size
	 */

	n = (((u.u_arg[0]+63)>>6) & 01777) - nseg(u.u_tsize)*128;
	if(n < 0)
		n = 0;
	d = n - u.u_dsize;
	n =+ USIZE+u.u_ssize;
	if(estabur(u.u_tsize, u.u_dsize+d, u.u_ssize))
		return;
	u.u_dsize =+ d;
	if(d > 0)
		goto bigger;
	a = u.u_procp->p_addr + n - u.u_ssize;
	for(i=0; i<u.u_ssize; i++) {
		copyseg(a-d, a);
		a++;
	}
	expand(n);
	return;

bigger:
	expand(n);
	a = u.u_procp->p_addr + n;
	for(i=0; i<u.u_ssize; i++) {
		a--;
		copyseg(a-d, a);
	}
	while(d--)
		clearseg(--a);
}
