/*	trap.c	4.10	84/02/09	*/

#include "../machine/psl.h"
#include "../machine/reg.h"
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../machine/trap.h"
#include "../h/acct.h"
#include "../h/kernel.h"
#include "../machine/mtpr.h"
#ifdef SYSCALLTRACE
#include "../sys/syscalls.c"
#endif
#include "../machine/fp_in_krnl.h"

#define	USER	040		/* user-mode flag added to type */

struct	sysent	sysent[];
int nsysent;

/*
 * Called from the trap handler when a processor trap occurs.
 */
trap(sp, type, hfs, accmst, acclst, dbl, code, pc, psl)
unsigned code;
{
	/* Next 2 dummy variables MUST BE the first local */
	/* variables; leaving place for registers 0 and 1 */
	/* which are not preserved by the 'cct' */

	int	dumm1;		/* register 1 */
	int	dumm0;		/* register 0 */
	register dumm3;		/* register 12 is the 1'st register variable */
				/* in TAHOE  (register 11 in VAX) */

	register int *locr0 = ((int *)&psl)-PS;
	register int i;
	register struct proc *p;
	struct timeval syst;
	char	*typename;

	syst = u.u_ru.ru_stime;
	if (USERMODE(locr0[PS])) {
		type |= USER;
		u.u_ar0 = locr0;
	}
	switch (type) {

	default: switch (type) {
		case T_RESADFLT:
			typename = "reserved addressing mode";break;
		case T_PRIVINFLT:
			typename = "illegal opcode";break;
		case T_RESOPFLT:
			typename = "reserved operand";break;
		case T_BPTFLT:
			typename = "breakpoint";break;
		case T_SYSCALL:
			typename = "kernel call";break;
		case T_ARITHTRAP:
			typename = "arithmetic exception";break;
		case T_ASTFLT:
			typename = "system forced exception";break;
		case T_SEGFLT:
			typename = "limit fault";break;
		case T_PROTFLT:
			typename = "illegal access type";break;
		case T_TRCTRAP:
			typename = "trace trap";break;
		case T_PAGEFLT:
			typename = "page fault";break;
		case T_TABLEFLT:
			typename = "page table fault";break;
		case T_ALIGNFLT:
			typename = "alignment fault";break;
		case T_KSPNOTVAL:
			typename = "kernel stack not valid";break;
		}
		printf("System trap (%s), code = %x, pc = %x\n", 
				typename, code, pc);
		panic("trap");

	case T_PROTFLT + USER:	/* protection fault */
		i = SIGBUS;
		break;

	case T_PRIVINFLT + USER:	/* privileged instruction fault */
	case T_RESADFLT + USER:	/* reserved addressing fault */
	case T_RESOPFLT + USER:	/* resereved operand fault */
	case T_ALIGNFLT + USER:	/* unaligned data fault */
		u.u_code = type &~ USER;
		i = SIGILL;
		break;

	case T_ASTFLT + USER:	/* Allow process switch */
	case T_ASTFLT:
		astoff();
		if ((u.u_procp->p_flag & SOWEUPC) && u.u_prof.pr_scale) {
			addupc(pc, &u.u_prof, 1);
			u.u_procp->p_flag &= ~SOWEUPC;
		}
		goto out;

	case T_ARITHTRAP + USER:
		u.u_code = code;
		i = SIGFPE;
		break;

	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case T_SEGFLT + USER:
		if (grow((unsigned)locr0[SP]) || grow(code))
			goto out;
		i = SIGSEGV;
		break;

	case T_TABLEFLT:		/* allow page table faults in kernel mode */
	case T_TABLEFLT + USER:   /* page table fault */
		panic("ptable fault");

	case T_PAGEFLT:		/* allow page faults in kernel mode */
	case T_PAGEFLT + USER:	/* page fault */
		i = u.u_error;
		if(fastreclaim(code) == 0)
			pagein(code, 0);
		u.u_error = i;
		if (type == T_PAGEFLT)
			return;
		goto out;

	case T_BPTFLT + USER:	/* bpt instruction fault */
	case T_TRCTRAP + USER:	/* trace trap */
		locr0[PS] &= ~PSL_T;
		i = SIGTRAP;
		break;
	case T_KSPNOTVAL:
	case T_KSPNOTVAL + USER:
		i = SIGKILL;	/* There is nothing to do but to kill the 
				 * process.. */
		printf("KSP NOT VALID.\n");
		break;

	}
	psignal(u.u_procp, i);
out:
	p = u.u_procp;
	if (p->p_cursig || ISSIG(p))
		psig();
	p->p_pri = p->p_usrpri;
	if (runrun) {
		/*
		 * Since we are u.u_procp, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) spl8();
		setrq(p);
		u.u_ru.ru_nivcsw++;
		swtch();
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(locr0[PC], &u.u_prof, ticks);
	}
	curpri = p->p_pri;
}

#ifdef SYSCALLTRACE
int syscalltrace = 0;
#endif

/*
 * Called from the trap handler when a system call occurs
 */
syscall(sp, type, hfs, accmst, acclst, dbl, code, pc, psl)
unsigned code;
{
	/* Next 2 dummy variables MUST BE the first local */
	/* variables; leaving place for registers 0 and 1 */
	/* which are not preserved by the 'cct' */

	int	dumm1;		/* register 1 */
	int	dumm0;		/* register 0 */
	register dumm3;		/* register 12 is the 1'st register variable */
				/* in TAHOE  (register 11 in VAX) */

	register int *locr0 = ((int *)&psl)-PS;
	register caddr_t params;		/* known to be r10 below */
	register int i;				/* known to be r9 below */
	register struct sysent *callp;
	register struct proc *p;
	struct	timeval syst;
	int opc;

	syst = u.u_ru.ru_stime;
	if (!USERMODE(locr0[PS]))
		panic("syscall");
	u.u_ar0 = locr0;
	if (code == 139) {			/* XXX */
		sigcleanup();			/* XXX */
		goto done;			/* XXX */
	}
	params = (caddr_t)locr0[FP] + NBPW;
	u.u_error = 0;
	/*------ DIRTY CODE !!!!!!!!!---------*/
	/* try to reconstruct pc, assuming code is an immediate constant */
	opc = pc - 2;		/* short literal */
	if (code > 0x3f) {
		opc--;	/* byte immediate */
		if (code > 0x7f) {
			opc--;	/* word immediate */
			if (code > 0x7fff)
				opc -= 2;	/* long immediate */
		}
	}
	/*------------------------------------*/
	callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	if (callp == sysent) {
		i = fuword(params);
		params += NBPW;
	callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	}
	if (i = callp->sy_narg * sizeof (int)) {
		asm("prober $1,(r10),r9");		/* GROT */
		asm("bnequ ok");			/* GROT */
		u.u_error = EFAULT;			/* GROT */
		goto bad;				/* GROT */
asm("ok:");						/* GROT */
		bcopy(params,u.u_arg,i);
	}
	u.u_ap = u.u_arg;
	u.u_dirp = (caddr_t)u.u_arg[0];
	u.u_r.r_val1 = 0;
	u.u_r.r_val2 = locr0[R1]; /*------------ CHECK again */
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0 && u.u_eosys == JUSTRETURN)
			u.u_error = EINTR;
	} else {
		u.u_eosys = JUSTRETURN;
#ifdef SYSCALLTRACE
		if (syscalltrace) {
			register int i;
			char *cp;

			if (code >= nsysent)
				printf("0x%x", code);
			else
				printf("%s", syscallnames[code]);
			cp = "(";
			for (i= 0; i < callp->sy_narg; i++) {
				printf("%s%x", cp, u.u_arg[i]);
				cp = ", ";
			}
			if (i)
				putchar(')', 0);
			putchar('\n', 0);
		}
#endif

		(*(callp->sy_call))();
	}
	if (u.u_eosys == RESTARTSYS)
		pc = opc;
	else if (u.u_error) {
bad:
		locr0[R0] = u.u_error;
		locr0[PS] |= PSL_C;	/* carry bit */
	} else {
		locr0[PS] &= ~PSL_C;	/* clear carry bit */
		locr0[R0] = u.u_r.r_val1;
		locr0[R1] = u.u_r.r_val2;
	}
done:
	p = u.u_procp;
	if (p->p_cursig || ISSIG(p))
		psig();
	p->p_pri = p->p_usrpri;
	if (runrun) {
		/*
		 * Since we are u.u_procp, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) spl8();
		setrq(p);
		u.u_ru.ru_nivcsw++;
		swtch();
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(locr0[PC], &u.u_prof, ticks);
	}
	curpri = p->p_pri;
}

/*
 * nonexistent system call-- signal process (may want to handle it)
 * flag error if process won't see signal immediately
 * Q: should we do that all the time ??
 */
nosys()
{
	if (u.u_signal[SIGSYS] == SIG_IGN || u.u_signal[SIGSYS] == SIG_HOLD)
		u.u_error = EINVAL;
	psignal(u.u_procp, SIGSYS);
}

/*
 * Ignored system call
 */
nullsys()
{

}

fpemulate(hfsreg,acc_most,acc_least,dbl,op_most,op_least,opcode,pc,psl)
{
/*
 * Emulate the F.P. 'opcode'. Update psl flags as necessary.
 * If all OK, set 'opcode' to 0, else to the F.P. exception #.
 * Not all parameter longwords are relevant - depends on opcode.
 *
 * The entry mask is set so ALL registers are saved - courtesy of
 *  locore.s. This enables F.P. opcodes to change 'user' registers
 *  before return.
 */

 /* WARNING!!!! THIS CODE MUST NOT PRODUCE ANY FLOATING POINT EXCEPTIONS. */

	/* Next 2 dummy variables MUST BE the first local */
	/* variables; leaving place for registers 0 and 1 */
	/* which are not preserved by the 'cct' */

	int	dumm1;		/* register 1 */
	int	dumm0;		/* register 0 */
	register dumm3;		/* register 12 is the 1'st register variable */
				/* in TAHOE  (register 11 in VAX) */

	register int *locr0 = ((int *)&psl)-PS; /* R11 */
	int hfs = 0; 			/* returned data about exceptions */
	float (*f_proc)();		/* fp procedure to be called.	*/
	double (*d_proc)();		/* fp procedure to be called.	*/
	int dest_type;			/* float or double.	*/
	union{
		float ff;			/* float result. 	*/
		int fi;
	}f_res;
	union{
		double	dd;			/* double result.	*/
		int	di[2] ;
	}d_res;
	extern float 	Kcvtlf(), Kaddf(), Ksubf(), Kmulf(), Kdivf();
	extern double 	Kcvtld(), Kaddd(), Ksubd(), Kmuld(), Kdivd();
	extern float   	Ksinf(), Kcosf(), Katanf(), Klogf(), Ksqrtf(), Kexpf();
	
	

	switch(opcode & 0x0FF){

	case CVLF:	f_proc = Kcvtlf; dest_type = FLOAT; 
			locr0[PS] &= ~PSL_DBL;break;	/* clear double bit */
	case CVLD:	d_proc = Kcvtld; dest_type = DOUBLE; 
			locr0[PS] |= PSL_DBL; break;	/* turn on double bit */
	case ADDF:	f_proc = Kaddf; dest_type = FLOAT;
			break;
	case ADDD:	d_proc = Kaddd; dest_type = DOUBLE;
			break;
	case SUBF:	f_proc = Ksubf; dest_type = FLOAT;
			break;
	case SUBD:	d_proc = Ksubd; dest_type = DOUBLE;
			break;
	case MULF:	f_proc = Kmulf; dest_type = FLOAT;
			break;
	case MULD:	d_proc = Kmuld; dest_type = DOUBLE;
			break;
	case DIVF:	f_proc = Kdivf; dest_type = FLOAT;
			break;
	case DIVD:	d_proc = Kdivd; dest_type = DOUBLE;
			break;
	case SINF:	f_proc = Ksinf; dest_type = FLOAT;
			break;
	case COSF:	f_proc = Kcosf; dest_type = FLOAT;
			break;
	case ATANF:	f_proc = Katanf; dest_type = FLOAT;
			break;
	case LOGF:	f_proc = Klogf; dest_type = FLOAT;
			break;
	case SQRTF:	f_proc = Ksqrtf; dest_type = FLOAT;
			break;
	case EXPF:	f_proc = Kexpf; dest_type = FLOAT;
			break;
	}

	switch(dest_type){

	case FLOAT: 
		f_res.ff = (*f_proc)(acc_most,acc_least,op_most,op_least,&hfs);

		if (f_res.fi == 0 ) locr0[PS] |= PSL_Z;
		if (f_res.fi < 0 ) locr0[PS] |= PSL_N;
		break;
	case DOUBLE:
		d_res.dd = (*d_proc)(acc_most,acc_least,op_most,op_least,&hfs);
		if ((d_res.di[0] == 0) && (d_res.di[1] == 0))
						locr0[PS] |= PSL_Z;
		if (d_res.di[0] < 0 ) locr0[PS] |= PSL_N;
		break;
	}

	if (hfs & HFS_OVF){
		locr0[PS] |= PSL_V;	/* turn on overflow bit */
		/* if (locr0[PS] & PSL_IV)   {  /* overflow elabled?	*/
			opcode = OVF_EXC;
			u.u_error = (hfs & HFS_DOM) ? EDOM : ERANGE;
			return;
		/*}*/
	}
	else if (hfs & HFS_UNDF){
		if (locr0[PS] & PSL_FU){  /* underflow elabled?	*/
			opcode = UNDF_EXC;
			u.u_error = (hfs & HFS_DOM) ? EDOM : ERANGE;
			return;
		} 
	}
	else if (hfs & HFS_DIVZ){
		opcode = DIV0_EXC;
		return;
	}
	else if (hfs & HFS_DOM)
		u.u_error = EDOM;
	else if (hfs & HFS_RANGE)
		u.u_error = ERANGE;

	switch(dest_type){
	case FLOAT:
		if ((hfs & HFS_OVF) || (hfs & HFS_UNDF)) {
			f_res.ff = 0.0;
			locr0[PS] |= PSL_Z;
		}
		mvtofacc(f_res.ff, &acc_most);
		break;
	case DOUBLE:
		if ((hfs & HFS_OVF) || (hfs & HFS_UNDF)) {
			d_res.dd = 0.0;
			locr0[PS] |= PSL_Z;
		}
		mvtodacc(d_res.di[0], d_res.di[1], &acc_most);
		break;
	}
	opcode=0;
}
