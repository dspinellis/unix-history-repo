/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)genassym.c	7.10 (Berkeley) %G%
 */

#define KERNEL
#define	VAX780	1
#define	VAX750	1
#define	VAX730	1
#define	VAX630	1
#define	VAX650	1

#include "../include/pte.h"

#include "sys/param.h"
#include "sys/buf.h"
#include "sys/vmmeter.h"
#include "sys/vmparam.h"
#include "sys/user.h"
#include "sys/cmap.h"
#include "sys/map.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"
#include "sys/proc.h"
#include "sys/text.h"
#include "rpb.h"
#include "sys/mbuf.h"
#include "sys/msgbuf.h"

struct uba_hd uba_hd[1];

main()
{
	register struct proc *p = (struct proc *)0;
	register struct uba_regs *uba = (struct uba_regs *)0;
	register struct uba_hd *uh = (struct uba_hd *)0;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register struct user *up = (struct user *)0;
	register struct rusage *rup = (struct rusage *)0;
	struct rpb *rp = (struct rpb *)0;
	struct text *tp = (struct text *)0;

	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_XLINK %d\n", &p->p_xlink);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_TSIZE %d\n", &p->p_tsize);
	printf("#define\tP_DSIZE %d\n", &p->p_dsize);
	printf("#define\tP_SSIZE %d\n", &p->p_ssize);
	printf("#define\tP_P0BR %d\n", &p->p_p0br);
	printf("#define\tP_SZPT %d\n", &p->p_szpt);
	printf("#define\tP_TEXTP %d\n", &p->p_textp);
	printf("#define\tP_FLAG %d\n", &p->p_flag);
	printf("#define\tSSLEEP %d\n", SSLEEP);
	printf("#define\tSRUN %d\n", SRUN);
	printf("#define\tUBA_BRRVR %d\n", uba->uba_brrvr);
	printf("#define\tUH_UBA %d\n", &uh->uh_uba);
	printf("#define\tUH_VEC %d\n", &uh->uh_vec);
	printf("#define\tUH_SIZE %d\n", sizeof (struct uba_hd));
	printf("#define\tRP_FLAG %d\n", &rp->rp_flag);
	printf("#define\tX_CADDR %d\n", &tp->x_caddr);
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tV_TRAP %d\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %d\n", &vm->v_syscall);
	printf("#define\tV_INTR %d\n", &vm->v_intr);
	printf("#define\tV_SOFT %d\n", &vm->v_soft);
	printf("#define\tV_PDMA %d\n", &vm->v_pdma);
	printf("#define\tV_FAULTS %d\n", &vm->v_faults);
	printf("#define\tV_PGREC %d\n", &vm->v_pgrec);
	printf("#define\tV_FASTPGREC %d\n", &vm->v_fastpgrec);
	printf("#define\tUPAGES %d\n", UPAGES);
	printf("#define\tCLSIZE %d\n", CLSIZE);
	printf("#define\tMAXPHYS %d\n", MAXPHYS);
	printf("#define\tSYSPTSIZE %d\n", SYSPTSIZE);
	printf("#define\tUSRPTSIZE %d\n", USRPTSIZE);
	printf("#define\tMSGBUFPTECNT %d\n", btoc(sizeof (struct msgbuf)));
	printf("#define\tMCLBYTES %d\n", MCLBYTES);
	printf("#define\tNMBCLUSTERS %d\n", NMBCLUSTERS);
	printf("#define\tNKMEMCLUSTERS %d\n", NKMEMCLUSTERS);
#ifdef SYSVSHM
	printf("#define\tSHMMAXPGS %d\n", SHMMAXPGS);
#endif
	printf("#define\tU_PROCP %d\n", &up->u_procp);
	printf("#define\tU_RU %d\n", &up->u_ru);
	printf("#define\tRU_MINFLT %d\n", &rup->ru_minflt);
	printf("#define\tSZ_CMAP %d\n", sizeof(struct cmap));
	exit(0);
}
