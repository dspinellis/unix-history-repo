static char Sccsid[] = "am.c @(#)am.c	1.4	6/16/87 Berkeley ";
#include "apl.h"
#include <sgtty.h>
#ifdef VLIMIT
#include <sys/resource.h>
#endif

/* The following code is so system-dependent that it was moved to
 * a separate file.  It conditions the terminal for APL mode if called
 * with a non-zero argument, or returns it to its normal state if called
 * with a zero argument.
 */

extern int mkcore;


#ifndef PURDUE_EE

aplmod(n)
{
	static struct sgttyb m;

	if (n){
#ifdef VLIMIT
		if (!mkcore) {
			struct rlimit rl;

			getrlimit(RLIMIT_CORE, &rl);
			rl.rlim_cur = 0;
			setrlimit(RLIMIT_CORE, &rl);	/* no core file! */
		}
#endif
		gtty(0, &m);
		if ((m.sg_erase == '\b' || m.sg_kill == '\b') && !prwsflg)
			printf("[warning: erase char is ctl-h]\n");
	}
}

#else

#ifdef vax
#include <sys/ioctl.h>
static modesave;
static aplmode = LAPL;
static discsave;
static olddisc = OTTYDISC;
static turkey = LTURKEY;
static ctlech = LCTLECH;
#endif

aplmod(n)
{
	static struct sgttyb m;

	if(n){
		gtty(0, &m);
		if((m.sg_erase == '\b' || m.sg_kill == '\b') && !prwsflg)
			printf("[warning: erase char is ctl-h]\n");
#ifdef VLIMIT
		if (!mkcore) {
			struct rlimit rl;

			getrlimit(RLIMIT_CORE, &rl);
			rl.rlim_cur = 0;
			setrlimit(RLIMIT_CORE, &rl);	/* no core file! */
		}
#endif
#ifdef vax
		ioctl(0, TIOCLGET, &modesave);
		ioctl(0, TIOCLBIC, &ctlech);
		ioctl(0, TIOCLBIC, &turkey);
#else
		ioctl(0, TIOCNNOCTRL);
#endif
		/* Turn on apl mode if requested */
		if (apl_term){
#ifndef vax
			m.sg_flags |= APLMOD;
			stty(0,&m);
#else
			ioctl(0, TIOCGETD, &discsave);
			ioctl(0, TIOCSETD, &olddisc);
			ioctl(0, TIOCLBIS, &aplmode);
#endif
			printf("set terminal to apl mode\n");
		}
	} else {
		/* Turn off apl mode */
#ifndef vax
		ioctl(0, TIOCSNOCTRL);
		gtty(0, &m);
		m.sg_flags &= ~APLMOD;
		stty(0,&m);
#else
		if (apl_term)
			ioctl(0, TIOCSETD, &discsave);
		ioctl(0, TIOCLSET, &modesave);
#endif
	}
}
#endif
