/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#ifdef	BSD_WAIT
# include <sys/wait.h>
# define w_termsignum(w)	((w).w_termsig)
#else
# define WIFSTOPPED(w)		(((w).w_status & 0377) == 0177)
# define WIFEXITED(w)		(((w).w_status & 0377) == 0)
# define WIFSIGNALED(w)		((((w).w_status >> 8) & 0377) == 0)
# define w_termsignum(w)	((w).w_status & 0177)
# define wait2(w, x)		wait(w)

union wait {
	int	w_status;
};
#endif

extern void
	kill_off proto((int, union wait));
