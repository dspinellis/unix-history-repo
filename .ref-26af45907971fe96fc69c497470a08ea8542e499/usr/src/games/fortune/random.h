/* $Header: random.h,v 1.2 87/11/21 17:19:53 arnold Exp $ */

#ifdef SYSV
# define	srandom	srand48
# define	random	lrand48
# ifdef NO_RANDOM
#	undef	NO_RANDOM
# endif
#endif

void	srnd();

long	rnd();
