/*
 * Definition of signal handler frame.
 */

struct sigframe {
	int	sf_signum;
	int	sf_code;
	struct	sigcontext *sf_scp;
	int	(*sf_handler)();
	int	r1;
	int 	r0;
	struct	sigcontext *sf_scpcopy;
}; 

