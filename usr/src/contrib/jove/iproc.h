/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

struct process {
	Process	*p_next;
#ifdef PIPEPROCS
	int	p_toproc,	/* read p_fromproc and write p_toproc */
		p_portpid,	/* pid of child (the portsrv) */
		p_pid;		/* pid of real child i.e. not portsrv */
#else
	int	p_fd,		/* file descriptor of pty? opened r/w */
		p_pid;		/* pid of child (the shell) */
#endif
	Buffer	*p_buffer;	/* add output to end of this buffer */
	char	*p_name;	/* ... */
	char	p_state,	/* State */
		p_howdied,	/* Killed? or Exited? */
		p_reason;	/* If signaled, p_reason is the signal; else
				   it is the the exit code */
	Mark	*p_mark;	/* where output left us */
	char	p_dbx_mode;	/* whether to parse output for file/lineno
				   pairs */
};

extern int  NumProcs;

#ifdef PIPEPROCS
extern File	*ProcInput;
extern int	kbd_pid;
#else
extern long global_fd;
#endif

extern void
	read_proc proto((int)),
	pinit proto((void)),
	KillProcs proto((void)),
	pbuftiedp proto((Buffer *));

extern char *
	pstate proto((Process *));
