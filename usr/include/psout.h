/*
 * psout: structure output by 'ps -r'.
 * Most fields are copies of the proc (p_...) or user (u_...)
 * structures for the given process, see <sys/user.h> & <sys/proc.h>
 */

#ifndef makedev
# include <sys/types.h>
#endif

struct psout {
	dev_t o_ttyd;		/* u_ttyd */
	short o_pid;		/* p_pid */
	char o_tty[2];		/* 1st 2 chars of tty name with 'tty' stripped, if present */
	char o_flag;		/* p_flag */
	char o_stat;		/* p_stat */
	short o_uid;		/* p_uid */
	char o_uname[9];	/* login name of process owner */
	short o_ppid;		/* p_ppid */
	char o_cpu;		/* p_cpu */
	float o_pctcpu;		/* ??? */
	char o_pri;		/* p_pri */
	char o_nice;		/* p_nice */
	short o_addr0;		/* p_addr[0] */
	short o_size;		/* p_size */
	caddr_t o_wchan;	/* p_wchan */
	time_t o_utime;		/* u_utime */
	time_t o_stime;		/* u_stime */
	time_t o_cutime;	/* u_cutime */
	time_t o_cstime;	/* u_cstime */
	short int o_pgrp;	/* p_pgrp */
	size_t o_dsize;		/* p_dsize */
	size_t o_ssize;		/* p_ssize */
	size_t o_rssize;	/* p_rssize */
	char o_time;		/* p_time */
	char o_slptime;		/* p_slptime */
	unsigned o_stksize;	/* computed - size of stack */
	int o_minorflt;		/* u_minorflt */
	int o_majorflt;		/* u_majorflt */
	short o_aveflt;		/* p_aveflt */
	char *o_text;		/* p_textp */
	size_t o_swrss;		/* p_swrss */
	size_t o_xsize;		/* xp_size from text.h */
	short o_xrssize;	/* xp_rssize from text.h */
	int o_sigs;		/* sum of SIGINT & SIGQUIT, if == 2 proc is ignoring both.*/
	char o_comm[15];	/* u_comm */
	char o_args[128];	/* best guess at args to process */
};
