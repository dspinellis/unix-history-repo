struct proc {
	char	p_stat;
	char	p_flag;
	char	p_pri;
	char	p_sig;
	char	p_uid;
	char	p_time;
	int	p_ttyp;
	int	p_pid;
	int	p_ppid;
	int	p_addr;
	int	p_size;
	int	p_wchan;
	int	*p_textp;
} proc[NPROC];

/* stat codes */
#define	SSLEEP	1
#define	SWAIT	2
#define	SRUN	3
#define	SIDL	4
#define	SZOMB	5

/* flag codes */
#define	SLOAD	01
#define	SSYS	02
#define	SLOCK	04
#define	SSWAP	010
