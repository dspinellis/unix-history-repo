#ifdef LOCORE
#define	P_LINK 0
#define	P_RLINK 4
#define	P_XLINK 108
#define	P_ADDR 16
#define	P_PRI 21
#define	P_STAT 23
#define	P_WCHAN 96
#define	P_TSIZE 68
#define	P_DSIZE 72
#define	P_SSIZE 76
#define	P_P0BR 104
#define	P_SZPT 66
#define	P_TEXTP 100
#define	P_FLAG 44
#define	SSLEEP 1
#define	SRUN 3
#define	UBA_BRRVR 48
#define	UH_UBA 0
#define	UH_VEC 8
#define	UH_SIZE 64
#define	RP_FLAG 12
#define	X_CADDR 64
#define	V_SWTCH 0
#define	V_TRAP 4
#define	V_SYSCALL 8
#define	V_INTR 12
#define	V_SOFT 16
#define	V_PDMA 20
#define	V_FAULTS 92
#define	V_PGREC 52
#define	V_FASTPGREC 112
#define	UPAGES 10
#define	CLSIZE 2
#define	SYSPTSIZE 3584
#define	USRPTSIZE 4096
#define	MSGBUFPTECNT 8
#define	NMBCLUSTERS 256
#define	U_PROCP 128
#define	U_RU 1640
#define	RU_MINFLT 32
#else
asm(".set	U_ARG,156");
asm(".set	U_QSAVE,192");
#endif
