#ifdef LOCORE
#define	P_LINK 0
#define	P_RLINK 4
#define	P_XLINK 100
#define	P_ADDR 8
#define	P_PRI 13
#define	P_STAT 15
#define	P_WCHAN 88
#define	P_TSIZE 60
#define	P_SSIZE 68
#define	P_P0BR 96
#define	P_SZPT 58
#define	P_TEXTP 92
#define	P_FLAG 36
#define	SSLEEP 1
#define	SRUN 3
#define	UBA_BRRVR 48
#define	UH_UBA 0
#define	UH_VEC 8
#define	UH_SIZE 52
#define	RP_FLAG 12
#define	X_CADDR 56
#define	V_SWTCH 0
#define	V_TRAP 4
#define	V_SYSCALL 8
#define	V_INTR 12
#define	V_PDMA 16
#define	V_FAULTS 88
#define	V_PGREC 48
#define	V_FASTPGREC 108
#define	UPAGES 8
#define	CLSIZE 2
#define	SYSPTSIZE 3584
#define	USRPTSIZE 1024
#define	MSGBUFPTECNT 8
#define	NMBCLUSTERS 256
#define	U_PROCP 124
#define	U_RU 1296
#define	RU_MINFLT 32
#else
asm(".set	U_ARG,388");
asm(".set	U_QSAVE,-1881318947");
#endif
