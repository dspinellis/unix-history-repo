#ifdef MAIN
static char h_sccsid[] = "@(#)hyr_sym.h	2.1 Hyperchannel Routing Daemon 82/11/29";
#endif

struct sym {
	int	s_flags;
	char	s_name[32];
	long	s_lastok;
	long	s_fulladdr;
	unsigned short	s_dst;
	unsigned short	s_ctl;
	unsigned short	s_access;
	unsigned short	s_ngate;
	struct sym	*s_next;
	struct sym	*s_gate[32];
};

#define	HS_DIR		0x01
#define	HS_INDIR	0x02
#define	HS_GATE		0x04

	extern struct sym *curgate;
	extern struct sym *sym_head;
	extern int lex_error;
