#ifdef MAIN
static char h_rcsid[] = "$Header: hyr_sym.h,v 2.3 84/01/29 18:25:05 steveg Exp $$Locker:  $";
#endif

struct sym {
	int	sym_flags;
	char	sym_name[32];
	char	sym_nickname[32];
	long	sym_lastok;
	struct in_addr	sym_inaddr;
	unsigned short	sym_dst;
	unsigned short	sym_ctl;
	unsigned short	sym_access;
	unsigned short	sym_ngate;
	struct sym	*sym_next;
	struct sym	*sym_gate[32];
};

#define	HS_DIR		0x01
#define	HS_INDIR	0x02
#define	HS_GATE		0x04
#define	HS_LOOP		0x08
#define	HS_RLOOP	0x10

	extern struct sym *curgate;
	extern struct sym *sym_head;
	extern int lex_error;
