/*
 * config.h	1.5	81/05/22
 * Definitions that everybody needs to know
 */

#define eq(a,b) (strcmp(a,b) == 0)
#define TRUE 1
#define FALSE 0
#define DRIVER 1
#define NORMAL 2
#define INVISIBLE 3

#define TO_NEXUS -1

struct file_list {
	char *f_fn;
	int f_type;
	char *f_next;
	char *f_needs;
};

struct	idlst {
	char *id;
	struct idlst *id_next;
};

typedef char bool;

struct device {
	int d_type;			/* CONTROLLER, DEVICE, UBA or MBA */
	struct device *d_conn;		/* What it is connected to */
	char *d_name;			/* Name of device (e.g. rk11) */
	struct idlist *d_vec;		/* Interrupt vectors */
	int d_addr;			/* Address of csr */
	int d_unit;			/* Unit number */
	int d_drive;			/* Drive number */
	int d_slave;			/* Slave number */
#define QUES	-1	/* -1 means '?' */
#define	UNKNOWN -2	/* -2 means not set yet */
	bool d_dk;			/* if init 1 set to number for iostat */
	int d_flags;			/* Flags for device init */
	struct device *d_next;		/* Next one in list */
};

struct config {
	char *c_dev;
	char *s_sysname;
};

struct cputype {
	char *cpu_name;
	struct cputype *cpu_next;
} *cputype;
struct opt {
	char *op_name;
	struct cputype *op_next;
} *opt;
char *ident, *ns(), *malloc(), *tc(), *qu();
bool do_trace, seen_mba, seen_uba;
struct device *connect();
struct device *dtab;
char errbuf[80];
int yyline;
struct file_list *ftab, *conf_list, *confp;
char *PREFIX;
int hz, timezone, hadtz, maxusers, dst;
