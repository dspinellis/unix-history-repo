/*	config.h	1.9	82/10/24	*/

#define	machinename	machname

/*
 * Definitions for config.
 */

/*
 * Structures representing objects in config's world.
 */

#define TO_NEXUS	-1

struct file_list {
	char	*f_fn;			/* the name */
	int	f_type;			/* see below */
	struct	file_list *f_next;	
	char	*f_needs;
};
#define DRIVER		1
#define NORMAL		2
#define	INVISIBLE	3
#define	PROFILING	4

struct	idlst {
	char	*id;
	struct	idlst *id_next;
};

struct device {
	int	d_type;			/* CONTROLLER, DEVICE, UBA or MBA */
	struct	device *d_conn;		/* what it is connected to */
	char	*d_name;		/* name of device (e.g. rk11) */
	struct	idlist *d_vec;		/* interrupt vectors */
	int	d_pri;			/* interrupt priority */
	int	d_addr;			/* address of csr */
	int	d_unit;			/* unit number */
	int	d_drive;		/* drive number */
	int	d_slave;		/* slave number */
#define QUES	-1	/* -1 means '?' */
#define	UNKNOWN -2	/* -2 means not set yet */
	int	d_dk;			/* if init 1 set to number for iostat */
	int	d_flags;		/* nlags for device init */
	struct	device *d_next;		/* Next one in list */
};

struct config {
	char	*c_dev;
	char	*s_sysname;
};

/*
 * Config has a global notion of which machine type is
 * being used.  It uses the name of the machine in choosing
 * files and directories.  Thus if the name of the machine is ``vax'',
 * it will build from ``makefile.vax'' and use ``../vax/asm.sed''
 * in the makerules, etc.
 */
int	machine;
char	*machinename;
#define	MACHINE_VAX	1
#define	MACHINE_SUN	2

/*
 * For each machine, a set of CPU's may be specified as supported.
 * These and the options (below) are put in the C flags in the makefile.
 */
struct cputype {
	char	*cpu_name;
	struct	cputype *cpu_next;
} *cputype;

/*
 * A set of options may also be specified which are like CPU types,
 * but which may also specify values for the options.
 */
struct opt {
	char	*op_name;
	char	*op_value;
	struct	cputype *op_next;
} *opt;

char	*ident, *ns(), *malloc(), *tc(), *qu();
int	do_trace;

#if MACHINE_VAX
int	seen_mba, seen_uba;
#endif

struct	device *connect();
struct	device *dtab;

char	errbuf[80];
int	yyline;

struct	file_list *ftab, *conf_list, *confp;
char	*PREFIX;

int	hz, timezone, hadtz;
int	dst;
int	profiling;

int	maxusers;

#define eq(a,b)	(!strcmp(a,b))
