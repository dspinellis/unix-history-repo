/*
 * config.h	1.1	81/02/25
 * Definitions that everybody needs to know
 */

#define eq(a,b) (strcmp(a,b) == 0)
#define PREFIX "_unix/"
#define path(file) "_unix/file"
#define TRUE 1
#define FALSE 0
#define DRIVER 1
#define NORMAL 2
#define INVISIBLE 3

struct file_list {
	char *f_fn;
	int f_type;
	char *f_next;
	char *f_needs;
};

typedef char bool;

struct device {
	int d_type;			/* CONTROLLER, DEVICE, UBA or MBA */
	struct device *d_conn;		/* What it is connected to */
	char *d_name;			/* Name of device (e.g. rk11) */
	char *d_vec1;			/* First interrupt vector */
	char *d_vec2;			/* Second interrupt vector */
	int d_addr;			/* Address of csr */
	int d_unit;			/* Unit number */
	int d_drive;			/* Drive number */
	int d_slave;			/* Slave number */
	bool d_dk;			/* if init 1 set to number for iostat */
	int d_flags;			/* Flags for device init */
	struct device *d_next;		/* Next one in list */
};

struct config {
	char *c_dev;
	char *s_sysname;
};

char *cpu_type, *ident, *ns(), *malloc(), *tc(), *qu();
bool do_trace, seen_mba, seen_uba;
struct device *connect();
struct device *dtab;
char errbuf[80];
int yyline;
struct file_list *ftab, *conf_list, *confp;
