/*
 * $Id: am.h,v 5.2 90/06/23 22:20:28 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)am.h	5.1 (Berkeley) %G%
 */

/*
 * Get this in now so that OS_HDR can use it
 */
#ifdef __STDC__
#define	P(x) x
#define	P_void void
#else
#define P(x) ()
#define P_void /* as nothing */
#define const /* as nothing */
#endif /* __STDC__ */

#ifdef __GNUC__
#define INLINE /* __inline */
#else
#define	INLINE
#endif /* __GNUC__ */

/*
 * Pick up target dependent definitions
 */
#include "os-defaults.h"
#include OS_HDR

#ifndef UPDATE_MTAB
#define	unlock_mntlist()
#endif /* UPDATE_MTAB */

#ifdef VOIDP
typedef void *voidp;
#else
typedef char *voidp;
#endif /* VOIDP */

/*
 * Global declarations
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/errno.h>
extern int errno;
#include <sys/socket.h>
#include <rpc/rpc.h>
#include "nfs_prot.h"
#ifdef MNTENT_HDR
#include MNTENT_HDR
#endif /* MNTENT_HDR */
#include <sys/time.h>
#include <assert.h>

#ifdef DEBUG_MEM
#include <malloc.h>
#endif /* DEBUG_MEM */

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MNTTYPE_AUTO
#define MNTTYPE_AUTO "auto"
#endif /* MNTTYPE_AUTO */

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif /* FALSE */

#ifndef ROOT_MAP
#define	ROOT_MAP "\"root\""
#endif /* ROOT_MAP */

/*
 * Flags from command line
 */
extern int print_pid;		/* Print pid to stdout */
extern int normalize_hosts;	/* Normalize host names before use */
extern int restart_existing_mounts;
#ifdef HAS_NIS_MAPS
extern char *domain;		/* NIS domain to use */
#endif /* HAS_NIS_MAPS */
extern FILE *logfp;		/* Log file */
extern int xlog_level;		/* Logging level */
#ifdef HAS_SYSLOG
extern int syslogging;		/* Really using syslog */
#endif /* HAS_SYSLOG */
extern int am_timeo;		/* Cache period */
extern int afs_timeo;		/* AFS timeout */
extern int afs_retrans;		/* AFS retrans */
extern int am_timeo_w;		/* Unmount timeout */
extern char *mtab;		/* Mount table */

#define	XLOG_FATAL	0x0001
#define	XLOG_ERROR	0x0002
#define	XLOG_USER	0x0004
#define	XLOG_WARNING	0x0008
#define	XLOG_INFO	0x0010
#define	XLOG_DEBUG	0x0020
#define	XLOG_MAP	0x0040
#define	XLOG_STATS	0x0080

#define XLOG_DEFSTR	"all,nomap,nostats"		/* Default log options */
#define XLOG_ALL	(XLOG_FATAL|XLOG_ERROR|XLOG_USER|XLOG_WARNING|XLOG_INFO|XLOG_MAP|XLOG_STATS)

#ifdef DEBUG
#ifdef DEBUG_MEM
#define free(x) xfree(__FILE__,__LINE__,x)
#endif /* DEBUG_MEM */

#define	DEBUG_MTAB	"./mtab"

extern int debug_flags;		/* Debug options */

#define	D_DAEMON	0x0001	/* Enter daemon mode */
#define	D_TRACE		0x0002	/* Do protocol trace */
#define	D_FULL		0x0004	/* Do full trace */
#define	D_MTAB		0x0008	/* Use local mtab */
#define	D_AMQ		0x0010	/* Register amq program */
#define	D_STR		0x0020	/* Debug string munging */
#define	D_MEM		0x0040	/* Trace memory allocations */

/*
 * Normally, don't enter daemon mode, and don't register amq
 */
#define	D_TEST	(~(D_DAEMON|D_MEM|D_STR))
#define	D_ALL	(~0)

#define Debug(x) if (!(debug_flags & (x))) ; else
#define dlog Debug(D_FULL) dplog
#endif /* DEBUG */

/*
 * Option tables
 */
struct opt_tab {
	char *opt;
	int flag;
};

typedef enum {
	Start,
	Run,
	Finishing,
	Quit,
	Done
} serv_state;

extern serv_state amd_state;	/* Should we go now */
extern int immediate_abort;	/* Should close-down unmounts be retried */
extern time_t do_mapc_reload;	/* Flush & reload mount map cache */

/*
 * Useful constants
 */
extern char *progname;		/* "amd" */
extern char pid_fsname[];	/* kiska.southseas.nz:(pid%d) */
extern char hostname[];		/* "kiska" */
extern char hostd[];		/* "kiska.southseas.nz" */
extern char *hostdomain;	/* "southseas.nz" */
extern char *op_sys;		/* "sos4" */
extern char *arch;		/* "sun4" */
extern char *karch;		/* "sun4c" */
extern char *cluster;		/* "r+d-kluster" */
extern char *endian;		/* "big" */
extern char *auto_dir;		/* "/a" */
extern char version[];		/* Version info */

typedef struct am_ops am_ops;
typedef struct am_node am_node;
typedef struct am_opts am_opts;
typedef struct mntfs mntfs;
typedef struct fserver fserver;
typedef struct fsrvinfo fsrvinfo;

/*
 * Global variables.
 */
extern unsigned short nfs_port;	/* Our NFS service port */
extern int mypid;		/* Current process id */
extern struct in_addr myipaddr;	/* (An) IP address of this host */

extern int foreground;		/* Foreground process */
extern int orig_umask;		/* umask() on startup */
#define clocktime() (clock_valid ? clock_valid : time(&clock_valid))
extern time_t time P((time_t *));
extern time_t clock_valid;	/* Clock needs recalculating */
extern time_t next_softclock;	/* Time to call softclock() */
extern int task_notify_todo;	/* Task notifier needs running */
#ifdef HAS_TFS
extern int nfs_server_code_available;
#endif /* HAS_TFS */
extern int last_used_map;	/* Last map being used for mounts */
extern AUTH *nfs_auth;		/* Dummy uthorisation for remote servers */
extern am_node *exported_ap[];	/* List of nodes */
extern int first_free_map;	/* First free node */
#define	NEXP_AP	(256)

typedef int (*task_fun)P((voidp));
typedef void (*cb_fun)P((int, int, voidp));
typedef void (*fwd_fun)P((voidp, int, struct sockaddr_in *,
				struct sockaddr_in *, voidp, int));

/*
 * String comparison macros
 */
#define STREQ(s1, s2) (strcmp((s1), (s2)) == 0)
#define FSTREQ(s1, s2) ((*(s1) == *(s2)) && STREQ((s1),(s2)))

/*
 * Linked list
 */
typedef struct qelem qelem;
struct qelem {
	qelem *q_forw;
	qelem *q_back;
};
#define	FIRST(ty, q)	((ty *) ((q)->q_forw))
#define	LAST(ty, q)	((ty *) ((q)->q_back))
#define	NEXT(ty, q)	((ty *) (((qelem *) q)->q_forw))
#define	PREV(ty, q)	((ty *) (((qelem *) q)->q_back))
#define	HEAD(ty, q)	((ty *) q)
#define	ITER(v, ty, q) \
	for ((v) = FIRST(ty,(q)); (v) != HEAD(ty,(q)); (v) = NEXT(ty,(v)))

/*
 * List of mount table entries
 */
typedef struct mntlist mntlist;
struct mntlist {
	struct mntlist *mnext;
	struct mntent *mnt;
};

/*
 * Mount map
 */
typedef struct mnt_map mnt_map;

/*
 * Global routines
 */
extern int atoi P((const char *)); /* C */
extern void am_mounted P((am_node*));
extern void am_unmounted P((am_node*));
extern int background(P_void);
extern int bind_resv_port P((int, unsigned short*));
extern int compute_mount_flags P((struct mntent *));
extern int softclock(P_void);
#ifdef DEBUG
extern int debug_option P((char*));
extern void dplog ();
/*extern void dplog P((char*, ...));*/
#endif /* DEBUG */
/*extern void domain_strip P((char*, char*));*/
extern mntfs* dup_mntfs P((mntfs*));
extern fserver* dup_srvr P((fserver*));
extern int eval_fs_opts P((am_opts*, char*, char*, char*, char*, char*));
extern char* expand_key P((char*));
extern am_node* exported_ap_alloc(P_void);
extern am_node* find_ap P((char*));
extern mntfs* find_mntfs P((am_ops*, am_opts*, char*, char*, char*));
extern void flush_mntfs(P_void);
extern FREE_RETURN_TYPE free P((voidp)); /* C */
extern void free_mntfs P((mntfs*));
extern void free_opts P((am_opts*));
extern void free_map P((am_node*));
extern void free_mntlist P((mntlist*));
extern int fwd_init(P_void);
extern int fwd_packet P((int, voidp, int, struct sockaddr_in *,
		struct sockaddr_in *, voidp, fwd_fun));
extern void fwd_reply(P_void);
extern void get_args P((int, char*[]));
extern void going_down P((int));
#ifdef NEED_MNTOPT_PARSER
extern char *hasmntopt P((struct mntent*, char*));
#endif /* NEED_MNTOPT_PARSER */
extern int hasmntval P((struct mntent*, char*));
extern void host_normalize P((char **));
extern void init_map P((am_node*, char*));
extern void insert_am P((am_node*, am_node*));
extern void ins_que P((qelem*, qelem*));
extern void make_root_node(P_void);
extern int make_rpc_packet P((char*, int, u_long, struct rpc_msg*, voidp, xdrproc_t, AUTH*));
extern void mapc_add_kv P((mnt_map*, char*, char*));
extern mnt_map* mapc_find P((char*, char*));
extern void mapc_free P((mnt_map*));
extern int mapc_search P((mnt_map*, char*, char**));
extern void mapc_reload(P_void);
extern void mapc_showtypes P((FILE*));
extern int mkdirs P((char*, int));
extern void mnt_free P((struct mntent*));
extern int mount_automounter P((int));
extern int mount_exported(P_void);
extern int mount_node P((am_node*));
extern mntfs* new_mntfs(P_void);
extern void new_ttl P((am_node*));
extern am_node* next_map P((int*));
extern int nfs_srvr_port P((fserver*, u_short*, voidp));
extern int pickup_rpc_reply P((voidp, int, voidp, xdrproc_t));
extern void plog ();
/*extern void plog P((int, char*, ...));*/
extern mntlist* read_mtab P((char*));
extern mntfs* realloc_mntfs  P((mntfs*, am_ops*, am_opts*, char*, char*, char*));
extern void rem_que P((qelem*));
extern void reschedule_timeout_mp(P_void);
extern void restart(P_void);
#ifdef UPDATE_MTAB
extern void rewrite_mtab P((mntlist *));
#endif /* UPDATE_MTAB */
extern void rmdirs P((char*));
extern am_node* root_ap P((char*, int));
extern void root_newmap P((char*, char*, char*));
extern void rpc_msg_init P((struct rpc_msg*, u_long, u_long, u_long));
extern void run_task P((task_fun, voidp, cb_fun, voidp));
extern void sched_task P((cb_fun, voidp, voidp));
extern void show_rcs_info P((const char*, char*));
extern void sigchld P((int));
extern void srvrlog P((fserver*, char*));
extern char* str3cat P((char*, char*, char*, char*));
extern char* strcat P((char*, const char*)); /* C */
extern char* strchr P((const char*, int)); /* C */
extern int strcmp P((const char*, const char*)); /* C */
extern char* strdup P((const char*));
extern int strlen P((const char*)); /* C */
extern char* strnsave P((const char*, int));
extern char* strrchr P((const char*, int)); /* C */
extern char* strealloc P((char*, char *));
extern char** strsplit P((char*, int));
extern int switch_option P((char*));
extern void task_notify(P_void);
extern int timeout P((unsigned int, void (*fn)(), voidp));
extern void timeout_mp(P_void);
extern void umount_exported(P_void);
/*extern int unmount_node P((am_node*));
extern int unmount_node_wrap P((voidp));*/
extern void unregister_amq(P_void);
extern void untimeout P((int));
extern int valid_key P((char*));
extern void wakeup P((voidp));
extern void wakeup_task P((int,int,voidp));
extern void wakeup_srvr P((fserver*));
extern void write_mntent P((struct mntent*));
extern voidp xmalloc P((int));
extern voidp xrealloc P((voidp, int));

#define	ALLOC(ty)	((struct ty *) xmalloc(sizeof(struct ty)))

/*
 * Options
 */
struct am_opts {
	char	*fs_glob;		/* Smashed copy of global options */
	char	*fs_local;		/* Expanded copy of local options */
	char	*fs_mtab;		/* Mount table entry */
	/* Other options ... */
	char	*opt_dev;
	char	*opt_delay;
	char	*opt_dir;
	char	*opt_fs;
	char	*opt_group;
	char	*opt_mount;
	char	*opt_opts;
	char	*opt_pref;
	char	*opt_cache;
	char	*opt_rfs;
	char	*opt_rhost;
	char	*opt_sublink;
	char	*opt_type;
	char	*opt_unmount;
	char	*opt_user;
};

/*
 * File Handle
 *
 * This is interpreted by indexing the exported array
 * by fhh_id.
 *
 * The whole structure is mapped onto a standard fhandle_t
 * when transmitted.
 */
struct am_fh {
	int	fhh_pid;		/* process id */
	int	fhh_id;			/* map id */
	int	fhh_gen;		/* generation number */
};

extern am_node *fh_to_mp P((nfs_fh*));
extern am_node *fh_to_mp3 P((nfs_fh*,int*,int));
extern void mp_to_fh P((am_node*, nfs_fh*));
#define	fh_to_mp2(fhp, rp) fh_to_mp3(fhp, rp, VLOOK_CREATE)

typedef int	(*vfs_match)P((am_opts*));
typedef int	(*vfs_init)P((mntfs*));
typedef int	(*vmount_fs)P((am_node*));
typedef int	(*vumount_fs)P((am_node*));
typedef am_node*(*vlookuppn)P((am_node*, char*, int*, int));
typedef int	(*vreaddir)P((am_node*, nfscookie, dirlist*, entry*));
typedef am_node*(*vreadlink)P((am_node*, int*));
typedef int	(*vmounted)P((mntfs*));
typedef void	(*vumounted)P((am_node*));
typedef fserver*(*vffserver)P((mntfs*));

struct am_ops {
	char		*fs_type;
	vfs_match	fs_match;
	vfs_init	fs_init;
	vmount_fs	mount_fs;
	vumount_fs	umount_fs;
	vlookuppn	lookuppn;
	vreaddir	readdir;
	vreadlink	readlink;
	vmounted	mounted;
	vumounted	umounted;
	vffserver	ffserver;
	int		fs_flags;
};
extern am_node *efs_lookuppn P((am_node*, char*, int*, int));
extern int efs_readdir P((am_node*, nfscookie, dirlist*, entry*));

#define	VLOOK_CREATE	0x1
#define	VLOOK_DELETE	0x2

#define	FS_RETRY	0x0001		/* Retry this type of mount */
#define	FS_MBACKGROUND	0x0002		/* Should background this mount */
#define	FS_NOTIMEOUT	0x0004		/* Don't bother with timeouts */
#define FS_MKMNT	0x0008		/* Need to make the mkdir point */
#define FS_UBACKGROUND	0x0010		/* Unmount in background */
#define	FS_BACKGROUND	(FS_MBACKGROUND|FS_UBACKGROUND)
#define	FS_DISCARD	0x0020		/* Discard immediately on last reference */
#define	FS_AMQINFO	0x0040		/* Amq is interested in this fs type */

#ifdef SUNOS4_COMPAT
extern am_ops *sunos4_match P((am_opts*, char*, char*, char*, char*, char*));
#endif /* SUNOS4_COMPAT */
extern am_ops *ops_match P((am_opts*, char*, char*, char*, char*, char*));
#include "fstype.h"

/*
 * Per-mountpoint statistics
 */
struct am_stats {
	time_t	s_mtime;	/* Mount time */
	u_short	s_uid;		/* Uid of mounter */
	int	s_getattr;	/* Count of getattrs */
	int	s_lookup;	/* Count of lookups */
	int	s_readdir;	/* Count of readdirs */
	int	s_readlink;	/* Count of readlinks */
	int	s_statfs;	/* Count of statfs */
};
typedef struct am_stats am_stats;

/*
 * System statistics
 */
struct amd_stats {
	int	d_drops;	/* Dropped requests */
	int	d_stale;	/* Stale NFS handles */
	int	d_mok;		/* Succesful mounts */
	int	d_merr;		/* Failed mounts */
	int	d_uerr;		/* Failed unmounts */
};
extern struct amd_stats amd_stats;

/*
 * List of fileservers
 */
struct fserver {
	qelem		fs_q;		/* List of fileservers */
	int		fs_refc;	/* Number of references to this node */
	char		*fs_host;	/* Normalized hostname of server */
	struct sockaddr_in *fs_ip;	/* Network address of server */
	int		fs_cid;		/* Callout id */
	int		fs_pinger;	/* Ping (keepalive) interval */
	int		fs_flags;	/* Flags */
	char		*fs_type;	/* File server type */
	voidp		fs_private;	/* Private data */
	void		(*fs_prfree)();	/* Free private data */
};
#define	FSF_VALID	0x0001		/* Valid information available */
#define	FSF_DOWN	0x0002		/* This fileserver is thought to be down */
#define	FSF_ERROR	0x0004		/* Permanent error has occured */
#define	FSF_WANT	0x0008		/* Want a wakeup call */
#define	FSF_PINGING	0x0010		/* Already doing pings */
#define	FSRV_ISDOWN(fs)	(((fs)->fs_flags & (FSF_DOWN|FSF_VALID)) == (FSF_DOWN|FSF_VALID))
#define	FSRV_ISUP(fs)	(((fs)->fs_flags & (FSF_DOWN|FSF_VALID)) == (FSF_VALID))

/*
 * List of mounted filesystems
 */
struct mntfs {
	qelem		mf_q;		/* List of mounted filesystems */
	am_ops		*mf_ops;	/* Operations on this mountpoint */
	am_opts		*mf_fo;		/* File opts */
	struct attrstat	mf_attr;	/* File attributes */
#define mf_fattr	mf_attr.attrstat_u.attributes
	char		*mf_mount;	/* "/a/kiska/home/kiska" */
	char		*mf_info;	/* Mount info */
	char		*mf_opts;	/* Mount opts */
	fserver		*mf_server;	/* File server */
	int		mf_flags;	/* Flags */
	int		mf_error;	/* Error code from background mount */
	int		mf_refc;	/* Number of references to this node */
	int		mf_cid;		/* Callout id */
	void		(*mf_prfree)();	/* Free private space */
	voidp		mf_private;	/* Private - per-fs data */
};

#define	MFF_MOUNTED	0x0001		/* Node is mounted */
#define	MFF_MOUNTING	0x0002		/* Mount is in progress */
#define	MFF_UNMOUNTING	0x0004		/* Unmount is in progress */
#define	MFF_RESTART	0x0008		/* Restarted node */
#define MFF_MKMNT	0x0010		/* Delete this node's am_mount */
#define	MFF_ERROR	0x0020		/* This node failed to mount */
#define	MFF_LOGDOWN	0x0040		/* Logged that this mount is down */
#define	MFF_RSTKEEP	0x0080		/* Don't timeout this filesystem - restarted */
#define	MFF_WANTTIMO	0x0100		/* Need a timeout call when not busy */

/*
 * Map of auto-mount points.
 */
struct am_node {
	int		am_mapno;	/* Map number */
	mntfs		*am_mnt;	/* Mounted filesystem */
	char		*am_name;	/* "kiska"
					   Name of this node */
	char		*am_path;	/* "/home/kiska"
					   Path of this node's mount point */
	char		*am_link;	/* "/a/kiska/home/kiska/this/that"
					   Link to sub-directory */
	am_node		*am_parent,	/* Parent of this node */
			*am_ysib,	/* Younger sibling of this node */
			*am_osib,	/* Older sibling of this node */
			*am_child;	/* First child of this node */
	int		am_flags;	/* Boolean flags */
	int		am_error;	/* Specific mount error */
	time_t		am_ttl;		/* Time to live */
	int		am_timeo_w;	/* Wait interval */
	int		am_timeo;	/* Timeout interval */
	unsigned int	am_gen;		/* Generation number */
	char		*am_pref;	/* Mount info prefix */
	am_stats	am_stats;	/* Statistics gathering */
};

#define	AMF_NOTIMEOUT	0x0001		/* This node never times out */
#define	AMF_ROOT	0x0002		/* This is a root node */
#define AMF_MKPATH	0x0004		/* Delete this node's am_path */

#define	ONE_HOUR	(60 * 60)	/* One hour in seconds */

/*
 * The following values can be tuned...
 */
#define	ALLOWED_MOUNT_TIME	40		/* 40s for a mount */
#define	AM_TTL			(5 * 60)	/* Default cache period */
#define	AM_TTL_W		(2 * 60)	/* Default unmount interval */
#define	AM_PINGER		30		/* NFS ping interval for live systems */
#define	AFS_TIMEO		8		/* Default afs timeout - .8s */
#define	AFS_RETRANS		((ALLOWED_MOUNT_TIME*10+2*afs_timeo)/afs_timeo)
						/* Default afs timeout - 1/10th seconds */

#define	RPC_XID_PORTMAP		0
#define	RPC_XID_MOUNTD		1
#define	RPC_XID_NFSPING		2
#define	RPC_XID_MASK		(0x0f)		/* 16 id's for now */
#define	MK_RPC_XID(type_id, uniq)	((type_id) | ((uniq) << 4))
