/*	%M%	%I%	%E%	*/

/*
 * VERSAbus disk controller (vd) disk formatter.
 */
#include <setjmp.h>
#include "tahoe/mtpr.h"
#include "param.h"
#include "buf.h"
#include "ioctl.h" 
#include "disklabel.h" 
#include "inode.h" 
#include "fs.h"
#include "tahoevba/vbaparam.h"
#include "tahoevba/vdreg.h"

#define agets gets
/*
 * Configuration parameters
 */
#define	MAXCTLR		8		/* Maximum # of controllers */
#define	MAXDRIVE	16		/* Max drives per controller */

#define NUMMAP		1		/* # Cyls in bad sector map */
#define	NUMMNT		1		/* # cyls for diagnostics */
#define	NUMREL		3		/* # Cyls in relocation area */
#define	NUMSYS	(NUMREL+NUMMNT+NUMMAP)	/* Total cyls used by system */

#define	MAXTRKS		24
#define	MAXSECS_PER_TRK	72		/* at 512 bytes/sector */
#define	MAXERR		1000
#define	MAXTRKSIZ	((512/sizeof(long)) * MAXSECS_PER_TRK)
#define bytes_trk 	(lab->d_nsectors * lab->d_secsize)

#define	HARD_ERROR \
    (DCBS_NRDY|DCBS_IVA|DCBS_NEM|DCBS_DPE|DCBS_OAB|DCBS_WPT|DCBS_SKI|DCBS_OCYL)
#define DATA_ERROR \
    (DCBS_HCE|DCBS_UDE|DCBS_DCE|DCBS_DSE|DCBS_DSL|DCBS_TOP|DCBS_TOM|DCBS_CCD|\
     DCBS_HARD|DCBS_SOFT)
#define HEADER_ERROR	(DCBS_HCRC|DCBS_HCE)
#define	NRM		(short)0
#define	BAD		(short)VDUF
#define WPT		(short)(NRM | VDWPT)
#define RELOC_SECTOR	(short)(VDALT)
#define	ALT_SECTOR	(short)(VDALT)

typedef enum { false, true } boolean;
typedef enum { u_false, u_true, u_unknown } uncertain;

/*
 * Free bad block allocation bit map
 */
typedef struct {
	long	free_error;
	enum { ALLOCATED, NOTALLOCATED } free_status;
} fmt_free;

typedef enum { SINGLE_SECTOR, FULL_TRACK } rel_type;	/* relocation type */

/*
 * Error table format
 */
typedef struct {
	dskadr	err_adr;
	long	err_stat;
} fmt_err;

/* utilities */
int	to_sector();
int	to_track();
int	data_ok();
boolean	get_yes_no();
boolean	is_in_map();
boolean	is_formatted();
boolean	read_bad_sector_map();
dskadr	*from_sector();
dskadr	*from_track();
dskadr	*from_unix();
dskadr	is_relocated();
dskadr	*new_location();

/*
 * Operation table
 */
typedef struct {
	int	(*routine)();
	char	*op_name;
	char	*op_action;
} op_tbl;

#define	NUMOPS	7
op_tbl	operations[NUMOPS];

/*
 * Operation bit mask values (must match order in operations table)
 */
#define	FORMAT_OP	0x01	/* Format operation bit */
#define	VERIFY_OP	0x02	/* Verify operation bit */
#define	RELOCATE_OP	0x04	/* Relocate operation bit */
#define	INFO_OP		0x08	/* Info operation bit */
#define	CORRECT_OP	0x10	/* Correct operation bit */
#define	PROFILE_OP	0x20	/* Profile operation bit */
#define	EXERCISE_OP	0x40	/* Exercise operation bit */

extern	int format(), verify(), relocate(), info();
extern	int correct(), profile(), exercise();


/*
 * Operation table type and definitions
 */
typedef struct {
	int	op;
	int	numpat;
} op_spec;
op_spec	ops_to_do[MAXCTLR][MAXDRIVE];

/*
 * Contains all the current parameters
 */
typedef enum {
	formatted,
	half_formatted,
	unformatted,
	unknown
} drv_stat;
typedef enum {
	fmt,
	vfy,
	rel,
	cor,
	inf,
	cmd,
	exec,
	prof,
	setup
} state;
typedef enum {
	sub_chk,
	sub_rcvr,
	sub_stat,
	sub_rel,
	sub_vfy,
	sub_fmt,
	sub_sk
} substate;

/*
 * Different environments for setjumps
 */
jmp_buf	reset_environ;	/* Use when reset is issued */
jmp_buf	quit_environ;	/* Use when you want to quit what your doing */
jmp_buf	abort_environ;	/* Use when nothing can be done to recover */

/*
 * Flaw map definitions and storage
 */
typedef struct {
	short	bs_cyl;			/* Cylinder position of flaw */
	short	bs_trk;			/* Track position of flaw */
	long	bs_offset;		/* (byte) Position of flaw on track */
	long	bs_length;		/* Length (in bits) of flaw */
	dskadr	bs_alt;			/* Addr of alt sector (all 0 if none) */
	enum { flaw_map, scanning, operator } bs_how; /* How it was found */
} bs_entry ;

struct {
	int		controller;
	int		drive;
	state		state;
	substate	substate;
	int		error;
	dskadr		daddr;
} cur;

/*
 * Controller specific information
 */
typedef struct {
	uncertain	alive;
	struct	vddevice *addr;
	char		*name;
	int		type;
	fmt_err		(*decode_pos)();
	bs_entry	(*code_pos)();
} ctlr_info;

ctlr_info	c_info[MAXCTLR];
ctlr_info	*C_INFO;

/*
 * Drive specific information
 */
typedef struct {
	uncertain	alive;
	int		id;
	struct		disklabel label;
	drv_stat	condition;
} drive_info;
#define	d_traksize	d_drivedata[1]
#define	d_pat		d_drivedata[2]

drive_info	d_info[MAXCTLR][MAXDRIVE];
drive_info	*D_INFO;
struct disklabel *lab;

struct	disklabel vdproto[];
int	ndrives;
int	smddrives;

typedef struct {
	unsigned int	bs_id;		/* Pack id */
	unsigned int	bs_count;	/* number of known bad sectors */
	unsigned int	bs_max;		/* Maximum allowable bad sectors */
	bs_entry	list[1];
} bs_map;

#define MAX_FLAWS (((MAXTRKSIZ*sizeof(long))-sizeof(bs_map))/sizeof(bs_entry))

long	bs_map_space[MAXTRKSIZ];
bs_map	*bad_map;

boolean	kill_processes;
int	num_controllers;
extern	int vdtimeout;

/*
 * Pattern buffers and the sort
 */
fmt_free	free_tbl[NUMREL*MAXTRKS][MAXSECS_PER_TRK];
struct	mdcb	mdcb;		/* Master device control block */
struct	dcb	dcb;		/* Device control blocks */

long	pattern_0[MAXTRKSIZ],  pattern_1[MAXTRKSIZ];
long	pattern_2[MAXTRKSIZ],  pattern_3[MAXTRKSIZ];
long	pattern_4[MAXTRKSIZ],  pattern_5[MAXTRKSIZ];
long	pattern_6[MAXTRKSIZ],  pattern_7[MAXTRKSIZ];
long	pattern_8[MAXTRKSIZ],  pattern_9[MAXTRKSIZ];
long	pattern_10[MAXTRKSIZ], pattern_11[MAXTRKSIZ];
long	pattern_12[MAXTRKSIZ], pattern_13[MAXTRKSIZ];
long	pattern_14[MAXTRKSIZ], pattern_15[MAXTRKSIZ];

long	*pattern_address[16];	/* pointers to pattern_* */

/*
 * Double buffer for scanning existing
 * file systems and general scratch
 */
long	scratch[MAXTRKSIZ];
long	save[MAXTRKSIZ];

/* XXX */
/*
 * Flaw map stuff 
 */
typedef struct {
	long	flaw_sync;
	short	flaw_cyl;
	char	flaw_trk;
	char	flaw_sec;
	struct {
		short	flaw_offset;
		short	flaw_length;
	} flaw_pos[4];
	char	flaw_status;
	char	flaw_junk[1024]; /* Fill up 518 byte block */
} flaw;

typedef struct {
	long		smde_sync;
	unsigned	adr_cyl  : 12;
	unsigned	adr_trk  : 8;
	unsigned	adr_sec  : 8;
	unsigned	sec_flgs : 4;
	unsigned	alt_cyl  : 12;
	unsigned	alt_trk  : 8;
	unsigned	alt_sec  : 8;
	char		smde_junk[1024];
} smde_hdr;

/* for MAXTOR */

typedef struct {
	unsigned long	esdi_flaw_sync;
	unsigned short	esdi_flaw_cyl;
	unsigned char	esdi_flaw_trk;
	unsigned char	esdi_flaw_sec;
	unsigned char	esdi_flags;
	unsigned char	esdi_ecc_1[2];
	unsigned char	esdi_pad_1[2];
	unsigned char	esdi_plo_sync[26];
} esdi_flaw_header;

typedef struct {
	unsigned long	esdi_data_sync;
	unsigned char	esdi_month;
	unsigned char	esdi_day;
	unsigned char	esdi_year;
	unsigned char	esdi_head;
	unsigned char	esdi_pad_2[2];
	unsigned char	esdi_flaws[50][5];  /* see esdi_flaw_entry */
	unsigned char	esdi_ecc_2[2];
	unsigned char	esdi_pad_3[2];
	char		esdi_flaw_junk[1024]; /* Fill up block */
} esdi_flaw_data;



typedef struct {
	esdi_flaw_header	esdi_header;
	esdi_flaw_data		esdi_data;
} esdi_flaw;



/*
**  since each flaw entry is 5 bytes and this forces alignment problems we
** define a structure here so that the entries can be BCOPYed into a
** reasonable work area before access.
*/

typedef struct {
	unsigned short	esdi_flaw_cyl;
	unsigned short	esdi_flaw_offset;
	unsigned char	esdi_flaw_length;
} esdi_flaw_entry;

#define	CDCSYNC		0x1919
#define	SMDSYNC		0x0019
#define	SMDESYNC	0x0009
#define	SMDE1SYNC	0x000d
#define	ESDISYNC	0x00fe
#define	ESDI1SYNC	0x00fe /* 0x00f8 */

/* XXX */

/*
 * Flaw testing patterns.
 */
struct	flawpat {
	u_int	fp_pat[16];
};

int diskfd;
