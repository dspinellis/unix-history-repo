/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsivar.h	5.3 (Berkeley) %G%
 *
 * from: $Header: scsivar.h,v 1.7 92/12/02 03:54:05 torek Exp $ (LBL)
 */

/*
 * SCSI variables.
 *
 * Each SCSI Host Bus Adapter (hba) has:
 *	a target queue head and tail
 *	eight targets (for units to enqueue on)
 *	a list of all units on all targets
 *	its target number (the number cpu uses in initiating requests)
 *	a driver
 * Each SCSI target has:
 *	a forward link so that it can sit on a SCSI host bus adapter queue
 *	a unit queue head and tail
 * Each SCSI unit has:
 *	a forward link so that it can sit on a SCSI target queue
 *	a driver
 *	an hba & driver (so that we need not chase parent pointers)
 */

/*
 * Downcalls.  These are usually made from hba to unit, but can be
 * hba->target->unit (when there are multiple units on a target).
 */
/* device go function (`you got bus') */
typedef void (*scdgo_fn) __P((struct device *, struct scsi_cdb *));

/* intr function (`you no got bus no more') */
typedef void (*scintr_fn) __P((struct device *, int stat, int resid));

/*
 * Upcalls.  These are usually made from unit to hba, but can be
 * unit->target->hba.
 */
/* bus alloc function (`please get me bus') */
struct sq; struct buf;
typedef void (*scstart_fn) __P((struct device *, struct sq *, struct buf *,
				scdgo_fn, struct device *));

/* bus go function (`I have bus and I set up cmd, so start it up') */
typedef int (*scbusgo_fn) __P((struct device *, int targ,
				scintr_fn, struct device *,
				struct buf *, int pad));

/* bus release function (`I have bus but do not need it after all') */
typedef void (*scbusrel_fn) __P((struct device *));

/*
 * SCSI Queue.  This is an element in a queue of devices (targets
 * and/or units) waiting for the bus.
 */
struct sq {
	struct	sq *sq_forw;		/* forward link */
	struct	buf *sq_bp;		/* buffer for transfer */
	scdgo_fn sq_dgo;		/* device-go to call when got bus */
	struct	device *sq_dev;		/* device argument to sq_dgo */
};

struct hba_softc {
	struct	device hba_dev;		/* generic part */
	struct	sq *hba_head, *hba_tail;/* io queue (u's/t's wanting bus) */
	char	hba_busy;		/* true => will inspect qhead later */
	struct	targ *hba_targets[8];	/* the 8 possible targets */
	struct	hbadriver *hba_driver;	/* hba driver */
	scintr_fn hba_intr;		/* current interrupt function */
	struct	device *hba_intrdev;	/* arg 0 for hba_intr */
};

struct targ {
	struct	device t_dev;		/* generic part */
	struct	sq t_forw;		/* forward link, etc, on hba queue */
	struct	sq *t_head, *t_tail;	/* io queue */
	char	t_busy;			/* true => will inspect qhead later */
	char	t_targ;			/* target number */
	char	t_nunits;		/* count of live units */
	char	t_firstunit;		/* the first live unit */
	struct	unit *t_units[8];	/* the 8 possible units */
	scintr_fn t_intr;		/* current interrupt function */
	struct	device *t_intrdev;	/* arg 0 for t_intr */
};

/* since a unit may be a disk, tape, etc., it has only pointer to dev */
struct unit {
	struct	device *u_dev;		/* backpointer to generic */
	int	u_unit;			/* unit number on target */
	scstart_fn u_start;		/* upcall to get bus */
	scbusgo_fn u_go;		/* upcall to use bus */
	scbusrel_fn u_rel;		/* upcall to release bus early */
	struct	device *u_updev;	/* device for upcalls */
	struct	sq u_forw;		/* forward link on target or hba q */
	struct	unitdriver *u_driver;	/* unit driver */
/* the following three fields are copied from target & hba, for quick lookup */
	int	u_targ;			/* target number */
	struct	hba_softc *u_hba;	/* hba, from parent */
	struct	hbadriver *u_hbd;	/* hba driver, from parent */
};

/*
 * SCSI hba driver.
 */
struct hbadriver {
	/* immediate command; should not depend on receiving interrupts */
	int	(*hd_icmd) __P((struct hba_softc *, int targ,
				struct scsi_cdb *cmd,
				caddr_t addr, int len, int rw));
	/* crash dump: like icmd(B_WRITE), but possibly from physmem */
	int	(*hd_dump) __P((struct hba_softc *, int targ,
				struct scsi_cdb *cmd, caddr_t addr, int len));
	scstart_fn hd_start;	/* allocate DMA & bus */
	scbusgo_fn hd_go;	/* start DMA xfer on bus */
	scbusrel_fn hd_rel;	/* release bus early */
	void	(*hd_reset) __P((struct hba_softc *, int));
};

/*
 * SCSI unit driver (`downcalls' from hba to unit).
 */
struct unitdriver {
	void	(*ud_reset) __P((struct unit *));	/* SCSI bus reset */
};

/*
 * The generic SCSI target probe code passes the following to
 * unit configuration `match' routines.
 */
struct scsi_attach_args {
	int	sa_targ;		/* target number */
	int	sa_unit;		/* unit number */
	int	sa_req_status;		/* status from REQUEST SENSE */
	struct	scsi_sense sa_sn;	/* contents from same */
	int	sa_inq_status;		/* status from INQUIRY command */
	struct	scsi_inquiry sa_si;	/* contents from same */
};

/*
 * The SCSICMDLEN macro gives the SCSI-standard-defined length of
 * a given SCSI command.  This is 0 if the command is in an undefined
 * group (see scsi.h).
 */
extern const char scsicmdlen[8];
#define	SCSICMDLEN(cmd) scsicmdlen[(cmd) >> 5]

/*
 * The SCSIMSGLEN macro gives the SCSI-standard-defined length of
 * a given SCSI message byte.  This is -1 if the message byte is
 * undefined, -3 if it is an identify, -2 for an extended message,
 * 0 if it is normal completion, otherwise positive.
 */
#define	SMLEN_IDENTIFY	-3
#define	SMLEN_EXTENDED	-2
#define	SMLEN_UNDEF	-1
#define	SMLEN_DONE	0
extern const signed char scsimsglen[0x24];
#define	SCSIMSGLEN(msg) ((msg) & MSG_IDENTIFY ? SMLEN_IDENTIFY : \
			 (msg) > 0x24 ? SMLEN_UNDEF : scsimsglen[msg])

/*
 * Declarations for exported functions in scsi_subr.c
 */
int	scsi_test_unit_ready __P((struct hba_softc *, int targ, int unit));
int	scsi_request_sense __P((struct hba_softc *, int, int, caddr_t, int));
void	scsi_hbaattach __P((struct hba_softc *));
void	scsi_establish __P((struct unit *, struct device *, int));
void	scsi_printinq __P((struct scsi_inquiry *));
void	scsi_inq_ansi __P((struct scsi_inq_ansi *, char *, char *, char *));
void	scsi_reset_units __P((struct hba_softc *));

#define	SCSI_FOUNDTARGET(hba, targ) { \
	extern int scsi_targprint(void *, char *); \
	int _t = targ; \
	config_found(&(hba)->hba_dev, (void *)&_t, scsi_targprint); \
}
