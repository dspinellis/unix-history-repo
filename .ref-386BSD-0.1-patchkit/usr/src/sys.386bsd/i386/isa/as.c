/*
 * Adaptech 1542 SCSI driver for 386bsd
 *
 * Pace Willisson     pace@blitz.com    March 28, 1992
 *
 * Placed in the public domain with NO WARRANTIES, not even the
 * implied warranties for MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE.
 *
 *
 * This is a very early version - use with care.
 *
 * Here is the config info:
 *
 * controller	as0	at isa? port 0x330 bio irq 11 drq 5 vector asintr
 * disk		dk6	at as0 drive 0 
 *
 * Also, don't forget to update sys/i386/conf/files.i386.
 *
 * So far, used with:
 *
 *   CDC WREN 5 600 Megabyte magnetic disk
 *   EXABYTE EXB-8200 8mm tape drive
 *   SONY CDU-541 cdrom
 *
 * The the tape stuff still needs a lot of working concerning
 * file marks, end of tape handling and rewinding, but I have
 * extracted tar tapes to a file system mounted on the CDC disk.
 *
 * minor number bits:
 *
 *      7  6  5  4  3  2  1  0
 *                     +-----+  partition number
 *            +-----+           scsi target number
 *      +--+                    unused (should be 0)
 *
 * For tape drives, set the partition number to 0 for regular,
 * 1 for no rewind.
 *
 * Only supports LUN 0.
 *
 * To use with a read-write disk, first use diskpart to create
 * a disktab entry, then use disklabel.  Since I don't have
 * the boot programs done yet, I faked it with:
 *
 *		# cd /usr/mdec
 *		# cp wdboot asboot
 *		# cp bootwd bootas
 *
 * Now you can run disklabel, newfs, etc.
 *
 * Please send patches and names other perpherials that work to
 * pace@blitz.com.  If you have trouble that you can't fix, please
 * wait for the next release before contacting me.
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00088
 * --------------------         -----   ----------------------
 *
 * 23 Oct 92	Joerg Lohse		changed ccb opcode for compatibility
 *					with Adaptec AHA-1542A
 * 27 Feb 93	James da Silva		Tapedrive fixes. 
 */

#include "as.h"
#if NAS > 0

#include "param.h"
#include "dkbad.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "stat.h"
#include "ioctl.h"
#include "disklabel.h"
#include "buf.h"
#include "uio.h"
#include "i386/isa/isa_device.h"
#include "i386/isa/icu.h"
#include "syslog.h"
#include "vm/vm.h"
#include "kernel.h"

#include "asreg.h"

int asstrategy ();
int asabort ();
extern int hz;

int asverbose = 0;


/* target id 7 is the controller itself */
#define NTARGETS 7

struct mailbox_entry mailboxes[NTARGETS * 2] = {0};

#define b_cylin	b_resid /* fake cylinder number for disksort */

/* maximum scatter list size for Adaptech controller */
#define NSCATTER 17

/* this array must reside in contiguous physical memory */
struct asinfo {
	dev_t dev;
	struct buf requests;
	struct mailbox_entry *mailbox;
	int active;
	struct ccb ccb;
	unsigned int ccb_phys;
	char scatter_list[NSCATTER * 6];

	struct disklabel label;
	struct dos_partition dospart[NDOSPART];
	int have_label;

	int scsi_lock;
	struct buf *scsi_bp;
	int scsi_cdb_len;
	unsigned char scsi_cdb[MAXCDB];

	int tape;		/* sequential */
	int disk;		/* nonsequential */
	int read_only;		/* CDROM */
	int removable;		/* unsupported and tested */
	char vendor[9];
	char model[17];
	char revision[5];
	int bs;			/* device block size */

	int open_lock;
	int open;
	int units_open;

	int wlabel;

	int retry_count;
	int start_time;
	int restart_pending;

} asinfo[NTARGETS] = {0};

#define dev_part(dev) (minor (dev) & 7)
#define dev_target(dev) ((minor (dev) >> 3) & 7)
#define dev_rewind(dev) ((minor (dev) & 1) == 0)

#define makeasdev(major, target, part) \
	makedev ((major), ((target) << 3) | (part))

int as_port;

int	asprobe(struct isa_device *), asattach(struct isa_device *),
	asintr(dev_t);

struct	isa_driver asdriver = {
	asprobe, asattach, "as",
};

int
asprobe (struct isa_device *dvp)
{
	int val;

	as_port = dvp->id_iobase;

	outb (as_port + AS_CONTROL, AS_CONTROL_SRST);
	DELAY (30000);
	val = inb (as_port + AS_STATUS);

	if (val == (AS_STATUS_INIT | AS_STATUS_IDLE))
		return (1);
	as_port = 0;
	return (0);
}

asattach (struct isa_device *dvp)
{
	int i;
	unsigned int physaddr;
	int val;
	int s;

	for (i = 0; i < NTARGETS; i++) {
		asinfo[i].mailbox = &mailboxes[i];
		asinfo[i].ccb_phys = vtophys (&asinfo[i].ccb);
	}

	isa_dmacascade(dvp->id_drq);
	
	physaddr = vtophys (mailboxes);
	
	s = splbio ();
	if (as_put_byte (AS_CMD_MAILBOX_INIT) < 0
	    || as_put_byte (NTARGETS) < 0
	    || as_put_byte (physaddr >> 16) < 0
	    || as_put_byte (physaddr >> 8) < 0
	    || as_put_byte (physaddr) < 0) {
		splx (s);
		return (EIO);
	}
	splx (s);
	DELAY (300);
	val = inb (as_port + AS_STATUS);

	if (val & AS_STATUS_INIT)
		printf ("as: mailbox init error: 0x%x\n", val);
}

int
ascmd (as, bp, direction, count, retrycount)
struct asinfo *as;
struct buf *bp;
int direction;
int count;
int retrycount;
{
	int err;

	do {
		if (asverbose)
			printf ("ascmd ");
		bp->b_bcount = count;
		bp->b_error = 0;
		bp->b_flags &= ~(B_READ | B_ERROR | B_DONE);
		if (direction == B_READ)
			bp->b_flags |= B_READ;

		bp->b_dev = as->dev;
		bp->b_blkno = 0;

		as->scsi_bp = bp;
		/* scsi_cdb, scsi_cdb_len set up by caller */

		asstrategy (bp);
		err = biowait (bp);
		as->scsi_bp = NULL;
		
	} while (err && --retrycount);

	return (err);
}

asstring (dest, src, size)
char *dest;
char *src;
int size;
{
	size--;
	bcopy (src, dest, size);
	while (size > 0 && dest[size - 1] == ' ')
		size--;
	dest[size] = 0;
}

asopen (dev, flag)
dev_t dev;
int flag;
{
	struct asinfo *as;
	unsigned int physaddr;
	struct buf *bp = NULL;
	int retry;
	unsigned char *cdb;
	char *p, *q;
	int n;
	int error;
	char vendor[9];
	char model[17];
	int disksize;

	if (as_port == 0 || dev_target (dev) >= NTARGETS)
		return (ENXIO);

	as = &asinfo[dev_target (dev)];
	as->dev = dev;

	while (as->open_lock)
		if (error = tsleep ((caddr_t)as, PZERO|PCATCH, "scsiopen", 0))
			return (error);

	if (as->open) {
		if (as->tape)
			return (EBUSY);

		if (as->have_label == 0 && dev_part (dev) != 3)
			return (ENXIO);
		
		as->units_open |= 1 << dev_part (dev);
		return (0);
	}

	as->open_lock = 1;

	/* it seems like we might have to block here in case someone
	 * opens the device just after someone else closes
	 */
	while (as->scsi_lock)
		if (error = tsleep ((caddr_t)as, PZERO|PCATCH, "scsicmd", 0))
			return (error);

	as->scsi_lock = 1;

	error = EIO;

	as->have_label = 0;
	as->tape = 0;
	as->disk = 0;
	as->read_only = 0;
	as->removable = 0;
	bcopy(as->vendor, vendor, sizeof(vendor));
	bcopy(as->model, model, sizeof(model));
	as->vendor[0] = 0;
	as->model[0] = 0;
	as->revision[0] = 0;

	bp = geteblk (DEV_BSIZE);

	if (asverbose) {
		printf ("openbuf = 0x%x phys 0x%x\n",
			bp->b_un.b_addr, vtophys (bp->b_un.b_addr));
		printf ("mailboxes = 0x%x\n", mailboxes);
	}

	/* first, find out if a device is present, and just what it is */
	as->scsi_cdb_len = 6;
	cdb = as->scsi_cdb;
	bzero (cdb, 6);
	cdb[0] = 0x12; /* INQUIRY */
	cdb[4] = 255; /* allocation length */
	if (error = ascmd (as, bp, B_READ, DEV_BSIZE, 2))
		/* does not respond to inquiry, obviously not CCS, give up */
		goto done;

	
	/* blather on console about it */
	p = bp->b_un.b_addr;
	if (asverbose) {
		printf ("inquiry: ");
		for (n = 0; n < 20; n++)
			printf ("%x ", p[n] & 0xff);
		printf ("\n");
		for (n = 0; n < 40; n++) {
			if (p[n] >= ' ' && p[n] < 0177)
				printf ("%c", p[n]);
			else
				printf (".");
		}
		printf ("\n");
	}

	switch (p[0]) {
	case 0: /* normal disk */
	case 4: /* write once disk */
		as->disk = 1;
		break;
	case 5: /* read only disk */
		as->read_only = 1;
		as->disk = 1;
		break;
	case 1: /* tape */
		as->tape = 1;
		break;
	case 0x7f:
		printf ("logical unit not present\n");
		goto done;
	default:
		printf ("unknown peripheral device type: 0x%x\n", p[0]);
		goto done;
	}

	as->removable = (p[1] & 0x80) ? 1 : 0;

	n = p[4] & 0xff;
	if (n >= 31) {
		asstring (as->vendor, p + 8, sizeof as->vendor);
		asstring (as->model, p + 16, sizeof as->model);
		asstring (as->revision, p + 32, sizeof as->revision);
	}

	if(bcmp(as->vendor,vendor, sizeof(vendor)) != 0 ||
		bcmp(as->model,model, sizeof(model)) != 0) {
		printf("as%d: attached tgt %d <%s %s %s> ",  0, dev_target(dev),
			as->vendor, as->model, as->revision);
		if (as->read_only) printf("readonly ");
		if (!as->removable) printf("winchester ");
		if (as->tape) printf("tape ");
		if (as->disk) printf("disk ");
		printf("\n");
	}

	/* probe for desired block size */

	/* assume default of 512, except if CDROM (2048) */
	if (as->read_only)
		as->bs = 2048;
	else
		as->bs = 512;

	bzero(cdb, 6);
	cdb[0] = 0x1A;  /* SCSI_MDSENSE */
	cdb[4] = 255;
	if (as->tape && ascmd (as, bp, B_READ, 12, 2) == 0)  {
		int minblk, maxblk;

#ifdef notdef
		/* blather about device more */
		if(bcmp(as->vendor,vendor, sizeof(vendor)) != 0 ||
			bcmp(as->model,model, sizeof(model)) != 0) {
			p = bp->b_un.b_addr;
			printf("as%d: data len %d medium %d speed/bufmode 0x%x desc len %d\n",
				dev_target(dev), p[0], p[1], p[2], p[3]);
			printf("as%d: density %d nblocks %d block len %d\n",
				dev_target(dev), p[4],
				(long)p[5]*65536+p[6]*256+p[7],
				(long)p[9]*65536+p[10]*256+p[11]);
		}
#endif
		
		/* obtain possible block sizes */
		bzero(cdb, 6);
		cdb[0] = 0x05; /* SCSI_RDLIMITS; */
		if (ascmd (as, bp, B_READ, 12, 2) == 0) {
			p = bp->b_un.b_addr;
			minblk = p[4]*256+p[5];
			maxblk = p[1]*65536+p[2]*256+p[3];
#ifdef notdef
			if(bcmp(as->vendor,vendor, sizeof(vendor)) != 0 ||
				bcmp(as->model,model, sizeof(model)) != 0) {
				printf("as%d: limits: min block len %ld  max block len %ld\n",
					dev_target(dev), minblk, maxblk);
			}
#endif
			if ( minblk == maxblk )
				as->bs = minblk;
			else if (as->tape)
				as->bs = 1;
		}
	}
	
	as->scsi_cdb_len = 10;
	bzero(cdb, 10);
	cdb[0] = 0x25;  /* SCSI_READCAPACITY */
	disksize = 0;
	if (as->disk && ascmd (as, bp, B_READ, 12, 2) == 0)  {
		p = bp->b_un.b_addr;
		disksize = ntohl(*(long *)p);
		as->bs = ntohl(*(long *)(p+4));
		
	}

if(asverbose)
	printf("block size %d disksize %d ", as->bs, disksize);


	/* for standard disk, negotiate block size */
	if (as->read_only == 0 && as->disk) {
		/* do mode select to set the logical block size */
		as->scsi_cdb_len = 6;
		cdb = as->scsi_cdb;
		bzero (cdb, 6);
		cdb[0] = 0x15; /* MODE SELECT */
		cdb[4] = 12; /* parameter list length */
	
		p = bp->b_un.b_addr;
		bzero (p, 12);
		p[3] = 8; /* block descriptor length */
		n = as->bs == 1 ? 0 : as->bs;
		p[9] = n >> 16;
		p[10] = n >> 8;
		p[11] = n;
	
		(void) ascmd (as, bp, B_WRITE, 12, 2);
	}

	/* device online and ready? */
	as->scsi_cdb_len = 6;
	bzero(cdb, 6);
	cdb[0] = 0x00;  /* SCSI_UNITRDY */
	if (error = ascmd (as, bp, B_READ, 12, 2)) {
		printf("as%d: drive not online\n", dev_target(dev));
		goto done;
	}

	if (as->disk && as->read_only == 0) {
		/* read disk label */
		bzero ((caddr_t)&as->label, sizeof as->label);
		as->label.d_secsize = as->bs;
		as->label.d_secpercyl = 64*32;
		as->label.d_type = DTYPE_SCSI;

		
		/* read label using "d" partition */
		if ((p = readdisklabel (
			makeasdev (major (dev), dev_target (dev), 3),
			asstrategy, &as->label, as->dospart, 0, 0)) == NULL){
			as->have_label = 1;
		} else {
			if (disksize) {
				as->label.d_subtype = DSTYPE_GEOMETRY;
				as->label.d_npartitions = 3;
				/* partition 0  holds bios, partition 1 ESDI */
				as->label.d_partitions[2].p_size = disksize;
				as->label.d_partitions[2].p_offset = 0;
			}
			if (asverbose || dev_part (dev) != 3)
				printf ("error reading label: %s\n", p);
			if (dev_part (dev) != 3) {
				error = EINVAL;
				goto done;
			}
		}
	}

	/* may want to set logical block size here ? */
	error = 0;

 done:
	if (bp) {
		bp->b_flags |= B_INVAL | B_AGE;
		brelse (bp);
	}

	if (error == 0)
		as->open = 1;

	as->open_lock = 0;
	as->scsi_lock = 0;
	wakeup (as);

	return (error);
}

asclose (dev, flag)
dev_t dev;
{
	struct asinfo *as;
	int error = 0;
	unsigned char *cdb;
	struct buf *bp;
	int n;

	as = &asinfo[dev_target (dev)];

	while (as->open_lock)
		if (error = tsleep ((caddr_t)as, PZERO|PCATCH, "scsiclose", 0))
			return (error);

	as->open_lock = 1;

	if (as->tape) {
		while (as->scsi_lock)
			if (error = tsleep ((caddr_t)as, PZERO|PCATCH,
					    "scsicmd", 0))
				return (error);

		as->scsi_lock = 1;
		
		bp = geteblk (DEV_BSIZE);

             if ((flag & FWRITE) != 0) {
                             /* presume user will use tape again */
                     as->scsi_cdb_len = 6;
                     cdb = as->scsi_cdb;
                     bzero (cdb, 6);
                     cdb[0] = 0x10; /* write filemarks */
                     cdb[4] = 1; /* one of them */
                     error = ascmd (as, bp, B_READ, 0, 1);
             }
             if (dev_rewind (dev) || error) {
                     if ( error == 0 && (flag & FWRITE) != 0) {
                                     /* presumption error correction */
                             as->scsi_cdb_len = 6;
                             cdb = as->scsi_cdb;
                             bzero (cdb, 6);
                             cdb[0] = 0x10; /* write filemarks */
                             cdb[4] = 1; /* one of them */
                             error |= ascmd (as, bp, B_READ, 0, 1);
                     }
                     as->scsi_cdb_len = 6;
                     cdb = as->scsi_cdb;
                     bzero (cdb, 6);
                     cdb[0] = 0x1; /* rewind */
                     cdb[1] = 1; /* don't wait until done */
                     error |= ascmd (as, bp, B_READ, 0, 1);
             }
#ifdef notdef
		} else {
			cdb[0] = 0x11; /* backspace */
			cdb[1] = 1; /* look at filemarks (instead of blocks) */
			n = -1;
			cdb[2] = n >> 16;
			cdb[3] = n >> 8;
			cdb[4] = n;
			error = ascmd (as, bp, B_READ, 0, 1);
		} 
#endif

		bp->b_flags |= B_INVAL | B_AGE;
		brelse (bp);

		as->scsi_lock = 0;
	}

	as->units_open &= ~(1 << dev_part (dev));

	if (as->units_open == 0)
		as->open = 0;

	as->open_lock = 0;

	wakeup (as);

	return (error);
}

int
asioctl (dev, cmd, addr, flag)
dev_t dev;
int cmd;
caddr_t addr;
int flag;
{
	struct scsicmd *cmdp;
	struct asinfo *as;
	int ccblen;
	struct buf *bp;
	int error = 0;
	int direction;
	struct disklabel *dl;
	int old_wlabel;

	as = &asinfo[dev_target (dev)];

	switch (cmd) {
	case DIOCGDINFO:
		*(struct disklabel *)addr = as->label;
		break;

        case DIOCSDINFO:
                if ((flag & FWRITE) == 0) {
                        error = EBADF;
			break;
		}
		dl = (struct disklabel *)addr;
		if (error = setdisklabel(&as->label, dl, 0, as->dospart))
			break;
		as->have_label = 1;
                break;

        case DIOCWLABEL:
                if ((flag & FWRITE) == 0) {
                        error = EBADF;
			break;
		}
		as->wlabel = *(int *)addr;
                break;

        case DIOCWDINFO:
                if ((flag & FWRITE) == 0) {
                        error = EBADF;
			break;
		}

		dl = (struct disklabel *)addr;

		if (error = setdisklabel (&as->label, dl, 0, as->dospart))
			break;

		as->have_label = 1;

		old_wlabel = as->wlabel;
		as->wlabel = 1;
		error = writedisklabel(dev, asstrategy, &as->label,
				as->dospart);
		as->wlabel = old_wlabel;
                break;

	case SCSICMD:
		cmdp = (struct scsicmd *)addr;

		/* limited by max sizeof of geteblk */
		if (cmdp->datalen >= 8192 
		    || cmdp->cdblen >= MAXCDB) {
			error = EINVAL;
			break;
		}

		ccblen = cmdp->ccblen;
		if (ccblen > sizeof (struct ccb))
			ccblen = sizeof (struct ccb);

		while (as->scsi_lock)
			if (error = tsleep ((caddr_t)as, PZERO|PCATCH,
					    "scsicmd", 0))
				break;

		as->scsi_lock = 1;
			
		bp = geteblk (cmdp->datalen);

		as->scsi_cdb_len = cmdp->cdblen;
		if (error = copyin (cmdp->cdb, as->scsi_cdb, cmdp->cdblen))
			goto done;
		
		direction = cmdp->readflag ? B_READ : B_WRITE;

		if (direction == B_WRITE)
			if (error = copyin (cmdp->data,
					    bp->b_un.b_addr, cmdp->datalen))
				goto done;
		
		ascmd (as, bp, direction, cmdp->datalen, 1);

		copyout (&as->ccb, cmdp->ccb, ccblen);
		if (direction == B_READ)
			copyout (bp->b_un.b_addr, cmdp->data, cmdp->datalen);
	done:
		bp->b_flags |= B_INVAL | B_AGE;
		brelse (bp);
		as->scsi_lock = 0;
		wakeup (as);
		break;
	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

int
asstrategy (bp)
struct buf *bp;
{
	struct asinfo *as;
	int s;

	if (asverbose)
		printf ("asstrategy %d %d ", bp->b_blkno, bp->b_bcount);
	s = splbio ();

	as = &asinfo[dev_target (bp->b_dev)];
	
	if (as->tape) {
		bp->av_forw = NULL;
		if (as->requests.b_actf)
			as->requests.b_actl->av_forw = bp;
		else
			as->requests.b_actf = bp;
		as->requests.b_actl = bp;
	} else {
		if (bp != as->scsi_bp
		    && as->have_label == 0
		    && dev_part (bp->b_dev) != 3)
			goto bad;

		bp->b_cylin = bp->b_blkno;
		disksort (&as->requests, bp);
	}
	
	if (as->active == 0)
		asstart (as);

	splx (s);
	return;

 bad:
	bp->b_flags |= B_ERROR;
	biodone (bp);
}

asrestart (as)
struct asinfo *as;
{
	int s;
	s = splbio ();
	as->restart_pending = 0;
	as->retry_count++;
	asstart (as);
	splx (s);
}

asstart (as)
struct asinfo *as;
{
	struct buf *bp;
	int blknum;
	unsigned int physaddr;
	struct ccb *ccb;
	unsigned char *cdb;
	int target;
	char *p;
	int n;
	char *sp;
	int nscatter;
	int thistime;
	int nbytes;
	struct partition *part;
	int blkno;
	int nblocks;
	int total;
	int bs = as->bs;


	if (as->restart_pending) {
		as->restart_pending = 0;
		untimeout (asrestart, as);
	}

 again:
	if ((bp = as->requests.b_actf) == NULL)
		return;

	bp->b_error = 0;

	if (asverbose)
		printf ("asstart %x ", bp);

	if (as->mailbox->cmd != 0) {
		/* this can't happen, unless the card flakes */
		printf ("asstart: mailbox not available\n");
		bp->b_error = EIO;
		goto bad;
	}

	if (as->retry_count == 0) {
		as->start_time = time.tv_sec;
	} else {
		if (time.tv_sec - as->start_time > 60) {
			printf ("as: command timed out\n");
			bp->b_error = EIO;
			goto done;
		}
	}

	if (bp != as->scsi_bp) {
		if (bp->b_bcount == 0)
			goto done;

		if ((bp->b_bcount % bs) != 0) {
			printf("as: partial block read\n");
			bp->b_error = EIO;
			goto bad;
		}
	}

	if (bp != as->scsi_bp) {

		blkno = bp->b_blkno;
		nblocks = bp->b_bcount / bs;

		if (as->have_label && dev_part(bp->b_dev) != 3) {
			part = &as->label.d_partitions[dev_part (bp->b_dev)];
			
			if (blkno > part->p_size) {
				bp->b_error = EINVAL;
				goto bad;
			}
			if (blkno == part->p_size) {
				bp->b_resid = bp->b_bcount;
				goto done;
			}
			
			if (blkno + nblocks >= part->p_size)
				nblocks = part->p_size - blkno;
			
			blkno += part->p_offset;
		} else
			blkno = (blkno * DEV_BSIZE)/bs;
if(asverbose)
	printf("trans %d ", blkno);
		if (nblocks > 255)
			nblocks = 255;
		total = nblocks * bs;
if(asverbose)
printf("total %d nblocks %d ", total, nblocks);
		/*bp->b_bcount = total;	/* XXX partial tape block read - wrong */
	} else {
#ifdef nomore
		if (as->fixed == 0) {
			total = bp->b_bcount;
		} else {
			total = bp->b_bcount;
			blkno = bp->b_blkno;
			nblocks = bp->b_bcount / as->fixed;
		}
#else
		total = bp->b_bcount;
#endif
	}

	p = bp->b_un.b_addr;
	n = 0;
	sp = as->scatter_list;
	nscatter = 0;
	if (as->tape && as->bs == 1)
		total = bp->b_bcount;
	while (n < total && nscatter < NSCATTER) {
		thistime = page_size - ((vm_offset_t)p - trunc_page (p));

		if (n + thistime > total)
			thistime = total - n;

		physaddr = vtophys (p);
		
		if (asverbose)
			printf ("%d bytes to %x (%x)\n",
				thistime, p, physaddr);
		sp[0] = thistime >> 16;
		sp[1] = thistime >> 8;
		sp[2] = thistime;
		sp[3] = physaddr >> 16;
		sp[4] = physaddr >> 8;
		sp[5] = physaddr;
		
		p += thistime;
		n += thistime;
		sp += 6;
		nscatter++;
	}
	
	if (nscatter == NSCATTER) {
		printf("out of range, cannot happen?");
		bp->b_error = ENXIO;
		goto bad;
	}

	ccb = &as->ccb;

	/* this only needed to make debugging easier */
	bzero ((caddr_t)ccb, sizeof *ccb); 

	if (nscatter)
		ccb->ccb_opcode = 2; /* scatter cmd, return resid */
	else
		ccb->ccb_opcode = 0;
	target = dev_target (bp->b_dev);
	ccb->ccb_addr_and_control = target << 5;
	if (bp->b_bcount != 0)
		ccb->ccb_addr_and_control |= (bp->b_flags & B_READ) ? 8 : 0x10;
	else
		ccb->ccb_addr_and_control |= 0x18;

	nbytes = nscatter * 6;
	ccb->ccb_data_len_msb = nbytes >> 16;
	ccb->ccb_data_len_mid = nbytes >> 8;
	ccb->ccb_data_len_lsb = nbytes;

	ccb->ccb_requst_sense_allocation_len = MAXSENSE;

	physaddr = vtophys (as->scatter_list);
	ccb->ccb_data_ptr_msb = physaddr >> 16;
	ccb->ccb_data_ptr_mid = physaddr >> 8;
	ccb->ccb_data_ptr_lsb = physaddr;

	ccb->ccb_link_msb = 0;
	ccb->ccb_link_mid = 0;
	ccb->ccb_link_lsb = 0;
	ccb->ccb_link_id = 0;
	ccb->ccb_host_status = 0;
	ccb->ccb_target_status = 0;
	ccb->ccb_zero1 = 0;
	ccb->ccb_zero2 = 0;

	cdb = ccb->ccb_cdb;
	if (bp == as->scsi_bp) {
		ccb->ccb_scsi_command_len = as->scsi_cdb_len;
		bcopy (as->scsi_cdb, cdb, as->scsi_cdb_len);
	} else if (as->tape) {
		ccb->ccb_scsi_command_len = 6;
		cdb[0] = (bp->b_flags & B_READ) ? 8 : 0xa;
		if (as->bs == 1) {
			cdb[1] = 0; /* logical unit 0, variable block size */
			cdb[2] = bp->b_bcount >> 16;
			cdb[3] = bp->b_bcount >> 8;
			cdb[4] = bp->b_bcount;
		} else {
			cdb[1] = 1;     /* fixed block size */
			cdb[2] = nblocks >> 16;
			cdb[3] = nblocks >> 8;
			cdb[4] = nblocks;
		}
		cdb[5] = 0; /* control byte (used in linking) */
	} else {
		ccb->ccb_scsi_command_len = 10;
		cdb[0] = (bp->b_flags & B_READ) ? 0x28 : 0x2a;
		cdb[1] = 0;
		*(long *) (cdb+2) = htonl(blkno);
		*(short *) (cdb+7) = htons(nblocks);
		cdb[9] = 0; /* control byte (used in linking) */
	}

#ifdef notdef
	if (asverbose) {
		printf ("ccb: ");
		for (n = 0; n < 48; n++)
			printf ("%02x ", ((unsigned char *)ccb)[n]);
		printf ("\n");
	}
#endif

	physaddr = vtophys (ccb);
	as->mailbox->msb = physaddr >> 16;
	as->mailbox->mid = physaddr >> 8;
	as->mailbox->lsb = physaddr;
	as->mailbox->cmd = 1;
	
	/* tell controller to look in its mailbox */
	as_put_byte (AS_CMD_START_SCSI_COMMAND);
	as->active = 1;
	timeout (asabort, as, hz * 60 * 2);
	return;

 bad:
	bp->b_flags |= B_ERROR;
 done:
	asdone (as, 0);
	goto again;
}

asabort (as)
struct asinfo *as;
{
	int s;
	int physaddr;
	struct buf *bp;

	s = splbio ();
	if (as->active) {
		printf ("asabort %d\n", as - asinfo);
		physaddr = vtophys (&as->ccb);
		as->mailbox->msb = physaddr >> 16;
		as->mailbox->mid = physaddr >> 8;
		as->mailbox->lsb = physaddr;
		as->mailbox->cmd = 2;
		as_put_byte (AS_CMD_START_SCSI_COMMAND);

		as->active = 0;
		bp = as->requests.b_actf;
		if (bp) {
			bp->b_flags |= B_ERROR;
			asdone (as, 1);
		}
	}
	splx (s);
}

asintr (dev_t dev)
{
	int didwork;
	int i, j;
	struct mailbox_entry *mp;
	unsigned int physaddr;
	int val;

	outb (as_port + AS_CONTROL, AS_CONTROL_IRST);
#ifdef notdef
	if (asverbose)
		printf ("asintr %x ", cpl);
#endif
 again:
	didwork = 0;
	for (i = NTARGETS; i < NTARGETS * 2; i++) {
		mp = &mailboxes[i];
		
		if ((val = mp->cmd) == 0)
			continue;
		
		didwork = 1;

		physaddr = (mp->msb << 16)
			| (mp->mid << 8)
				| mp->lsb;
		
		for (j = 0; j < NTARGETS; j++) {
			if (asinfo[j].ccb_phys == physaddr) {
				mp->cmd = 0;
				asintr1 (&asinfo[j], val);
				break;
			}
		}
		if (j == NTARGETS) {
			printf ("as: unknown mailbox paddr 0x%x\n", physaddr);
			mp->cmd = 0;
		}
	}

	if (didwork)
		goto again;
}

asintr1 (as, val)
struct asinfo *as;
int val;
{
	struct buf *bp;
	struct ccb *ccb;
	int n;
	int bad;
	char *msg;
	char msgbuf[100];
	unsigned char *sp;
	int i,key;

	if (asverbose)
		printf ("asintr1 %x ", val);
	if (as->active == 0) {
		printf ("as: stray intr 0x%x\n", as->dev);
		return;
	}

	as->active = 0;
	untimeout (asabort, as);
	
	bp = as->requests.b_actf;
	ccb = &as->ccb;

	if (bp == as->scsi_bp) {
		/* no fancy error recovery in this case */
		if (asverbose)
			printf ("asintr1:scsicmd ");
#if 0
		if (val != 1)
			bp->b_flags |= B_ERROR;
		goto next;
#endif
	}

	bad = 0;
	msg = NULL;

	if (val != 1 && val != 4) {
		bad = 1;
		sprintf (msgbuf, "funny mailbox message 0x%x\n", val);
		msg = msgbuf;
		goto wrapup;
	}

	if (ccb->ccb_host_status != 0) {
		bad = 1;
		sprintf (msgbuf, "controller error 0x%x",
			 ccb->ccb_host_status);
		msg = msgbuf;
		goto wrapup;
	}

	if (ccb->ccb_target_status == 0)
		/* good transfer */
		goto wrapup;

	if (ccb->ccb_target_status == 8) {
		/* target rejected command because it is busy
		 * and wants us to try again later.  We'll wait 1 second
		 */
		as->restart_pending = 1;
		timeout (asrestart, as, hz);
		return;
	}

	if (ccb->ccb_target_status != 2) {
		bad = 1;
		sprintf (msgbuf, "target error 0x%x",
			 ccb->ccb_target_status);
		msg = msgbuf;
		goto wrapup;
	}

	/* normal path for errors */

	sp = ccb_sense (ccb);
	/* check for extended sense information */
	if ((sp[0] & 0x7f) != 0x70) {
		/* none */
		bad = 1;
		sprintf (msgbuf, "scsi error 0x%x", sp[0] & 0x7f);
		msg = msgbuf;
		goto wrapup;
	}
	
	if (as->tape && (sp[2] & 0xf) == 0) {
		if (sp[2] & 0xe0) {
			/* either we read a file mark, the early warning EOT,
			 * or the block size did not match.  In any case, the
			 * normal residue handling will work (I think)
			 */
			goto wrapup;
		}
	}

	bad = 1;

	switch (key = sp[2] & 0xf) {
	case 1: 
		msg = "soft error";
		bad = 0;
		break;
	case 2:
		msg = "not ready";
		break;
	case 3:
		msg = "hard error";
		break;
	case 4:
		msg = "target hardware error";
		break;
	case 5:
		msg = "illegal request";
		break;
	case 6:
		msg = "unit attention error";
		break;
	case 7: 
		msg = "write protect error";
		break;
	case 0xd:
		msg = "volume overflow";
		break;
	default:
		sprintf (msgbuf, "scsi extended error 0x%x", sp[2] & 0xf);
		msg = msgbuf;
		break;
	}

 wrapup:

	if (bad && msg == NULL)
		msg = "unknown error";

	if (msg && key != 6) {
		diskerr (bp, "as", msg,
			 LOG_PRINTF,
			 -1, /* number of successful blks */
			 as->have_label ? &as->label : NULL);
		printf ("\n");
	}

	if (bad && key != 6) {
		bp->b_flags |= B_ERROR;
		printf ("scsi sense: ");
		sp = ccb_sense (ccb);
		for (i = 0; i < 30; i++)
			printf ("%x ", sp[i] & 0xff);
		printf ("\n");
	}

	/* this assignment mixed sizes of controller commands
           and data to read/write.
 	bp->b_resid = (ccb->ccb_data_len_msb << 16)
 		| (ccb->ccb_data_len_mid << 8)
 			| ccb->ccb_data_len_lsb;
        */
	bp->b_resid = 0;

 next:
	asdone (as, 1);
}

asdone (as, restart)
struct asinfo *as;
int restart;
{
	struct buf *bp;

	bp = as->requests.b_actf;
	as->requests.b_actf = bp->av_forw;
	biodone (bp);
	as->retry_count = 0;
	if (restart && as->requests.b_actf)
		asstart (as);
}

int
assize (dev)
dev_t dev;
{
	struct asinfo *as;
	struct disklabel *lp;
	int val;

	if (as_port == 0 || dev_target (dev) >= NTARGETS)
		return (ENXIO);

	as = &asinfo[dev_target (dev)];

	if (as->open == 0
	    && asopen (dev, FREAD, S_IFBLK, NULL) != 0)
		return (0);

	if (as->have_label == 0)
		return (0);

	lp = &as->label;
	val = lp->d_partitions[dev_part (dev)].p_size
		* lp->d_secsize / DEV_BSIZE;
	(void) asclose(dev, FREAD, S_IFBLK, NULL);
	return (val);
}

int
as_put_byte (val)
int val;
{
	int i;

	for (i = 100; i > 0; i--) {
		if ((inb (as_port + AS_STATUS) & AS_STATUS_CDF) == 0)
			break;
		DELAY (100);
	}
	if (i == 0) {
		printf ("as: put byte timed out\n");
		return (-1);
	}
	outb (as_port + AS_DATA_OUT, val);
	return (0);
}
		
int
as_get_byte (as)
{
	int i;

	for (i = 100; i > 0; i--) {
		if ((inb (as_port + AS_STATUS) & AS_STATUS_DF) != 0)
			break;
		DELAY (100);
	}
	if (i == 0) {
		printf ("as_get_byte timed out\n");
		return (-1);
	}
	return (inb (as_port + AS_DATA_OUT) & 0xff);
}
#endif /* NAS */
