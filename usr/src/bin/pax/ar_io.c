/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ar_io.c	1.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/param.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include "pax.h"
#include "extern.h"

/*
 * Routines which handle the archive I/O device/file.
 */

#define DMOD		0666		/* default mode of created archives */
#define EXT_MODE	O_RDONLY	/* open mode for list/extract */
#define AR_MODE		(O_WRONLY | O_CREAT | O_TRUNC)	/* mode for archive */
#define APP_MODE	O_RDWR		/* mode for append */
#define STDO		"<STDOUT>"	/* psuedo name for stdout */
#define STDN		"<STDIN>"	/* psuedo name for stdin */
static int arfd = -1;			/* archive file descriptor */
static int artyp;			/* archive type: file/FIFO/tape */
static int arvol = 1;			/* archive volume number */
static int lstrval = -1;		/* return value from last i/o */
static int io_ok;			/* i/o worked on volume after resync */
static int did_io;			/* ok i/o did occur on volume */
static int done;			/* set via tty termination */
static struct stat arsb;		/* stat of archive device at open */
static int invld_rec;			/* tape has out of spec record size */
static int phyblk; 			/* size of physical block on TAPE */
char *arcname;                  	/* printable name of archive */

static int get_phys __P((void));
extern sigset_t s_mask;

/*
 * ar_open()
 *	Opens the next archive volume. Determines the type of the device and
 *	sets up block sizes as required by the archive device and the format.
 *	Note: we may be called with name == NULL on the first open only.
 * Return:
 *	-1 on failure, 0 otherwise
 */

#if __STDC__
int
ar_open(char *name)
#else
int
ar_open(name)
	char *name;
#endif
{
        struct mtget mb;

	if (arfd != -1)
		(void)close(arfd);
	arfd = -1;
	phyblk = did_io = io_ok = invld_rec = 0;
	flcnt = 0;

	/*
	 * open based on overall operation mode
	 */
	switch (act) {
	case LIST:
	case EXTRACT:
		if (name == NULL) {
			arfd = STDIN_FILENO;
			arcname = STDN;
		} else if ((arfd = open(name, EXT_MODE, DMOD)) < 0)
			syswarn(1, errno, "Failed open to read on %s", name);
		break;
	case ARCHIVE:
		if (name == NULL) {
			arfd = STDOUT_FILENO;
			arcname = STDO;
		} else if ((arfd = open(name, AR_MODE, DMOD)) < 0)
			syswarn(1, errno, "Failed open to write on %s", name);
		break;
	case APPND:
		if (name == NULL) {
			arfd = STDOUT_FILENO;
			arcname = STDO;
		} else if ((arfd = open(name, APP_MODE, DMOD)) < 0)
			syswarn(1, errno, "Failed open to read/write on %s",
				name);
		break;
	case COPY:
		/*
		 * arfd not used in COPY mode
		 */
		arcname = "<NONE>";
		lstrval = 1;
		return(0);
	}
	if (arfd < 0)
		return(-1);

	/*
	 * set up is based on device type
	 */
	if (fstat(arfd, &arsb) < 0) {
		syswarn(1, errno, "Failed stat on %s", arcname);
		return(-1);
	}
	if (S_ISDIR(arsb.st_mode)) {
		warn(1, "Cannot write an archive on top of a directory %s",
		    arcname);
		return(-1);
	}
	if (S_ISCHR(arsb.st_mode))
		artyp = ioctl(arfd, MTIOCGET, &mb) ? ISCHR : ISTAPE;
	else if (S_ISBLK(arsb.st_mode))
		artyp = ISBLK;
	else if ((lseek(arfd, (off_t)0L, SEEK_CUR) == -1) && (errno == ESPIPE))
		artyp = ISPIPE;
	else
		artyp = ISREG;

	/*
	 * if we are writing, were are done
	 */
	if (act == ARCHIVE) {
		blksz = rdblksz = wrblksz;
		lstrval = 1;
		return(0);
	}

	/*
	 * set default blksz on read. APPNDs writes rdblksz on the last volume
	 * On all new archive volumes, we shift to wrblksz (if the user
	 * specified one, otherwize we will continue to use rdblksz). We
	 * must to set blocksize based on what kind of device the archive is
	 * stored.
	 */
	switch(artyp) {
	case ISTAPE:
		/*
		 * Tape drives come in at least two flavors. Those that support
		 * variable sized records and those that have fixed sized
		 * records. They must be treated differently. For tape drives
		 * that support variable sized records, we must make large
		 * reads to make sure we get the entire record, otherwise we
		 * will just get the first part of the record (up to size we
		 * asked). Tapes with fixed sized records may or may not return
		 * multiple records in a single read. We really do not care
		 * what the physical record size is UNLESS we are going to
		 * append. (We will need the physical block size to rewrite
		 * the trailer). Only when we are appending do we go to the
		 * effort to figure out the true* PHYSICAL record size.
		 */
		blksz = rdblksz = MAXBLK;
		break;
	case ISPIPE:
	case ISBLK:
	case ISCHR:
		/*
		 * Blocksize is not a major issue with these devices (but must
		 * be kept a multiple of 512). If the user specified a write
		 * block size, we use that to read. Under append, we must
		 * always keep blksz == rdblksz. Otherwise we go ahead and use
		 * the device optimal blocksize as (and if) returned by stat
		 * and if it is within pax specs.
		 */
		if ((act == APPND) && wrblksz) {
			blksz = rdblksz = wrblksz;
			break;
		}

		if ((arsb.st_blksize > 0) && (arsb.st_blksize < MAXBLK) &&
		    ((arsb.st_blksize % BLKMULT) == 0))
			rdblksz = arsb.st_blksize;
		else
			rdblksz = DEVBLK;
		/*
		 * For performance go for large reads when we can without harm
		 */
		if ((act == APPND) || (artyp == ISCHR))
			blksz = rdblksz;
		else
			blksz = MAXBLK;
		break;
	case ISREG:
		/*
		 * if the user specified wrblksz works, use it. Under appends
		 * we must always keep blksz == rdblksz
		 */
		if ((act == APPND) && wrblksz && ((arsb.st_size%wrblksz)==0)){
			blksz = rdblksz = wrblksz;
			break;
		}
		/*
		 * See if we can find the blocking factor from the file size
		 */
		for (rdblksz = MAXBLK; rdblksz > 0; rdblksz -= BLKMULT)
			if ((arsb.st_size % rdblksz) == 0)
				break;
		/*
		 * When we cannont find a match, we may have a flawed archive.
		 */
		if (rdblksz <= 0)
			rdblksz = FILEBLK;
		/*
		 * for performance go for large reads when we can
		 */
		if (act == APPND)
			blksz = rdblksz;
		else
			blksz = MAXBLK;
		break;
	default:
		/*
		 * should never happen, worse case, slow... 
		 */
		blksz = rdblksz = BLKMULT;
		break;
	}
	lstrval = 1;
	return(0);
}

/*
 * ar_close()
 *	closes archive device, increments volume number, and prints i/o summary
 */
#if __STDC__
void
ar_close(void)
#else
void
ar_close()
#endif
{
	FILE *outf;

	(void)close(arfd);
	arfd = -1;
	if (!io_ok && !did_io) {
		flcnt = 0;
		return;
	}
	did_io = io_ok = 0;

	/*
	 * The volume number is only increased when the last device has data
	 */
	++arvol;
	if (!vflag) {
		flcnt = 0;
		return;
	}

	/*
	 * Print out a summary of I/O for this archive volume.
	 */
	if (act == LIST)
		outf = stdout;
	else
		outf = stderr;

	/*
	 * we need to go to the next line, partial output may be present
	 */
	if (vfpart) {
		(void)putc('\n', outf);
		vfpart = 0;
	}

	(void)fprintf(outf,
#	ifdef NET2_STAT
	    "Pax %s vol %d: %lu files, %lu bytes read, %lu bytes written\n",
#	else
	    "Pax %s vol %d: %lu files, %qu bytes read, %qu bytes written\n",
#	endif
	    frmt->name, arvol-1, flcnt, rdcnt, wrcnt);
	(void)fflush(outf);
	flcnt = 0;
}

/*
 * ar_set_wr()
 *	special device dependent handling to switch from archive read to
 *	archive write on a single volume (an append). VERY device dependent.
 *	Note: for tapes, head is already positioned at the place we want to
 *	start writing.
 * Return:
 *	0 if all ready to write, -1 otherwise
 */

#if __STDC__
int
ar_set_wr(void)
#else
int
ar_set_wr()
#endif
{
	off_t cpos;

	/* 
	 * Add any device dependent code as required here
	 */
	if (artyp != ISREG)
		return(0);
	/*
	 * Get rid of all the stuff after the current offset
	 */
	if (((cpos = lseek(arfd, (off_t)0L, SEEK_CUR)) < 0) ||
	    (ftruncate(arfd, cpos) < 0))
		return(-1);
	return(0);
}

/*
 * ar_app_ok()
 *	check if the last volume in the archive allows appends. We cannot check
 *	this until we are ready to write since there is no spec that says all 
 *	volumes in a single archive have to be of the same type...
 * Return:
 *	0 if we can append, -1 otherwise.
 */

#if __STDC__
int
ar_app_ok(void)
#else
int
ar_app_ok()
#endif
{
	if (artyp == ISPIPE) {
		warn(1, "Cannot append to an archive obtained from a pipe.");
		return(-1);
	}

	if (!invld_rec)
		return(0);

	warn(1,"Cannot append, device record size %d does not support pax spec",
		rdblksz);
	return(-1);
}

/*
 * ar_read()
 *	read up to a specified number of bytes from the archive into the
 *	supplied buffer. When dealing with tapes we may not always be able to
 *	read what we want.
 * Return:
 *	Number of bytes in buffer. 0 for end of file, -1 for a read error.
 */

#if __STDC__
int
ar_read(register char *buf, register int cnt)
#else
int
ar_read(buf, cnt)
	register char *buf;
	register int cnt;
#endif
{
	register int res = 0;

	/*
	 * if last i/o was in error, no more reads until reset or new volume
	 */
	if (lstrval <= 0)
		return(lstrval);

	/*
	 * how we read must be based on device type
	 */
	switch (artyp) {
	case ISTAPE:
		if ((res = read(arfd, buf, cnt)) > 0) {
			/*
			 * CAUTION: tape systems may not always return the same
			 * sized records so we leave blksz == MAXBLK. The
			 * physical record size that a tape drive supports is
			 * very hard to determine in a uniform and portable
			 * manner.
			 */
			io_ok = 1;
			if (res != rdblksz) {
				/*
				 * Record size changed. If this is happens on
				 * any record after the first, it may cause
				 * problem if we try to append. (We may not be
				 * able to space backwards the proper number
				 * of BYTES). Watch out for blocking which
				 * violates pax spec.
				 */
				rdblksz = res;
				if (rdblksz % BLKMULT)
					invld_rec = 1;
			}
			return(res);
		}
		break;
	case ISREG:
	case ISBLK:
	case ISCHR:
	case ISPIPE:
	default:
		/*
		 * Files are so easy to deal with. These other things cannot
		 * be trusted at all. So when we are dealing with character
		 * devices and pipes we just take what they have ready for us
		 * and return. Trying to do anything else with them runs the
		 * risk of failure.
		 */
		if ((res = read(arfd, buf, cnt)) > 0) {
			io_ok = 1;
			return(res);
		}
		break;
	}

	/*
	 * We are in trouble at this point, something is broken...
	 */
	lstrval = res;
	if (res < 0)
		syswarn(1, errno, "Failed read on archive volume %d", arvol);
	else
		warn(0, "End of archive volume %d reached", arvol);
	return(res);
} 

/*
 * ar_write()
 *	Write a specified number of bytes in supplied buffer to the archive
 *	device so it appears as a single "block". Deals with errors and tries
 *	to recover when faced with short writes.
 * Return:
 *	Number of bytes written. 0 indicates end of volume reached and with no
 *	flaws (as best that can be detected). A -1 indicates an unrecoverable
 *	error in the archive occured.
 */

#if __STDC__
int
ar_write(register char *buf, register int bsz)
#else
int
ar_write(buf, bsz)
	register char *buf;
	register int bsz;
#endif
{
	register int res;
	off_t cpos;

	/*
	 * do not allow pax to create a "bad" archive. Once a write fails on
	 * an archive volume prevent further writes to it.
	 */
	if (lstrval <= 0)
		return(lstrval);

	if ((res = write(arfd, buf, bsz)) == bsz) {
		io_ok = 1;
		return(bsz);
	}

	/*
	 * write broke, see what we can do with it. We try to send any partial
	 * writes that violate pax spec to the next archive volume.
	 */
	if (res < 0)
		lstrval = res;
	else
		lstrval = 0;

	switch (artyp) {
	case ISREG:
		if ((res > 0) && (res % BLKMULT)) {
			/*
		 	 * try to fix up partial writes which are not BLKMULT
			 * in size by forcing the runt record to next archive
			 * volume
		 	 */
			if ((cpos = lseek(arfd, (off_t)0L, SEEK_CUR)) < 0)
				break;
			cpos -= (off_t)res;
			if (ftruncate(arfd, cpos) < 0)
				break;
			res = lstrval = 0;
			break;
		}
		if (res >= 0)
			break;
		/*
		 * if file is out of space, handle it like a return of 0
		 */
		if ((errno == ENOSPC) || (errno == EFBIG) || (errno == EDQUOT))
			res = lstrval = 0;
		break;
	case ISTAPE:
	case ISCHR:
	case ISBLK:
		if (res >= 0)
			break;
		if (errno == EACCES) {
			warn(0, "Write failed, archive is write protected.");
			res = lstrval = 0;
			return(0);
		}
		/*
		 * see if we reached the end of media, if so force a change to
		 * the next volume
		 */
		if ((errno == ENOSPC) || (errno == EIO) || (errno == ENXIO))
			res = lstrval = 0;
		break;
	case ISPIPE:
	default:
		/*
		 * we cannot fix errors to these devices
		 */
		break;
	}

	/*
	 * Better tell the user the bad news...
	 * if this is a block aligned archive format, it may be a bad archive.
	 * the format wants the header to start at a BLKMULT boundry. While
	 * we can deal with the mis-aligned data, it violates spec and other
	 * archive readers will likely fail. if the format is not block
	 * aligned, the user may be lucky.
	 */
	if (res >= 0)
		io_ok = 1;
	if (res == 0)
		warn(0, "End of archive volume %d reached", arvol);
	else if (res < 0)
		syswarn(1, errno, "Failed write to archive volume: %d", arvol);
	else if (!frmt->blkalgn || ((res % frmt->blkalgn) == 0))
		warn(0,"WARNING: partial archive write. Archive MAY BE FLAWED");
	else
		warn(1,"WARNING: partial archive write. Archive IS FLAWED");
	return(res);
}

/*
 * ar_rdsync()
 *	Try to move past a bad spot on a flawed archive as needed to continue
 *	I/O. Clears error flags to allow I/O to continue.
 * Return:
 *	0 when ok to try i/o again, -1 otherwise.
 */

#if __STDC__
int
ar_rdsync(void)
#else
int
ar_rdsync()
#endif
{
	long fsbz;
	off_t cpos;
	off_t mpos;
        struct mtop mb;

	/*
	 * Fail resync attempts at user request (done) or this is going to be
	 * an update/append to a existing archive. if last i/o hit media end,
	 * we need to go to the next volume not try a resync
	 */
	if ((done > 0) || (lstrval == 0))
		return(-1);

	if ((act == APPND) || (act == ARCHIVE)) {
		warn(1, "Cannot allow updates to an archive with flaws.");
		return(-1);
	}
	if (io_ok)
		did_io = 1;

	switch(artyp) {
	case ISTAPE:
		/*
		 * if the last i/o was a successful data transfer, we assume
		 * the fault is just a bad record on the tape that we are now
		 * past. If we did not get any data since the last resync try
		 * to move the tape foward one PHYSICAL record past any
		 * damaged tape section. Some tape drives are stubborn and need
		 * to be pushed.
		 */
		if (io_ok) {
			io_ok = 0;
			lstrval = 1;
			break;
		}
		mb.mt_op = MTFSR;
		mb.mt_count = 1;
		if (ioctl(arfd, MTIOCTOP, &mb) < 0)
			break;
		lstrval = 1;
		break;
	case ISREG:
	case ISCHR:
	case ISBLK:
		/*
		 * try to step over the bad part of the device.
		 */
		io_ok = 0;
		if (((fsbz = arsb.st_blksize) <= 0) || (artyp != ISREG))
			fsbz = BLKMULT;
		if ((cpos = lseek(arfd, (off_t)0L, SEEK_CUR)) < 0)
			break;
		mpos = fsbz - (cpos % (off_t)fsbz);
		if (lseek(arfd, mpos, SEEK_CUR) < 0) 
			break;
		lstrval = 1;
		break;
	case ISPIPE:
	default:
		/*
		 * cannot recover on these archive device types
		 */
		io_ok = 0;
		break;
	}
	if (lstrval <= 0) {
		warn(1,"Unable to recover from an archive read failure.");
		return(-1);
	}
	warn(0, "Attempting to recover from an archive read failure.");
	return(0);
}

/*
 * ar_fow()
 *	Move the I/O position within the archive foward the specified number of
 *	bytes as supported by the device. If we cannot move the requested
 *	number of bytes, return the actual number of bytes moved in skipped.
 * Return:
 *	0 if moved the requested distance, -1 on complete failure, 1 on
 *	partial move (the amount moved is in skipped)
 */

#if __STDC__
int
ar_fow(off_t sksz, off_t *skipped)
#else
int
ar_fow(sksz, skipped)
	off_t sksz;
	off_t *skipped;
#endif
{
	off_t cpos;
	off_t mpos;

	*skipped = 0;
	if (sksz <= 0)
		return(0);

	/*
	 * we cannot move foward at EOF or error
	 */
	if (lstrval <= 0)
		return(lstrval);

	/*
	 * Safer to read forward on devices where it is hard to find the end of
	 * the media without reading to it. With tapes we cannot be sure of the
	 * number of physical blocks to skip (we do not know physical block
	 * size at this point), so we must only read foward on tapes!
	 */
	if (artyp != ISREG) 
		return(0);

	/*
	 * figure out where we are in the archive
	 */
	if ((cpos = lseek(arfd, (off_t)0L, SEEK_CUR)) >= 0) {
		/* 
	 	 * we can be asked to move farther than there are bytes in this
		 * volume, if so, just go to file end and let normal buf_fill()
		 * deal with the end of file (it will go to next volume by
		 * itself)
	 	 */
		if ((mpos = cpos + sksz) > arsb.st_size) {
			*skipped = arsb.st_size - cpos;
			mpos = arsb.st_size;
		} else
			*skipped = sksz;
		if (lseek(arfd, mpos, SEEK_SET) >= 0)
			return(0);
	}
	syswarn(1, errno, "Foward positioning operation on archive failed");
	lstrval = -1;
	return(-1);
}

/*
 * ar_rev()
 *	move the i/o position within the archive backwards the specified byte
 *	count as supported by the device. With tapes drives we RESET rdblksz to
 *	the PHYSICAL blocksize.
 *	NOTE: We should only be called to move backwards so we can rewrite the
 *	last records (the trailer) of an archive (APPEND).
 * Return:
 *	0 if moved the requested distance, -1 on complete failure
 */

#if __STDC__
int
ar_rev(off_t sksz)
#else
int
ar_rev(sksz)
	off_t sksz;
#endif
{
	off_t cpos;
        struct mtop mb;

	if (sksz <= 0)
		return(0);

	/*
	 * make sure we do not have a flawed archive
	 */
	if (lstrval < 0)
		return(lstrval);

	switch(artyp) {
	case ISPIPE:
		/*
		 * cannot go backwards on these critters
		 */
		break;
	case ISREG:
	case ISBLK:
	case ISCHR:
	default:
		/*
		 * For things other than files, backwards movement has a very
		 * high probability of failure as we really do not know the
		 * true attributes of the device we are talking to (the device
		 * may not even have the ability to lseek() in any direction).
		 * first we figure out where we are in the archive
		 */
		if ((cpos = lseek(arfd, (off_t)0L, SEEK_CUR)) < 0)
			break;

		/*
		 * we may try to go backwards past the start when the archive
		 * is only a single record. If this hapens and we are on a
		 * multi volume archive, we need to go to the end of the
		 * previous volume and continue our movement backwards from
		 * there. (This is really hard to do and is NOT IMPLEMENTED)
		 */
		if ((cpos -= sksz) < (off_t)0L) {
			if (arvol > 1) {
				warn(1,"End of archive is on previous volume.");
				lstrval = -1;
				return(-1);
			}
			cpos = (off_t)0L;
		}
		if (lseek(arfd, cpos, SEEK_SET) < 0)
			break;
		lstrval = 1;
		return(0);
	case ISTAPE:
		/*
	 	 * Calculate and move the proper number of PHYSICAL tape
		 * records. If the sksz is not an even multiple of the physical
		 * tape size, we cannot do the move (this should never happen).
		 * (We also cannot handler trailers spread over two vols).
	 	 */
		if (get_phys() < 0) {
			warn(1, "Cannot determine archive tape blocksize.");
			break;
		}

		if (sksz % phyblk) {
			warn(1,"Tape drive cannot backspace %d bytes (%d phys)",
			    sksz, phyblk);
			lstrval = -1;
			return(-1);
		}

		mb.mt_op = MTBSR;
		mb.mt_count = sksz/phyblk;
		if (ioctl(arfd, MTIOCTOP, &mb) < 0)
			break;

		/*
		 * reset rdblksz to be the device physical blocksize.
		 */
		rdblksz = phyblk;
		lstrval = 1;
		return(0);
	}
	syswarn(1, errno, "Reverse positioning operation on archive failed");
	lstrval = -1;
	return(-1);
}

/*
 * get_phys()
 *	Determine the physical block size on a tape drive. Should only be
 *	when at EOF.
 * Return:
 *	0 if ok, -1 otherwise
 */

#if __STDC__
static int
get_phys(void)
#else
static int
get_phys()
#endif
{
        struct mtop mb;
	char scbuf1[MAXBLK];
	char scbuf2[MAXBLK];

	/*
	 * We can only use this technique when we are at tape EOF (so the
	 * MTBSR will leave just a SINGLE PHYSICAL record between the head
	 * and the end of the tape). Since we may be called more than once,
	 * only the first phyblk detection will be used.
	 */
	if (phyblk > 0)
		return(0);

	mb.mt_op = MTBSR;
	mb.mt_count = 1;
	if ((ioctl(arfd, MTIOCTOP, &mb) < 0) ||
	    ((phyblk = read(arfd, scbuf1, sizeof(scbuf1))) <= 0))
		return(-1);

	/*
	 * check for consistancy, if we cannot repeat, abort. This can only be
	 * a guess, trailer blocks tend to be zero filled!
	 */
	if ((ioctl(arfd, MTIOCTOP, &mb) < 0) ||
	    (read(arfd, scbuf2, sizeof(scbuf2)) != phyblk) ||
	    (bcmp(scbuf1, scbuf2, phyblk) != 0))
		return(-1);
	return(0);
}

/*
 * ar_next()
 *	prompts the user for the next volume in this archive. For devices we
 *	may allow the media to be changed. otherwise a new archive is prompted
 *	for. By pax spec, if there is no controlling tty or an eof is read on
 *	tty input, we quit pax.
 * Return:
 *	0 when ready to continue, -1 when all done
 */

#if __STDC__
int
ar_next(void)
#else
int
ar_next()
#endif
{
	char buf[PAXPATHLEN+2];
	static int freeit = 0;
	sigset_t o_mask;

	/*
	 * WE MUST CLOSE the device. A lot of devices must see last close, (so
	 * things like writing EOF etc will be done) (Watch out ar_close() can
	 * also be called on a signal, so we must prevent a race.
	 */
	if (sigprocmask(SIG_BLOCK, &s_mask, &o_mask) < 0)
		syswarn(1, errno, "Unable to set signal mask");
	ar_close();
	if (sigprocmask(SIG_SETMASK, &o_mask, (sigset_t *)NULL) < 0)
		syswarn(1, errno, "Unable to restore signal mask");

	if (done)
		return(-1);

	tty_prnt("\nATTENTION! Pax archive volume change required.\n");

	/*
	 * if i/o is on stdin or stdout, we cannot reopen it (we do not know
	 * the name), the user will have to type it in.
	 */
	if (strcmp(arcname, STDO) && strcmp(arcname, STDN) && (artyp != ISREG)
	    && (artyp != ISPIPE)) {
		if (artyp == ISTAPE) {
			tty_prnt("%s ready for archive tape volume: %d\n",
				arcname, arvol);
			tty_prnt("Load the NEXT TAPE on the tape drive");
		} else {
			tty_prnt("%s ready for archive volume: %d\n",
				arcname, arvol);
			tty_prnt("Load the NEXT STORAGE MEDIA (if required)");
		}

		if ((act == ARCHIVE) || (act == APPND))
			tty_prnt(" and make sure it is WRITE ENABLED.\n");
		else
			tty_prnt("\n");

		for(;;) {
			tty_prnt("Type \"y\" to continue, \".\" to quit pax,");
			tty_prnt(" or \"s\" to switch to new device.\nIf you");
			tty_prnt(" cannot change storage media, type \"s\"\n");
			tty_prnt("Is the device ready and online? > ");

			if ((tty_read(buf,sizeof(buf))<0) || !strcmp(buf,".")){
				done = 1;
				lstrval = -1;
				tty_prnt("Quitting pax!\n");
				vfpart = 0;
				return(-1);
			}

			if ((buf[0] == '\0') || (buf[1] != '\0')) {
				tty_prnt("%s unknown command, try again\n",buf);
				continue;
			}

			switch (buf[0]) {
			case 'y':
			case 'Y':
				/*
				 * we are to continue with the same device
				 */
				if (ar_open(arcname) >= 0) 
					return(0);
				tty_prnt("Cannot re-open %s, try again\n",
					arcname);
				continue;
			case 's':
			case 'S':
				/*
				 * user wants to open a different device
				 */
				tty_prnt("Switching to a different archive\n");
				break;
			default:
				tty_prnt("%s unknown command, try again\n",buf);
				continue;
			}
			break;
		}
	} else
		tty_prnt("Ready for archive volume: %d\n", arvol);

	/*
	 * have to go to a different archive
	 */
	for (;;) {
		tty_prnt("Input archive name or \".\" to quit pax.\n");
		tty_prnt("Archive name > ");

		if ((tty_read(buf, sizeof(buf)) < 0) || !strcmp(buf, ".")) {
			done = 1;
			lstrval = -1;
			tty_prnt("Quitting pax!\n");
			vfpart = 0;
			return(-1);
		}
		if (buf[0] == '\0') {
			tty_prnt("Empty file name, try again\n");
			continue;
		}
                if (!strcmp(buf, "..")) {
                        tty_prnt("Illegal file name: .. try again\n");
                        continue;
                }
		if (strlen(buf) > PAXPATHLEN) {
			tty_prnt("File name too long, try again\n");
			continue;
		}

		/*
		 * try to open new archive
		 */
		if (ar_open(buf) >= 0) {
			if (freeit) {
				(void)free(arcname);
				freeit = 0;
			}
			if ((arcname = strdup(buf)) == NULL) {
				done = 1;
				lstrval = -1;
				warn(1, "Cannot save archive name.");
				return(-1);
			}
			freeit = 1;
			break;
		}
		tty_prnt("Cannot open %s, try again\n", buf);
		continue;
	}
	return(0);
}
