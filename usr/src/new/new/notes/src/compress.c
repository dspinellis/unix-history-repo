static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"

/*
 *	compress(io) struct io_f
 *	compresses the notefile specified. All wasted space
 *	reclaimed. The process is a simple one which, like dcheck,
 *	does not work so well on active file systems.
 *	As a consequence, the director options (which call this)
 *	require the notefile to be closed before allowing compression
 *	to take place. 
 *	The code generates 3 scratch files, corresponding with the
 *	two index files and the text file. These are made to
 *	represent a virgin notefile. The descriptor is copied over
 *	with the appropriate fields zapped, and then we go through
 *	a cycle of (read note; write note; (read resp; write resp))
 *	until all the notes and responses are moved over.
 *	the new files are then copied back into place.
 *	
 *	Returns:	0 - all successful
 *			-1 - if notesfiles compressed already
 *
 *			otherwise will core dump with the notefile
 *			in a shambles from the users view point,
 *			but still recoverable by a hotshot-pro.
 *
 *	Original Coding:	Ray Essick	January 1981
 */

compress(io, lockflag, numnotes, numresps)
struct io_f *io;
int lockflag;
int *numnotes, *numresps;
{
	struct io_f tmpio;		/* scratch notefile */
	struct note_f   note;		/* hold a note record */
	struct resp_f   resp;		/* hold the response format */
	char    fn1[WDLEN], fn2[WDLEN], fn3[WDLEN];
	char 	on1[WDLEN], on2[WDLEN], on3[WDLEN];
	char	txtfn[WDLEN];		/* hold text going between files */
	char	cmd[CMDLEN];		/* monster move command */
	struct daddr_f  where;
	FILE *txtfile;
	int     nnotes, nresps, dint, roffset, num, rblock;
	register int    newnum, presps, rnum;
	struct daddr_f  daddr;


	/* build names of files - in notefile directory */
	sprintf(fn1, "%s/%s/tmp.%s", MSTDIR, io->nf, INDEXN); /* new files */
	sprintf(fn2, "%s/%s/tmp.%s", MSTDIR, io->nf, INDEXR);
	sprintf(fn3, "%s/%s/tmp.%s", MSTDIR, io->nf, TEXT);
	sprintf(on1, "%s/%s/%s", MSTDIR, io->nf, INDEXN);/* old files */
	sprintf(on2, "%s/%s/%s", MSTDIR, io->nf, INDEXR);
	sprintf(on3, "%s/%s/%s", MSTDIR, io->nf, TEXT);

	/*
	 * if the notesfile is not already locked,
	 * do so now.  This also insures signals will
	 * be ignored
	 */
	if (lockflag)
		lock(io, 'n');			/* lock up the notefile */
	x ((tmpio.fidndx = creat(fn1, 0600)) < 0, "compress: create nindex");
	x ((tmpio.fidrdx = creat(fn2, 0600)) < 0, "compress: create rindex");
	x ((tmpio.fidtxt = creat(fn3, 0600)) < 0, "compress: create txt");

	dint = 0;				/* resp index free pointer */
	daddr.addr = sizeof daddr;		/* and for text file */
	x (write(tmpio.fidrdx, &dint, sizeof(dint)) != sizeof(dint),
		"compress: resp ptr");
	x (write(tmpio.fidtxt, &daddr, sizeof(daddr)) != sizeof(daddr),
		"Compress: text ptr");


	closenf(&tmpio);				/* close them up */

#ifdef BSD4.1c
	/* open R/W */
	x ((tmpio.fidndx = open(fn1, O_RDWR, 0)) < 0, "compress: reopen 1");
	x ((tmpio.fidrdx = open(fn2, O_RDWR, 0)) < 0, "compress: reopen 2");
	x ((tmpio.fidtxt = open(fn3, O_RDWR, 0)) < 0, "compress: reopen 3");
#else
	x ((tmpio.fidndx = open(fn1, 2)) < 0, "compress: reopen 1");
	x ((tmpio.fidrdx = open(fn2, 2)) < 0, "compress: reopen 2");
	x ((tmpio.fidtxt = open(fn3, 2)) < 0, "compress: reopen 3");
#endif BSD4.1c

	strmove(io->nf, tmpio.nf);	/* copy over notefile name */
	getdscr(io, &tmpio.descr);		/* grab descriptor */
	if (io->descr.d_stat & NFINVALID) {
		/* fix to clean up things, from Steve Potter, Tektronix */
		closenf(&tmpio);
		x (unlink(fn1) < 0, "compress: remove tmp 1");
		x (unlink(fn2) < 0, "compress: remove tmp 2");
		x (unlink(fn3) < 0, "compress: remove tmp 3");
		/* end of fix */

		if (lockflag)
			unlock(io, 'n');
		return(-1);
	}
	nnotes = nresps = 0;		/* init counts of living notes */
	sprintf(txtfn, "/tmp/nfc%d", getpid());
	tmpio.descr.d_nnote = 0;		/* reset note count */
	putdscr(&tmpio, &tmpio.descr);	/* place it into the file */

	if (io->descr.d_plcy) {		/* copy the policy note over */

		getnrec(io, 0, &note);		/* descriptor */
		x ((txtfile = fopen(txtfn, "w")) == NULL, "compress:bad txt");
		pageout(io, &note.n_addr, txtfile);
		fclose(txtfile);
		x ((txtfile = fopen(txtfn, "r")) == NULL,
			"compress: bad txt read");
		pagein(&tmpio, txtfile, &where);
		fclose(txtfile);

		putnote(&tmpio, &where, note.ntitle, note.n_stat, &note,
			&note.n_auth, POLICY, NOLOCKIT, NOADDID, note.n_from,
			NOADDTIME);
	}

	for (num = 1; num <= io->descr.d_nnote; num++) {
		getnrec(io, num, &note);
		if (note.n_stat & DELETED)
			continue;	/* deleted - we throw away */
		x ((txtfile = fopen(txtfn, "w")) == NULL, "compress:bad txt");
		pageout(io, &note.n_addr, txtfile);
		fclose(txtfile);

		x ((txtfile = fopen(txtfn, "r")) == NULL,
			"compress: bad txt read");
		pagein(&tmpio, txtfile, &where);
		fclose(txtfile);

		/* save max number of responses */
		presps = note.n_nresp;
		newnum = putnote (&tmpio, &where, note.ntitle, note.n_stat,
			&note, &note.n_auth, NOPOLICY, NOLOCKIT, NOADDID,
			note.n_from, NOADDTIME);
		nnotes++;				/* add a note */

		for (rnum = 1; rnum <= presps; rnum++) {
			/* process responses */
			if (lrsp(io, num, rnum, &resp, &roffset, &rblock) != 0) {
				/* bad response chain - drop rest */
				break;
			}
			x ((txtfile = fopen(txtfn, "w")) == NULL,
				"compress:bad txt");
			pageout(io, &resp.r_addr[roffset], txtfile);
			fclose(txtfile);
			x ((txtfile = fopen(txtfn, "r")) == NULL,
				"compress: bad txt read");
			pagein(&tmpio, txtfile, &where);
			fclose(txtfile);
			putresp(&tmpio, &where, resp.r_stat[roffset], newnum,
				&resp.r_when[roffset], &resp.r_auth[roffset],
				&note, NOLOCKIT, &resp.r_id[roffset], NOADDID,
				resp.r_from[roffset], NOADDTIME,
				&resp.r_rcvd[roffset]);
			nresps++;			/* count responses */
		}
	}

	/*
	 * well, we have now copied the entire notefile over,
	 * so the time has come to move it back into the correct
	 * file names - we will do this by linking
	 */
	closenf(&tmpio);				/* close the new one */

	getdscr(io, &io->descr);
	io->descr.d_stat |= NFINVALID;		/* mark it bad */
	putdscr(io, &io->descr);
	closenf(io);				/* close the old one */

#ifdef BSD4.1c
	x (rename(fn1,on1) < 0, "compress: rename 1");
	x (rename(fn2,on2) < 0, "compress: rename 1");
	x (rename(fn3,on3) < 0, "compress: rename 1");
#else
	x (unlink(on1) < 0, "compress: remove old 1");
	x (link(fn1, on1) < 0, "compress: link new 1");
	x (unlink(fn1) < 0, "compress: remove tmp 1");
	x (unlink(on2) < 0, "compress: remove old 2");
	x (link(fn2, on2) < 0, "compress: link new 2");
	x (unlink(fn2) < 0, "compress: remove tmp 2");
	x (unlink(on3) < 0, "compress: remove old 3");
	x (link(fn3, on3) < 0, "compress: link new 3");
	x (unlink(fn3) < 0, "compress: remove tmp 3");
#endif BSD4.1c
	unlink(txtfn);

	opennf(io, io->nf);			/* relink to new one */
	getdscr(io, &io->descr);		/* get the new descriptor */

	if (lockflag)
		unlock(io, 'n');
	*numnotes = nnotes;
	*numresps = nresps;

	return(0);					/* return ok */
}
