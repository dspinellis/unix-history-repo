static char Sccsid[] = "ay.c @(#)ay.c	1.1	10/1/82 Berkeley ";
#

/*
 *      APL Buffered I/O routines
 *
 *
 * The following routines perform all of the I/O required by APL.
 * They are designed to maximize buffer utilization while maintaining
 * reliable operation.  For a machine such as the PDP-11, the number
 * of buffers, defined in apl.h, should be around 4 for best results.
 * For a machine such as the VAX 11/780, a larger number may be desirable
 * for better results.
 *
 */

#include "apl.h"                /* Header definitions */
#ifdef NBUF                     /* Don't do any of this unless buffering
				 * was requested by defining NBUF
				 */

#define	realfd(x)	files[x].fd_dup

openf(fname, mode)
char *fname;
{

	int fd;


	/* The first order of business is to open the
	 * file.  If this fails, return -1.
	 */

	if ((fd=open(fname,mode)) < 0) return(fd);


	/* Now, perform all of the work necessary for setting
	 * up the file tables.  This code is shared by "openf"
	 * and "creatf" since both open files.
	 */

	openup(fd);
	return(fd);
}

creatf(fname, mode)
char *fname;
{

	int fd;


	/* The first order of business is to create the
	 * file.  If this fails, return -1.
	 */

	if ((fd=creat(fname,mode)) < 0) return(fd);


	/* Now, perform all of the work necessary for setting
	 * up the file tables.  This code is shared by "openf"
	 * and "creatf" since both open files.
	 */

	openup(fd);
	return(fd);
}

openup(fd)
{
	struct stat stat_buf;
	register struct fds *p, *q;
	register j;


	/* Try to perform an "fstat" call on the file.  This
	 * information is required for all file descriptors
	 * to prevent reading from a file while data destined
	 * for that file is still buffered for output.  If for
	 * some reason the "fstat" fails, arbitrarily set
	 * the major/minor number and inode number to zero.
	 */

	if (fstat(fd, &stat_buf) < 0){
		write(2, "Can't \"stat\" new fd\n", 20);
		stat_buf.st_dev = 0;
		stat_buf.st_ino = 0;
	}


	/* Copy the information into the "files" structure, which
	 * maintains such data on all open files.
	 */

	p = &files[fd];
	p->fd_dev = stat_buf.st_dev;	/* Major/minor device numbers */
	p->fd_ind = stat_buf.st_ino;	/* Inode number */
	p->fd_open = 1;                 /* File is open */
	p->fd_buf = -1;                 /* No buffer is currently assigned */
	p->fd_dup = fd;                 /* All it duplicates is itself */
	p->fd_lastop = 0;               /* Lastop was read (default) */
	p->fd_pipe = !!lseek(fd, 0L, 1);	/* Seek will fail if a pipe */


	/* Determine if the new fd is unique or not.  If not,
	 * make sure any matching fd's know that they no longer
	 * are either.
	 */

	p->fd_uniq = 1;                 /* Assume uniqueness */
	for(j=0; j<NFDS; j++){
		if (!files[j].fd_open) continue;        /* File not open */

		q = &files[realfd(j)];
		if (p->fd_ind == q->fd_ind && p->fd_dev == q->fd_dev)
			p->fd_uniq = q->fd_uniq = 0;
	}
	check();                                        /*DEBUG*/
}

dupf(fd)
{

	int fd2;
	register struct fds *p, *q;


	/* Duplicating a file descriptor does not require an "fstat"
	 * on the new file descriptor, because its device and inode
	 * are already recorded.  However, duplicate file descriptors
	 * complicate I/O because they MUST be linked to the same
	 * buffer.
	 */

	if ((fd2=dup(fd)) < 0) return(fd2);     /* Get new fd */

	/* Copy all of the information into the "files" for the new
	 * file descriptor.  The uniqueness of the original fd is copied
	 * to the new fd -- they both represent the same thing.
	 */

	p = &files[fd];                 /* Point to appropriate places */
	q = &files[fd2];

	q->fd_dev = p->fd_dev;          /* Major/minor device number */
	q->fd_ind = p->fd_ind;          /* Inode number */
	q->fd_buf = -1;                 /* Buffer is assigned to princ. fd */
	q->fd_uniq = p->fd_uniq;        /* Uniqueness of dev/inode */
	q->fd_dup = p->fd_dup;          /* Point new entry to old one */
	q->fd_open = 1;                 /* File is now open */
	q->fd_lastop = p->fd_lastop;    /* Last operation is the same */

	return(fd2);                    /* Return new fd */
}

closef(fd)
{

	register j;
	register struct fds *p, *q;
	int fd2, count;

	/* Closing a file is easier than opening it, but some precautions
	 * are necessary.  For instance, if a "dup" was performed to
	 * obtain some new file descriptor and the original file is
	 * now being closed, the duplicated file must retain all
	 * pertinent information.  Thus, the first order of business
	 * is to scan the list of file descriptors for duplicates.
	 */

	p = &files[fd];                 /* Entry for dying fd */

	count = 0;                      /* Number of remaining dups */
	fd2 = -1;                       /* New fd reference for dups */

	if (p->fd_dup == fd) for (j=0; j<NFDS; j++){
		if (j == fd) continue;          /* Don't look at fd */
		q = &files[j];
		if (!q->fd_open) continue;      /* Not open */
		if (q->fd_dup != fd) continue;
		if (fd2 == -1){
			fd2 = j;                /* New reference */
			q->fd_buf = p->fd_buf;  /* Vital data */
			q->fd_open = 1;
			q->fd_lastop = p->fd_lastop;
		}
		q->fd_dup = fd2;
		count++;
	}


	/* Flush and release the buffer associated with this fd. */

	newbuf(files[realfd(fd)].fd_buf, -1);


	/* Mark the entry in the file descriptor table as "closed"
	 * and check the uniqueness of any remaining fd's pointing to
	 * the same area.  Be sure the buffer is released.
	 */

	p->fd_open = 0;
	p->fd_dup = fd;
	p->fd_buf = -1;

	for(j=0; j<NFDS; j++){
		if (j == fd) continue;          /* Skip the same fd */
		q = &files[fd];
		if (!q->fd_open) continue;      /* Skip closed files */
		if (p->fd_dev == q->fd_dev && p->fd_ind == q->fd_ind) break;
	}

	if (j<NFDS) q->fd_uniq = 1;             /* Assume new fd is unique */
	while(++j < NFDS){                      /* Now check to be sure */
		p = &files[j];
		if (p->fd_dev == q->fd_dev && p->fd_ind == q->fd_ind)
			p->fd_uniq = q->fd_uniq = 0;
	}


	/* Actually perform the close of the fd, and return the status
	 * of that system call.
	 */

	return(close(fd));
}


initbuf()
{

	/* Initialize buffer structure, then use "openup" to set up
	 * file descriptors 0, and 1, which should already be
	 * open from parent process.  If 0 and 1 have the same inode
	 * and major/minor number they will be considered to be
	 * duplicates.  Fils descriptor 2 will be closed and set to
	 * duplicate file descriptor 1.
	 */

	register j;
	register struct fds *p, *q;


	/* Initialize file descriptor table */

	for(j=0; j<NFDS; j++){
		files[j].fd_open = 0;			/* Closed */
		files[j].fd_dup = j; 			/* Dup = fd */
		files[j].fd_buf = -1;			/* No buffer */
		files[j].fd_pipe = 0;			/* Not pipe */
	}


	/* Initialize buffer table */

	for(j=0; j<NBUF; j++) iobuf[j].b_fd = -1;       /* Available */


	/* "Open" devices 0, and 1 */

	openup(0);
	openup(1);
	p = &files[0];
	q = &files[1];
	if (p->fd_ind == q->fd_ind && p->fd_dev == q->fd_dev){
		q->fd_dup = 0;
		p->fd_uniq = q->fd_uniq = 1;
	}


	/* File descriptor 2 duplicates fd 1 */

	close(2);
	dupf(1);
}

newbuf(bufnum, fd)
{

	register struct iobuf *bp;
	register struct fds *fp;
	register j;
	static int bufcnt = 0;


	/* The two arguments for this routine specify a buffer to
	 * be operated upon and a file descriptor.  There are three
	 * legal cases:
	 *
	 * bufnum < 0,  fd >= 0 -- assign new buffer to file descriptor fd
	 * bufnum >= 0, fd >= 0 -- assign buffer "bufnum" to fd
	 * bufnum >= 0, fd < 0  -- de-assign buffer "bufnum"
	 */

	if ((bufnum < 0 && fd < 0) || bufnum >= NBUF || fd >= NFDS)
		return(-1);                             /* Invalid args */

	fd = (fd < 0) ? fd : realfd(fd);                /* Handle dups */


	/* Step one -- if a buffer was specified, flush it.  If the
	 * last operation was a write, then write out the rest of
	 * the buffer.  If the last operation was a read, then perform
	 * a seek backwards on the corresponding fd to reposition the
	 * next read.
	 */

	if (bufnum >= 0){
		bp = &iobuf[bufnum];
		if (bp->b_len && (bp->b_fd >= 0))
			if (files[bp->b_fd].fd_lastop)
				write(bp->b_fd, bp->b_buf, bp->b_len);
			else
				lseek(bp->b_fd, (long)-bp->b_len, 1);

		bp->b_len = 0;                  /* Reset length */
		bp->b_next = 0;                 /* Reset next position */


		/* Step one point five -- if a file descriptor was
		 * specified, check to insure that it is open.  Then,
		 * reassign the buffer to that fd.
		 */

		if(bp->b_fd >= 0)               /* Drop old assignment */
			files[bp->b_fd].fd_buf = -1;

		bp->b_fd = fd;                  /* Give buffer new fd */
		if (fd < 0) return(0);          /* If fd < 0, done */

		fp = &files[fd];                /* Point to new structure */

		if (!fp->fd_open){              /* New file is not open */
			bp->b_fd = -1;          /* Drop assignment */
			return(-1);
		}

		fp->fd_buf = bufnum;            /* New assignment */
		return(0);

	}


	/* Step two -- if no buffer was specified, but a file descriptor
	 * was, then some existing buffer must be allocated to that fd.
	 */

	fp = &files[fd];


	/* First, check to see if a buffer is already assigned */

	for (j=0; j<NBUF; j++)
		if (iobuf[j].b_fd == fd){
			bufcnt = j;
			goto recursive;
		}


	/* Next, scan the table of buffers looking for one
	 * which is not assigned.
	 */

	for (j=0; j<NBUF; j++) if (iobuf[j].b_fd < 0){
		bufcnt = j;
		goto recursive;
	}


	/* If no vacant buffer was found, then a buffer must
	 * be allocated arbitrarily.  The static local variable
	 * "bufcnt" is used for this purpose.  It contains the
	 * number of the last assigned buffer.  Buffers are
	 * switched in a sort-of "round robin" approach.
	 */

	j = bufcnt;
	do {
		bp = &iobuf[j];
		fp = &files[bp->b_fd];
		if (fp->fd_pipe && (!fp->fd_lastop) && bp->b_len > 0)
			continue;
		bufcnt = j;
		goto recursive;
	} while((j = (j+1)%NBUF) != bufcnt);
	return(-1);


	/* Now, with a recursive call to this routine,
	 * switch to the new buffer.
	 */

recursive: return(newbuf(bufcnt, fd));

}

#ifndef realfd
realfd(fd)
{

	register struct fds *fp;


	/* Process duplicate file descriptors.  If the main fd is
	 * nonnegative, use the value of the dup number for the true fd.
	 * No change is made if the file is closed.
	 */

	fp = &files[fd];
	if (!fp->fd_open) return(fd);
	return(fp->fd_dup < 0 ? fd : fp->fd_dup);
}
#endif


readf(fd, buffer, length)
char *buffer;
{

	register struct fds *fp;
	register struct iobuf *bp;
	register j;
	int change, trlog, again;


	/* Handle duplicate files first.  If this file descriptor is
	 * a duplicate of another one, then the other's buffer and
	 * other information must be used.
	 */

	fd = realfd(fd);


	/* If a read is to be performed on a file, all output to that
	 * file must first be flushed.  If the file descriptor's last
	 * function was output, use "newbuf" to flush the output,
	 * retaining the buffer, and then set the mode to input.  This
	 * must be done for all entries in "files" which have the same
	 * major/minor number as the current file.
	 */



	fp = &files[fd];
	if (!fp->fd_open) return(-1);           /* File not open */
	if (fp->fd_uniq){
		if (fp->fd_lastop && fp->fd_buf >= 0)
			newbuf(fp->fd_buf, fd);
	} else for(j=0; j<NFDS; j++){
		fp = &files[realfd(j)];
		if (fp->fd_dev == files[fd].fd_dev
			&& fp->fd_ind == files[fd].fd_ind
			&& fp->fd_open
			&& fp->fd_lastop
			&& fp->fd_buf >= 0){
				newbuf(fp->fd_buf, j);
		}
	}


	/* The above code nicely took care of any buffer associated
	 * with the current fd.  Now, set the last operation on this fd
	 * to read, and if no buffer is currently assigned, get one.
	 */

	fp = &files[fd];
	fp->fd_lastop = 0;

	if (fp->fd_buf < 0)
		if (newbuf(-1, fd) < 0) return(read(fd, buffer, length));


	/* The flag "again" determines whether or not the read buffer
	 * should be refilled when the end of it is reached.  Basically,
	 * "again" is set to 0 when a read of less than one buffer length
	 * occurs.  This way, terminal reads can stop at the carriage
	 * return, but disc reads can still buffer in BLEN-byte chunks
	 */

	again = 1;


	/* The variable "trlog" keeps track of how many bytes have been
	 * transferred into the buffer supplied by the routine which
	 * called "readf".  Initially, this number is -1 (EOF).
	 */

	trlog = -1;


	/* The main loop is rather simple -- the buffer is continually
	 * emptied and refilled until all of the requested data has been
	 * transferred.
	 */

	bp = &iobuf[fp->fd_buf];
	while(1){
		if (length <= 0) return(trlog);
		if (bp->b_len == 0 && again){
			again = (bp->b_len=read(fd,bp->b_buf,BLEN)) == BLEN;
			bp->b_next = 0;
		}
		if (bp->b_len <= 0) return(trlog);
		if (trlog < 0) trlog++;
		change = (bp->b_len < length) ? bp->b_len : length;
		for (j=0; j<change; j++)
			buffer[trlog+j] = bp->b_buf[bp->b_next+j];
		trlog += change;
		bp->b_len -= change;
		bp->b_next += change;
		length -= change;
	}
}


writef(fd, buffer, length)
char *buffer;
{

	register struct fds *fp;
	register struct iobuf *bp;
	register j;
	int change, trlog, again;


	/* Handle duplicate files first.  If this file descriptor is
	 * a duplicate of another one, then the other's buffer and
	 * other information must be used.
	 */

	fd = realfd(fd);


	/* If a write is to be performed on a file, all input from that
	 * file must first be flushed.  If the file descriptor's last
	 * function was input, use "newbuf" to flush the input,
	 * retaining the buffer, and then set the mode to output.  This
	 * must be done for all entries in "files" which have the same
	 * major/minor number as the current file.
	 */

	fp = &files[fd];
	if (!fp->fd_open) return(-1);            /* File not open */
	if (fp->fd_uniq){
		if ((!fp->fd_lastop) && fp->fd_buf >= 0)
			newbuf(fp->fd_buf, fd);

	} else for(j=0; j<NFDS; j++){
		fp = &files[realfd(j)];
		if (fp->fd_dev == files[fd].fd_dev
			&& fp->fd_ind == files[fd].fd_ind
			&& fp->fd_open
			&& (!fp->fd_lastop)
			&& fp->fd_buf >= 0){
				newbuf(fp->fd_buf, j);
		}
	}


	/* The above code nicely took care of any buffer associated
	 * with the current fd.  Now, set the last operation on this fd
	 * to write, and if no buffer is currently assigned, get one.
	 */

	fp = &files[fd];
	fp->fd_lastop = 1;

	if (fp->fd_buf < 0)
		if (newbuf(-1, fd) < 0) return(write(fd,buffer,length));


	/* The main loop of output, like the main loop for input, is
	 * rather simple.  In fact, output is easier.  It is only
	 * necessary to check when the buffer is full and to flush it
	 * as necessary to the file.
	 */

	bp = &iobuf[fp->fd_buf];
	trlog = 0;
	while(1){
		if (!length) return(trlog);
		if (bp->b_len >= BLEN){
			write(fd, bp->b_buf, bp->b_len);
			bp->b_next = bp->b_len = 0;
		}

		change = ((BLEN - bp->b_len) < length) ?
			(BLEN - bp->b_len) : length;

		for (j=0; j<change; j++)
			bp->b_buf[j+bp->b_next] = buffer[j+trlog];

		trlog += change;
		bp->b_next = bp->b_len += change;
		length -= change;
	}
}

long lseekf(fd, p1, p2)
int fd, p2;
long p1;
{

	register struct fds *fp;
	register j;
	long lseek();


	/* A seek on a file requires a flush of the buffer for that
	 * file and for any other file descriptors which are pointing
	 * to the same file.
	 */

	fd = realfd(fd);                /* Take care of dups */
	fp = &files[fd];

	if (!fp->fd_open) return(-1);   /* not open */

	if (fp->fd_uniq){
		if (fp->fd_buf >= 0) newbuf(fp->fd_buf, fd);
	} else for(j=0; j<NFDS; j++){
		fp = &files[j];
		if (fp->fd_dev == files[fd].fd_dev
			&& fp->fd_ind == files[fd].fd_ind
			&& fp->fd_open
			&& fp->fd_buf >= 0)
				newbuf(fp->fd_buf, j);
	}


	/* Now that all of the preliminaries are over, the actual
	 * seek can be performed.
	 */

	return(lseek(fd, p1, p2));
}

fstatf(fd, stat_buf)
struct stat *stat_buf;
{

	register fd2;

	fd2 = realfd(fd);
	newbuf(files[fd2].fd_buf, fd2);
	return(fstat(fd, stat_buf));

}

bflush(){

	register j;

	/* Flush all buffers */

	for(j=0; j<NBUF; j++) newbuf(j, -1);
}

check()
{
	register j, k;

	for (j=0; j<NFDS; j++)
		if ((k=files[j].fd_buf) >= 0)
			if (iobuf[k].b_fd != j){
				write(2, "CONSISTENCY CHECK!\n", 19);
				if (files[j].fd_dup != j)
					write(2, "BAD DUP ENTRY\n", 14);
			}


	for (j=0; j<NBUF; j++)
		if ((k=iobuf[j].b_fd) >= 0)
			if (files[k].fd_buf != j)
				write(2, "CONSISTENCY CHECK2!\n", 20);

}

#endif

empty(fd){

	struct stat sbuf;
	register struct fds *fp;

	/* Simulate the Rand Corp.'s "empty" system call on a
	 * V7 system by seeing if the given fd is a pipe, and if
	 * so, whether or not it is empty.
	 */

#ifdef NBUF
	fp = &files[realfd(fd)];

	if (!fp->fd_pipe || !fp->fd_open)
		return(-1);		/* Not a pipe */

	if (fp->fd_buf >= 0 && iobuf[fp->fd_buf].b_len > 0)
		return(0);		/* Data pending in buffer */
#endif

	if (fstat(fd, &sbuf) < 0)
		return(-1);		/* Can't "stat" it */

	return(sbuf.st_size == 0);
}
