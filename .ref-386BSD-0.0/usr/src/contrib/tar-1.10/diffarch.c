/* Diff files from a tar archive.
   Copyright (C) 1988 Free Software Foundation

This file is part of GNU Tar.

GNU Tar is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Tar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Tar; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * Diff files from a tar archive.
 *
 * Written 30 April 1987 by John Gilmore, ihnp4!hoptoad!gnu.
 *
 * @(#) diffarch.c 1.10 87/11/11 - gnu
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef BSD42
#include <sys/file.h>
#endif

#ifndef MSDOS
#include <sys/ioctl.h>
#if !defined(USG) || defined(HAVE_MTIO)
#include <sys/mtio.h>
#endif
#endif

#ifdef USG
#include <fcntl.h>
#endif

/* Some systems don't have these #define's -- we fake it here. */
#ifndef O_RDONLY
#define	O_RDONLY	0
#endif
#ifndef	O_NDELAY
#define	O_NDELAY	0
#endif

#ifndef S_IFLNK
#define lstat stat
#endif

extern int errno;			/* From libc.a */
extern char *valloc();			/* From libc.a */

#include "tar.h"
#include "port.h"
#include "rmt.h"

extern union record *head;		/* Points to current tape header */
extern struct stat hstat;		/* Stat struct corresponding */
extern int head_standard;		/* Tape header is in ANSI format */

extern void print_header();
extern void skip_file();
extern void skip_extended_headers();

extern FILE *msg_file;

int now_verifying = 0;		/* Are we verifying at the moment? */

int	diff_fd;		/* Descriptor of file we're diffing */

char	*diff_buf = 0;		/* Pointer to area for reading
					   file contents into */

char	*diff_dir;		/* Directory contents for LF_DUMPDIR */

int different = 0;

/*struct sp_array *sparsearray;
int 		sp_ar_size = 10;*/
/*
 * Initialize for a diff operation
 */
diff_init()
{

	/*NOSTRICT*/
	diff_buf = (char *) valloc((unsigned)blocksize);
	if (!diff_buf) {
		msg("could not allocate memory for diff buffer of %d bytes",
			blocksize);
		exit(EX_ARGSBAD);
	}
}

/*
 * Diff a file against the archive.
 */
void
diff_archive()
{
	register char *data;
	int check, namelen;
	int err;
	long offset;
	struct stat filestat;
	int compare_chunk();
	int compare_dir();
#ifndef __MSDOS__
	dev_t	dev;
	ino_t	ino;
#endif
	char *get_dir_contents();
	long from_oct();
	long lseek();

	errno = EPIPE;			/* FIXME, remove perrors */

	saverec(&head);			/* Make sure it sticks around */
	userec(head);			/* And go past it in the archive */
	decode_header(head, &hstat, &head_standard, 1);	/* Snarf fields */

	/* Print the record from 'head' and 'hstat' */
	if (f_verbose) {
		if(now_verifying)
			fprintf(msg_file,"Verify ");
		print_header();
	}

	switch (head->header.linkflag) {

	default:
		msg("Unknown file type '%c' for %s, diffed as normal file",
			head->header.linkflag, head->header.name);
		/* FALL THRU */

	case LF_OLDNORMAL:
	case LF_NORMAL:
	case LF_SPARSE:
	case LF_CONTIG:
		/*
		 * Appears to be a file.
		 * See if it's really a directory.
		 */
		namelen = strlen(head->header.name)-1;
		if (head->header.name[namelen] == '/')
			goto really_dir;

		
		if(do_stat(&filestat)) {
			if (head->header.isextended)
				skip_extended_headers();
			skip_file((long)hstat.st_size);
			different++;
			goto quit;
		}

		if ((filestat.st_mode & S_IFMT) != S_IFREG) {
			fprintf(msg_file, "%s: not a regular file\n",
				head->header.name);
			skip_file((long)hstat.st_size);
			different++;
			goto quit;
		}

		filestat.st_mode &= ~S_IFMT;
		if (filestat.st_mode != hstat.st_mode)
			sigh("mode");
		if (filestat.st_uid  != hstat.st_uid)
			sigh("uid");
		if (filestat.st_gid  != hstat.st_gid)
			sigh("gid");
		if (filestat.st_mtime != hstat.st_mtime)
			sigh("mod time");
		if (head->header.linkflag != LF_SPARSE &&
				filestat.st_size != hstat.st_size) {
			sigh("size");
			skip_file((long)hstat.st_size);
			goto quit;
		}

		diff_fd = open(head->header.name, O_NDELAY|O_RDONLY);

		if (diff_fd < 0 && !f_absolute_paths) {
			char tmpbuf[NAMSIZ+2];

			tmpbuf[0]='/';
			strcpy(&tmpbuf[1],head->header.name);
			diff_fd=open(tmpbuf, O_NDELAY|O_RDONLY);
		}
		if (diff_fd < 0) {
			msg_perror("cannot open %s",head->header.name);
			if (head->header.isextended)
				skip_extended_headers();
			skip_file((long)hstat.st_size);
			different++;
			goto quit;
		}
		/*
		 * Need to treat sparse files completely differently here.
		 */
		if (head->header.linkflag == LF_SPARSE)
			diff_sparse_files(hstat.st_size);
		else 
			wantbytes((long)(hstat.st_size),compare_chunk);

		check = close(diff_fd);
		if (check < 0)
			msg_perror("Error while closing %s",head->header.name);

	quit:
		break;

#ifndef __MSDOS__
	case LF_LINK:
		if(do_stat(&filestat))
			break;
		dev = filestat.st_dev;
		ino = filestat.st_ino;
		err = stat(head->header.linkname, &filestat);
		if (err < 0) {
			if (errno==ENOENT) {
				fprintf(msg_file, "%s: does not exist\n",head->header.name);
			} else {
				msg_perror("cannot stat file %s",head->header.name);
			}
			different++;
			break;
		}
		if(filestat.st_dev!=dev || filestat.st_ino!=ino) {
			fprintf(msg_file, "%s not linked to %s\n",head->header.name,head->header.linkname);
			break;
		}
		break;
#endif

#ifdef S_IFLNK
	case LF_SYMLINK:
	{
		char linkbuf[NAMSIZ+3];
		check = readlink(head->header.name, linkbuf,
				 (sizeof linkbuf)-1);
		
		if (check < 0) {
			if (errno == ENOENT) {
				fprintf(msg_file,
					"%s: no such file or directory\n",
					head->header.name);
			} else {
				msg_perror("cannot read link %s",head->header.name);
			}
			different++;
			break;
		}

		linkbuf[check] = '\0';	/* Null-terminate it */
		if (strncmp(head->header.linkname, linkbuf, check) != 0) {
			fprintf(msg_file, "%s: symlink differs\n",
				head->header.linkname);
			different++;
		}
	}
		break;
#endif

	case LF_CHR:
		hstat.st_mode |= S_IFCHR;
		goto check_node;

#ifdef S_IFBLK
	/* If local system doesn't support block devices, use default case */
	case LF_BLK:
		hstat.st_mode |= S_IFBLK;
		goto check_node;
#endif

#ifdef S_IFIFO
	/* If local system doesn't support FIFOs, use default case */
	case LF_FIFO:
		hstat.st_mode |= S_IFIFO;
		hstat.st_rdev = 0;		/* FIXME, do we need this? */
		goto check_node;
#endif

	check_node:
		/* FIXME, deal with umask */
		if(do_stat(&filestat))
			break;
		if(hstat.st_rdev != filestat.st_rdev) {
			fprintf(msg_file, "%s: device numbers changed\n", head->header.name);
			different++;
			break;
		}
		if(hstat.st_mode != filestat.st_mode) {
			fprintf(msg_file, "%s: mode or device-type changed\n", head->header.name);
			different++;
			break;
		}
		break;

	case LF_DUMPDIR:
		data=diff_dir=get_dir_contents(head->header.name,0);
		wantbytes((long)(hstat.st_size),compare_dir);
		free(data);
		/* FALL THROUGH */

	case LF_DIR:
		/* Check for trailing / */
		namelen = strlen(head->header.name)-1;
	really_dir:
		while (namelen && head->header.name[namelen] == '/')
			head->header.name[namelen--] = '\0';	/* Zap / */

		if(do_stat(&filestat))
			break;
		if((filestat.st_mode&S_IFMT)!=S_IFDIR) {
			fprintf(msg_file, "%s is no longer a directory\n",head->header.name);
			different++;
			break;
		}
		if((filestat.st_mode&~S_IFMT) != hstat.st_mode)
			sigh("mode");
		break;

	case LF_VOLHDR:
		break;

	case LF_MULTIVOL:
		namelen = strlen(head->header.name)-1;
		if (head->header.name[namelen] == '/')
			goto really_dir;

		if(do_stat(&filestat))
			break;

		if ((filestat.st_mode & S_IFMT) != S_IFREG) {
			fprintf(msg_file, "%s: not a regular file\n",
				head->header.name);
			skip_file((long)hstat.st_size);
			different++;
			break;
		}

		filestat.st_mode &= ~S_IFMT;
		offset = from_oct(1+12, head->header.offset);
		if (filestat.st_size != hstat.st_size + offset) {
			sigh("size");
			skip_file((long)hstat.st_size);
			different++;
			break;
		}

		diff_fd = open(head->header.name, O_NDELAY|O_RDONLY);

		if (diff_fd < 0) {
			msg_perror("cannot open file %s",head->header.name);
			skip_file((long)hstat.st_size);
			different++;
			break;
		}
		err = lseek(diff_fd, offset, 0);
		if(err!=offset) {
			msg_perror("cannot seek to %ld in file %s",offset,head->header.name);
			different++;
			break;
		}

		wantbytes((long)(hstat.st_size),compare_chunk);

		check = close(diff_fd);
		if (check < 0) {
			msg_perror("Error while closing %s",head->header.name);
		}
		break;

	}

	/* We don't need to save it any longer. */
	saverec((union record **) 0);	/* Unsave it */
}

int
compare_chunk(bytes,buffer)
long bytes;
char *buffer;
{
	int err;

	err=read(diff_fd,diff_buf,bytes);
	if(err!=bytes) {
		if(err<0) {
			msg_perror("can't read %s",head->header.name);
		} else {
			fprintf(msg_file,"%s: could only read %d of %d bytes\n",head->header.name,err,bytes);
		}
		different++;
		return -1;
	}
	if(bcmp(buffer,diff_buf,bytes)) {
		fprintf(msg_file, "%s: data differs\n",head->header.name);
		different++;
		return -1;
	}
	return 0;
}

int
compare_dir(bytes,buffer)
long bytes;
char *buffer;
{
	if(bcmp(buffer,diff_dir,bytes)) {
		fprintf(msg_file, "%s: data differs\n",head->header.name);
		different++;
		return -1;
	}
	diff_dir+=bytes;
	return 0;
}

/*
 * Sigh about something that differs.
 */
sigh(what)
	char *what;
{

	fprintf(msg_file, "%s: %s differs\n",
		head->header.name, what);
}

verify_volume()
{
	int status;
#ifdef MTIOCTOP
	struct mtop t;
	int er;
#endif

	if(!diff_buf)
		diff_init();
#ifdef MTIOCTOP
	t.mt_op = MTBSF;
	t.mt_count = 1;
	if((er=rmtioctl(archive,MTIOCTOP,&t))<0) {
		if(errno!=EIO || (er=rmtioctl(archive,MTIOCTOP,&t))<0) {
#endif
			if(rmtlseek(archive,0L,0)!=0) {
				/* Lseek failed.  Try a different method */
				msg_perror("Couldn't rewind archive file for verify");
				return;
			}
#ifdef MTIOCTOP
		}
	}
#endif
	ar_reading=1;
	now_verifying = 1;
	fl_read();
	for(;;) {
		status = read_header();
		if(status==0) {
			unsigned n;

			n=0;
			do {
				n++;
				status=read_header();
			} while(status==0);
			msg("VERIFY FAILURE: %d invalid header%s detected!",n,n==1?"":"s");
		}
		if(status==2 || status==EOF)
			break;
		diff_archive();
	}
	ar_reading=0;
	now_verifying = 0;

}

int do_stat(statp)
struct stat *statp;
{
	int err;

	err = f_follow_links ? stat(head->header.name, statp) : lstat(head->header.name, statp);
	if (err < 0) {
		if (errno==ENOENT) {
			fprintf(msg_file, "%s: does not exist\n",head->header.name);
		} else
			msg_perror("can't stat file %s",head->header.name);
/*		skip_file((long)hstat.st_size);
		different++;*/
		return 1;
	} else
		return 0;
}

/*
 * JK
 * Diff'ing a sparse file with its counterpart on the tar file is a 
 * bit of a different story than a normal file.  First, we must know
 * what areas of the file to skip through, i.e., we need to contruct
 * a sparsearray, which will hold all the information we need.  We must
 * compare small amounts of data at a time as we find it.  
 */

diff_sparse_files(filesize)
int	filesize;

{
	int		sparse_ind = 0;
	char		*buf;
	int		buf_size = RECORDSIZE;
	union record 	*datarec;	
	int		err;
	long		numbytes;
	int		amt_read = 0;
	int		size = filesize;

	buf = (char *) malloc(buf_size * sizeof (char));
	
	fill_in_sparse_array();
	

	while (size > 0) {
		datarec = findrec();
		if (!sparsearray[sparse_ind].numbytes)
			break;

		/*
		 * 'numbytes' is nicer to write than
		 * 'sparsearray[sparse_ind].numbytes' all the time ...
		 */
		numbytes = sparsearray[sparse_ind].numbytes;
		
		lseek(diff_fd, sparsearray[sparse_ind].offset, 0);
		/*
		 * take care to not run out of room in our buffer
		 */
		while (buf_size < numbytes) {
			buf = (char *) realloc(buf, buf_size * 2 * sizeof(char));
			buf_size *= 2;
		}
		while (numbytes > RECORDSIZE) {
			if ((err = read(diff_fd, buf, RECORDSIZE)) != RECORDSIZE) {
	 			if (err < 0) 
					msg_perror("can't read %s", head->header.name);
				else
					fprintf(msg_file, "%s: could only read %d of %d bytes\n", 
						err, numbytes);
				break;
			}
			if (bcmp(buf, datarec->charptr, RECORDSIZE)) {
				different++;
				break;
			}
			numbytes -= err;
			size -= err;
			userec(datarec);
			datarec = findrec();
		}
		if ((err = read(diff_fd, buf, numbytes)) != numbytes) {
 			if (err < 0) 
				msg_perror("can't read %s", head->header.name);
			else
				fprintf(msg_file, "%s: could only read %d of %d bytes\n", 
						err, numbytes);
			break;
		}

		if (bcmp(buf, datarec->charptr, numbytes)) {
			different++;
			break;
		}
/*		amt_read += numbytes;
		if (amt_read >= RECORDSIZE) {
			amt_read = 0;
			userec(datarec);
			datarec = findrec();
		}*/
		userec(datarec);
		sparse_ind++;
		size -= numbytes;
	}
	/* 
	 * if the number of bytes read isn't the
	 * number of bytes supposedly in the file,
	 * they're different
	 */
/*	if (amt_read != filesize)
		different++;*/
	userec(datarec);
	free(sparsearray);
	if (different)
		fprintf(msg_file, "%s: data differs\n", head->header.name);

}

/*
 * JK
 * This routine should be used more often than it is ... look into
 * that.  Anyhow, what it does is translate the sparse information
 * on the header, and in any subsequent extended headers, into an
 * array of structures with true numbers, as opposed to character
 * strings.  It simply makes our life much easier, doing so many
 * comparisong and such.
 */
fill_in_sparse_array()
{
	int 	ind;

	/*
	 * allocate space for our scratch space; it's initially
	 * 10 elements long, but can change in this routine if
	 * necessary
	 */
	sp_array_size = 10;
	sparsearray = (struct sp_array *) malloc(sp_array_size * sizeof(struct sp_array));

	/*
	 * there are at most five of these structures in the header
	 * itself; read these in first
	 */
	for (ind = 0; ind < SPARSE_IN_HDR; ind++) {
		if (!head->header.sp[ind].numbytes)
			break;
		sparsearray[ind].offset =
			from_oct(1+12, head->header.sp[ind].offset);
		sparsearray[ind].numbytes =
			from_oct(1+12, head->header.sp[ind].numbytes);
	}
	/*
	 * if the header's extended, we gotta read in exhdr's till
	 * we're done
	 */
	if (head->header.isextended) {
 	    /* how far into the sparsearray we are 'so far' */
	    static int so_far_ind = SPARSE_IN_HDR;	
	    union record *exhdr;
   	    
	    for (;;) {
		exhdr = findrec();
		for (ind = 0; ind < SPARSE_EXT_HDR; ind++) {
			if (ind+so_far_ind > sp_array_size-1) {
				/*
 				 * we just ran out of room in our
				 *  scratch area - realloc it
 				 */
				sparsearray = (struct sp_array *)
					realloc(sparsearray, 
						sp_array_size*2*sizeof(struct sp_array));
				sp_array_size *= 2;
			}
			/*
			 * convert the character strings into longs
			 */
			sparsearray[ind+so_far_ind].offset = 
			    from_oct(1+12, exhdr->ext_hdr.sp[ind].offset);
			sparsearray[ind+so_far_ind].numbytes =
			    from_oct(1+12, exhdr->ext_hdr.sp[ind].numbytes);
		}
		/* 
		 * if this is the last extended header for this
		 * file, we can stop
		 */
		if (!exhdr->ext_hdr.isextended)
			break;
		else {
			so_far_ind += SPARSE_EXT_HDR;
			userec(exhdr);
		}
	    }
	    /* be sure to skip past the last one  */
	    userec(exhdr);
	}
}
