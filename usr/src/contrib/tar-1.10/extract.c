/* Extract files from a tar archive.
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
 * Extract files from a tar archive.
 *
 * Written 19 Nov 1985 by John Gilmore, ihnp4!hoptoad!gnu.
 *
 * @(#) extract.c 1.32 87/11/11 - gnu
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef BSD42
#include <sys/file.h>
#endif

#ifdef USG
#include <fcntl.h>
#endif

#ifdef	MSDOS
#include <fcntl.h>
#endif	/* MSDOS */

/*
 * Some people don't have a #define for these.
 */
#ifndef	O_BINARY
#define	O_BINARY	0
#endif
#ifndef O_NDELAY
#define	O_NDELAY	0
#endif

#ifdef NO_OPEN3
/* We need the #define's even though we don't use them. */
#include "open3.h"
#endif

#ifdef EMUL_OPEN3
/* Simulated 3-argument open for systems that don't have it */
#include "open3.h"
#endif

extern int errno;			/* From libc.a */
extern time_t time();			/* From libc.a */
extern char *index();			/* From libc.a or port.c */

#include "tar.h"
#include "port.h"

extern FILE *msg_file;

extern union record *head;		/* Points to current tape header */
extern struct stat hstat;		/* Stat struct corresponding */
extern int head_standard;		/* Tape header is in ANSI format */

extern char *save_name;
extern long save_totsize;
extern long save_sizeleft;

extern void print_header();
extern void skip_file();
extern void skip_extended_headers();
extern void pr_mkdir();

int make_dirs();			/* Makes required directories */

static time_t now = 0;			/* Current time */
static we_are_root = 0;			/* True if our effective uid == 0 */
static int notumask = ~0;		/* Masks out bits user doesn't want */

/*
 * "Scratch" space to store the information about a sparse file before
 * writing the info into the header or extended header
 */
/*struct sp_array	*sparsearray;*/

/* number of elts storable in the sparsearray */
/*int	sp_array_size = 10;*/

/*
 * Set up to extract files.
 */
extr_init()
{
	int ourmask;

	now = time((time_t *)0);
	if (geteuid() == 0)
		we_are_root = 1;

	/*
	 * We need to know our umask.  But if f_use_protection is set,
	 * leave our kernel umask at 0, and our "notumask" at ~0.
	 */
	ourmask = umask(0);		/* Read it */
	if (!f_use_protection) {
		(void) umask (ourmask);	/* Set it back how it was */
		notumask = ~ourmask;	/* Make umask override permissions */
	}
}


/*
 * Extract a file from the archive.
 */
void
extract_archive()
{
	register char *data;
	int fd, check, namelen, written, openflag;
	long size;
	time_t acc_upd_times[2];
	register int skipcrud;
	register int i;
	int sparse_ind = 0;
	union record *exhdr;	
	int end_nulls;
	
	saverec(&head);			/* Make sure it sticks around */
	userec(head);			/* And go past it in the archive */
	decode_header(head, &hstat, &head_standard, 1);	/* Snarf fields */

	if(f_confirm && !confirm("extract",head->header.name)) {
		if (head->header.isextended)
			skip_extended_headers();
		skip_file((long)hstat.st_size);
		saverec((union record **)0);
		return;
	}

	/* Print the record from 'head' and 'hstat' */
	if (f_verbose)
		print_header();

	/*
	 * Check for fully specified pathnames and other atrocities.
	 *
	 * Note, we can't just make a pointer to the new file name,
	 * since saverec() might move the header and adjust "head".
	 * We have to start from "head" every time we want to touch
	 * the header record.
	 */
	skipcrud = 0;
	while (!f_absolute_paths && '/' == head->header.name[skipcrud]) {
		static int warned_once = 0;

		skipcrud++;	/* Force relative path */
		if (!warned_once++) {
			msg("Removing leading / from absolute path names in the archive.");
		}
	}

	switch (head->header.linkflag) {

	default:
		msg("Unknown file type '%c' for %s, extracted as normal file",
			head->header.linkflag, skipcrud+head->header.name);
		/* FALL THRU */

	/* 
	 * JK - What we want to do if the file is sparse is loop through
	 * the array of sparse structures in the header and read in
	 * and translate the character strings representing  1) the offset
	 * at which to write and 2) how many bytes to write into numbers,
	 * which we store into the scratch array, "sparsearray".  This
	 * array makes our life easier the same way it did in creating
	 * the tar file that had to deal with a sparse file.
	 *
	 * After we read in the first five (at most) sparse structures,
	 * we check to see if the file has an extended header, i.e., 
	 * if more sparse structures are needed to describe the contents
	 * of the new file.  If so, we read in the extended headers
	 * and continue to store their contents into the sparsearray.
	 */
	case LF_SPARSE:
		sp_array_size = 10;
		sparsearray = (struct sp_array *) malloc(sp_array_size * sizeof(struct sp_array));
		for (i = 0; i < SPARSE_IN_HDR; i++) {
			sparsearray[i].offset = 
				from_oct(1+12, head->header.sp[i].offset);
			sparsearray[i].numbytes = 
				from_oct(1+12, head->header.sp[i].numbytes);
			if (!sparsearray[i].numbytes)
				break;
		}
		
/*		end_nulls = from_oct(1+12, head->header.ending_blanks);*/
		
		if (head->header.isextended) {
			/* read in the list of extended headers
			   and translate them into the sparsearray 
			   as before */

			/* static */ int ind = SPARSE_IN_HDR;
			
			for (;;) {
				
				exhdr = findrec();
				for (i = 0; i < SPARSE_EXT_HDR; i++) {
					
					if (i+ind > sp_array_size-1) {
					/*
					 * realloc the scratch area
					 * since we've run out of room --
		 			 */
						sparsearray = (struct sp_array *) 
								realloc(sparsearray,
 								2 * sp_array_size * (sizeof(struct sp_array)));
						sp_array_size *= 2;
					}
					if (!exhdr->ext_hdr.sp[i].numbytes)
						break;
					sparsearray[i+ind].offset = 
						from_oct(1+12, exhdr->ext_hdr.sp[i].offset);
					sparsearray[i+ind].numbytes = 
						from_oct(1+12, exhdr->ext_hdr.sp[i].numbytes);
				}
				if (!exhdr->ext_hdr.isextended) 
					break;
				else {
					ind += SPARSE_EXT_HDR;
					userec(exhdr);
				}
			}
			userec(exhdr);
		}
		
		/* FALL THRU */
	case LF_OLDNORMAL:
	case LF_NORMAL:
	case LF_CONTIG:
		/*
		 * Appears to be a file.
		 * See if it's really a directory.
		 */
		namelen = strlen(skipcrud+head->header.name)-1;
		if (head->header.name[skipcrud+namelen] == '/')
			goto really_dir;

		/* FIXME, deal with protection issues */
	again_file:
		openflag = (f_keep?
			O_BINARY|O_NDELAY|O_WRONLY|O_CREAT|O_EXCL:
			O_BINARY|O_NDELAY|O_WRONLY|O_CREAT|O_TRUNC)
			| ((head->header.linkflag == LF_SPARSE) ? 0 : O_APPEND);			
			/*
			 * JK - The last | is a kludge to solve the problem
			 * the O_APPEND flag  causes with files we are
			 * trying to make sparse:  when a file is opened
			 * with O_APPEND, it writes  to the last place
			 * that something was written, thereby ignoring
			 * any lseeks that we have done.  We add this
			 * extra condition to make it able to lseek when
			 * a file is sparse, i.e., we don't open the new
			 * file with this flag.  (Grump -- this bug caused
			 * me to waste a good deal of time, I might add)
  			 */

		if(f_exstdout) {
			fd = 1;
			goto extract_file;
		}
#ifdef O_CTG
		/*
		 * Contiguous files (on the Masscomp) have to specify
		 * the size in the open call that creates them.
		 */
		if (head->header.linkflag == LF_CONTIG)
			fd = open(skipcrud+head->header.name, openflag | O_CTG,
				hstat.st_mode, hstat.st_size);
		else
#endif
		{
#ifdef NO_OPEN3
			/*
			 * On raw V7 we won't let them specify -k (f_keep), but
			 * we just bull ahead and create the files.
			 */
			fd = creat(skipcrud+head->header.name, 
				hstat.st_mode);
#else
			/*
			 * With 3-arg open(), we can do this up right.
			 */
			fd = open(skipcrud+head->header.name, openflag,
				hstat.st_mode);
#endif
		}

		if (fd < 0) {
			if (make_dirs(skipcrud+head->header.name))
				goto again_file;
			msg_perror("Could not create file %s",skipcrud+head->header.name);
			if (head->header.isextended)
				skip_extended_headers();
			skip_file((long)hstat.st_size);
			goto quit;
		}

	extract_file:
		if (head->header.linkflag == LF_SPARSE) {
			char	*name;
			int	namelen;

			/*
			 * Kludge alert.  NAME is assigned to header.name
			 * because during the extraction, the space that
			 * contains the header will get scribbled on, and
			 * the name will get munged, so any error messages
			 * that happen to contain the filename will look
			 * REAL interesting unless we do this.
			 */
			namelen = strlen(skipcrud+head->header.name);
			name = (char *) malloc((sizeof(char)) * namelen);
			bcopy(skipcrud+head->header.name, name, namelen);
			size = hstat.st_size;
			extract_sparse_file(fd, &size, hstat.st_size,
 						name);
		}			
		else 		
		  for (size = hstat.st_size;
		       size > 0;
		       size -= written) {

			long	offset,
 				numbytes;

			if(f_multivol) {
				save_name=head->header.name;
				save_totsize=hstat.st_size;
				save_sizeleft=size;
			}
			
			/*
			 * Locate data, determine max length
			 * writeable, write it, record that
			 * we have used the data, then check
			 * if the write worked.
			 */
			data = findrec()->charptr;
			if (data == NULL) {	/* Check it... */
				msg("Unexpected EOF on archive file");
				break;
			}
			/*
			 * JK - If the file is sparse, use the sparsearray
			 * that we created before to lseek into the new
			 * file the proper amount, and to see how many
			 * bytes we want to write at that position.
			 */
/*			if (head->header.linkflag == LF_SPARSE) {
				off_t pos;
				
				pos = lseek(fd, (off_t) sparsearray[sparse_ind].offset, 0);
				printf("%d at %d\n", (int) pos, sparse_ind);
				written = sparsearray[sparse_ind++].numbytes;
			} else*/
			written = endofrecs()->charptr - data;
			if (written > size)
				written = size;
			errno = 0;
			check = write(fd, data, written);
			/*
			 * The following is in violation of strict
			 * typing, since the arg to userec
			 * should be a struct rec *.  FIXME.
			 */
			userec((union record *)(data + written - 1));
			if (check == written) continue;
			/*
			 * Error in writing to file.
			 * Print it, skip to next file in archive.
			 */
			if(check<0)
				msg_perror("couldn't write to file %s",skipcrud+head->header.name);
			else
				msg("could only write %d of %d bytes to file %s",written,check,skipcrud+head->header.name);
			skip_file((long)(size - written));
			break;	/* Still do the close, mod time, chmod, etc */
		}

		if(f_multivol)
			save_name = 0;

			/* If writing to stdout, don't try to do anything
			   to the filename; it doesn't exist, or we don't
			   want to touch it anyway */
		if(f_exstdout)
			break;
			
/*		if (head->header.isextended) {
			register union record *exhdr;
			register int i;
			
			for (i = 0; i < 21; i++) {
				long offset;
				
				if (!exhdr->ext_hdr.sp[i].numbytes)
					break;
				offset = from_oct(1+12,
 						exhdr->ext_hdr.sp[i].offset);
				written = from_oct(1+12,
 						exhdr->ext_hdr.sp[i].numbytes);
				lseek(fd, offset, 0);
				check = write(fd, data, written);
				if (check == written) continue;

			}
			

		}*/
 		check = close(fd);
		if (check < 0) {
			msg_perror("Error while closing %s",skipcrud+head->header.name);
		}

		
	set_filestat:

		/*
		 * If we are root, set the owner and group of the extracted
		 * file.  This does what is wanted both on real Unix and on
		 * System V.  If we are running as a user, we extract as that
		 * user; if running as root, we extract as the original owner.
		 */
		if (we_are_root || f_do_chown) {
			if (chown(skipcrud+head->header.name, hstat.st_uid,
				  hstat.st_gid) < 0) {
				msg_perror("cannot chown file %s to uid %d gid %d",skipcrud+head->header.name,hstat.st_uid,hstat.st_gid);
			}
		}

		/*
		 * Set the modified time of the file.
		 * 
		 * Note that we set the accessed time to "now", which
		 * is really "the time we started extracting files".
		 * unless f_gnudump is used, in which case .st_atime is used
		 */
		if (!f_modified) {
			/* fixme if f_gnudump should set ctime too, but how? */
			if(f_gnudump)
				acc_upd_times[0]=hstat.st_atime;
			else acc_upd_times[0] = now;	         /* Accessed now */
			acc_upd_times[1] = hstat.st_mtime; /* Mod'd */
			if (utime(skipcrud+head->header.name,
			    acc_upd_times) < 0) {
				msg_perror("couldn't change access and modification times of %s",skipcrud+head->header.name);
			}
		}
		/* We do the utime before the chmod because some versions of
		   utime are broken and trash the modes of the file.  Since
		   we then change the mode anyway, we don't care. . . */

		/*
		 * If '-k' is not set, open() or creat() could have saved
		 * the permission bits from a previously created file,
		 * ignoring the ones we specified.
		 * Even if -k is set, if the file has abnormal
		 * mode bits, we must chmod since writing or chown() has
		 * probably reset them.
		 *
		 * If -k is set, we know *we* created this file, so the mode
		 * bits were set by our open().   If the file is "normal", we
		 * skip the chmod.  This works because we did umask(0) if -p
		 * is set, so umask will have left the specified mode alone.
		 */
		if ((!f_keep)
		    || (hstat.st_mode & (S_ISUID|S_ISGID|S_ISVTX))) {
			if (chmod(skipcrud+head->header.name,
				  notumask & (int)hstat.st_mode) < 0) {
				msg_perror("cannot change mode of file %s to %ld",skipcrud+head->header.name,notumask & (int)hstat.st_mode);
			}
		}

	quit:
		break;

	case LF_LINK:
	again_link:
	{
		struct stat st1,st2;

		check = link (head->header.linkname,
			      skipcrud+head->header.name);
		if (check == 0)
			break;
		if (make_dirs(skipcrud+head->header.name))
			goto again_link;
		if(f_gnudump && errno==EEXIST)
			break;
		if(   stat(head->header.linkname, &st1) == 0
		   && stat(skipcrud+head->header.name, &st2)==0
		   && st1.st_dev==st2.st_dev
		   && st1.st_ino==st2.st_ino)
			break;
		msg_perror("Could not link %s to %s",
			skipcrud+head->header.name,head->header.linkname);
	}
		break;

#ifdef S_IFLNK
	case LF_SYMLINK:
	again_symlink:
		check = symlink(head->header.linkname,
			        skipcrud+head->header.name);
		/* FIXME, don't worry uid, gid, etc... */
		if (check == 0)
			break;
		if (make_dirs(skipcrud+head->header.name))
			goto again_symlink;
		msg_perror("Could not create symlink to %s",head->header.linkname);
		break;
#endif

#ifdef S_IFCHR
	case LF_CHR:
		hstat.st_mode |= S_IFCHR;
		goto make_node;
#endif

#ifdef S_IFBLK
	case LF_BLK:
		hstat.st_mode |= S_IFBLK;
		goto make_node;
#endif

#ifdef S_IFIFO
	/* If local system doesn't support FIFOs, use default case */
	case LF_FIFO:
		hstat.st_mode |= S_IFIFO;
		hstat.st_rdev = 0;		/* FIXME, do we need this? */
		goto make_node;
#endif

	make_node:
		check = mknod(skipcrud+head->header.name,
			      (int) hstat.st_mode, (int) hstat.st_rdev);
		if (check != 0) {
			if (make_dirs(skipcrud+head->header.name))
				goto make_node;
			msg_perror("Could not make %s",skipcrud+head->header.name);
			break;
		};
		goto set_filestat;

	case LF_DIR:
	case LF_DUMPDIR:
		namelen = strlen(skipcrud+head->header.name)-1;
	really_dir:
		/* Check for trailing /, and zap as many as we find. */
		while (namelen && head->header.name[skipcrud+namelen] == '/')
			head->header.name[skipcrud+namelen--] = '\0';
		if(f_gnudump) {		/* Read the entry and delete files
					   that aren't listed in the archive */
			gnu_restore(skipcrud);
		
		} else if(head->header.linkflag==LF_DUMPDIR)
			skip_file((long)(hstat.st_size));

	
	again_dir:
		check = mkdir(skipcrud+head->header.name,
			      (we_are_root ? 0 : 0300) | (int)hstat.st_mode);
		if (check != 0) {
			struct stat st1;

			if (make_dirs(skipcrud+head->header.name))
				goto again_dir;
			/* If we're trying to create '.', let it be. */
			if (head->header.name[skipcrud+namelen] == '.' && 
			    (namelen==0 ||
			     head->header.name[skipcrud+namelen-1]=='/'))
				goto check_perms;
			if(   errno==EEXIST
 			   && stat(skipcrud+head->header.name,&st1)==0
 			   && (st1.st_mode&S_IFMT)==S_IFDIR)
				break;
			msg_perror("Could not create directory %s",skipcrud+head->header.name);
			break;
		}
		
	check_perms:
		if (!we_are_root && 0300 != (0300 & (int) hstat.st_mode)) {
			hstat.st_mode |= 0300;
			msg("Added write and execute permission to directory %s",
			  skipcrud+head->header.name);
		}

		goto set_filestat;
		/* FIXME, Remember timestamps for after files created? */
		/* FIXME, change mode after files created (if was R/O dir) */
	case LF_VOLHDR:
		if(f_verbose) {
			printf("Reading %s\n",head->header.name);
		}
		break;

	case LF_NAMES:
		extract_mangle(head);
		break;

	case LF_MULTIVOL:
		msg("Can't extract '%s'--file is continued from another volume\n",head->header.name);
		skip_file((long)hstat.st_size);
		break;

	}

	/* We don't need to save it any longer. */
	saverec((union record **) 0);	/* Unsave it */
}

/*
 * After a file/link/symlink/dir creation has failed, see if
 * it's because some required directory was not present, and if
 * so, create all required dirs.
 */
int
make_dirs(pathname)
	char *pathname;
{
	char *p;			/* Points into path */
	int madeone = 0;		/* Did we do anything yet? */
	int save_errno = errno;		/* Remember caller's errno */
	int check;

	if (errno != ENOENT)
		return 0;		/* Not our problem */

	for (p = index(pathname, '/'); p != NULL; p = index(p+1, '/')) {
		/* Avoid mkdir of empty string, if leading or double '/' */
		if (p == pathname || p[-1] == '/')
			continue;
		/* Avoid mkdir where last part of path is '.' */
		if (p[-1] == '.' && (p == pathname+1 || p[-2] == '/'))
			continue;
		*p = 0;				/* Truncate the path there */
		check = mkdir (pathname, 0777);	/* Try to create it as a dir */
		if (check == 0) {
			/* Fix ownership */
			if (we_are_root) {
				if (chown(pathname, hstat.st_uid,
					  hstat.st_gid) < 0) {
					msg_perror("cannot change owner of %s to uid %d gid %d",pathname,hstat.st_uid,hstat.st_gid);
				}
			}
			pr_mkdir(pathname, p-pathname, notumask&0777);
			madeone++;		/* Remember if we made one */
			*p = '/';
			continue;
		}
		*p = '/';
		if (errno == EEXIST)		/* Directory already exists */
			continue;
		/*
		 * Some other error in the mkdir.  We return to the caller.
		 */
		break;
	}

	errno = save_errno;		/* Restore caller's errno */
	return madeone;			/* Tell them to retry if we made one */
}

extract_sparse_file(fd, sizeleft, totalsize, name)
	int	fd;
	long	*sizeleft,
		totalsize;
	char	*name;
{		
	register char	*data;
	union record	*datarec;
	int	sparse_ind = 0;
	int	written,
		count;
	
	/* assuming sizeleft is initially totalsize */


	while (*sizeleft > 0) {
		datarec = findrec();
		if (datarec == NULL) {
			msg("Unexpected EOF on archive file");
			return;
		}
		lseek(fd, sparsearray[sparse_ind].offset, 0);
		written = sparsearray[sparse_ind++].numbytes;
		while (written > RECORDSIZE) {
			count = write(fd, datarec->charptr, RECORDSIZE);
			if (count < 0) 
				msg_perror("couldn't write to file %s", name);
			written -= count;
			*sizeleft -= count;
			userec(datarec);
			datarec = findrec();
		}

		count = write(fd, datarec->charptr, written);
	        
		if (count < 0) {
			msg_perror("couldn't write to file %s", name);
		} else if (count != written) {
			msg("could only write %d of %d bytes to file %s", totalsize - *sizeleft, totalsize, name);
			skip_file((long) (*sizeleft));
		}

		written -= count;
		*sizeleft -= count;		
		userec(datarec);
	}
	free(sparsearray);
/*	if (end_nulls) {
		register int i;

		printf("%d\n", (int) end_nulls);
		for (i = 0; i < end_nulls; i++)
			write(fd, "\000", 1);
	}*/
	userec(datarec);
}
