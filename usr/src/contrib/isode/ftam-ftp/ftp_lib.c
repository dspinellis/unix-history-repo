/* ftp_lib.c - FTP subroutines */

/* 
 * $Header: /f/osi/ftam-ftp/RCS/ftp_lib.c,v 7.2 91/02/22 09:23:29 mrose Interim $
 *
 *
 * $Log:	ftp_lib.c,v $
 * Revision 7.2  91/02/22  09:23:29  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/12/23  18:39:51  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:55:06  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *	The MITRE Corporation (hereafter MITRE) makes this software available 
 *	on an "as is" basis.  No guarantees, either explicit or implied, are 
 *	given as to performance or suitability.  
 *
 */

/*
 * Library interface routines.  Design of routines is specific for
 * FTAM.
 */

/*
 * FTP User Program -- Command Routines.
 */
#include "config.h"
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/socket.h>

#include <arpa/ftp.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <netdb.h>

#include "ftp_var.h"
#include "general.h"
#include "logger.h"

void	advise ();
#ifndef	NULLCP
#define	NULLCP	((char *) 0)
#endif

#define FTP_PORT 21

/* Virtual filesystem file types */

#define	VFS_UBF	0		/* offset to FTAM-3 */
#define	VFS_UTF	1		/*  ..       FTAM-1 */
#define	VFS_FDF	2		/*  ..       NBS-9 */

#ifndef NOTOK
#define NOTOK (-1)
#define OK	0
#define DONE	1
#endif /* NOTOK */


/*
 * ftp_login: establish command connection with remote host
 * then execute login process.
 */
int
ftp_login(host,user,passwd,acct)
char *host, *user, *passwd, *acct;
{

	if (connected) return NOTOK; /* already connected */

	ftp_init(); /* initialize control state structures */
	if (hookup(host,FTP_PORT) == NOTOK) return NOTOK;
		

	/* execute login process */
	if (login(user,passwd,acct) == NOTOK) return NOTOK;

	return OK;
}

/*
 * ftp_quit: send quit command and shutdown communications link.
 */
int
ftp_quit()
{

	extern FILE *cout;
	extern int data;
	int n;

	if (!connected) return OK;
	n = command("QUIT");
	(void) fclose(cout);
	connected = 0;
	data = -1;

	if (n == 0 || n == COMPLETE) return OK;
	return NOTOK;
}

/*
 * ftp_abort: send abort command
 */
int
ftp_abort()
{
int n;

	if (!connected) return NOTOK;
	n = command("ABOR");

	if (n == COMPLETE) return OK;
	return NOTOK;

}

/*
 * ftp_exist: perform NLST command and count number of records in data
 * stream.  If 0 or reply code is failure, file does not exist.  If 1
 * or more and reply code is COMPLETE, file exists.  ftp_directory is
 * a global flag.  It is set if more than 1 record in data stream and
 * reply code is COMPLETE.  Yes this is hokey but it works for all the
 * test systems and is faster than trying a case sensitive, then case
 * insensitive scan then falling back on record counts in the case of
 * directories.
 */
int 
ftp_exist(filename)
char *filename;
{
int n, count;
int fd;
FILE *fp, *fdopen();
char lineX[BUFSIZ];

	ftp_directory = 0;

	if (!connected) return NOTOK;

	/* set ascii transfer */
	if (ftp_type(VFS_FDF) != OK) return NOTOK;

	/* begin list transfer */
	if ((fd = recvrequest("NLST",filename)) == NOTOK) return NOTOK;
	if ((fp = fdopen(fd,"r")) == NULL){
		(void)close(fd);
		(void) getreply(0);
		(void)sprintf(ftp_error,"Out of memory");
		return NOTOK;
	}

	/* count number of records (lines) in data transfer */
	for(count=0; fgets(lineX,BUFSIZ,fp)!=NULL; count++);
	(void)fclose(fp);

	/* transfer complete reply */
	n = getreply(0);

	if (n != COMPLETE) /* directory command not accepted */
		return NOTOK;

	/* if more than one record in reply, guess that it is a directory */
	if (count > 1) {
		ftp_directory = 1;
		if (verbose)
		    advise (LLOG_DEBUG, NULLCP, "directory found");
	}

	/* if any records in reply, assume that file existed */
	if (count) return OK;

	return NOTOK;

}

/* Basicly set transfer type to ascii and issue NLST command
 * and returning the socket descriptor for the data stream.
 */
int
ftp_ls(dir)
char *dir;
{
int fd;

	if (!connected) return NOTOK;

	/* set ascii transfer */
	if (ftp_type(VFS_FDF) != OK) return NOTOK;

	/* begin list transfer */
	if ((fd = recvrequest("NLST",dir)) == NOTOK) return NOTOK;

	return(fd);
}
int
ftp_delete(file)
char *file;
{

	
	if (!connected) return NOTOK;

	/* send delete command, return OK if complete, NOTOK otherwise */
	if (command("DELE %s", file) == COMPLETE) return OK;
	/* Hummm, try directory delete */
	if (command("XRMD %s", file) == COMPLETE) return OK;
	/* No dice, return error */
	return NOTOK;

}

int
ftp_mkdir(dir)
char *dir;
{

	if (!connected) return NOTOK;

	/* send MKDIR command, return OK if complete, NOTOK otherwise */
	if (command("XMKD %s", dir) == COMPLETE) return OK;
	return NOTOK;

}

int
ftp_rename(from,to)
char *from, *to;
{
int n;

	if (!connected) return NOTOK;

	/* send RNFR command followed by RNTO if successful */
	if ((n = command("RNFR %s",from)) == CONTINUE)
		n = command("RNTO %s",to);
	if (n == COMPLETE) return OK;
	return NOTOK;

}

int
ftp_write(file)
char *file;
{

	if (!connected) return NOTOK;

	return(sendrequest("STOR",file));
}
int 
ftp_append(file)
char *file;
{
	if (!connected) return NOTOK;

	return(sendrequest("APPE",file));
}

int
ftp_read(file)
char *file;
{

	if (!connected) return NOTOK;

	return(recvrequest("RETR", file));
}

int
ftp_type(modeX)
int modeX;
{
int n;
char cmd[10];

	/* The current transfer type is stored in ``type''.
	 * The TYPE command is issued if the type changes.
         * (this cuts down on the number of FTP transactions).
         */
	if (!connected) return NOTOK;
	n = COMPLETE;

	switch(modeX) {
		/* unstructured binary file */
		case VFS_UBF:
			if (type == TYPE_L) break;
			(void)sprintf(cmd, "TYPE L %s", bytename);
			type = TYPE_L;
			n = command(cmd,0);
			break;
		/* unstructured text file */
		case VFS_UTF:
		/* directory file */
		case VFS_FDF:
		default:
			if (type == TYPE_A) break;
			(void)sprintf(cmd, "TYPE A");
			type = TYPE_A;
			n = command(cmd,0);
		}

	if (n == COMPLETE) return OK;
	return NOTOK;

}

int
ftp_reply()
{
int n;

	/* process an FTP response */

	n = getreply(0);
	if (n == COMPLETE) return OK;
	return NOTOK;
}

int
ftp_create(filename)
char *filename;
{
int fd,n;

	if (!connected) return NOTOK;

	/* open file */
	fd = sendrequest("STOR",filename);
	if (fd == NOTOK) return NOTOK;

	/* close file (create empty file) */
	(void)close(fd);
	n = getreply(0);
	if (n == COMPLETE) return OK;
	return NOTOK;

}

