#ifndef lint
static char *rcsid = "$Header: system_interface.c,v 1.6 87/05/14 11:35:19 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	system_interface.c,v $
 * Revision 1.6  87/05/14  11:35:19  ed
 * Enhanced fileID to be 32 bit inode (previous oversight).
 * Also get_name_from_fileID now uses -a on ls to look at all files.
 * 
 * Revision 1.5  87/04/16  15:26:17  ed
 * Fixed bug if count was Filing4_unlimitedCount. (from jqj)
 * Resolved lingering Subset pathname bugs.
 * 
 * Revision 1.4  87/04/01  10:10:42  ed
 * Added recognition of 'file drawers' (directories in root) 
 * 	in make_attribute_sequence.
 * 
 * Revision 1.3  87/03/31  14:17:54  ed
 * Fixed bug in access_file (per JQ Johnson) passed dir_handle,
 * 	expected pathname, check for -1 failure, not success.
 * 
 * Revision 1.2  87/03/31  09:46:46  ed
 * New procedures: Create, ChangeAttributes(name only), Copy, Move,
 * 		Replace, Serialize, Deserialize.
 * Added conditional disabling of root logins.
 * Support for GetAttributes (allAttributeTypes).
 * Support for filter of type all.
 * 
 * Revision 1.1  87/01/14  11:26:12  ed
 * Initial revision
 * 
 */

#include <stdio.h>
#include <pwd.h>
#include <signal.h>
#include <errno.h>
#include <ctype.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#include "authenticationV2.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#include "authenticationV2.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#include "authenticationV3.h"
#endif FILING5
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#include "authenticationV3.h"
#endif FILINGSUBSET1
#include <xnscourier/filing_server.h>
#include <xnscourier/filetypes.h>

#define XNS_TIME_DIFFERENCE	2177452800	/* [(1970-1901) years * 365 days/year + 17 leap days */
						/* * 24 hours/day * 60 min/hour * 60 sec/min */

#define SERVICE_ROOT	"/"			/* root directory for service */
#ifdef DEBUG
FILE *msgs;
#endif DEBUG

extern int errno;

Cardinal continuance;				/* continuance value, in seconds */
extern continuance_expiration();		/* expiration routine */

/*
 * routine:
 *	verifyandposition_user
 * input:
 *	user_name	- derived from secondary credentials
 *	user_password	- derived form secondary credentials
 * returns:
 *	-1	- success
 *	else	Filing Error, Problem
 */

verifyandposition_user(user_name,user_password)
char *user_name;
char *user_password;
{
	struct passwd	*pwd_entry;
	struct passwd	*getpwnam();
	char		*crypt();

#ifdef DEBUG
	fprintf(msgs, "user= '%s'\n", user_name);
#endif DEBUG


						/* determine if user is valid */
	if ( (pwd_entry= getpwnam(user_name)) == (struct passwd *)0 ) {
		char *lowercase();
#ifdef DEBUG
		fprintf(msgs, "name= '%s'\n",lowercase(user_name));
#endif DEBUG
		if ( (pwd_entry= getpwnam(lowercase(user_name))) == (struct passwd *)0 ) {
#if FILING4 | FILING5
			ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
#else FILING4 | FILING5
			ReturnAuthenticationError(FILING_secondaryCredentialsValueInvalid);
#endif FILING4 | FILING5
			/* NOT REACHED */
		}
	}

#if !(FILING4 | FILING5)
	if ( strcmp(pwd_entry->pw_passwd, crypt(user_password,pwd_entry->pw_passwd)) ) {
		ReturnAuthenticationError(FILING_secondaryCredentialsValueInvalid);
		/* NOT REACHED */
	}
#endif !(FILING4 | FILING5)
						/* set process group ID */
	if ( setgid(pwd_entry->pw_gid) == -1 ) {
#if FILING4 | FILING5
		ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
#else FILING4 | FILING5
		ReturnAuthenticationError(FILING_secondaryCredentialsValueInvalid);
#endif FILING4 | FILING5
		/* NOT REACHED */
	}
						/* set process user ID */
	if ( setuid(pwd_entry->pw_uid) == -1 ) {
#if FILING4 | FILING5
		ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
#else FILING4 | FILING5
		ReturnAuthenticationError(FILING_secondaryCredentialsValueInvalid);
#endif FILING4 | FILING5
		/* NOT REACHED */
	}
						/* position in service root */
	if ( chdir(SERVICE_ROOT) == -1 ) {
		ReturnServiceError(FILING_serviceUnavailable);
		/* NOT REACHED */
	}

	return(-1);
}


/*
 * routine:
 *	set_continuance_timer
 */

set_continuance_timer()
{
	alarm(0);					/* cancel any previous alarm */
	signal(SIGALRM, continuance_expiration);	/* set routine to catch alarm */
	alarm(continuance);				/* set alarm */
}

/*
 * routine:
 *	reset_continuance_timer
 */

reset_continuance_timer()
{
	alarm(0);					/* cancel previous alarm */
	alarm(continuance);				/* then, reset alarm */
}

/*
 * routine:
 *	cancel_continuance_timer
 */

cancel_continuance_timer()
{
	alarm(0);					/* cancel any previous alarm */
	signal(SIGALRM,SIG_IGN);			/* set routine to ignore alarm */
}

/*
 * routine:
 *	open_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 *	else FILING_ error, problem
 */

open_file(file_context_block)
file_handle *file_context_block;
{
#ifdef DEBUG
	fprintf(msgs, "open_file\n");
#endif DEBUG

	if ( (file_context_block->file_desc= 
				fopen(file_context_block->pathname, "r")) == NULL ) {
		switch (errno) {
			case EACCES :				/* user has no access */
				ReturnAccessError(FILING_accessRightsInsufficient);
				/* NOT REACHED */
			case ENOENT :				/* no such file */
			case ENOTDIR :				/* no such directory */
				ReturnHandleError(FILING_fileNotFound);
				/* NOT REACHED */

			default :				/* all other errors */
				ReturnAccessError(FILING_accessRightsIndeterminate);
				/* NOT REACHED */
		}

	}

	return(-1);
}

/*
 * routine:
 *	close_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 */

close_file(file_context_block)
file_handle *file_context_block;
{
#ifdef DEBUG
	fprintf(msgs, "closing...\n");
#endif DEBUG
	if ( file_context_block->file_desc  != NULL ) {
		fclose(file_context_block->file_desc);
		file_context_block->file_desc= 0;
	}

	return(-1);
}

/*
 * routine:
 *	stat_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 *	else Filing Error, Problem
 *
 *	file_context_block entries filled in
 */

stat_file(file_context_block)
file_handle *file_context_block;
{
	struct stat file_stat;
	LongCardinal get_type();

#ifdef DEBUG
	fprintf(msgs, "stating '%s'\n",file_context_block->pathname);
#endif DEBUG

	if ( stat(file_context_block->pathname,&file_stat) == -1 ) {
		switch (errno) {
			case EACCES :			/* user has no access */
				ReturnAccessError(FILING_accessRightsInsufficient);
				/* NOT REACHED */
			case ENOTDIR :			/* directory doesn't exist */
			case ENOENT :			/* file doesn't exist */
				ReturnAccessError(FILING_fileNotFound);

			default :			/* all other errors */
				ReturnAccessError(FILING_accessRightsIndeterminate);
		}
	}

	file_context_block->datasize= file_stat.st_size;	/* dataSize */

								/* file type */
	if ( (file_stat.st_mode & S_IFDIR) != 0 ) {		/* directory */
		file_context_block->isdirectory= TRUE;
		file_context_block->truetype= FILING_tDirectory;
	} else {
		file_context_block->isdirectory= FALSE;		/* non-directory */
		file_context_block->truetype= get_type(file_context_block->pathname);
	}

	return(-1);
}

/*
 * routine:
 *	create_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 *	else FILING_  Error, Problem
 *
 *	file_context_block->file_desc filled in
 */

create_file(file_context_block)
file_handle *file_context_block;

{

	if ( access(file_context_block->pathname, F_OK) == 0 ) {
		ReturnInsertionError(FILING_fileNotUnique);
		/* NOT REACHED */
	}

	if ( (file_context_block->file_desc= 
			fopen(file_context_block->pathname, "w")) ) {
		switch (errno) {
			case EACCES :				/* user has no access */
				ReturnAccessError(FILING_accessRightsInsufficient);
				/* NOT REACHED */

			case EEXIST :				/* file exists */
				ReturnInsertionError(FILING_fileNotUnique);
				/* NOT REACHED */

			case ENOENT :				/* no such file, OK */
				break;

			case ENOTDIR :				/* no such directory */
				ReturnAccessError(FILING_fileNotFound);
				/* NOT REACHED */

			case EMFILE :				/* process file table full */
			case ENFILE :				/* system file table full */
				ReturnSpaceError(FILING_allocationExceeded);
				/* NOT REACHED */

			default :				/* all other errors */
				ReturnAccessError(FILING_accessRightsIndeterminate);
				/* NOT REACHED */
		}
	}

	return(1);
}
/*
 * routine:
 *	create_directory
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 *	else FILING_  Error, Problem
 *
 */

create_directory(file_context_block)
file_handle *file_context_block;

{
	int status;

#ifdef DEBUG
	fprintf(msgs, "createdir '%s'\n",file_context_block->pathname);
#endif DEBUG
	status= 0;
	if ( fork() == 0 ) {			/* execute command */
		execl("/bin/mkdir", "mkdir", file_context_block->pathname, 0);
		ReturnAccessError(FILING_accessRightsInsufficient);
		/* NOT REACHED */
	}

	wait(&status);
	if ( status ) {				/* error reports accessRightsInsufficient */
		ReturnAccessError(FILING_accessRightsInsufficient);
		/* NOT REACHED */
	}

	return(-1);
}

/*
 * routine:
 *	rename_file
 * input:
 *	pointer to old name
 *	pointer to file handle (containing new name)
 * returns:
 *	-1 - success
 *	else Filing Error, Problem
 *
 */

rename_file(oldname, file_context_block)
char *oldname;
file_handle *file_context_block;
{

	if ( access(file_context_block->pathname, F_OK) == 0 ) {
		ReturnInsertionError(FILING_fileNotUnique);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "renaming '%s' to '%s'\n",oldname, file_context_block->pathname);
#endif DEBUG

	if ( rename(oldname, file_context_block->pathname) == -1 ) {
		switch (errno) {
			case EACCES :			/* user has no access */
				ReturnAccessError(FILING_accessRightsInsufficient);
				/* NOT REACHED */
			case ENOTDIR :			/* directory doesn't exist */
			case ENOENT :			/* file doesn't exist */
			case EXDEV :			/* no cross file system move */
				ReturnAccessError(FILING_fileChanged);
				/* NOT REACHED */

			case EINVAL :			/* old is parent of new */
				ReturnInsertionError(FILING_loopInHierarchy);
				/* NOT REACHED */

			default :			/* all other errors */
				ReturnAccessError(FILING_accessRightsIndeterminate);
				/* NOT REACHED */
		}
	}

	return(-1);
}

/*
 * routine:
 *	copy_file
 * input:
 *	pointer to old file handle
 *	pointer to new file handle
 * returns:
 *	-1 - success
 *	else Filing Error, Problem
 *
 */

copy_file(old_file_context_block, new_file_context_block)
file_handle *old_file_context_block;
file_handle *new_file_context_block;
{
	int pid, s;

	if ( strncmp(old_file_context_block->pathname, new_file_context_block->pathname, strlen(old_file_context_block->pathname)) == 0 ) {
		ReturnInsertionError(FILING_loopInHierarchy);
		/* NOT REACHED */
	}

	if ( access(new_file_context_block->pathname, F_OK) == 0 ) {
		ReturnInsertionError(FILING_fileNotUnique);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "copying '%s' to '%s'\n",old_file_context_block->pathname, new_file_context_block->pathname);
#endif DEBUG

	if ( copy(old_file_context_block->pathname, new_file_context_block->pathname) != -1 ) {
		ReturnAccessError(FILING_fileChanged);
		/* NOT REACHED */
	}

	return(-1);
}

copy(from, to)
char *from;
char *to;
{
	int pid, s;

	if ( (pid= fork()) == 0 ) {
		/* child */

		execl("/bin/cp", "cp", "-r", from, to, 0);
		exit(1);
	}

	if ( pid == -1 ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

	while ( wait(&s) != pid ) ;

	/*
	 * would be nice if cp returned useful errors, but ...
	 */

	if ( s != 0 ) {
		ReturnAccessError(FILING_fileChanged);
		/* NOT REACHED */
	}

	return(-1);
}

list_directory(conn, directory, attr, file_spec, count)
CourierConnection *conn;
file_handle *directory;
FILING_AttributeTypeSequence attr;
char *file_spec;
Cardinal count;
{
	FILING_StreamOfAttributeSequence stream_of_attrseq;
	FILING_AttributeSequence attribute_sequence;
	FILE *pipe_desc;
	FILE *popen();
	Boolean first= TRUE;
	char command[256];
	char filename[MAX_FILE_NAME_LENGTH];

	stream_of_attrseq.nextSegment_case.segment.length= 1;
	stream_of_attrseq.nextSegment_case.segment.sequence= &attribute_sequence;

	strcpy(command, "/bin/ls -1d ");			/* form appropriate command */

	strcat(command, directory->pathname);
	if ( strcmp(directory->pathname, "/") != 0 ) {
		strcat(command, "/");
	}

	strcat(command, file_spec);

	if ( get_types(attr, &attribute_sequence) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "listing '%s'\n",command);
#endif DEBUG

	if ( (pipe_desc= popen(command, "r")) == NULL ) {		/* issue command */
		ReturnAccessError(FILING_accessRightsInsufficient);
		/* NOT REACHED */
	}

	while ( fgets(filename, MAX_FILE_NAME_LENGTH, pipe_desc) != NULL ) {
		first= FALSE;
		filename[strlen(filename)-1]= '\0';

#ifdef DEBUG
		fprintf(msgs,"got '%s'     ",filename);
		fprintf(msgs, "count= %d\n",count);
#endif DEBUG

		if ( (count != FILING_unlimitedCount) && (--count < 0) ) {
			break;
		}

		make_attribute_sequence(filename,&attribute_sequence);

		put_next_attribute_sequence(conn, &stream_of_attrseq);
	}

	if ( first == TRUE ) {
		pclose(pipe_desc);
/*		ReturnAccessError(FILING_fileNotFound);		??? */
		/* NOT REACHED */
	}

	put_last_attribute_sequence(conn);

	BDTclosewrite(conn);
	pclose(pipe_desc);

	return(-1);
}

/*
 * routine:
 *	delete_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 * 	else Filing Error, Problem
 */

delete_file(file_context_block)
file_handle *file_context_block;
{
	int status;

#ifdef DEBUG
	fprintf(msgs," deleting '%s'",file_context_block->pathname);
#endif DEBUG
	if ( file_context_block->isdirectory ) {
		if ( fork() == 0 ) {			/* use rm -rf for directories */
			execl("/bin/rm", "rm", "-rf", file_context_block->pathname, 0);
			ReturnAccessError(FILING_accessRightsInsufficient);
			/* NOT REACHED */
		}
		wait(&status);
		if ( status ) {
			ReturnAccessError(FILING_accessRightsInsufficient);
			/* NOT REACHED */
		}
	} else {					/* use unlink for non-directories */
		if ( unlink(file_context_block->pathname) == -1 ) {
			switch (errno) {
				case EACCES :			/* user has no access */
					ReturnAccessError(FILING_accessRightsInsufficient);
					/* NOT REACHED */

				case ENOENT :			/* no such file */
				case ENOTDIR :			/* no such directory */
					ReturnAccessError(FILING_fileNotFound);
					/* NOT REACHED */

				default :			/* all other errors */
					ReturnAccessError(FILING_accessRightsIndeterminate);
					/* NOT REACHED */
			}
		}
	}
}

/*
 * routine:
 *	delete_partial_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 */

delete_partial_file(file_context_block)
file_handle *file_context_block;
{
	unlink(file_context_block->pathname);
	return(-1);
}

/*
 * routine:
 *	access_file
 * input:
 *	pointer to file handle
 * returns:
 *	-1 - success
 */

access_file(file_context_block)
file_handle *file_context_block;
{
#ifdef DEBUG
	fprintf(msgs, "access_file\n");
#endif DEBUG

	if ( access(file_context_block->pathname,R_OK | F_OK) == -1 ) {
		switch (errno) {
			case EACCES :				/* user has no access */
				ReturnAccessError(FILING_fileChanged);
				/* NOT REACHED */
			case ENOENT :				/* no such file */
			case ENOTDIR :				/* no such directory */
				ReturnHandleError(FILING_invalid);
				/* NOT REACHED */

			default :				/* all other errors */
				ReturnAccessError(FILING_accessRightsIndeterminate);
				/* NOT REACHED */
		}

	}

	return(-1);
}

/*
 * routine:
 *	set_create_time
 * input:
 *	pointer to file context block
 *		where
 *		if no createdOn value was specified on Store, createdon = 0
 *		if createdOn value was specified on Store, createdOn != 0,
 *			value is in XNS time format
 * returns:
 *	none
 */

set_create_time(file_context_block)
file_handle *file_context_block;

{
	time_t time_buffer[2];
	time_t time();


	if ( file_context_block->createdon )		/* save createdOn if specified */
		time_buffer[1]= file_context_block->createdon - XNS_TIME_DIFFERENCE;
	else						/* else, set to current date/time */
		time_buffer[1]= time(0);

	time_buffer[0]= time(0);			/* set modifiedOn to current date/time */

	utime(file_context_block->pathname,time_buffer);
}

/*
 * routine:
 *	make_attribute_sequence
 * inputs:
 *	pointer to file name
 *	pointer to sequence of attributes to fill in
 * returns:
 *	-1 - success
 */

make_attribute_sequence(pathname, attrseq)
char *pathname;
FILING_AttributeSequence *attrseq;
{
	int i;
	struct stat file_stat;
	FILING_AttributeType t;

	LongCardinal createdon, modifiedon;
	LongCardinal type, get_type();
	LongCardinal datasize;
	Boolean isdirectory;
	Boolean all_attributes= FALSE;
	Cardinal unix_version= 1;
	Boolean istemporary= FALSE;
#ifdef FILETOOLCOMPATIBILITY
	Cardinal fileid[6];
	AUTHENTICATION_Clearinghouse_Name user;
	AUTHENTICATION_Clearinghouse_Name CH_StringToName();
	char *name, *pwname;
	char *rindex();
	struct passwd *getpwuid(), *pwd;
#endif FILETOOLCOMPATIBILITY
#ifdef EXTENSIONS
	Boolean inroot= FALSE;
	FILE *fd;
#endif EXTENSIONS

#ifdef DEBUG
	fprintf(msgs, "make_attrseq '%s'\n", pathname);
#endif DEBUG

#ifndef FILINGSUBSET1
	if ( (name= rindex(pathname, '/')) == 0 )
		name= pathname;
	else {
#ifdef EXTENSIONS
		if ( name == pathname ) inroot= TRUE;
#endif EXTENSIONS
		name++;
	}
#endif FILINGSUBSET1

	if ( stat(pathname, &file_stat) == -1 ) {
		ReturnAccessError(FILING_accessRightsInsufficient);
		/* NOT REACHED */
	}	

	createdon= file_stat.st_mtime + XNS_TIME_DIFFERENCE;	/* createdOn */
	modifiedon= file_stat.st_atime + XNS_TIME_DIFFERENCE;	/* modifiedOn */

	datasize= file_stat.st_size;				/* dataSize */

								/* type and isDirectory */
	if ( (file_stat.st_mode & S_IFDIR) != 0 ) {
		isdirectory= TRUE;
#ifdef EXTENSIONS
		if ( inroot )				/* if root & directory, assume file drawer */
			type= TYPE_VPDrawer;
		else
			type= FILING_tDirectory;
#else EXTENSIONS
		type= FILING_tDirectory;
#endif EXTENSIONS
	} else {
		type= get_type(pathname);
#ifdef EXTENSIONS
		if ( (type > LAST_FILING_TYPE) && (type != TYPE_Interpress) &&
					(type != TYPE_VPCanvas) ) {
			if ( (fd= fopen(pathname, "r")) == NULL ) {
				ReturnAccessError(FILING_fileChanged);
				/* NOT REACHED */
			}
			isdirectory= GetDirectoryAttribute(fd);
			fclose(fd);
		} else {
			isdirectory= FALSE;
		}
#else EXTENSIONS
		isdirectory= FALSE;
#endif EXTENSIONS
	}

#ifdef EXTENSIONS
	if ( attrseq->length == -1 ) {
		all_attributes= TRUE;
		if ( (type > LAST_FILING_TYPE) && (type != TYPE_Interpress) &&
					(type != TYPE_VPCanvas) )
			make_required_attributes(attrseq);
		else
			make_supported_attributes(attrseq);
	}
#endif EXTENSIONS

	for ( i= 0 ; i < attrseq->length ; i++ ) {
		t= attrseq->sequence[i].type;
#ifdef DEBUG
		fprintf(msgs,"#%d  type= %d \n", i, t);
#endif DEBUG

		if ( t == FILING_pathname ) {
			StringToAttr(pathname, &attrseq->sequence[i]);
		} else if ( t == FILING_type ) {
			LongCardinalToAttr(type, &attrseq->sequence[i]);
		} else if ( t == FILING_dataSize ) {
			LongCardinalToAttr(datasize, &attrseq->sequence[i]);
		} else if ( t == FILING_isDirectory ) {
			BooleanToAttr(isdirectory, &attrseq->sequence[i]);
		} else if ( t == FILING_createdOn ) {
			LongCardinalToAttr(createdon, &attrseq->sequence[i]);
		} else if ( t == FILING_modifiedOn ) {
			LongCardinalToAttr(modifiedon, &attrseq->sequence[i]);
		} else if ( t == FILING_version ) {
			CardinalToAttr(unix_version, &attrseq->sequence[i]);
		} else if ( t == FILING_isTemporary ) {
			BooleanToAttr(istemporary, &attrseq->sequence[i]);
#ifdef FILETOOLCOMPATIBILITY
		} else if ( t == FILING_name ) {
			StringToAttr(name, &attrseq->sequence[i]);
		} else if ( t == FILING_fileID ) {
			fileid[0]= (file_stat.st_ino >> 16) & 0xffff;
			fileid[1]= file_stat.st_ino & 0xffff;
			fileid[2]= fileid[3]= fileid[4]= 0;
			FileIDToAttr(fileid, &attrseq->sequence[i]);
		} else if ( t == FILING_readOn ) {
			LongCardinalToAttr(modifiedon, &attrseq->sequence[i]);
		} else if ( t == FILING_createdBy ) {
			if ( (pwd= getpwuid(file_stat.st_uid)) == 0 )
				pwname= "Unkown";
			else
				pwname= pwd->pw_name;
			user= CH_StringToName(pwname,NULL);
			UserToAttr(user,&attrseq->sequence[i]);
#endif FILETOOLCOMPATIBILITY
		} else {
			attrseq->sequence[i].value.length= 0;
			attrseq->sequence[i].value.sequence= (Unspecified *)0;
		}
	}

#ifdef EXTENSIONS
	if ( all_attributes ) {
		if ( (type > LAST_FILING_TYPE) && (type != TYPE_Interpress) &&
					(type != TYPE_VPCanvas) ) {
			if ( (fd= fopen(pathname, "r")) == NULL ) {
				ReturnAccessError(FILING_fileChanged);
				/* NOT REACHED */
			}
			if ( AddAllExtendedAttributes(fd, attrseq) != -1 ) {
				fclose(fd);
				ReturnAccessError(FILING_fileChanged);
				/* NOT REACHED */
			}
			fclose(fd);
		}
	}
#endif EXTENSIONS
}

int
getBDTch(conn,bpp)
	CourierConnection *conn;
	u_char **bpp;
{
	static u_char buffer[SPPMAXDATA];
	static int count;

	if (*bpp == NULL) {*bpp = buffer; count = 0;}
	if (*bpp >= buffer+count) {
		count=BDTread(conn,buffer,sizeof(buffer));
		*bpp = buffer;
	}
	if (count <= 0) return(EOF);
	else return(*((*bpp)++));
		
}

storeproc(conn,handle)
CourierConnection *conn;
file_handle *handle;
{
	int count, ocount, ch, hashbytes;
	char buffer[SPPMAXDATA];
	int charset, charset16;
	char *bp;
	register FILE *fout;
	register int fd;

	fout= handle->file_desc;
	fd= fileno(fout);

#ifdef FILETOOLCOMPATIBILITY
	if ( handle->type == FILING_tText ) {
		charset = 0; charset16 = 0; bp = NULL;
		while ((ch = getBDTch(conn,&bp)) != EOF) {
			if (ch == '\377') {
				ch = getBDTch(conn,&bp);
				if (ch == '\377') charset16 = 1;
				else charset = ch;
				continue;
			}
			if (charset16) {
				charset = ch;
				ch = getBDTch(conn,&bp);
			}
			switch (charset) {
			case 0:	/* normal character set -- minimal xlation */
				if (ch == '\r') {
				    	int nextch;

				    	putc('\n',fout);
				    	if ( (nextch = getBDTch(conn,&bp)) != '\n'){
					    if (nextch == '\r')
						putc('\n',fout);
					    else if (nextch == ','+0200) 
					        putc('_',fout);
					    else if (nextch != EOF)
				    		putc(nextch,fout);
					    else
						continue;
					}

					break;
				}
				else if (ch == ','+0200) ch = '_';
				/* more mapping here */
				putc(ch,fout);
				break;
			default:
				break; /* ignore */
			}
		}
		/* if (count < 0) perror("netin"); */
	} else {
#else FILETOOLCOMPATIBILITY
	{
#endif FILETOOLCOMPATIBILITY
		errno = ocount = 0;
		fflush(fout);
		while ((count = BDTread(conn, buffer, sizeof(buffer))) > 0) {
			if ((ocount = write(fd,buffer,count)) < 0) {
				perror("write");
				BDTabort(conn);
				return(0);
			}
		}
		if (count < 0) {
			perror("netin");
			return(0);
		}
	}

	return(-1);
}

retrieveproc(conn, handle)
CourierConnection *conn;
file_handle *handle;
{
	int count, ocount;
	u_char buffer[SPPMAXDATA];
	u_char *bp;

	errno = ocount = 0;
	clearerr(handle->file_desc);

	if ( handle->type == -1 )
		handle->type= handle->truetype;

#ifdef DEBUG
	fprintf(msgs, "transferring data type= %d\n", handle->type);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	if (handle->type == FILING_tText) {
		while ((count = fread(buffer, sizeof(char), SPPMAXDATA, handle->file_desc)) > 0) {
			ocount = count;
			for (bp = buffer; count > 0; count--, bp++) {
				if (*bp == '\n') *bp = '\r';
				else if (*bp == '_') *bp = ','+0200;
				/* more translations here */
			}
			if ((ocount = BDTwrite(conn, buffer, ocount)) <= 0)
				break;
		}
	} else {
#else FILETOOLCOMPATIBILITY
	{
#endif FILETOOLCOMPATIBILITY
		while ((count = fread(buffer, sizeof(char), SPPMAXDATA, handle->file_desc)) > 0
		       && (ocount = BDTwrite(conn, buffer, count)) > 0) {
		}
	}

	if (ocount < 0) {
		BDTabort(conn);
		perror("netout");
		return(0);
	}
	else if (ferror(handle->file_desc)) {
		BDTabort(conn);
		perror("read");
		return(0);
	}
	else
		BDTclosewrite(conn);

	return(-1);
}

#ifdef FILETOOLCOMPATIBILITY
get_name_from_fileID(handle, fileid)
file_handle *handle;
Unspecified *fileid;
{
	char cmd[256];
	char pathname[256];
	char buffer[256];
	int inode;
	FILE *fd, *popen();
	Boolean first= TRUE;

	if ( *(handle->pathname) == '\0' ) {
		ReturnAttributeTypeError(FILING_unreasonable, FILING_fileID);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "looking for fileid %x %x\n", fileid[0], fileid[1]);
#endif DEBUG

	strcpy(cmd, "/bin/ls -1ai ");
	strcat(cmd, handle->pathname);

	if ( (fd= popen(cmd, "r")) == NULL ) {
		ReturnAccessError(FILING_accessRightsInsufficient);
		/* NOT REACHED */
	}

	while ( fgets(buffer, sizeof(buffer), fd) != NULL ) {
		buffer[strlen(buffer)-1]= '\0';

		sscanf(buffer, "%d %s", &inode, pathname);
#ifdef DEBUG
		fprintf(msgs, "inode= %d '%s'\n",inode, pathname);
#endif DEBUG

		if ( (fileid[0] == ((inode>>16)&0xffff)) && 
				(fileid[1] == (inode&0xffff)) ) {
			if ( strcmp(handle->pathname, "/") != 0 )
				strcat(handle->pathname, "/");
			strcat(handle->pathname, pathname);
			pclose(fd);
			return(-1);
		}
	}

	pclose(fd);
	ReturnAccessError(FILING_fileNotFound);
	/* NOT REACHED */
}
#endif FILETOOLCOMPATIBILITY

check_pathname(pathname)
char *pathname;
{
	char *ptr;

	for ( ptr= pathname; *ptr != '\0' ;  ptr++ ) {
		if ( isspace(*ptr) || iscntrl(*ptr) ) {
			ReturnAttributeValueError(FILING_illegal, FILING_pathname);
			/* NOT REACHED */
		}
	}

	return(-1);
}

#ifdef EXTENSIONS
/*
 * make_backup
 *
 */

make_backup(handle)
file_handle *handle;

{
	char buffer[2048];
	char backup_name[MAX_FILE_NAME_LENGTH];
	int fin, fout, count;

	strcpy(backup_name, handle->pathname);
	strcat(backup_name, ".REP");

	if ( (fin= open(handle->pathname, O_RDONLY, 0)) < 0 ) {
		return(0);
		/* NOT REACHED */
	}

	if ( (fout= open(backup_name, O_WRONLY|O_CREAT, 0600)) < 0 ) {
		return(0);
		/* NOT REACHED */
	}

	while ( (count= read(fin, buffer, sizeof(buffer))) > 0 ) {
		if ( write(fout, buffer, count) < 0 ) {
			close(fin);
			close(fout);
			unlink(backup_name);
			return(0);
			/* NOT REACHED */
		}
	}

	close(fin);
	close(fout);
	return(-1);
}

/*
 * recall_backup
 *
 */

recall_backup(handle)
file_handle *handle;

{
	char backup_name[MAX_FILE_NAME_LENGTH];

	strcpy(backup_name, handle->pathname);
	strcat(backup_name, ".REP");

	/*
	 * we better not see an error here, since we have already
	 * munged the original file
	 */

	if ( rename(backup_name, handle->pathname) == -1 ) {
		return(0);
	}

	return(-1);
}

unlink_backup(handle)
file_handle *handle;

{
	char backup_name[MAX_FILE_NAME_LENGTH];

	strcpy(backup_name, handle->pathname);
	strcat(backup_name, ".REP");

	if ( unlink(backup_name) == -1 ) {
		return(0);
	}

	return(-1);
}
#endif EXTENSIONS
