#ifndef lint
static char *rcsid = "$Header: filingd.c,v 1.6 87/05/14 11:33:26 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	filingd.c,v $
 * Revision 1.6  87/05/14  11:33:26  ed
 * Open: don't set cur_dir_handle unless directory is opened.
 * 
 * Revision 1.5  87/05/05  14:46:31  ed
 * Don't close connection in continuance_expiration if BDT in progress.
 * 
 * Revision 1.4  87/04/16  15:30:29  ed
 * Fixed lingering Subset bugs.
 * 
 * Revision 1.3  87/03/31  14:22:53  ed
 * Initialize got_matches in get_filter.
 * 
 * Revision 1.2  87/03/31  09:05:15  ed
 * New procedures: Create, ChangeAttributes(name only), Copy, Move,
 * 		Replace, Serialize, Deserialize.
 * Added conditional disabling of root logins.
 * Support for GetAttributes (allAttributeTypes).
 * Support for filter of type all.
 * 
 * Revision 1.1  87/01/14  11:25:59  ed
 * Initial revision
 * 
 */

#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#include "clearinghouseV2.h"
#include "authenticationV2.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#include "clearinghouseV2.h"
#include "authenticationV2.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#include "clearinghouseV3.h"
#include "authenticationV3.h"
#endif FILING6
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#include "clearinghouseV3.h"
#include "authenticationV3.h"
#endif FILINGSUBSET1
#include <xnscourier/filing_server.h>
#include <xnscourier/CH.h>
#include <xnscourier/filetypes.h>

#define SERVICE_ROOT	"/"			/* root directory for service */

#ifdef DEBUG
FILE *msgs= 0;
#endif DEBUG

session_handle SessionHandle= 0;
file_handle RootHandle= { FILE_OPEN, SERVICE_ROOT, 0, 0, 0, TRUE, 0, 0, NULL };
extern CourierConnection *_serverConnection;	/* current connection */

	/*
	 * The continuance value is slightly lower than the 90 second
	 * value in lookahead.c. This should insure that the client will
	 * send a Continue before this service stops looking for the next
	 * procedure call.
	 */
Cardinal continuance= 80;			/* continuance value in seconds */

Boolean BDTabort_expected= FALSE;		/* should BDT attn be sent */

#ifdef FILETOOLCOMPATIBILITY
file_handle *cur_dir_handle= &RootHandle;
#endif FILETOOLCOMPATIBILITY


FILING_LogonResults
FILING_Logon(ServerConnection, BDTProc, service_name, user_credentials, user_verifier)
CourierConnection *ServerConnection;
int (*BDTProc)();
CLEARINGHOUSE_ObjectName service_name;
FILING_Credentials user_credentials;
FILING_Verifier user_verifier;
{
	FILING_LogonResults result;
	AUTHENTICATION_ThreePartName chs_name;
	session_handle *session_ptr;
	Unspecified *bp, buffer[SPPMAXDATA];
	char user[40];
	char pass[40];
	Cardinal credentials_type;
	Cardinal len;
	char *lowercase();
#if FILING4 | FILING5
	char *rindex();
	char *ptr;
#endif FILING4 | FILING5

	BDTabort_expected= FALSE;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs,"Logon\n");
#endif DEBUG

#if FILING4 | FILING5
	if (user_credentials.type != AUTHENTICATION_simpleCredentials) {
		ReturnAuthenticationError(AUTHENTICATION_inappropriateCredentials);
		/* NOT REACHED */
	} else {
		CLEARINGHOUSE_externalize_Item(&user_credentials.value, buffer);
		bp= buffer;
		bp += internalize_Cardinal(&len, bp);
		AUTHENTICATION_internalize_SimpleCredentials(&chs_name, bp);
#ifdef DEBUG
		fprintf(msgs,"chs_name= %s:%s:%s\n",chs_name.object,chs_name.domain,chs_name.organization);
#endif DEBUG
	}

#ifdef ROOTNOTALLOWED
	/*
	 * We don't allow root access from the file server
	 */

	if ( strcmp(lowercase(chs_name.object), "root") == 0 ) {
		ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
		/* NOT REACHED */
	}
#endif ROOTNOTALLOWED

	/*
	 * Assumption (for Filing4/Filing5 implementation):
	 * may receive fully specified Clearinghouse name, so we should try
	 * to strip off last name (look for last space) assuming there is a
	 * similary named account on this service. If the user credentials
	 * pass network authentiction, then we will not check passwords on
	 * this service (in fact, we can't since the verifier is hashed).
	 * It is assumed that the name will be found in /etc/passwd and 
	 * everything will work regardless of password checking.
	 */

	if ( (ptr= rindex(chs_name.object, ' ')) == 0 ) {
		strcpy(user, chs_name.object);
	} else {
		ptr++;
		strcpy(user, ptr);
	}

	if ( !Auth_CredCheck(user_credentials, user_verifier) ) {
		ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
		/* NOT REACHED */
	}
#else FILING4 | FILING5
	/*
	 * assumption (for FILING6 and FILINGSUBSET1):
	 * no primary credentials are ok
	 * simple primary credentials will be validated with Authentication
	 * strong credentials will be ignored
	 *
	 * secondary credentials must contain userName and userPassword
	 * which are assumed to be Unix name and password
	 */

	if (user_credentials.primary.type == AUTHENTICATION_simpleCredentials) {
		if ( !Auth_CredCheck(user_credentials.primary, user_verifier) ) {
			ReturnAuthenticationError(FILING_primaryCredentialsInvalid);
			/* NOT REACHED */
		}
	}

	if ( get_name_and_pwd(&user_credentials.secondary, user, pass) != -1 ) {
		ReturnAuthenticationError(FILING_secondaryCredentialsRequired);
		/* NOT REACHED */
	}

#ifdef ROOTNOTALLOWED
	/*
	 * We don't allow root access from the file server
	 */

	if ( strcmp(lowercase(user), "root") == 0 ) {
		ReturnAuthenticationError(AUTHENTICATION_credentialsInvalid);
		/* NOT REACHED */
	}
#endif ROOTNOTALLOWED

#endif FILING4 | FILING5

#ifdef DEBUG
	fprintf(msgs, "user= %s\n", user);
#endif DEBUG

	if ( verifyandposition_user(user, pass) != -1 ) {
		/* NOT REACHED */
	}

	session_ptr= &SessionHandle;
	SessionHandle.state= SESSION_OPEN;
	SessionHandle.verifier= user_verifier.sequence[0];

#ifdef DEBUG
/*	fprintf(msgs,"handle= %x, ver= %x\n",session_ptr,session_ptr->verifier);
*/
#endif DEBUG

	copyhandle(result.session.token, &session_ptr);
	result.session.verifier.length= sizeof(AUTHENTICATION_SimpleVerifier) / sizeof(Cardinal);
	result.session.verifier.sequence= Allocate(sizeof(AUTHENTICATION_SimpleVerifier));
	result.session.verifier.sequence[0]= user_verifier.sequence[0];

	set_continuance_timer();
#ifdef DEBUG
	fprintf(msgs, "out of logon\n");
#endif DEBUG
	return(result);

}

copyhandle(dest, src)
Unspecified *dest, *src;
{
    	if ( dest == (Unspecified *)0 ) {
#ifdef DEBUG
		fprintf(msgs, "Oops, dest is null in copyhandle\n");
#else DEBUG
		fprintf(stderr, "Oops, dest is null in copyhandle\n");
#endif DEBUG
		exit(1);
	}

	dest[0]= src[0];
	dest[1]= src[1];
}


void FILING_Logoff(ServerConnection, BDTProc, session)
CourierConnection *ServerConnection;
int (*BDTProc)();
FILING_Session session;
{
	session_handle *session_ptr;

	BDTabort_expected= FALSE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	copyhandle(&session_ptr,session.token);

#ifdef SOMEDAY
	if ( session_ptr->state == SESSION_IN_USE ) {
		ReturnServiceError(FILING_sessionInUse);
	} 
#endif SOMEDAY

	session_ptr->state= SESSION_CLOSED;
	reset_continuance_timer();

	return;
}


FILING_OpenResults FILING_Open(ServerConnection, BDTProc, attributes,  directory, controls, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_AttributeSequence attributes;
FILING_Handle directory;
FILING_ControlSequence controls;
FILING_Session session;
{
	FILING_OpenResults results;
	file_handle *handle;

#ifdef FILETOOLCOMPATIBILITY
	file_handle *dir_handle;
#endif FILETOOLCOMPATIBILITY

	BDTabort_expected= FALSE;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs,"Open\n");
	fflush(msgs);
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

#ifndef FILETOOLCOMPATIBILITY
	if ( is_nullControls(controls) != -1 ) {
		ReturnControlTypeError(FILING_disallowed,0);
		/* NOT REACHED */
	}

	if ( is_nullHandle(directory) != -1 ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY
 
	if ( (handle= (file_handle *)malloc(sizeof(file_handle))) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */	
	}

#ifdef DEBUG
	fprintf(msgs,"Open-- file handle= %x\n",handle);
	fflush(msgs);
#endif DEBUG

	if ( (handle->pathname= (char *)malloc(MAX_FILE_NAME_LENGTH)) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */	
	}

#ifdef FILETOOLCOMPATIBILITY
	copyhandle(&dir_handle,directory);

	if ( dir_handle != 0 ) {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		strcpy(handle->pathname,dir_handle->pathname);
		if ( strcmp(handle->pathname, "/") != 0 )
			strcat(handle->pathname,"/");
	} else {
		strcpy(handle->pathname, SERVICE_ROOT);
	}
#else FILETOOLCOMPATIBILITY
	strcpy(handle->pathname, SERVICE_ROOT);
#endif FILETOOLCOMPATIBILITY

	if ( verify_open_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( stat_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	handle->state= FILE_OPEN;
	handle->file_desc= NULL;
	handle->createdon= handle->modifiedon= 0;

	copyhandle(results.file,&handle);

#ifdef FILETOOLCOMPATIBILITY
	if ( handle->isdirectory == TRUE )
		cur_dir_handle= handle;
#endif FILETOOLCOMPATIBILITY

	reset_continuance_timer();

	return(results);
}

verify_session(session)
FILING_Session session;
{
	session_handle *session_ptr;

	copyhandle(&session_ptr, session.token);

	if ( session_ptr == 0  ||
			(session_ptr->state != SESSION_OPEN) ) {
		ReturnSessionError(FILING_tokenInvalid);
		/* NOT REACHED */
	}

	if ( session_ptr->verifier != *(session.verifier.sequence) ) {
		ReturnAuthenticationError(AUTHENTICATION_verifierInvalid);
		/* NOT REACHED */
	}

	return(-1);

}

is_nullControls(controls)
FILING_ControlSequence controls;
{

	if ( controls.length != 0 && controls.sequence != 0 ) {
		return(0);
	}

	return(-1);
}

is_nullHandle(handle)
FILING_Handle handle;
{
	if ( handle[0] != 0 && handle[1] != 0 ) {
		return(0);
	}

	return(-1);
}

verify_open_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	int got_parentID, got_pathname, got_type, got_version;
	char *pathname;
	Unspecified *parentid;
	FILING_Version version;
	char *AttrToString();
	Unspecified *AttrToFileID();
	LongCardinal AttrToLongCardinal();

#ifdef FILETOOLCOMPATIBILITY
	int got_name, got_fileID;
	char *name;
	Unspecified *fileid;
#endif FILETOOLCOMPATIBILITY

#ifdef DEBUG
	fprintf(msgs,"verify_open_attribute %d attributes	",attr.length);
#endif DEBUG

	got_parentID= got_pathname= got_type= got_version= 0;

#ifdef FILETOOLCOMPATIBILITY
	got_name= got_fileID= 0;
#endif FILETOOLCOMPATIBILITY

#ifdef FILETOOLCOMPATIBILITY
	if ( attr.length == 0 ) {
		return;
	}
#endif FILETOOLCOMPATIBILITY

	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;

		if ( t == FILING_parentID ) {
#ifdef DEBUG
			fprintf(msgs,"parentID  ");
#endif DEBUG
			if ( got_parentID ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_parentID++;

			parentid= AttrToFileID(&attr.sequence[i]);
			if ( !is_nullID(parentid) ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}
			continue;
		} 
		if ( t == FILING_pathname ) {
#ifdef DEBUG
			fprintf(msgs,"pathname  ");
#endif DEBUG
			if ( got_pathname ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_pathname++;

			pathname= AttrToString(&attr.sequence[i]);
			if ( check_pathname(pathname) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}			
		if ( t == FILING_type ) {
#ifdef DEBUG
			fprintf(msgs,"type  ");
#endif DEBUG
			if ( got_type ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_type++;

			handle->type= AttrToLongCardinal(&attr.sequence[i]);
#ifndef EXTENSIONS
			if ( (handle->type != FILING_tText) &&
					(handle->type != FILING_tUnspecified) ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}
#endif EXTENSIONS
			continue;
		}
		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion && version != FILING_lowestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}

#ifdef FILETOOLCOMPATIBILITY
		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_fileID ) {
#ifdef DEBUG
			fprintf(msgs, "fileID  ");
#endif DEBUG
			if ( got_fileID ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_fileID++;

			fileid= AttrToFileID(&attr.sequence[i]);
			if ( is_nullID(fileid) == -1 ) {
				ReturnAttributeValueError(FILING_disallowed, t);
				/* NOT REACHED */
			}
			strcpy(handle->pathname, cur_dir_handle->pathname);
			if ( get_name_from_fileID(handle, fileid) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}
#else FILETOOLCOMPATIBILITY
		if ( t == FILING_fileID ) {
			ReturnAttributeTypeError(FILING_unimplemented, t);
			/* NOT REACHED */
		}

		if ( t == FILING_name ) {
			ReturnAttributeTypeError(FILING_unimplemented, t);
			/* NOT REACHED */
		}
#endif FILETOOLCOMPATIBILITY

		if ( ( t < 0 ) || (t > FILING_subtreeSizeLimit) ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		ReturnAttributeTypeError(FILING_disallowed, t);
	}

#ifdef FILETOOLCOMPATIBILITY
	if ( !got_pathname && !got_name) {
		if ( !got_fileID ) {
			handle->pathname= SERVICE_ROOT;
		}
	} else 	if ( !got_pathname )  {
		strcat(handle->pathname,name);
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname,pathname);
		Deallocate(&pathname);
	}

#else FILETOOLCOMPATIBILITY
	if ( !got_pathname ) {
		handle->pathname= SERVICE_ROOT;
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname, pathname);
		Deallocate(&pathname);
	}
#endif FILETOOLCOMPATIBILITY

	if ( !got_type )
		handle->type= -1;

	return(-1);
}

is_nullID(fileid)
Unspecified *fileid;
{
	int i;

	for ( i= 0 ; i < 6 ; i++ ) {
		if ( fileid[i] != 0 )
			return(0);
	}

	return(-1);
}


void FILING_Close(ServerConnection, BDTProc, file, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_Session session;
{
	file_handle *handle;

	BDTabort_expected= FALSE;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Close\n");
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);
#ifdef DEBUG
	fprintf(msgs, "closing %x\n",handle);
#endif DEBUG

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);				/* do it now */

	if ( handle->createdon != 0 )			/* set date if needed */
		set_create_time(handle);

	handle->state= FILE_CLOSED;
	handle->pathname= (char *)0;

#ifdef FILETOOLCOMPATIBILITY
	if ( handle == cur_dir_handle )
		cur_dir_handle= &RootHandle;
#endif FILETOOLCOMPATIBILITY

	free(handle);
	reset_continuance_timer();

	return;
}


FILING_CreateResults FILING_Create(ServerConnection, BDTProc, directory, attributes, controls, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_AttributeSequence attributes;
FILING_ControlSequence controls;
FILING_Session session;
{
#ifdef EXTENSIONS
	FILING_CreateResults results;
	file_handle *handle, *dir_handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Create  ");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

#ifndef FILETOOLCOMPATIBILITY
	if ( is_nullControls(controls) != -1) {
		ReturnControlTypeError(FILING_disallowed, 0);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY

	if ( (handle= (file_handle *)malloc(sizeof(file_handle))) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

	if ( (handle->pathname= (char *)malloc(MAX_FILE_NAME_LENGTH)) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "create handle= %x\n",handle);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	copyhandle(&dir_handle,directory);

	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
		strcpy(handle->pathname, SERVICE_ROOT);
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		strcpy(handle->pathname,dir_handle->pathname);
		if ( strcmp(handle->pathname, "/") != 0 )
			strcat(handle->pathname,"/");
	} 
#else FILETOOLCOMPATIBILITY
	strcpy(handle->pathname, SERVICE_ROOT);
#endif FILETOOLCOMPATIBILITY

	if ( verify_create_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "creating '%s'\n", handle->pathname);
#endif DEBUG

	if ( handle->isdirectory == TRUE ) {
		if ( create_directory(handle) != -1 ) {
			/* NOT REACHED */
		}
	} else {
		if ( create_file(handle) != -1 ) {
			/* NOT REACHED */
		}
	}

	close_file(handle);

	handle->state= FILE_OPEN;
	handle->file_desc= NULL;

	copyhandle(results.file, &handle);
	reset_continuance_timer();

	return(results);
#else EXTENSIONS
	NoSuchProcedureValue("Filing", 4);
#endif EXTENSIONS
}

#ifdef EXTENSIONS
verify_create_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_childrenuniquelynamed, got_createdon, got_datasize;
	int got_defaultaccesslist, got_isdirectory, got_istemporary, got_ordering;
	int got_pathname, got_subtreesizelimit, got_type, got_version;
	char *pathname;
	FILING_Version version;
	Boolean childrenuniquelynamed, istemporary;
	Cardinal ordering;		
	Cardinal subtreesizelimit;
	char *AttrToString();
	Unspecified *AttrToFileID();
	LongCardinal AttrToLongCardinal();

#ifdef FILETOOLCOMPATIBILITY
	int got_name;
	char *name;
#endif FILETOOLCOMPATIBILITY

#ifdef DEBUG
	fprintf(msgs,"%d create attributes	",attr.length);
#endif DEBUG

	if ( attr.length <= 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_childrenuniquelynamed= got_createdon= 0;
	got_datasize= got_defaultaccesslist= got_isdirectory= 0;
	got_istemporary= got_ordering= got_pathname= 0;
	got_subtreesizelimit= got_type= got_version= 0;

#ifdef FILETOOLCOMPATIBILITY
	got_name= 0;
#endif FILETOOLCOMPATIBILITY


	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG
		if ( t == FILING_createdOn ) {
#ifdef DEBUG
			fprintf(msgs,"createdOn  ");
#endif DEBUG
			if ( got_createdon ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_createdon++;

			handle->createdon= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_dataSize ) {
#ifdef DEBUG
			fprintf(msgs,"dataSize  ");
#endif DEBUG
			if ( got_datasize ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_datasize++;

			handle->datasize= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_isDirectory ) {
#ifdef DEBUG
			fprintf(msgs,"isDirectory  ");
#endif DEBUG
			if ( got_isdirectory ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_isdirectory++;

			handle->isdirectory= AttrToBoolean(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_pathname ) {
#ifdef DEBUG
			fprintf(msgs,"pathname  ");
#endif DEBUG
			if ( got_pathname ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_pathname++;

			pathname= AttrToString(&attr.sequence[i]);
			if ( check_pathname(pathname) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}			
		if ( t == FILING_type ) {
#ifdef DEBUG
			fprintf(msgs,"type  ");
#endif DEBUG
			if ( got_type ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_type++;

			handle->type= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}
		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}

#ifdef FILETOOLCOMPATIBILITY
		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}
#endif FILETOOLCOMPATIBILITY

#ifdef SOMEDAY
		if ( t == FILING_accessList ) {
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_accesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_childrenUniquelyNamed ) {
			if ( got_childrenuniquelynamed ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_childrenuniquelynamed++;

			childrenuniquelynamed= AttrToBoolean(&attr.sequence[i]);
			if ( childrenuniquelynamed != TRUE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}


#ifdef SOMEDAY
		if ( t == FILING_defaultAccessList ) {
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_defaultaccesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_isTemporary ) {
			if ( got_istemporary ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_istemporary++;

			istemporary= AttrToBoolean(&attr.sequence[i]);
			if ( istemporary != FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_ordering ) {
			if ( got_ordering ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_ordering++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_subtreesizelimit++;

			subtreesizelimit= AttrToCardinal(&attr.sequence[i]);
			if ( subtreesizelimit != FILING_nullSubtreeSizeLimit ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( (t == FILING_fileID) || (t == FILING_modifiedBy) ||
				(t == FILING_modifiedOn) || 
				(t == FILING_numberOfChildren) || (t == FILING_parentID) ||
				(t == FILING_readBy) || (t == FILING_readOn) ||
				(t == FILING_storedSize) || (t == FILING_subtreeSize) ) {
			ReturnAttributeTypeError(FILING_disallowed, t);
		}

		if ( t < 0 ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		if ( handle->type != TYPE_VP ) {
			if ( (t == FILING_checksum) || (t == FILING_createdBy) ||
						(t == FILING_position) )
				ReturnAttributeTypeError(FILING_unimplemented, t);
			else
				ReturnAttributeTypeError(FILING_disallowed, t);
		}
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	if ( !got_pathname && !got_name) {
		handle->pathname= SERVICE_ROOT;
	} else 	if ( !got_pathname )  {
		strcat(handle->pathname,name);
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname,pathname);
		Deallocate(&pathname);
	}

#else FILETOOLCOMPATIBILITY
	if ( !got_pathname ) {
		ReturnAttributeTypeError(FILING_missing, t);
		/* NOT REACHED */
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname, pathname);
		Deallocate(&pathname);
	}
#endif FILETOOLCOMPATIBILITY

	if ( !got_type )
		handle->type= FILING_tUnspecified;

	if ( !got_createdon )
		handle->createdon= 0;

	if ( !got_isdirectory ) {
		if ( handle->type == FILING_tDirectory )
			handle->isdirectory= TRUE;
		else
			handle->isdirectory= FALSE;
	} else {
		if ( ((handle->isdirectory == TRUE) && (handle->type != FILING_tDirectory)) ||
				((handle->isdirectory == FALSE) && (handle->type == FILING_tDirectory)) ) {
			ReturnAttributeTypeError(FILING_unreasonable, FILING_isDirectory);
			/* NOT REACHED */
		}
	}

	return(-1);
}
#endif EXTENSIONS


void FILING_Delete(ServerConnection, BDTProc, file, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_Session session;
{
	file_handle *handle;

	BDTabort_expected= FALSE;
#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Delete\n");
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);				/* do it now */

	if ( delete_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	handle->state= FILE_CLOSED;
	handle->pathname= (char *)0;

	free(handle);
	reset_continuance_timer();

	return;
}


FILING_GetControlsResults FILING_GetControls(ServerConnection, BDTProc, file, types, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_ControlTypeSequence types;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 6);
}


void FILING_ChangeControls(ServerConnection, BDTProc, file, controls, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_ControlSequence controls;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 7);
}


FILING_GetAttributesResults FILING_GetAttributes(ServerConnection, BDTProc, file, types, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_AttributeTypeSequence types;
FILING_Session session;
{
#ifdef EXTENSIONS
	file_handle *handle;
	FILING_GetAttributesResults results;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Getattributes\n");
#endif DEBUG

	BDTabort_expected= FALSE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);				/* do it now */

	if ( get_types(types,&results.attributes) != -1 ) {
		/* NOT REACHED */
	}

	make_attribute_sequence(handle->pathname,&results.attributes);

	return(results);
#else EXTENSIONS
	NoSuchProcedureValue("Filing", 8);
#endif EXTENSIONS
}

get_types(types, attrseq)
FILING_AttributeTypeSequence types;
FILING_AttributeSequence *attrseq;
{
	int i;
	LongCardinal t;

	if ( types.length <= 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

#ifdef EXTENSIONS
	if ( *(types.sequence) == 037777777777 ) {
#ifdef DEBUG
		fprintf(msgs, "get_types: asking for all\n");
#endif DEBUG
		attrseq->length= -1;
		attrseq->sequence= (FILING_Attribute *)
				Allocate((SUPPORTEDATTRIBUTES + OPTIONALATTRIBUTES) * sizeof(FILING_Attribute)/sizeof(Unspecified));
	} else {
#endif EXTENSIONS
		attrseq->length= types.length;
		attrseq->sequence= (FILING_Attribute *)
				Allocate(types.length * sizeof(FILING_Attribute)/sizeof(Unspecified));

#ifdef DEBUG
		fprintf(msgs, "get_types: asking for ");
#endif DEBUG

		for ( i = 0; i < types.length ; i ++ ) {
			t= types.sequence[i];

#ifdef DEBUG
			fprintf(msgs, "%d  ",t);
#endif DEBUG

			
			if ( (t < 0) || ( t > FILING_subtreeSizeLimit) ) {
#ifdef FILETOOLCOMPATIBILITY
				if ( t != 4938 ) {
					ReturnAttributeTypeError(FILING_illegal, t);
					/* NOT REACHED */
				}
#else FILETOOLCOMPATIBILITY
				ReturnAttributeTypeError(FILING_illegal, t);
				/* NOT REACHED */
#endif FILETOOLCOMPATIBILITY
			}

			if ( (t != FILING_createdOn) && (t != FILING_modifiedOn) &&
					(t != FILING_isDirectory) && (t != FILING_isTemporary) &&
					(t != FILING_name) && (t != FILING_pathname) &&
#ifndef FILETOOLCOMPATIBILITY
					(t != FILING_dataSize) && (t != FILING_type) &&
					(t != FILING_version) ) {
#else FILETOOLCOMPATIBILITY
					(t != FILING_dataSize) && (t != FILING_type) &&
					(t != FILING_createdBy) && (t != FILING_readOn) &&
					(t != FILING_version) && (t != FILING_fileID)  &&
					(t != 4938) ) {
#endif FILETOOLCOMPATIBILITY
				ReturnAttributeTypeError(FILING_disallowed, t);
				/* NOT REACHED */
			}
			attrseq->sequence[i].type= t;
		}
#ifdef EXTENSIONS
	}
#endif EXTENSIONS
#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG
}

#ifdef EXTENSIONS
make_supported_attributes(attrseq)
FILING_AttributeSequence *attrseq;

{
	attrseq->length= SUPPORTEDATTRIBUTES;
	attrseq->sequence[0].type= FILING_createdOn;
	attrseq->sequence[1].type= FILING_isDirectory;
	attrseq->sequence[2].type= FILING_modifiedOn;
	attrseq->sequence[3].type= FILING_name;
	attrseq->sequence[4].type= FILING_dataSize;
	attrseq->sequence[5].type= FILING_type;
	attrseq->sequence[6].type= FILING_version;
	attrseq->sequence[7].type= FILING_pathname;
}

make_required_attributes(attrseq)
FILING_AttributeSequence *attrseq;

{
	attrseq->length= REQUIREDATTRIBUTES;
	attrseq->sequence[0].type= FILING_createdOn;
	attrseq->sequence[1].type= FILING_modifiedOn;
	attrseq->sequence[2].type= FILING_name;
	attrseq->sequence[3].type= FILING_dataSize;
	attrseq->sequence[4].type= FILING_version;
	attrseq->sequence[5].type= FILING_pathname;
}
#endif EXTENSIONS

void FILING_ChangeAttributes(ServerConnection, BDTProc, file, attributes, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_AttributeSequence attributes;
FILING_Session session;
{
#ifdef EXTENSIONS
	file_handle *handle;
	char oldname[MAX_FILE_NAME_LENGTH];

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "ChangeAttributes\n");
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);				/* do it now */

	strcpy(oldname, handle->pathname);

	if ( verify_change_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( strcmp(oldname, handle->pathname) != 0 ) {
		rename_file(oldname, handle);
	}

	set_create_time(handle);

	return;
#else EXTENSIONS
	NoSuchProcedureValue("Filing", 9);
#endif EXTENSIONS
}
#ifdef EXTENSIONS
verify_change_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_childrenuniquelynamed, got_createdon, got_datasize;
	int got_defaultaccesslist, got_ordering;
	int got_subtreesizelimit, got_type, got_version;
	FILING_Version version;
	Boolean childrenuniquelynamed;
	Cardinal ordering;		
	Cardinal subtreesizelimit;
	char *AttrToString();
	LongCardinal AttrToLongCardinal();

	int got_name;
	char *name;

#ifdef DEBUG
	fprintf(msgs,"%d change attributes	",attr.length);
#endif DEBUG

	if ( attr.length < 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_childrenuniquelynamed= got_createdon= 0;
	got_datasize= got_defaultaccesslist= 0;
	got_ordering= got_subtreesizelimit= got_type= got_version= 0;
	got_name= 0;

	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG
		if ( t == FILING_createdOn ) {
#ifdef DEBUG
			fprintf(msgs,"createdOn  ");
#endif DEBUG
			if ( got_createdon ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_createdon++;

			handle->createdon= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_dataSize ) {
#ifdef DEBUG
			fprintf(msgs,"dataSize  ");
#endif DEBUG
			if ( got_datasize ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_datasize++;

			handle->datasize= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_type ) {
#ifdef DEBUG
			fprintf(msgs,"type  ");
#endif DEBUG
			if ( got_type ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_type++;

			handle->type= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}
		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}

		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

#ifdef SOMEDAY
		if ( t == FILING_accessList ) {
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_accesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_childrenUniquelyNamed ) {
			if ( got_childrenuniquelynamed ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_childrenuniquelynamed++;

			childrenuniquelynamed= AttrToBoolean(&attr.sequence[i]);
			if ( childrenuniquelynamed != TRUE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}


#ifdef SOMEDAY
		if ( t == FILING_defaultAccessList ) {
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_defaultaccesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_ordering ) {
			if ( got_ordering ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_ordering++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_subtreesizelimit++;

			subtreesizelimit= AttrToCardinal(&attr.sequence[i]);
			if ( subtreesizelimit != FILING_nullSubtreeSizeLimit ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( (t == FILING_fileID) || (t == FILING_isDirectory) ||
				(t == FILING_isTemporary) || (t == FILING_modifiedBy) ||
				(t == FILING_modifiedOn) || (t == FILING_numberOfChildren) ||
				(t == FILING_parentID) || (t == FILING_pathname) ||
				(t == FILING_readBy) || (t == FILING_readOn) ||
				(t == FILING_storedSize) || (t == FILING_subtreeSize) ) {
			ReturnAttributeTypeError(FILING_disallowed, t);
		}

		if ( t < 0 ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		if ( (t == FILING_checksum) || (t == FILING_createdBy) ||
					(t == FILING_position) )
			ReturnAttributeTypeError(FILING_unimplemented, t);
		else
			ReturnAttributeTypeError(FILING_disallowed, t);

	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

	if ( got_name ) {
		char *ptr, *rindex();
		if ( (ptr= rindex(handle->pathname, '/')) == 0 ) {
			ptr= handle->pathname;
		} else {
			ptr++;
			*ptr= '\0';
		}
		strcat(handle->pathname, name);
	}

	return(-1);
}
#endif EXTENSIONS


FILING_CopyResults FILING_Copy(ServerConnection, BDTProc, file, destdir, attributes, controls,  session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_Handle destdir;
FILING_AttributeSequence attributes;
FILING_ControlSequence controls;
FILING_Session session;
{
#ifdef EXTENSIONS
	FILING_CopyResults results;
	file_handle *handle;
	file_handle *dir_handle;
	file_handle *new_handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Copy\n");
#endif DEBUG

	BDTabort_expected= FALSE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	copyhandle(&handle, file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( (new_handle= (file_handle *)malloc(sizeof(file_handle))) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

	if ( (new_handle->pathname= (char *)malloc(MAX_FILE_NAME_LENGTH)) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "copy handle= %x\n",new_handle);
#endif DEBUG

	copyhandle(&dir_handle,destdir);

	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
		strcpy(new_handle->pathname, SERVICE_ROOT);
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		strcpy(new_handle->pathname,dir_handle->pathname);
		if ( strcmp(new_handle->pathname, "/") != 0 )
			strcat(new_handle->pathname,"/");
	} 

	if ( verify_copy_attributes(attributes,new_handle,handle) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);

#ifdef DEBUG
	fprintf(msgs, "copying %s to %s\n", handle->pathname, new_handle->pathname);
	fflush(msgs);
#endif DEBUG

	if ( copy_file(handle, new_handle) != -1 ) {
		/* NOT REACHED */
	}

	new_handle->state= FILE_OPEN;
	new_handle->file_desc= NULL;

	copyhandle(results.newFile, &new_handle);
	reset_continuance_timer();
	return(results);

#else EXTENSIONS
	NoSuchProcedureValue("Filing", 10);
#endif EXTENSIONS
}
#ifdef EXTENSIONS
verify_copy_attributes(attr, tohandle, fromhandle)
FILING_AttributeSequence attr;
file_handle *tohandle;
file_handle *fromhandle;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_defaultaccesslist, got_istemporary;
	int got_name, got_pathname, got_subtreesizelimit, got_version;
	char *pathname;
	char *name;
	FILING_Version version;
	Boolean istemporary;
	Cardinal subtreesizelimit;
	char *AttrToString();
	LongCardinal AttrToLongCardinal();

#ifdef DEBUG
	fprintf(msgs,"%d copy attributes	",attr.length);
#endif DEBUG

	if ( attr.length < 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_defaultaccesslist= got_istemporary= 0;
	got_name= got_pathname= got_subtreesizelimit= got_version= 0;

	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG

		if ( t == FILING_pathname ) {
#ifdef DEBUG
			fprintf(msgs,"pathname  ");
#endif DEBUG
			if ( got_pathname ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_pathname++;

			pathname= AttrToString(&attr.sequence[i]);
			if ( check_pathname(pathname) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}			

		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}


		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

#ifdef SOMEDAY
		if ( t == FILING_accessList ) {
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_accesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_defaultAccessList ) {
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_defaultaccesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_isTemporary ) {
			if ( got_istemporary ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_istemporary++;

			istemporary= AttrToBoolean(&attr.sequence[i]);
			if ( istemporary != FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_subtreesizelimit++;

			subtreesizelimit= AttrToCardinal(&attr.sequence[i]);
			if ( subtreesizelimit != FILING_nullSubtreeSizeLimit ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_position ) {
			ReturnAttributeTypeError(FILING_unimplemented, t);
			/* NOT REACHED */
		}

		if ( ( t < 0 ) || (t > FILING_subtreeSizeLimit) ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		ReturnAttributeTypeError(FILING_disallowed, t);
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	if ( !got_pathname && !got_name) {
		char *ptr, *rindex();
		if ( (ptr= rindex(fromhandle->pathname,'/')) == 0 ) 
			ptr= fromhandle->pathname;
		else
			ptr++;
		strcat(tohandle->pathname, ptr);
	} else 	if ( !got_pathname )  {
		strcat(tohandle->pathname,name);
	} else {
		if ( *pathname == '/' )
			strcat(tohandle->pathname, pathname+1);
		else
			strcat(tohandle->pathname,pathname);
		Deallocate(&pathname);
	}

#else FILETOOLCOMPATIBILITY
	if ( !got_pathname ) {
		ReturnAttributeTypeError(FILING_missing, t);
		/* NOT REACHED */
	} else {
		if ( *pathname == '/' )
			strcat(tohandle->pathname, pathname+1);
		else
			strcat(tohandle->pathname, pathname);
		Deallocate(&pathname);
	}
#endif FILETOOLCOMPATIBILITY

	return(-1);
}
#endif EXTENSIONS


void FILING_Move(ServerConnection, BDTProc, file, destdir, attributes, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_Handle destdir;
FILING_AttributeSequence attributes;
FILING_Session session;
{
#ifdef EXTENSIONS
	file_handle *handle;
	file_handle *dir_handle;
	char oldname[MAX_FILE_NAME_LENGTH];

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Move\n");
#endif DEBUG

	BDTabort_expected= FALSE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	copyhandle(&handle, file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	strcpy(oldname, handle->pathname);

	copyhandle(&dir_handle,destdir);

	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
		strcpy(handle->pathname, SERVICE_ROOT);
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		strcpy(handle->pathname,dir_handle->pathname);
		if ( strcmp(handle->pathname, "/") != 0 )
			strcat(handle->pathname,"/");
	} 


	if ( verify_move_attributes(attributes,handle,oldname) != -1 ) {
		/* NOT REACHED */
	}

	close_file(handle);

#ifdef DEBUG
	fprintf(msgs, "moving %s to %s\n", oldname, handle->pathname);
#endif DEBUG

	if ( rename_file(oldname, handle) != -1 ) {
		/* NOT REACHED */
	}

	reset_continuance_timer();
	return;

#else EXTENSIONS
	NoSuchProcedureValue("Filing", 11);
#endif EXTENSIONS
}
#ifdef EXTENSIONS
verify_move_attributes(attr, tohandle, fromname)
FILING_AttributeSequence attr;
file_handle *tohandle;
char *fromname;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_defaultaccesslist, got_istemporary;
	int got_name, got_subtreesizelimit, got_version;
	char *name;
	FILING_Version version;
	Boolean istemporary;
	Cardinal subtreesizelimit;
	char *AttrToString();
	LongCardinal AttrToLongCardinal();

#ifdef DEBUG
	fprintf(msgs,"%d move attributes	",attr.length);
#endif DEBUG

	if ( attr.length < 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_defaultaccesslist= got_istemporary= 0;
	got_name= got_subtreesizelimit= got_version= 0;

	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG

		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}


		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

#ifdef SOMEDAY
		if ( t == FILING_accessList ) {
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_accesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_defaultAccessList ) {
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_defaultaccesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_isTemporary ) {
			if ( got_istemporary ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_istemporary++;

			istemporary= AttrToBoolean(&attr.sequence[i]);
			if ( istemporary != FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_subtreesizelimit++;

			subtreesizelimit= AttrToCardinal(&attr.sequence[i]);
			if ( subtreesizelimit != FILING_nullSubtreeSizeLimit ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_position ) {
			ReturnAttributeTypeError(FILING_unimplemented, t);
			/* NOT REACHED */
		}

		if ( ( t < 0 ) || (t > FILING_subtreeSizeLimit) ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		ReturnAttributeTypeError(FILING_disallowed, t);
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

	if ( !got_name) {
		char *ptr, *rindex();
		if ( (ptr= rindex(fromname,'/')) == 0 ) 
			ptr= fromname;
		else
			ptr++;
		strcat(tohandle->pathname, ptr);
	} else {
		strcat(tohandle->pathname,name);
	} 

	return(-1);
}
#endif EXTENSIONS


FILING_StoreResults FILING_Store(ServerConnection, BDTProc, directory, attributes, controls, content, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_AttributeSequence attributes;
FILING_ControlSequence controls;
BulkData1_Descriptor content;
FILING_Session session;
{
	FILING_StoreResults results;
	file_handle *handle, *dir_handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Store  ");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

#ifndef FILETOOLCOMPATIBILITY
	if ( is_nullControls(controls) != -1) {
		ReturnControlTypeError(FILING_disallowed, 0);
		/* NOT REACHED */
	}

	if ( is_nullHandle(directory) != -1 ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY

	if ( content.designator == BulkData1_null ) {
		return;
	} else  if ( content.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	if ( (handle= (file_handle *)malloc(sizeof(file_handle))) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

	if ( (handle->pathname= (char *)malloc(MAX_FILE_NAME_LENGTH)) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "store handle= %x\n",handle);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	copyhandle(&dir_handle,directory);

	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
		strcpy(handle->pathname, SERVICE_ROOT);
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		strcpy(handle->pathname,dir_handle->pathname);
		if ( strcmp(handle->pathname, "/") != 0 )
			strcat(handle->pathname,"/");
	} 
#else FILETOOLCOMPATIBILITY
	strcpy(handle->pathname, SERVICE_ROOT);
#endif FILETOOLCOMPATIBILITY

	if ( verify_store_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "creating '%s'\n", handle->pathname);
#endif DEBUG

	if ( handle->isdirectory == TRUE ) {
		if ( dir_storeproc(ServerConnection,handle,content) != -1 ) {
			/* NOT REACHED */
		}
	} else {
		if ( create_file(handle) != -1 ) {
			/* NOT REACHED */
		}

#ifdef EXTENSIONS
		if ( (handle->type > LAST_FILING_TYPE) && (handle->type != TYPE_Interpress) &&
					(handle->type != TYPE_VPCanvas) ) {
			SaveExtendedAttributes(handle->file_desc, attributes);
		}
#endif EXTENSIONS
		if ( storeproc(ServerConnection,handle) != -1 ) {
			close_file(handle);
			delete_partial_file(handle);
			ReturnTransferError(FILING_aborted);
			/* NOT REACHED */
		}

		close_file(handle);
	}

	handle->state= FILE_OPEN;
	handle->file_desc= NULL;

	copyhandle(results.file, &handle);
	reset_continuance_timer();

	return(results);
}

verify_store_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_childrenuniquelynamed, got_createdon, got_datasize;
	int got_defaultaccesslist, got_isdirectory, got_istemporary, got_ordering;
	int got_parentID, got_pathname, got_subtreesizelimit, got_type, got_version;
	char *pathname;
	FILING_Version version;
	Boolean childrenuniquelynamed, istemporary;
	Cardinal ordering;		
	Cardinal subtreesizelimit;
	char *AttrToString();
	Unspecified *AttrToFileID();
	LongCardinal AttrToLongCardinal();

#ifdef FILETOOLCOMPATIBILITY
	int got_name, got_4938;
	char *name;
#endif FILETOOLCOMPATIBILITY

#ifdef DEBUG
	fprintf(msgs,"%d store attributes	",attr.length);
#endif DEBUG

	if ( attr.length <= 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_childrenuniquelynamed= got_createdon= 0;
	got_datasize= got_defaultaccesslist= got_isdirectory= 0;
	got_istemporary= got_ordering= got_parentID= got_pathname= 0;
	got_subtreesizelimit= got_type= got_version= 0;

#ifdef FILETOOLCOMPATIBILITY
	got_name= got_4938= 0;
#endif FILETOOLCOMPATIBILITY


	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG
		if ( t == FILING_createdOn ) {
#ifdef DEBUG
			fprintf(msgs,"createdOn  ");
#endif DEBUG
			if ( got_createdon ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_createdon++;

			handle->createdon= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_dataSize ) {
#ifdef DEBUG
			fprintf(msgs,"dataSize  ");
#endif DEBUG
			if ( got_datasize ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_datasize++;

			handle->datasize= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_isDirectory ) {
#ifdef DEBUG
			fprintf(msgs,"isDirectory  ");
#endif DEBUG
			if ( got_isdirectory ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_isdirectory++;

			handle->isdirectory= AttrToBoolean(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_pathname ) {
#ifdef DEBUG
			fprintf(msgs,"pathname  ");
#endif DEBUG
			if ( got_pathname ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_pathname++;

			pathname= AttrToString(&attr.sequence[i]);
			if ( check_pathname(pathname) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}			
		if ( t == FILING_type ) {
#ifdef DEBUG
			fprintf(msgs,"type  ");
#endif DEBUG
			if ( got_type ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_type++;

			handle->type= AttrToLongCardinal(&attr.sequence[i]);
#ifndef EXTENSIONS
			if ( (handle->type != FILING_tText) &&
					(handle->type != FILING_tUnspecified) &&
					(handle->type != FILING_tDirectory) ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}
#endif EXTENSIONS
			continue;
		}
		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs,"version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
					ReturnAttributeValueError(FILING_unimplemented, t);
					/* NOT REACHED */

			}
			continue;
		}

#ifdef FILETOOLCOMPATIBILITY
		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

		if ( t == 4938 ) {
			if ( got_4938 ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_4938++;
			continue;
		}
#endif FILETOOLCOMPATIBILITY

#ifdef SOMEDAY
		if ( t == FILING_accessList ) {
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_accesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_childrenUniquelyNamed ) {
			if ( got_childrenuniquelynamed ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_childrenuniquelynamed++;

			childrenuniquelynamed= AttrToBoolean(&attr.sequence[i]);
			if ( childrenuniquelynamed != TRUE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}


#ifdef SOMEDAY
		if ( t == FILING_defaultAccessList ) {
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_defaultaccesslist++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_isTemporary ) {
			if ( got_istemporary ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_istemporary++;

			istemporary= AttrToBoolean(&attr.sequence[i]);
			if ( istemporary != FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_ordering ) {
			if ( got_ordering ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_ordering++;

			if ( FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}

			got_subtreesizelimit++;

			subtreesizelimit= AttrToCardinal(&attr.sequence[i]);
			if ( subtreesizelimit != FILING_nullSubtreeSizeLimit ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}

			continue;
		}

		if ( (t == FILING_fileID) || (t == FILING_modifiedBy) ||
				(t == FILING_modifiedOn) || (t == FILING_name) ||
				(t == FILING_numberOfChildren) || (t == FILING_parentID) ||
				(t == FILING_readBy) || (t == FILING_readOn) ||
				(t == FILING_storedSize) || (t == FILING_subtreeSize) ) {
			ReturnAttributeTypeError(FILING_disallowed, t);
		}

#ifndef EXTENSIONS
		if ( ( t < 0 ) || (t > FILING_subtreeSizeLimit) ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		if ( (t == FILING_checksum) || (t == FILING_createdBy) ||
						(t == FILING_position) )
			ReturnAttributeTypeError(FILING_unimplemented, t);
		else
			ReturnAttributeTypeError(FILING_disallowed, t);

#else EXTENSIONS
		if ( t < 0 ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}
		if ( handle->type != TYPE_VP ) {
			if ( (t == FILING_checksum) || (t == FILING_createdBy) ||
						(t == FILING_position) )
				ReturnAttributeTypeError(FILING_unimplemented, t);
			else
				ReturnAttributeTypeError(FILING_disallowed, t);
		}
#endif EXTENSIONS
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	if ( !got_pathname && !got_name) {
		handle->pathname= SERVICE_ROOT;
	} else 	if ( !got_pathname )  {
		strcat(handle->pathname,name);
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname,pathname);
		Deallocate(&pathname);
	}

#else FILETOOLCOMPATIBILITY
	if ( !got_pathname ) {
		ReturnAttributeTypeError(FILING_missing, t);
		/* NOT REACHED */
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname, pathname);
		Deallocate(&pathname);
	}
#endif FILETOOLCOMPATIBILITY

	if ( !got_type )
		handle->type= FILING_tUnspecified;

	if ( !got_createdon )
		handle->createdon= 0;

	if ( !got_isdirectory ) {
		if ( handle->type == FILING_tDirectory )
			handle->isdirectory= TRUE;
		else
			handle->isdirectory= FALSE;
	} else {
		if ( ((handle->isdirectory == TRUE) && (handle->type != FILING_tDirectory)) ||
				((handle->isdirectory == FALSE) && (handle->type == FILING_tDirectory)) ) {
			ReturnAttributeTypeError(FILING_unreasonable, FILING_isDirectory);
			/* NOT REACHED */
		}
	}

	return(-1);
}


void FILING_Retrieve(ServerConnection, BDTProc, file, content, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
BulkData1_Descriptor content;
FILING_Session session;
{
	file_handle *handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Retrieve\n");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( content.designator == BulkData1_null ) {
		return;
	} else  if ( content.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	if ( open_file(handle) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "retrieving '%s'\n", handle->pathname);
#endif DEBUG

	if ( retrieveproc(ServerConnection,handle) != -1 ) {
		close_file(handle);
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	close_file(handle);
	handle->file_desc= NULL;
	reset_continuance_timer();

	return;
}


void FILING_Replace(ServerConnection, BDTProc, file, attributes, content, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_AttributeSequence attributes;
BulkData1_Descriptor content;
FILING_Session session;
{
#ifdef EXTENSIONS
	file_handle *handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Replace\n");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( content.designator == BulkData1_null ) {
		return;
	} else  if ( content.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	if ( verify_replace_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "replacing '%s'\n", handle->pathname);
#endif DEBUG

	if ( make_backup(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( open_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( storeproc(ServerConnection,handle) != -1 ) {
		close_file(handle);
		recall_backup(handle);
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	close_file(handle);
	unlink_backup(handle);

	handle->file_desc= NULL;
	reset_continuance_timer();

	return;

#else EXTENSIONS
	NoSuchProcedureValue("Filing", 14);
#endif EXTENSIONS
}

#ifdef EXTENSIONS
verify_replace_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	LongCardinal datasize;
	int got_createdon, got_datasize;
	LongCardinal AttrToLongCardinal();

#ifdef DEBUG
	fprintf(msgs,"%d replace attributes	",attr.length);
#endif DEBUG

	if ( attr.length < 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_createdon= 0;

	for ( i= 0 ; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;
#ifdef DEBUG
		fprintf(msgs, "%d ",t);
#endif DEBUG
		if ( t == FILING_createdOn ) {
#ifdef DEBUG
			fprintf(msgs,"createdOn  ");
#endif DEBUG
			if ( got_createdon ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_createdon++;

			handle->createdon= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_dataSize ) {
#ifdef DEBUG
			fprintf(msgs,"dataSize  ");
#endif DEBUG
			if ( got_datasize ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_datasize++;

			datasize= AttrToLongCardinal(&attr.sequence[i]);
			continue;
		}

		if ( t < 0 ) {
			ReturnAttributeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		if ( (t == FILING_checksum) || (t == FILING_createdBy) ||
						(t == FILING_position) )
			ReturnAttributeTypeError(FILING_unimplemented, t);
		else
			ReturnAttributeTypeError(FILING_disallowed, t);

	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

	return(-1);
}
#endif EXTENSIONS


void FILING_Serialize(ServerConnection, BDTProc, file, serializedFile, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
BulkData1_Descriptor serializedFile;
FILING_Session session;
{
#ifdef EXTENSIONS
	file_handle *handle;
	LongCardinal type;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Serialize\n");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	if ( is_nullHandle(file) == -1 ) {
		ReturnHandleError(FILING_nullDisallowed);
		/* NOT REACHED */
	}

	copyhandle(&handle,file);

	if ( handle->state != FILE_OPEN ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}

	if ( access_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( serializedFile.designator == BulkData1_null ) {
		return;
	} else  if ( serializedFile.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	/*
	 * for now, can only serialize files which are already in 'serialized'
	 * form. This can be assumed if the file is of a Viewpoint file type
	 * and the stored isDirectory attribute is TRUE.
	 * NOTE: like all other cases, this is no guarantee...
	*/

	type= get_type(handle->pathname);

	if ( (type < LAST_FILING_TYPE) || (type == TYPE_Interpress) || (type == TYPE_VPCanvas) ) {
		ReturnAccessError(FILING_fileChanged);
		/* NOT REACHED */
	}

	if ( open_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( GetDirectoryAttribute(handle->file_desc) != TRUE ) {
		close_file(handle);
		ReturnAccessError(FILING_fileChanged);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "serializing '%s'\n", handle->pathname);
#endif DEBUG

	if ( retrieveproc(ServerConnection,handle) != -1 ) {
		close_file(handle);
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	close_file(handle);
	handle->file_desc= NULL;
	reset_continuance_timer();
#else EXTENSIONS
	NoSuchProcedureValue("Filing", 15);
#endif EXTENSIONS
}


FILING_DeserializeResults FILING_Deserialize(ServerConnection, BDTProc,
		 directory, attributes, controls, serializedFile, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_AttributeSequence attributes;
FILING_ControlSequence controls;
BulkData1_Descriptor serializedFile;
FILING_Session session;
{
#ifdef EXTENSIONS
	FILING_DeserializeResults results;
	file_handle *handle, *dir_handle;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Deserialize  ");
#endif DEBUG

	BDTabort_expected= TRUE;

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

#ifndef FILETOOLCOMPATIBILITY
	if ( is_nullControls(controls) != -1) {
		ReturnControlTypeError(FILING_disallowed, 0);
		/* NOT REACHED */
	}

	if ( is_nullHandle(directory) != -1 ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY

	if ( serializedFile.designator == BulkData1_null ) {
		return;
	} else  if ( serializedFile.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	if ( (handle= (file_handle *)malloc(sizeof(file_handle))) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

	if ( (handle->pathname= (char *)malloc(MAX_FILE_NAME_LENGTH)) == NULL ) {
		ReturnUndefinedError(0);
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "deserialize handle= %x\n",handle);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	copyhandle(&dir_handle,directory);

	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
		strcpy(handle->pathname, SERVICE_ROOT);
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		strcpy(handle->pathname,dir_handle->pathname);
		if ( strcmp(handle->pathname, "/") != 0 )
			strcat(handle->pathname,"/");
	} 
#else FILETOOLCOMPATIBILITY
	strcpy(handle->pathname, SERVICE_ROOT);
#endif FILETOOLCOMPATIBILITY

	if ( verify_deserialize_attributes(attributes, handle) != -1 ) {
		/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "creating '%s'\n", handle->pathname);
#endif DEBUG

	if ( create_file(handle) != -1 ) {
		/* NOT REACHED */
	}

	if ( storeproc(ServerConnection,handle) != -1 ) {
		close_file(handle);
		delete_partial_file(handle);
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	close_file(handle);

	handle->state= FILE_OPEN;
	handle->file_desc= NULL;

	copyhandle(results.file, &handle);
	reset_continuance_timer();

	return(results);
#else EXTENSIONS
	NoSuchProcedureValue("Filing", 16);
#endif EXTENSIONS
}

#ifdef EXTENSIONS
verify_deserialize_attributes(attr, handle)
FILING_AttributeSequence attr;
file_handle *handle;
{
	int i;
	FILING_AttributeType t;
	int got_accesslist, got_defaultaccesslist, got_istemporary;
	int got_name, got_pathname, got_subtreesizelimit, got_version;
	FILING_Version version;
	Boolean istemporary;
	char *pathname;
	char *name;
	char *AttrToString();

#ifdef DEBUG
	fprintf(msgs, "%d deserialize attributes     ", attr.length);
#endif DEBUG

	if ( attr.length <= 0 ) {
		ReturnAttributeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}

	got_accesslist= got_defaultaccesslist= got_istemporary= 0;
	got_name= got_pathname= got_subtreesizelimit= got_version= 0;

	for ( i= 0; i < attr.length ; i++ ) {
		t= attr.sequence[i].type;

		if ( t == FILING_accessList ) {
#ifdef DEBUG
			fprintf(msgs, "accessList  ");
#endif DEBUG
			if ( got_accesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_accesslist++;

			continue;
		}

#ifdef SOMEDAY
		if ( t == FILING_defaultAccessList ) {
#ifdef DEBUG
			fprintf(msgs, "defaultAccessList  ");
#endif DEBUG
			if ( got_defaultaccesslist ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_defaultaccesslist++;

			continue;
		}
#endif SOMEDAY

		if ( t == FILING_isTemporary ) {
#ifdef DEBUG
			fprintf(msgs, "isTemporary  ");
#endif DEBUG
			if ( got_istemporary ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_istemporary++;

			istemporary= AttrToBoolean(&attr.sequence[i]);
			if ( istemporary != FALSE ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/*  NOT REACHED */
			}
			continue;
		}

		if ( t == FILING_name ) {
#ifdef DEBUG
			fprintf(msgs, "name  ");
#endif DEBUG
			if ( got_name ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_name++;

			name= AttrToString(&attr.sequence[i]);
			continue;
		}

		if ( t == FILING_pathname ) {
#ifdef DEBUG
			fprintf(msgs, "pathname  ");
#endif DEBUG
			if ( got_pathname ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_pathname++;

			pathname= AttrToString(&attr.sequence[i]);
			if ( check_pathname(pathname) != 1 ) {
				/* NOT REACHED */
			}
			continue;
		}

		if ( t == FILING_subtreeSizeLimit ) {
#ifdef DEBUG
			fprintf(msgs, "subtreeSizeLimit  ");
#endif DEBUG
			if ( got_subtreesizelimit ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_subtreesizelimit++;

			continue;
		}

		if ( t == FILING_version ) {
#ifdef DEBUG
			fprintf(msgs, "version  ");
#endif DEBUG
			if ( got_version ) {
				ReturnAttributeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_version++;

			version= AttrToCardinal(&attr.sequence[i]);
			if ( version != FILING_highestVersion ) {
				ReturnAttributeValueError(FILING_unimplemented, t);
				/* NOT REACHED */
			}
			continue;
		}
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG

	if ( !got_pathname && !got_name ) {
		handle->pathname= SERVICE_ROOT;
	} else if ( !got_pathname ) {
		strcat(handle->pathname, name);
		Deallocate(&name);
	} else {
		if ( *pathname == '/' )
			strcat(handle->pathname, pathname+1);
		else
			strcat(handle->pathname, pathname);
		Deallocate(&pathname);
	}

	handle->type= FILING_tSerialized;
	handle->createdon= 0;

	return(-1);
}
#endif EXTENSIONS

FILING_FindResults FILING_Find(ServerConnection, BDTProc, directory, types, scope, controls, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_AttributeTypeSequence types;
FILING_ScopeSequence scope;
FILING_ControlSequence controls;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 17);
}


void FILING_List(ServerConnection, BDTProc, directory, types, scope, listing, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_AttributeTypeSequence types;
FILING_ScopeSequence scope;
BulkData1_Descriptor listing;
FILING_Session session;
{
	char *match_name;
	Cardinal count;
	file_handle *dir_handle;

	BDTabort_expected= TRUE;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "List\n");
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

#ifndef FILETOOLCOMPATIBILITY
	if ( is_nullHandle(directory) != -1 ) {
		ReturnHandleError(FILING_invalid);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY

	if ( listing.designator == BulkData1_null )
		return;
	else if ( listing.designator != BulkData1_immediate ) {
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	copyhandle(&dir_handle, directory);

#ifdef FILETOOLCOMPATIBILITY
	if ( dir_handle == 0 ) {
		dir_handle= &RootHandle;
	} else {
		if ( dir_handle->state != FILE_OPEN ) {
			ReturnHandleError(FILING_invalid);
			/* NOT REACHED */
		}

		if ( dir_handle->isdirectory != TRUE ) {
			ReturnHandleError(FILING_directoryRequired);
			/* NOT REACHED */
		}

		if ( access_file(dir_handle) != -1 ) {
			/* NOT REACHED */
		}

	}
#else FILETOOLCOMPATIBILITY
	dir_handle= &RootHandle;
#endif FILETOOLCOMPATIBILITY

	if ( get_scopes(scope, &count, &match_name) != -1 ) {
		/* NOT REACHED */
	}

	if ( list_directory(ServerConnection, dir_handle, types, match_name, count) != -1 ) {
		/* NOT REACHED */
	}

	reset_continuance_timer();

	return;
}

get_scopes(scope, count, matches_name)
FILING_ScopeSequence scope;
Cardinal *count;
char **matches_name;
{
	int got_count, got_filter, got_matches;
	int i, scopetype;
	FILING_ScopeType t;
#ifdef FILETOOLCOMPATIBILITY
	int got_equal;
	static char *wildcard_all= "*";

		*matches_name= wildcard_all;
		*count= FILING_unlimitedCount;
	if ( scope.length < 0 ) {
		ReturnScopeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	} else if ( scope.length == 0 ) {
		return(-1);
	}
#else FILETOOLCOMPATIBILITY
	if ( scope.length <= 0 ) {
		ReturnScopeTypeError(FILING_illegal, 0);
		/* NOT REACHED */
	}
#endif FILETOOLCOMPATIBILITY

#ifdef DEBUG
	fprintf(msgs, "get_scope %d scopes   ",scope.length);
#endif DEBUG

	got_count= got_filter= got_matches= 0;

	for ( i= 0 ; i < scope.length ; i++ ) {
		t= scope.sequence[i].designator;

		if ( t == FILING_filter ) {
#ifdef DEBUG
			fprintf(msgs, "filter  ");
#endif DEBUG
			if ( got_filter ) {
				ReturnScopeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_filter++;

			if ( get_filter(scope.sequence[i].FILING_filter_case,matches_name) != -1 ) {
				/* NOT REACHED */
			}
			continue;
		}

		if ( t == FILING_count ) {
#ifdef DEBUG
			fprintf(msgs, "count  ");
#endif DEBUG
			if ( got_count ) {
				ReturnScopeTypeError(FILING_duplicated, t);
				/* NOT REACHED */
			}
			got_count++;

			*count= AttrToCardinal(&scope.sequence[i].FILING_count_case);
#ifdef DEBUG
			fprintf(msgs, "count = %d  ",*count);
#endif DEBUG
			continue;
		}

		scopetype= (Cardinal) t;
		if ( (scopetype < 0) || (scopetype > (Cardinal) FILING_depth) ) {
			ReturnScopeTypeError(FILING_illegal, t);
			/* NOT REACHED */
		}

		ReturnScopeTypeError(FILING_unimplemented, t);
		/* NOT REACHED */
	}

	if ( !got_count ) {
		*count= FILING_unlimitedCount;
	}

#ifdef DEBUG
	fprintf(msgs, "\n");
#endif DEBUG
}

get_filter(filter,matches_name)
FILING_Filter filter;
char **matches_name;
{

	int got_matches= 0;
#ifdef FILETOOLCOMPATIBILITY
	int got_equal= 0;
	int got_all= 0;
#endif FILETOOLCOMPATIBILITY

	if ( filter.designator == FILING_matches ) {
		if ( got_matches ) {
			ReturnScopeValueError(FILING_duplicated, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}	

		got_matches++;

#ifdef DEBUG
		fprintf(msgs, "matches on %d attribute\n",filter.FILING_matches_case.attribute.type);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
		if ( (filter.FILING_matches_case.attribute.type != FILING_pathname) &&
				(filter.FILING_matches_case.attribute.type != FILING_name) ) {
			ReturnScopeValueError(FILING_unimplemented, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}
#else FILETOOLCOMPATIBILITY
		if ( filter.FILING_matches_case.attribute.type != FILING_pathname ) {
			ReturnScopeValueError(FILING_unimplemented, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}
#endif FILETOOLCOMPATIBILITY

		*matches_name= AttrToString(&filter.FILING_matches_case.attribute);

#ifdef DEBUG
		fprintf(msgs, "matches name= '%s'\n",*matches_name);
#endif DEBUG

#ifdef FILETOOLCOMPATIBILITY
	} else if ( filter.designator == FILING_equal ) {
		if ( got_equal ) {
			ReturnScopeValueError(FILING_duplicated, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}	

		got_equal++;

#ifdef DEBUG
		fprintf(msgs, "equal on %d attribute\n",filter.FILING_matches_case.attribute.type);
#endif DEBUG

		if ( (filter.FILING_matches_case.attribute.type != FILING_pathname) &&
				(filter.FILING_matches_case.attribute.type != FILING_name) ) {
			ReturnScopeValueError(FILING_unimplemented, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}

		*matches_name= AttrToString(&filter.FILING_matches_case.attribute);

#ifdef DEBUG
		fprintf(msgs, "matches name= '%s'\n",*matches_name);
#endif DEBUG

	} else if ( filter.designator == FILING_all ) {
		if ( got_all ) {
			ReturnScopeValueError(FILING_duplicated, (Cardinal) FILING_filter);
			/* NOT REACHED */
		}
		got_all++;

#ifdef DEBUG
		fprintf(msgs, "all");
#endif DEBUG
#endif FILETOOLCOMPATIBILITY
	} else {
		ReturnScopeValueError(FILING_unimplemented, (Cardinal) FILING_filter);
		/* NOT REACHED */
	}
	return(-1);
}


FILING_ContinueResults FILING_Continue(ServerConnection, BDTProc, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Session session;
{
	FILING_ContinueResults results;

	BDTabort_expected= FALSE;

#ifdef DEBUG
	if (msgs == 0) {
		char logfile[50];
		sprintf(logfile, "/tmp/filing%ld.msgs", getpid());
		msgs= fopen(logfile,"w");
	}
	fprintf(msgs, "Continue\n");
#endif DEBUG

	if ( verify_session(session) != -1 ) {
		/* NOT REACHED */
	}

	results.continuance= continuance;
	reset_continuance_timer();

	return(results);
}

continuance_expiration()
{
	/*
	 * if BDT in progress, don't close connection
	 */

	if ( !BDTabort_expected ) {
#ifdef DEBUG
		fprintf(msgs, "continuance_expiration, closing connection\n");
		fflush(msgs);
#endif DEBUG
		CourierClose(_serverConnection);
		exit(1);
	} else {
		reset_continuance_timer();
	}

}


void FILING_UnifyAccessLists(ServerConnection, BDTProc, directory, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle directory;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 20);
}


void FILING_RetrieveBytes(ServerConnection, BDTProc, file, range, sink, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_ByteRange range;
BulkData1_Descriptor sink;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 22);
}


void FILING_ReplaceBytes(ServerConnection, BDTProc, file, range, source, session)
CourierConnection *ServerConnection;
int ( *BDTProc)();
FILING_Handle file;
FILING_ByteRange range;
BulkData1_Descriptor source;
FILING_Session session;
{
	NoSuchProcedureValue("Filing", 23);
}


dir_storeproc(conn,handle,content)
CourierConnection *conn;
file_handle *handle;
BulkData1_Descriptor content;
{
	int count;
	char buffer[10];

	if ( content.designator == BulkData1_immediate ) {
		if ( (count= BDTread(conn,buffer,sizeof(buffer))) > 0 ) {
			BDTabort(conn);
			ReturnAttributeTypeError(FILING_unreasonable, FILING_isDirectory);
			/* NOT REACHED */
		}
	}

	if ( create_directory(handle) != -1 ) {
		/* NOT REACHED */
	}

	return(-1);
}


static Unspecified list_buffer[SPPMAXDATA];
static Unspecified *list_end= list_buffer + SPPMAXDATA -1;
static Unspecified *list_ptr= list_buffer;

put_next_attribute_sequence(conn,stream_of_attrseq)
CourierConnection *conn;
FILING_StreamOfAttributeSequence *stream_of_attrseq;
{
	int ocount;

	stream_of_attrseq->designator= nextSegment;

	if ( (list_ptr + FILING_sizeof_StreamOfAttributeSequence(stream_of_attrseq)) > list_end ) {
#ifdef DEBUG
		fprintf(msgs,"put_next writing %d bulk data\n",list_ptr-list_buffer);
#endif DEBUG
		if ( (ocount= BDTwrite(conn, list_buffer, (list_ptr-list_buffer)*sizeof(Cardinal))) <= 0 ) {
			list_ptr= list_buffer;
			BDTabort(conn);
			ReturnTransferError(FILING_aborted);
			/* NOT REACHED */
		}

		list_ptr= list_buffer;
	}

	list_ptr += FILING_externalize_StreamOfAttributeSequence(stream_of_attrseq, list_ptr);

#ifdef DEBUG
	fprintf(msgs, "put_next_attr_seq (out) buf= %x, ptr= %x\n",list_buffer, list_ptr);
#endif DEBUG

	return(-1);
}

put_last_attribute_sequence(conn)
CourierConnection *conn;
{

	int ocount;
	Cardinal zero= 0, lastseg= (Cardinal) lastSegment;

	if ( (list_ptr + (sizeof_Cardinal(0) * 3)) > list_end ) {
#ifdef DEBUG
	fprintf(msgs,"put_last writing %d bulk data\n",list_ptr-list_buffer);
#endif DEBUG
		if ( (ocount= BDTwrite(conn, list_buffer, (list_ptr-list_buffer)*sizeof(Cardinal))) <= 0 ) {
			list_ptr= list_buffer;
			BDTabort(conn);
			ReturnTransferError(FILING_aborted);
			/* NOT REACHED */
		}
		list_ptr= list_buffer;
	}

	list_ptr+= externalize_Cardinal(&lastseg, list_ptr);
	list_ptr+= externalize_Cardinal(&zero, list_ptr);

#ifdef DEBUG
	fprintf(msgs,"put_last writing %d bulk data\n",list_ptr-list_buffer);
#endif DEBUG
	if ( (ocount= BDTwrite(conn, list_buffer, (list_ptr-list_buffer)*sizeof(Cardinal))) <= 0 ) {
		list_ptr= list_buffer;
		BDTabort(conn);
		ReturnTransferError(FILING_aborted);
		/* NOT REACHED */
	}

	list_ptr= list_buffer;
	return(-1);
}

