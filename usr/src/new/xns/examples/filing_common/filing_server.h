/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	filing_server.h,v $
 * Revision 1.2  87/03/17  16:33:00  ed
 * Added defines for classes of attributes.
 * 
 * Revision 1.1  87/01/06  16:29:06  ed
 * Initial revision
 * 
 * 
 */

#define MAX_HANDLES	10		/* maximum number of open files */

#define MAX_FILE_NAME_LENGTH	256	/* maximum length of file name */

#define SUPPORTEDATTRIBUTES	8	/* see make_attribute_sequence */
#define REQUIREDATTRIBUTES	6	/* see make_attribute_sequence */
#define OPTIONALATTRIBUTES	50	/* for Viewpoint files */

/*
 * file handle
 *	one per open file
 */

typedef struct {
	int		state;			/* current state */
#define		FILE_CLOSED	0
#define		FILE_OPEN	1
	char		*pathname;		/* ptr to pathname value */
	LongCardinal	type;			/* client requested type (from Open) */
	LongCardinal	truetype;		/* file system file type */
	Cardinal	datasize;		/* dataSize value */
	Boolean		isdirectory;		/* isDirectory */
	LongCardinal	createdon;		/* createdOn */
	LongCardinal	modifiedon;		/* modifiedOn */
	FILE		*file_desc;		/* ptr to file descriptor for open file */
} file_handle;

/*
 * session handle
 *	one per session
 */

typedef struct {
	int				state;			/* current state */
#define		SESSION_CLOSED	0
#define		SESSION_OPEN	1

	CourierConnection		*connection;		/* connection id */
	FILING_Credentials		credentials;		/* user credentials */
	AUTHENTICATION_SimpleVerifier	verifier;		/* user verifier */
	file_handle			handle[MAX_HANDLES];	/* array of open files */
} session_handle;

