#include "dump.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: loadvars.c,v 1.7 85/01/18 15:42:20 notes Rel $";
#endif	RCSIDENT

/*
 *	contains the actual data used to parse RFC-822 style headers
 *	to determine which type of header line this is.
 *
 *	Contains tables for descriptors, notes, responses, and
 *	access lists.  Some of these will be null....
 *
 *	Ray Essick, March 1984
 */

struct dump_f   descrnames[] =
{
    "NF-Title", NF_TITLE,
    "NF-Director-Message", NF_DIRECTOR_MESSAGE,
    "NF-Last-Modified", NF_LAST_MODIFIED,
    "NF-Status", NF_STATUS,
    "NF-Id-Sequence", NF_ID_SEQUENCE,
    "NF-Number", NF_NUMBER,
    "NF-Last-Transmit", NF_LAST_TRANSMIT,
    "NF-Created", NF_CREATED,
    "NF-Last-Used", NF_LAST_USED,
    "NF-Days-Used", NF_DAYS_USED,
    "NF-Notes-Written", NF_NOTES_WRITTEN,
    "NF-Notes-Read", NF_NOTES_READ,
    "NF-Notes-Transmitted", NF_NOTES_TRANSMITTED,
    "NF-Notes-Received", NF_NOTES_RECEIVED,
    "NF-Notes-Dropped", NF_NOTES_DROPPED,
    "NF-Responses-Written", NF_RESPONSES_WRITTEN,
    "NF-Responses-Read", NF_RESPONSES_READ,
    "NF-Responses-Transmitted", NF_RESPONSES_TRANSMITTED,
    "NF-Responses-Received", NF_RESPONSES_RECEIVED,
    "NF-Responses-Dropped", NF_RESPONSES_DROPPED,
    "NF-Entries", NF_ENTRIES,
    "NF-Walltime", NF_WALLTIME,
    "NF-Orphans-Received", NF_ORPHANS_RECEIVED,
    "NF-Orphans-Adopted", NF_ORPHANS_ADOPTED,
    "NF-Transmits", NF_TRANSMITS,
    "NF-Receives", NF_RECEIVES,
    "NF-Expiration-Age", NF_EXPIRATION_AGE,
    "NF-Expiration-Action", NF_EXPIRATION_ACTION,
    "NF-Expiration-Status", NF_EXPIRATION_STATUS,
    "NF-Working-Set-Size", NF_WORKING_SET_SIZE,
    "NF-Longest-Text", NF_LONGEST_TEXT,
    "NF-Policy-Exists", NF_POLICY_EXISTS,
    "NF-Descriptor", NF_DESCRIPTOR,
    "", -1						/* null terminator */
/*
 *	Also catches an empty header type! What luck
 */
};

/*
 *	accessnames - strings and numbers used for access lists
 *	(this list isn't used too much)
 */

struct dump_f   accessnames[] =
{
    "NF-Access-Right", ACCESS_RIGHT,
    "Access-Right", ACCESS_RIGHT,
    "NF-Access-Finished", ACCESS_FINISHED,
    "", -1
};

/*
 *	notenames - strings and numbers used for a note descriptor
 */

struct dump_f   notenames[] =
{
    "Title", NOTE_TITLE,
    "Author", NOTE_AUTHOR,
    "Author-UID", NOTE_AUTHOR_UID,
    "Note-ID", NOTE_ID,
    "Date-Written", NOTE_WRITTEN,
    "Date-Received", NOTE_RECEIVED,
    "Date-Modified", NOTE_MODIFIED,
    "Source-System", NOTE_FROMSYS,
    "Status", NOTE_STATUS,
    "Text-Length", NOTE_LENGTH,
#ifdef	notdef
/*
 *		News compatibility
 *	These are untested and sure to fail since we make assumptions
 *	about formats. If the variable number were different then
 *	things would be OK.
 */
    "Posted", NOTE_WRITTEN,
    "Date", NOTE_WRITTEN,
    "Subject", NOTE_TITLE,
    "Message-ID", NOTE_ID,				/* different format */
    "Article-ID", NOTE_ID,				/* different format */
#endif	notdef
    "", -1
};

/*
 *	respnames - strings and numbers used for a response descriptor
 */

struct dump_f   respnames[] =
{
    "Title", RESP_TITLE,				/* ignored */
    "Parent-ID", RESP_PARENT,
    "Author", RESP_AUTHOR,
    "Author-UID", RESP_AUTHOR_UID,
    "Response-ID", RESP_ID,
    "Date-Written", RESP_WRITTEN,
    "Date-Received", RESP_RECEIVED,
    "Source-System", RESP_FROMSYS,
    "Status", RESP_STATUS,
    "Text-Length", RESP_LENGTH,
#ifdef	notdef
/*
 *	News compatibility (sort of)
 *		This stuff isn't really implemented, so be damn
 *	careful before using it.
 */
    "Article-ID", RESP_ID,				/* what the heck */
    "Message-Id", RESP_ID,				/* different format */
    "Posted", RESP_WRITTEN,
    "Date", RESP_WRITTEN,
    "Subject", RESP_TITLE,				/* will drop */
#endif	notdef
    "", -1
};
