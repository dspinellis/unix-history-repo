#if	defined(RCSIDENT) && defined (MAINLINE)
static char zzdumpid[] = "$Header: dump.h,v 1.7 85/01/18 15:47:35 notes Rel $";
#endif	defined(RCSIDENT) && defined(MAINLINE)

struct dump_f						/* variable relations */
{
    char   *du_name;
    int     du_number;
};

/*
 *	Variable numbers for the assorted fields of the
 *	notesfile descriptor.
 */

#define	NF_TITLE			1
#define	NF_DIRECTOR_MESSAGE		2
#define	NF_LAST_MODIFIED		3
#define	NF_STATUS			4
#define	NF_ID_SEQUENCE			5
#define	NF_NUMBER			6
#define	NF_LAST_TRANSMIT		7
#define	NF_CREATED			8
#define	NF_LAST_USED			9
#define	NF_DAYS_USED			10
#define	NF_NOTES_WRITTEN		11
#define	NF_NOTES_READ			12
#define	NF_NOTES_TRANSMITTED		13
#define	NF_NOTES_RECEIVED		14
#define	NF_NOTES_DROPPED		15
#define	NF_RESPONSES_WRITTEN		16
#define	NF_RESPONSES_READ		17
#define	NF_RESPONSES_TRANSMITTED	18
#define	NF_RESPONSES_RECEIVED		19
#define	NF_RESPONSES_DROPPED		20
#define	NF_ENTRIES			21
#define	NF_WALLTIME			22
#define	NF_ORPHANS_RECEIVED		23
#define	NF_ORPHANS_ADOPTED		24
#define	NF_TRANSMITS			25
#define	NF_RECEIVES			26
#define	NF_EXPIRATION_AGE		27
#define	NF_EXPIRATION_ACTION		28
#define	NF_EXPIRATION_STATUS		29
#define	NF_WORKING_SET_SIZE		30
#define	NF_LONGEST_TEXT			31
#define	NF_POLICY_EXISTS		32
#define	NF_DESCRIPTOR			33

/*
 *	Variable numbers for the assorted fields of the
 *	Access list
 *
 *	Currently unused, but what the hell.
 */

#define	ACCESS_RIGHT		1
#define	ACCESS_FINISHED		2

/*
 *	RFC-style lines for a note
 */

#define	NOTE_TITLE		1
#define	NOTE_AUTHOR		2
#define	NOTE_AUTHOR_UID		3
#define	NOTE_ID			4
#define	NOTE_WRITTEN		5
#define	NOTE_RECEIVED		6
#define	NOTE_MODIFIED		7
#define	NOTE_FROMSYS		8
#define	NOTE_STATUS		9
#define	NOTE_LENGTH		10

/*
 *	RFC-style lines for a response
 */

#define	RESP_TITLE		1
#define	RESP_PARENT		2
#define	RESP_AUTHOR		3
#define	RESP_AUTHOR_UID		4
#define	RESP_ID			5
#define	RESP_WRITTEN		6
#define	RESP_RECEIVED		7
#define	RESP_FROMSYS		8
#define	RESP_STATUS		9
#define	RESP_LENGTH		10

/*
 *	Declarations for assorted tables of variables.
 */

extern struct dump_f    descrnames[];			/* descriptor */
extern struct dump_f    accessnames[];			/* access rights */
extern struct dump_f    notenames[];			/* a note */
extern struct dump_f    respnames[];			/* a response */
