/*
**  Number of hash buckets, best performance if 2*number entries.
*/
#define BB_MAX_HASH			(2 * BB_MAX_IMP)
#define BB_HASH_TABLE_FULL		(-1)
#define BB_HASH_ID_NOT_FOUND		(-2)

/*
**  Defines for the names of the data files.
*/
#define BB_CODATA_FILE			"bb_data.company"
#define BB_PASSWD_FILE			"bb_data.passwd"
#define BB_PHASE_FILE			"bb_data.phases"
#define BB_BOARD_FILE			"bb_data.board"

/*
**  Defines for the company data file.
*/
#define BB_COMMENT_DESIGNATOR		'#'
#define BB_IP_DESIGNATOR		'P'
#define BB_CO_DESIGNATOR		'C'
#define BB_IMP_DESIGNATOR		'I'
#define BB_ID_DESIGNATOR		'D'
#define BB_BOOTH_DESIGNATOR		'B'
#define BB_FLAGS_DESIGNATOR		'F'
#define BB_END_DESIGNATOR		'E'
#define BB_DES_CHAR			1	/* Unique of all DESIGN.*/
#define BB_DES_START			3	/* Start of data in line*/

#define BB_IP_SEPARATOR			" "
#define BB_IP_ADDR_LEN			16
#define BB_MAX_IP			(2*BB_MAX_IMP)

#define BB_BOARD_UNSET			0
#define BB_BOARD_SET			1

#define BB_SUN_SERVER			"SUN"
#define BB_SUN_PHASE			1

/*
**  This is a bucket of the hash table used to hash the implementation
**  name to an index.
*/
typedef struct
{
    char	*id_ptr;	/* ptr to co_data.identifier */
    short	index;		/* The index assigned this implementation*/
} BB_bucket;

/*
**  The hash table structure is an array of buckets.  We are not worried
**  about the space limitations for this project.
*/
typedef BB_bucket	BB_hash[ BB_MAX_HASH];

typedef struct
{
    char	company[BB_COMPANY_NAME_LEN];
    char	imp[BB_IMP_NAME_LEN];
    BB_id	id;			/* Assigned reference name.	*/
    int		booth;
    short	flags;			/* BB_SERVER, BB_CLIENT, BB_NONE*/
    short	ip_cnt;			/* # of internet addresses	*/
    short	ip_idx;			/* index to ip table.		*/
} BB_co_data;

typedef char		BB_ip[BB_IP_ADDR_LEN];
typedef uchar		BB_phase;		/* BB_PHASE1, BB_PHASE2 */

