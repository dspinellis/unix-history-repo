/* vmhsbr.h - definitions for the vmh protocol */


#define	RC_VRSN	1

struct record {
    struct rcheader {
	char    rh_type;	/* type of record */
#define	RC_INI	0x01		/* must be greater than OK */
#define	RC_ACK	0x02
#define	RC_ERR	0x03
#define	RC_CMD	0x04
#define	RC_QRY	0x05
#define	RC_TTY	0x06
#define	RC_WIN	0x07
#define	RC_DATA	0x08
#define	RC_EOF	0x09
#define	RC_FIN	0x0a
#define	RC_XXX	0x0b

	int     rh_len;		/* length of data */
    }	    rc_header;
#define	rc_head(rc)	(&rc -> rc_header)
#define	RHSIZE(rc)	(sizeof rc -> rc_header)
#define	rc_type		rc_header.rh_type
#define	rc_len		rc_header.rh_len

    char    *rc_data;		/* extensible array */
};
#define	initrc(rc)	rc -> rc_data = NULL


int	rcinit (), rcdone (), rc2rc (), str2rc (), peer2rc (), rc2peer (),
	str2peer (), fmt2peer (), err2peer ();
