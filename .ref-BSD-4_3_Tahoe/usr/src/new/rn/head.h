/* $Header: head.h,v 4.3 85/05/01 11:38:31 lwall Exp $
 *
 * $Log:	head.h,v $
 * Revision 4.3  85/05/01  11:38:31  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#define HEAD_FIRST 1

/* types of header lines (if only C really believed in enums)
 * (These must stay in alphabetic order at least in the first letter.
 * Within each letter it helps to arrange in increasing likelihood.)
 */

#define PAST_HEADER	0	/* body */
#define SOME_LINE	1	/* unrecognized */
#define ARTID_LINE	2	/* article-i.d. */
#define APPR_LINE	3	/* approved */
#define DIST_LINE	4	/* distribution */
#define DATE_LINE	5	/* date */
#define RECEIVED_LINE	6	/* date-received */
#define EXPIR_LINE	7	/* expires */
#define FOLLOW_LINE	8	/* followup-to */
#define FROM_LINE	9	/* from */
#define KEYW_LINE	10	/* keywords */
#define LINES_LINE	11	/* lines */
#define MESSID_LINE	12	/* message-id */
#define NFFR_LINE	13	/* nf-from */
#define NFID_LINE	14	/* nf-id */
#define NGS_LINE	15	/* newsgroups */
#define ORG_LINE	16	/* organization */
#define PATH_LINE	17	/* path */
#define POSTED_LINE	18	/* posted */
#define PVER_LINE	19	/* posting-version */
#define REPLY_LINE	20	/* reply-to */
#define REFS_LINE	21	/* references */
#define RVER_LINE	22	/* relay-version */
#define SENDER_LINE	23	/* sender */
#define SUMRY_LINE	24	/* summary */
#define SUBJ_LINE	25	/* subject */
#define XREF_LINE	26	/* xref */

#define HEAD_LAST	27	/* one more than the last one above */

struct headtype {
    char *ht_name;		/* header line identifier */
#ifdef pdp11
    short ht_minpos;
    short ht_maxpos;
#else
    ART_POS ht_minpos;		/* pointer to beginning of line in article */
    ART_POS ht_maxpos;		/* pointer to end of line in article */
#endif
    char ht_length;		/* could make these into nybbles but */
    char ht_flags;		/* it wouldn't save space normally */
};				/* due to alignment considerations */

#define HT_HIDE 1	/* -h on this line */
#define HT_MAGIC 2	/* do any special processing on this line */

/* This array must stay in the same order as the list above */

#ifndef DOINIT
EXT struct headtype htype[HEAD_LAST];
#else
struct headtype htype[HEAD_LAST] = {
 /* name             minpos   maxpos  length   flag */
    {"BODY",		0,	0,	4,	0		},
    {"unrecognized",	0,	0,	12,	0		},
    {"article-i.d.",	0,	0,	12,	HT_HIDE		},
    {"approved",	0,	0,	8,	HT_HIDE		},
    {"distribution",	0,	0,	12,	0		},
    {"date",		0,	0,	4,	0		},
    {"date-received",	0,	0,	13,	0		},
    {"expires",		0,	0,	7,	HT_HIDE|HT_MAGIC},
    {"followup-to",	0,	0,	11,	0		},
    {"from",		0,	0,	4,	0		},
    {"keywords",	0,	0,	8,	0		},
    {"lines",		0,	0,	5,	0		},
    {"message-id",	0,	0,	10,	0		},
    {"nf-from",		0,	0,	7,	HT_HIDE		},
    {"nf-id",		0,	0,	5,	HT_HIDE		},
    {"newsgroups",	0,	0,	10,	HT_MAGIC|HT_HIDE},
    {"organization",	0,	0,	12,	0		},
    {"path",		0,	0,	4,	HT_HIDE		},
    {"posted",		0,	0,	6,	HT_HIDE		},
    {"posting-version",	0,	0,	15,	HT_HIDE		},
    {"reply-to",	0,	0,	8,	0		},
    {"references",	0,	0,	10,	0		},
    {"relay-version",	0,	0,	13,	HT_HIDE		},
    {"sender",		0,	0,	6,	0		},
    {"summary",		0,	0,	7,	0		},
    {"subject",		0,	0,	7,	HT_MAGIC	},
    {"xref",		0,	0,	4,	HT_HIDE		}
};
#endif

#ifdef ASYNC_PARSE
EXT ART_NUM parsed_art INIT(0);
#endif

EXT char in_header INIT(0);		/* are we decoding the header? */

#ifdef CACHESUBJ
    EXT char **subj_list INIT(Null(char **));
#endif

void	head_init();
int	set_line_type();
void	start_header();
bool    parseline();
#ifdef ASYNC_PARSE
    int		parse_maybe();
#endif
char	*fetchsubj();
char	*fetchlines();
