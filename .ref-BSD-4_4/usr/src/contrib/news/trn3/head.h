/* $Id: head.h,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#define HEAD_FIRST 1

/* types of header lines (if only C really believed in enums)
 * (These must stay in alphabetic order at least in the first letter.
 * Within each letter it helps to arrange in increasing likelihood.)
 */

#define PAST_HEADER	0			/* body */
#define SOME_LINE	(PAST_HEADER+1)		/* unrecognized */
#define ARTID_LINE	(SOME_LINE+1)		/* article-i.d. */
#define APPR_LINE	(ARTID_LINE+1)		/* approved */
#define ACAT_LINE	(APPR_LINE+1)		/* ACategory (ClariNet) */
#define ANPA_LINE	(ACAT_LINE+1)		/* ANPA (ClariNet) */
#define CODES_LINE	(ANPA_LINE+1)		/* Codes (ClariNet) */
#define CONTENT_LINE	(CODES_LINE+1)		/* MIME */
#define CANCEL_LINE	(CONTENT_LINE+1)	/* cancel */
#define DIST_LINE	(CANCEL_LINE+1)		/* distribution */
#define DATE_LINE	(DIST_LINE+1)		/* date */
#define RECEIVED_LINE	(DATE_LINE+1)		/* date-received */
#define EXPIR_LINE	(RECEIVED_LINE+1)	/* expires */
#define FOLLOW_LINE	(EXPIR_LINE+1)		/* followup-to */
#define FROM_LINE	(FOLLOW_LINE+1)		/* from */
#define FORM_LINE	(FROM_LINE+1)		/* Format (ClariNet) */
#define KEYW_LINE	(FORM_LINE+1)		/* keywords */
#define LINES_LINE	(KEYW_LINE+1)		/* lines */
#define MESSID_LINE	(LINES_LINE+1)		/* message-id */
#define NFFR_LINE	(MESSID_LINE+1)		/* nf-from */
#define NFID_LINE	(NFFR_LINE+1)		/* nf-id */
#define NGS_LINE	(NFID_LINE+1)		/* newsgroups */
#define NNTPHOST_LINE	(NGS_LINE+1)		/* nntp-posting-host */
#define NOTE_LINE	(NNTPHOST_LINE+1)	/* Note (ClariNet) */
#define ORG_LINE	(NOTE_LINE+1)		/* organization */
#define PATH_LINE	(ORG_LINE+1)		/* path */
#define POSTED_LINE	(PATH_LINE+1)		/* posted */
#define PVER_LINE	(POSTED_LINE+1)		/* posting-version */
#define PRI_LINE	(PVER_LINE+1)		/* Priority (ClariNet) */
#define REPLY_LINE	(PRI_LINE+1)		/* reply-to */
#define REFS_LINE	(REPLY_LINE+1)		/* references */
#define RVER_LINE	(REFS_LINE+1)		/* relay-version */
#define SENDER_LINE	(RVER_LINE+1)		/* sender */
#define SUMRY_LINE	(SENDER_LINE+1)		/* summary */
#define SUBJ_LINE	(SUMRY_LINE+1)		/* subject */
#define SUPR_LINE	(SUBJ_LINE+1)		/* supersedes */
#define SLUG_LINE	(SUPR_LINE+1)		/* Slugword (ClariNet) */
#define XREF_LINE	(SLUG_LINE+1)		/* xref */
#define XSUP_LINE	(XREF_LINE+1)		/* X-Supersedes (ClariNet) */
#define HEAD_LAST	(XSUP_LINE+1)		/* total # of headers */

struct headtype {
    char *ht_name;		/* header line identifier */
    ART_POS ht_minpos;		/* pointer to beginning of line in article */
    ART_POS ht_maxpos;		/* pointer to end of line in article */
    char ht_length;		/* could make these into nybbles but */
    char ht_flags;		/* it wouldn't save space normally */
};				/* due to alignment considerations */

#define HT_HIDE   1	/* -h on this line */
#define HT_MAGIC  2	/* do any special processing on this line */
#define HT_CACHED 4	/* this information is cached in the article data */

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
    {"acategory",	0,	0,	9,	HT_HIDE		},
    {"anpa",		0,	0,	4,	HT_HIDE		},
    {"codes",		0,	0,	5,	HT_HIDE		},
#ifdef METAMAIL
    {"content-type",	0,	0,	12,	HT_MAGIC	},
#else
    {"content-type",	0,	0,	12,	0		},
#endif
    {"control",		0,	0,	7,	0		},
    {"distribution",	0,	0,	12,	0		},
    {"date",		0,	0,	4,	HT_MAGIC	},
    {"date-received",	0,	0,	13,	0		},
    {"expires",		0,	0,	7,	HT_HIDE|HT_MAGIC},
    {"followup-to",	0,	0,	11,	0		},
    {"from",		0,	0,	4,	HT_CACHED	},
    {"format",		0,	0,	6,	HT_HIDE		},
    {"keywords",	0,	0,	8,	0		},
    {"lines",		0,	0,	5,	0		},
    {"message-id",	0,	0,	10,	HT_HIDE|HT_CACHED},
    {"nf-from",		0,	0,	7,	HT_HIDE		},
    {"nf-id",		0,	0,	5,	HT_HIDE		},
#ifdef DBM_XREFS
    {"newsgroups",	0,	0,	10,	HT_MAGIC|HT_HIDE|HT_CACHED},
#else
    {"newsgroups",	0,	0,	10,	HT_MAGIC|HT_HIDE},
#endif
    {"nntp-posting-host",0,	0,	17,	HT_HIDE		},
    {"note",		0,	0,	4,	0,		},
    {"organization",	0,	0,	12,	0		},
    {"path",		0,	0,	4,	HT_HIDE		},
    {"posted",		0,	0,	6,	HT_HIDE		},
    {"posting-version",	0,	0,	15,	HT_HIDE		},
    {"priority",	0,	0,	8,	HT_HIDE		},
    {"reply-to",	0,	0,	8,	HT_HIDE		},
    {"references",	0,	0,	10,	HT_HIDE		},
    {"relay-version",	0,	0,	13,	HT_HIDE		},
    {"sender",		0,	0,	6,	HT_HIDE		},
    {"summary",		0,	0,	7,	0		},
    {"subject",		0,	0,	7,	HT_MAGIC|HT_CACHED},
    {"supersedes",	0,	0,	10,	0		},
    {"slugword",	0,	0,	8,	HT_HIDE		},
#ifdef DBM_XREFS
    {"xref",		0,	0,	4,	HT_HIDE},
#else
    {"xref",		0,	0,	4,	HT_HIDE|HT_CACHED},
#endif
    {"x-supersedes",	0,	0,	12,	HT_HIDE		}
};
#endif

EXT ART_NUM parsed_art INIT(0);		/* the article number we've parsed */
EXT char in_header INIT(0);		/* are we decoding the header? */
EXT char *headbuf;
EXT long headbuf_size;

void	head_init _((void));
int	set_line_type _((char*,char*));
void	start_header _((ART_NUM));
void	end_header_line _((void));
void	end_header _((void));
bool    parseline _((char*,int,int));
bool	parseheader _((ART_NUM));
char	*fetchlines _((ART_NUM,int));	/* returns a malloc'ed string */
char	*prefetchlines _((ART_NUM,int,bool_int));

#ifdef DEBUG
void	dumpheader _((char*));
#endif

#ifdef USE_NNTP
#define PREFETCH_SIZE 5
#endif

#define fetchsubj(artnum,copy) prefetchlines(artnum,SUBJ_LINE,copy)
#define fetchfrom(artnum,copy) prefetchlines(artnum,FROM_LINE,copy)
#ifdef DBM_XREFS
#define fetchxref(artnum,copy) prefetchlines(artnum,NGS_LINE,copy)
#else
#define fetchxref(artnum,copy) prefetchlines(artnum,XREF_LINE,copy)
#endif
