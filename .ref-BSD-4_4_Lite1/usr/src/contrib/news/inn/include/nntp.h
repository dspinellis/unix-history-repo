/*  $Revision: 1.17 $
**
**  Here be a set of NNTP response codes as defined in RFC977 and elsewhere.
**  The reponse codes are three digits, RFI, defined like this:
**	R, Response:
**		1xx	Informative message
**		2xx	Command ok
**		3xx	Command ok so far, send the rest of it.
**		4xx	Command was correct, but couldn't be performed for
**			some reason.
**		5xx	Command unimplemented, or incorrect, or a serious
**			program error occurred.
**	F, Function:
**		x0x	Connection, setup, and miscellaneous messages
**		x1x	Newsgroup selection
**		x2x	Article selection
**		x3x	Distribution functions
**		x4x	Posting
**		x8x	Nonstandard extensions (AUTHINFO, XGTITLE)
**		x9x	Debugging output
**	I, Information:
**		No defined semantics
*/
#define NNTP_HELPOK_VAL			100
#define NNTP_BAD_COMMAND_VAL		500
#define NNTP_BAD_COMMAND		"500 Syntax error or bad command"
#define NNTP_TEMPERR_VAL		503
#define NNTP_ACCESS			"502 Permission denied"
#define NNTP_ACCESS_VAL			502
#define NNTP_GOODBYE_ACK		"205"
#define NNTP_GOODBYE_ACK_VAL		205
#define NNTP_GOODBYE			"400"
#define NNTP_GOODBYE_VAL		400
#define NNTP_HAVEIT			"435 Duplicate"
#define NNTP_HAVEIT_BADID		"435 Bad Message-ID"
#define NNTP_HAVEIT_VAL			435
#define NNTP_LIST_FOLLOWS		"215"
#define NNTP_LIST_FOLLOWS_VAL		215
#define NNTP_HELP_FOLLOWS		"100 Legal commands"
#define NNTP_HELP_FOLLOWS_VAL		100
#define NNTP_NOTHING_FOLLOWS_VAL	223
#define NNTP_ARTICLE_FOLLOWS		"220"
#define NNTP_ARTICLE_FOLLOWS_VAL	220
#define NNTP_NEWGROUPS_FOLLOWS_VAL	231
#define NNTP_HEAD_FOLLOWS		"221"
#define NNTP_HEAD_FOLLOWS_VAL		221
#define NNTP_BODY_FOLLOWS_VAL		222
#define NNTP_OVERVIEW_FOLLOWS_VAL	224
#define NNTP_DATE_FOLLOWS_VAL		111
#define NNTP_POSTOK			"200"
#define NNTP_POSTOK_VAL			200
#define NNTP_START_POST_VAL		340
#define NNTP_NOPOSTOK_VAL		201
#define NNTP_SLAVEOK_VAL		202
#define NNTP_REJECTIT_VAL		437
#define NNTP_REJECTIT_EMPTY		"437 Empty article"
#define NNTP_DONTHAVEIT			"430"
#define NNTP_DONTHAVEIT_VAL		430
#define NNTP_RESENDIT_NOHIST		"436 Can't write history"
#define NNTP_RESENDIT_NOSPACE		"436 No space"
#define NNTP_RESENDIT_VAL		436
#define NNTP_POSTEDOK			"240 Article posted"
#define NNTP_POSTEDOK_VAL		240
#define NNTP_POSTFAIL_VAL		441
#define NNTP_GROUPOK_VAL		211
#define NNTP_SENDIT			"335"
#define NNTP_SENDIT_VAL			335
#define NNTP_SYNTAX_USE			"501 Bad command use"
#define NNTP_SYNTAX_VAL			501
#define NNTP_TOOKIT			"235"
#define NNTP_TOOKIT_VAL			235
#define NNTP_NOTINGROUP			"412 Not in a newsgroup"
#define NNTP_NOTINGROUP_VAL		412
#define NNTP_NOSUCHGROUP		"411 No such group"
#define NNTP_NOSUCHGROUP_VAL		411
#define NNTP_NEWNEWSOK			"230 New news follows"
#define NNTP_NOARTINGRP			"423 Bad article number"
#define NNTP_NOARTINGRP_VAL		423
#define NNTP_NOCURRART			"420 No current article"
#define NNTP_NOCURRART_VAL		420
#define NNTP_NONEXT_VAL			421
#define NNTP_NOPREV_VAL			422
#define NNTP_CANTPOST			"440 Posting not allowed"
#define NNTP_CANTPOST_VAL		440


/*
**  The first character of an NNTP reply can be used as a category class.
*/
#define NNTP_CLASS_OK			'2'
#define NNTP_CLASS_ERROR		'4'
#define NNTP_CLASS_FATAL		'5'


/*
**  The NNTP protocol currently has no way to say "offer me this article
**  later, but don't close the connection."  That will be fixed in NNTP2.
#define NNTP_RESENDIT_LATER		"?"
#define NNTP_RESENDIT_LATER_VAL		?
*/


/*
**  Authentication commands from the RFC update (not official).
*/
#define NNTP_AUTH_NEEDED		"480"
#define NNTP_AUTH_NEEDED_VAL		480
#define NNTP_AUTH_BAD			"481"
#define NNTP_AUTH_NEXT			"381"
#define NNTP_AUTH_NEXT_VAL		381
#define NNTP_AUTH_OK			"281"
#define NNTP_AUTH_OK_VAL		281
#define NNTP_AUTH_REJECT_VAL		482

/*
**  XGTITLE, from ANU news.
*/
#define NNTP_XGTITLE_BAD		481	/* Yes, 481. */
#define NNTP_XGTITLE_OK			282

#define NNTP_STRLEN			512
