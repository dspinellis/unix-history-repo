typedef union {
  WORD_DESC *word;		/* the word that we read. */
  int number;			/* the number that we read. */
  WORD_LIST *word_list;
  COMMAND *command;
  REDIRECT *redirect;
  ELEMENT element;
  PATTERN_LIST *pattern;
} YYSTYPE;
#define	IF	258
#define	THEN	259
#define	ELSE	260
#define	ELIF	261
#define	FI	262
#define	CASE	263
#define	ESAC	264
#define	FOR	265
#define	WHILE	266
#define	UNTIL	267
#define	DO	268
#define	DONE	269
#define	FUNCTION	270
#define	IN	271
#define	BANG	272
#define	WORD	273
#define	NUMBER	274
#define	AND_AND	275
#define	OR_OR	276
#define	GREATER_GREATER	277
#define	LESS_LESS	278
#define	LESS_AND	279
#define	GREATER_AND	280
#define	SEMI_SEMI	281
#define	LESS_LESS_MINUS	282
#define	AND_GREATER	283
#define	LESS_GREATER	284
#define	GREATER_BAR	285
#define	yacc_EOF	286


extern YYSTYPE yylval;
