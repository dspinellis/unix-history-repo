typedef union {
	char *str;
	box *b;
	pile_box *pb;
	matrix_box *mb;
	int n;
	column *col;
} YYSTYPE;
#define	OVER	258
#define	SMALLOVER	259
#define	SQRT	260
#define	SUB	261
#define	SUP	262
#define	LPILE	263
#define	RPILE	264
#define	CPILE	265
#define	PILE	266
#define	LEFT	267
#define	RIGHT	268
#define	TO	269
#define	FROM	270
#define	SIZE	271
#define	FONT	272
#define	ROMAN	273
#define	BOLD	274
#define	ITALIC	275
#define	FAT	276
#define	ACCENT	277
#define	BAR	278
#define	UNDER	279
#define	ABOVE	280
#define	TEXT	281
#define	QUOTED_TEXT	282
#define	FWD	283
#define	BACK	284
#define	DOWN	285
#define	UP	286
#define	MATRIX	287
#define	COL	288
#define	LCOL	289
#define	RCOL	290
#define	CCOL	291
#define	MARK	292
#define	LINEUP	293
#define	TYPE	294
#define	VCENTER	295
#define	PRIME	296
#define	SPLIT	297
#define	NOSPLIT	298
#define	UACCENT	299
#define	SPACE	300
#define	GFONT	301
#define	GSIZE	302
#define	DEFINE	303
#define	NDEFINE	304
#define	TDEFINE	305
#define	SDEFINE	306
#define	UNDEF	307
#define	IFDEF	308
#define	INCLUDE	309
#define	DELIM	310
#define	CHARTYPE	311
#define	SET	312
#define	GRFONT	313
#define	GBFONT	314


extern YYSTYPE yylval;
