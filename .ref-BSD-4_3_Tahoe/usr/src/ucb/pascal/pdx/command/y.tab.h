
typedef union  {
	SYM *y_sym;
	NODE *y_node;
	int y_int;
	OP y_op;
	long y_long;
	double y_real;
	char *y_string;
	BOOLEAN y_bool;
} YYSTYPE;
extern YYSTYPE yylval;
# define ALIAS 257
# define ASSIGN 258
# define CALL 259
# define CHFILE 260
# define CONT 261
# define DUMP 262
# define EDIT 263
# define GRIPE 264
# define HELP 265
# define LIST 266
# define NEXT 267
# define QUIT 268
# define REMAKE 269
# define PRINT 270
# define RUN 271
# define SH 272
# define SOURCE 273
# define STATUS 274
# define STEP 275
# define STOP 276
# define STOPI 277
# define TRACE 278
# define TRACEI 279
# define DELETE 280
# define WHATIS 281
# define WHICH 282
# define WHERE 283
# define XI 284
# define XD 285
# define AT 286
# define IN 287
# define IF 288
# define FILENAME 289
# define INT 290
# define REAL 291
# define NAME 292
# define STRING 293
# define DIV 294
# define MOD 295
# define AND 296
# define OR 297
# define NOT 298
# define UNARYSIGN 299
