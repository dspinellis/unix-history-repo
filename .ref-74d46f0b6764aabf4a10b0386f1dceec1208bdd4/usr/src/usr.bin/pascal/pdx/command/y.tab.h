/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)y.tab.h	5.3 (Berkeley) %G%
 */

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
# define HELP 264
# define LIST 265
# define NEXT 266
# define QUIT 267
# define REMAKE 268
# define PRINT 269
# define RUN 270
# define SH 271
# define SOURCE 272
# define STATUS 273
# define STEP 274
# define STOP 275
# define STOPI 276
# define TRACE 277
# define TRACEI 278
# define DELETE 279
# define WHATIS 280
# define WHICH 281
# define WHERE 282
# define XI 283
# define XD 284
# define AT 285
# define IN 286
# define IF 287
# define FILENAME 288
# define INT 289
# define REAL 290
# define NAME 291
# define STRING 292
# define DIV 293
# define MOD 294
# define AND 295
# define OR 296
# define NOT 297
# define UNARYSIGN 298
# define GRIPE 299
