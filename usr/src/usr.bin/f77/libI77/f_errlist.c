/*
char id_f_errlist[] = "@(#)f_errlist.c	1.6";
 *
 * f77 I/O error messages
 */

char *f_errlist[] =
{
/* 100 */	"error in format",
/* 101 */	"illegal unit number",
/* 102 */	"formatted i/o not allowed",
/* 103 */	"unformatted i/o not allowed",
/* 104 */	"direct i/o not allowed",
/* 105 */	"sequential i/o not allowed",
/* 106 */	"can't backspace file",
/* 107 */	"off beginning of record",
/* 108 */	"can't stat file",
/* 109 */	"no * after repeat count",
/* 110 */	"off end of record",
/* 111 */	"truncation failed",
/* 112 */	"incomprehensible list input",
/* 113 */	"out of free space",
/* 114 */	"unit not connected",
/* 115 */	"invalid data for integer format term",
/* 116 */	"invalid data for logical format term",
/* 117 */	"'new' file exists",
/* 118 */	"can't find 'old' file",
/* 119 */	"opening too many files or unknown system error",
/* 120 */	"requires seek ability",
/* 121 */	"illegal argument",
/* 122 */	"negative repeat count",
/* 123 */	"illegal operation for unit",
/* 124 */	"invalid data for d,e,f, or g format term",
};

int f_nerr = (sizeof(f_errlist)/sizeof(char *));
