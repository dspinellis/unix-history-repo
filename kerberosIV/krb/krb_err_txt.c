/*
 * $Source: /usr/src/kerberosIV/krb/RCS/krb_err_txt.c,v $
 * $Author: karels $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef	lint
static char rcsid_krb_err_txt_c[] =
"$Header: /usr/src/kerberosIV/krb/RCS/krb_err_txt.c,v 4.9 91/06/01 13:53:20 karels Exp $";
#endif	lint

#include <mit-copyright.h>

/*
 * This file contains an array of error text strings.
 * The associated error codes (which are defined in "krb.h")
 * follow the string in the comments at the end of each line.
 */
static char _kreserved[] = "(reserved)";

char *krb_err_txt[256] = {
  "No Error",							/* 000 */
  "Principal expired",						/* 001 */
  "Service expired",				/* 002 */
  "Authentication expired",			/* 003 */
  "Unknown protocol version number", 	/* 004 */
  "Principal: Incorrect master key version", /* 005 */
  "Service: Incorrect master key version",   /* 006 */
  "Bad byte order",				/* 007 */
  "Principal unknown",			/* 008 */
  "Principal not unique",			/* 009 */
  "Principal has null key",			/* 010 */
  "Reserved error message 11",		/* 011 */
  "Reserved error message 12",		/* 012 */
  "Reserved error message 13",		/* 013 */
  "Reserved error message 14",		/* 014 */
  "Reserved error message 15",		/* 015 */
  "Reserved error message 16",		/* 016 */
  "Reserved error message 17",		/* 017 */
  "Reserved error message 18",		/* 018 */
  "Reserved error message 19",		/* 019 */
  "Permission Denied",			/* 020 */
  "Can't read ticket file",		/* 021 */
  "Can't find ticket",			/* 022 */
  "Reserved error message 23",		/* 023 */
  "Reserved error message 24",		/* 024 */
  "Reserved error message 25",		/* 025 */
  "Ticket granting ticket expired",	/* 026 */
  "Reserved error message 27",		/* 027 */
  "Reserved error message 28",		/* 028 */
  "Reserved error message 29",		/* 029 */
  "Reserved error message 30",		/* 030 */
  "Can't decode authenticator",		/* 031 */
  "Ticket expired",			/* 032 */
  "Ticket issue date too far in the future",/* 033 */
  "Repeat request",			/* 034 */
  "Ticket for wrong server",		/* 035 */
  "Request inconsistent",			/* 036 */
  "Time is out of bounds",			/* 037 */
  "Incorrect network address",		/* 038 */
  "Protocol version mismatch",		/* 039 */
  "Illegal message type",			/* 040 */
  "Message integrity error",		/* 041 */
  "Message duplicate or out of order",	/* 042 */
  "Unauthorized request",			/* 043 */
  "Reserved error message 44",		/* 044 */
  "Reserved error message 45",		/* 045 */
  "Reserved error message 46",		/* 046 */
  "Reserved error message 47",		/* 047 */
  "Reserved error message 48",		/* 048 */
  "Reserved error message 49",		/* 049 */
  "Reserved error message 50",		/* 050 */
  "Current password is NULL",		/* 051 */
  "Current password incorrect",		/* 052 */
  "Protocol error",				/* 053 */
  "Error returned by KDC",			/* 054 */
  "Null ticket returned by KDC",		/* 055 */
  "Retry count exceeded",			/* 056 */
  "Can't send request",			/* 057 */
  "Reserved error message 58",		/* 058 */
  "Reserved error message 59",		/* 059 */
  "Reserved error message 60",		/* 060 */
  "Warning: Not ALL tickets returned",			/* 061 */
  "Password incorrect",					/* 062 */
  "Protocol error",				/* 063 */
  "Reserved error message 64",		/* 064 */
  "Reserved error message 65",		/* 065 */
  "Reserved error message 66",		/* 066 */
  "Reserved error message 67",		/* 067 */
  "Reserved error message 68",		/* 068 */
  "Reserved error message 69",		/* 069 */
  "Unknown initial-ticket error",		/* 070 */
  "Don't have ticket granting ticket",	/* 071 */
  "Reserved error message 72",		/* 072 */
  "Reserved error message 73",		/* 073 */
  "Reserved error message 74",		/* 074 */
  "Reserved error message 75",		/* 075 */
  "No ticket file",				/* 076 */
  "Can't access ticket file",			/* 077 */
  "Can't lock ticket file; try later",	/* 078 */
  "Bad ticket file format",			/* 079 */
  "Read ticket file before tf_init",		/* 080 */
  "Bad Kerberos name format",		/* 081 */
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  _kreserved,
  "Generic kerberos error (KFAILURE)",			/* 255 */
};
