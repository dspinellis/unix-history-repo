/*
 * Support routines for PasswordLookup.
 */
#include "PasswordLookup_defs.h"

static int
sizeof_Passwd(p)
	register Passwd *p;
{
	register int size = 6;

	size += sizeof_String(&p->pw_name);
	size += sizeof_String(&p->pw_passwd);
	size += sizeof_String(&p->pw_comment);
	size += sizeof_String(&p->pw_gecos);
	size += sizeof_String(&p->pw_dir);
	size += sizeof_String(&p->pw_shell);
	return (size);
}

static int
externalize_Passwd(p, buf)
	register Passwd *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += externalize_String(&p->pw_name, bp);
	bp += externalize_String(&p->pw_passwd, bp);
	bp += externalize_LongCardinal(&p->pw_uid, bp);
	bp += externalize_LongCardinal(&p->pw_gid, bp);
	bp += externalize_LongCardinal(&p->pw_quota, bp);
	bp += externalize_String(&p->pw_comment, bp);
	bp += externalize_String(&p->pw_gecos, bp);
	bp += externalize_String(&p->pw_dir, bp);
	bp += externalize_String(&p->pw_shell, bp);
	return (bp - buf);
}

static int
sizeof_T_cn754_2(p)
	register T_cn754_2 *p;
{
	register int size = 0;

	size += sizeof_String(&p->errorstring);
	return (size);
}

static int
externalize_T_cn754_2(p, buf)
	register T_cn754_2 *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += externalize_String(&p->errorstring, bp);
	return (bp - buf);
}

static int
sizeof_LookupUidResults(p)
	register LookupUidResults *p;
{
	register int size = 0;

	size += sizeof_Passwd(&p->passwd);
	return (size);
}

static int
externalize_LookupUidResults(p, buf)
	register LookupUidResults *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += externalize_Passwd(&p->passwd, bp);
	return (bp - buf);
}

static int
sizeof_LookupUserResults(p)
	register LookupUserResults *p;
{
	register int size = 0;

	size += sizeof_Passwd(&p->passwd);
	return (size);
}

static int
externalize_LookupUserResults(p, buf)
	register LookupUserResults *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += externalize_Passwd(&p->passwd, bp);
	return (bp - buf);
}

static int
internalize_Passwd(p, buf)
	register Passwd *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += internalize_String(&p->pw_name, bp);
	bp += internalize_String(&p->pw_passwd, bp);
	bp += internalize_LongCardinal(&p->pw_uid, bp);
	bp += internalize_LongCardinal(&p->pw_gid, bp);
	bp += internalize_LongCardinal(&p->pw_quota, bp);
	bp += internalize_String(&p->pw_comment, bp);
	bp += internalize_String(&p->pw_gecos, bp);
	bp += internalize_String(&p->pw_dir, bp);
	bp += internalize_String(&p->pw_shell, bp);
	return (bp - buf);
}

static int
internalize_T_cn754_2(p, buf)
	register T_cn754_2 *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += internalize_String(&p->errorstring, bp);
	return (bp - buf);
}

static int
internalize_LookupUidResults(p, buf)
	register LookupUidResults *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += internalize_Passwd(&p->passwd, bp);
	return (bp - buf);
}

static int
internalize_LookupUserResults(p, buf)
	register LookupUserResults *p;
	register Unspecified *buf;
{
	register Unspecified *bp;

	bp = buf;
	bp += internalize_Passwd(&p->passwd, bp);
	return (bp - buf);
}
