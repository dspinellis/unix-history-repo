/*
 * $XConsortium: XlcLoad.c,v 1.44 92/12/14 09:22:48 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation, NTT Software Corporation,
 *                      and Nippon Telegraph and Telephone Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON, NTT Software, NTT, and M.I.T.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission. OMRON, NTT Software,
 * NTT, and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OMRON, NTT SOFTWARE, NTT, AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL OMRON, NTT SOFTWARE, NTT, OR M.I.T. BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *	Authors: Li Yuhong		OMRON Corporation
 *		 Tatsuya Kato		NTT Software Corporation
 *		 Hiroshi Kuribayashi	OMRON Corporation
 *   
 */

#include <stdio.h>
#include <ctype.h>
#include "Xlibint.h"
#include "Xi18nint.h"
#include <X11/Xos.h>

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif
#endif

#ifndef XNLSPATHDEFAULT
#define XNLSPATHDEFAULT "/usr/lib/X11/nls"
#endif

#define END_LINE    "END"

#define MAXLINEBUF  1024

#ifndef lint
static int lock_tbl;
#endif

static XLocaleDB *_Xlctbl_;

static char    *LoadLocaleName();
static _CSID    WhichCS();
static Codeset *LoadCodeset();
static Bool     LoadCSMappingTable();
static Bool     LoadCVMappingTable();
static char    *LoadDesignateSequence();
static Fontset *LoadFontList();
static void	GetCTidToCSid();

#define	COMMENTMARK	'#'

static void
rmblank(buf)
    char           *buf;
{
    char           *ptr;
    int             len;

    ptr = buf;
    while (*ptr != '\0' && isspace(*ptr))
	ptr++;
    len = strlen(ptr);
    bcopy(ptr, buf, len + 1);
    ptr = buf + len;
    while ((ptr > buf) && isspace(*(ptr - 1))) {
	*--ptr = '\0';
    }
}

#ifdef _FSTDIO
	/* do not conflict with fgetline in bsd stdio */
#define fgetline fgetlineX
#endif

static char *
fgetline(buf, len, fp)
    char       *buf;
    int		len;
    FILE       *fp;
{
    while (fgets(buf, len, fp)) {
	rmblank(buf);
	if ((*buf != '\0') && (*buf != COMMENTMARK)) {
	    return buf;
	}
    }
    return NULL;
}

static char *
fgettoken(buf, len, fp, sep, strch, endch)
    char       *buf;
    int		len;
    FILE       *fp;
    char       *sep, strch, endch;
{
    static char	buffer[256];
    static char	*ptr = NULL;
    static int	start_flag = 0, end_flag = 0;
    char	*last, *p;

    while (1) {
	if (ptr == NULL) {
            if (end_flag != 0) {
                start_flag = end_flag = 0;
                return NULL;
            }
	    if (fgetline(buffer, len, fp) == NULL) {
		return NULL;
	    }
            if (!strncmp(buffer, "END", 3) || !strncmp(buffer, "XLC", 3))
                return NULL;
	    ptr = buffer;
	}
	if (start_flag == 0) {
	    if ((ptr = index(ptr, strch)) == NULL) {
		continue;
	    }
	    ptr++;
	    start_flag = strch;
	}

	if (p = index(ptr, endch)) {
	    end_flag = endch;
	    *p = '\0';
	}
	for (; *ptr != '\0'; ptr++) {
	    if (!index(sep, *ptr)) {
		break;
	    }
	}
	
	if (*ptr == '\0') {
	    ptr = NULL;
	    if (end_flag != 0) {
		start_flag = 0;
		end_flag = 0;
		return NULL;
	    }
	    continue;
	}

	for (last = ptr; *ptr != '\0'; ptr++) {
	    if (index(sep, *ptr)) {
		break;
	    }
	}

	if (*ptr == '\0') {
	    ptr = NULL;
	}
	else {
	    *ptr++ = '\0';
	}

	strcpy(buf, last);
	rmblank(buf);
	if (*buf != '\0') {
	    return buf;
	}
    }
}

static char *
escape(str)
    char       *str;
{
    char	buf[128],
                *ptr,
                *ret;

    ptr = buf;
    while (*str) {
	switch (*str) {
	case '\\':
	    str++;
	    if ((*str >= '0') && (*str <= '7')) {
		int i, sum;

		sum = 0;
		for (i = 0; (i < 3) && *str; i++, str++) {
		    if ((*str < '0') || (*str > '7')) {
			break;
		    }
		    sum = sum * 8 + (*str - '0');
		}
		*ptr++ = (sum & 0xff);
		continue;
	    } else if (*str == 'e' || *str == 'E') {
		*ptr++ = 0x1b;
		continue;
	    }
	    /* through into default: */
	default:
	    *ptr++ = *str++;
	    break;
	}
    }
    *ptr = '\0';
    if (ret = (char *) Xmalloc((unsigned)strlen(buf) + 1))
	strcpy(ret, buf);
    return ret;
}


static XLocaleDB *
_XlcLoadLocale(file_name, errp)
    char           *file_name;
    int		   *errp;
{
    FILE           *fp;
    char            buf[MAXLINEBUF];
    XLocaleDB      *xlocale; 

    _XInitCTEncoding();          /* should done in system initial time. */
    *errp = BadAccess;
    if ((fp = fopen(file_name, "r")) == NULL)
        return ((XLocaleDB *)NULL);
    *errp = BadAlloc;
    xlocale = (XLocaleDB *) Xmalloc(sizeof(XLocaleDB));
    if (!xlocale) {
	fclose(fp);
	return ((XLocaleDB *)NULL);
    }

    while (fgetline(buf, MAXLINEBUF, fp) != NULL) {
        if (!strncmp(buf, "XLC_ALL", 7)) {
            if ((xlocale->lc_name = LoadLocaleName(fp)) == NULL)
                return (NULL);
        } else if (!strncmp(buf, "XLC_ENCODING", 12)) {
            if ((xlocale->lc_encoding = LoadLocaleName(fp)) == NULL)
                return (NULL);
        } else if (!strncmp(buf, "XLC_CODESET", 11)) {
            if ((xlocale->lc_codeset = LoadCodeset(fp)) == NULL)
                return (NULL);
        } else if (!strncmp(buf, "XLC_FONTSET", 11)) {
            if ((xlocale->lc_fontset = LoadFontList(fp)) == NULL)
                return (NULL);
        } else {
	    *errp = BadValue;
            return (NULL);
        }
    }
    GetCTidToCSid(xlocale->lc_fontset);
    /*
     * set locale name for each category.
     */
    fclose(fp);
    *errp = 0;
    return (xlocale);
}

static char *
LoadLocaleName(fp)
    FILE           *fp;
{
    char            buf[MAXLINEBUF];
    char           *s;

    if (fgetline(buf, MAXLINEBUF, fp) == NULL)
        return (NULL);
    if (s = Xmalloc((unsigned)strlen(buf) + 1)) {
	(void) strcpy(s, buf);
	if ((fgetline(buf, MAXLINEBUF, fp)) == NULL)
	    return (NULL);
    }
    return (s);
}

static Codeset *
LoadCodeset(fp)
    FILE           *fp;
{
    char            buf[MAXLINEBUF],
                   *ptr, *nptr;
    int             num;
    Codeset        *codeset;
    int		    mb_cur_max = 0;
    _CSID	    cds_GL, cds_GR;
	
#define MAGIC_NO 0xff

    if (fgetline(buf, MAXLINEBUF, fp) == NULL) {
        return (NULL);
    }
    codeset = (Codeset *) Xmalloc(sizeof(Codeset));
    if (!codeset)
	return ((Codeset *)NULL);
    ptr = buf;
    if (nptr = index(ptr, ':')) *nptr = '\0';

    if ((*ptr == 'l') || (*ptr == 'L')) {
	codeset->cds_type = CDS_STATELESS;
	cds_GL = cds_GR = CODESET0;
        LoadCSMappingTable(fp, codeset);
    } else if ((*ptr == 'f') || (*ptr == 'F')) {
	codeset->cds_type = CDS_STATEFUL;
	cds_GL = cds_GR = MAGIC_NO;	/* not indicate */
    } else {
	codeset->cds_type = CDS_SELFDEFINED;
	cds_GL = cds_GR = MAGIC_NO;	/* not indicate */
    }
    codeset->cds_mb_cur_max = 1;
    for (num = 0; ptr = nptr; num++) {
	ptr++;
	if (nptr = index(ptr, ':')) *nptr = '\0';
	mb_cur_max =
        codeset->cds_mblen[num] = atoi(ptr);
        if (codeset->cds_type == CDS_STATEFUL) {
            codeset->cds_dsg[num] = LoadDesignateSequence(fp);
	    mb_cur_max += strlen(codeset->cds_dsg[num]);
	    /* set inital designate */
	    if (_XctisGLdsg(codeset->cds_dsg[num])) {
		if (cds_GL == MAGIC_NO)
		    cds_GL = num;
	    } else if (cds_GR == MAGIC_NO) {
		cds_GR = num;
	    }
        }
	codeset->cds_mb_cur_max = max(codeset->cds_mb_cur_max, mb_cur_max);
    }
    if (cds_GR == MAGIC_NO)
	cds_GR = cds_GL;
    codeset->mb_init = (GL << 16) | cds_GR << 8 | cds_GL;
    codeset->cds_num = num;
    LoadCVMappingTable(fp, num, codeset);

    while (fgetline(buf, MAXLINEBUF, fp)) { /* read trailing garbage */
    }
    return codeset;
#undef MAGIC_NO
}

static          Bool
LoadCSMappingTable(fp, codeset)
    FILE           *fp;
    Codeset        *codeset;
{
    char            buf[MAXLINEBUF];
    int             count = 0;

    while (fgettoken(buf, MAXLINEBUF, fp, ", ", '{', '}')) {
        codeset->cds_map[count++] = WhichCS(buf);
    }
    if (count < 256) {
        return False;
    }
    return True;
}

#define MAXSPLITS   8096    /* should be max. 96 x 96 */

static          Bool
LoadCVMappingTable(fp, num_cs, ccs)
    FILE           *fp;
    int            num_cs;
    Codeset        *ccs;
{
    char            buf[MAXLINEBUF];
    Range          *cnv;
    int             num = 0,
                    i, lastnum;

    if ( ! ( cnv = (Range *)Xmalloc(MAXSPLITS*sizeof(Range))))
	return False;

    ccs->cds_msbon = 0;     /* use msb on */
    for (i = 1, ccs->cds_cnvindex[0] = 0; i <= num_cs; i++) {
        lastnum = num;
        while (fgettoken(buf, MAXLINEBUF, fp, ", ", '(', ')')) {
            sscanf(buf, "%x=%x:%x", &(cnv[num].mb_start),
                    &(cnv[num].cs_start), &(cnv[num].cs_end));
            cnv[num].mb_end = cnv[num].mb_start +
                              cnv[num].cs_end - cnv[num].cs_start;
	    if (cnv[num].mb_start & 0x80808080 || cnv[num].mb_end & 0x80808080)
		ccs->cds_msbon = 1;     /* use msb on */
            num++;
        }
        if (lastnum == num) /* no more conversion element */
            break;
        ccs->cds_cnvindex[i] = num;
    }
    /*
     * Notes: the number of charsets >= number of fonts
     *        No font, no conversion list, e.g, charset #3 of UJIS.
     *
     * The last one [num_cs] of index is guider.
     */
    for (; i <= num_cs; i++)
        ccs->cds_cnvindex[i] = num;

    /* set which half (GL/GR) of font should be used. */
    for (i = 0; i < num_cs; i++) {
	ccs->cs_offset[i] = cnv[ccs->cds_cnvindex[i]].cs_start & 0x8080;
    }

    ccs->cds_cnvlist = (Range *) Xmalloc((unsigned)sizeof(Range) * num);
    if (!ccs->cds_cnvlist) {
	Xfree((char *)cnv);
	return False;
    }
#ifdef  FASTCOPY
    /* not used */
    (void) bcopy((char *)cnv, (char *)ccs->cds_cnvlist,
                 sizeof(Range) * num);
#else
    for (i = 0; i < num; i++) {
        ccs->cds_cnvlist[i].mb_start = cnv[i].mb_start;
        ccs->cds_cnvlist[i].mb_end = cnv[i].mb_end;
        ccs->cds_cnvlist[i].cs_start = cnv[i].cs_start;
        ccs->cds_cnvlist[i].cs_end = cnv[i].cs_end;
    }
#endif
    Xfree((char *)cnv);
    return True;

}

static char *
LoadDesignateSequence(fp)
    FILE       *fp;
{
    char      	buf[MAXLINEBUF];

    if (fgetline(buf, MAXLINEBUF, fp) == NULL) {
        return False;
    }
    return escape(buf);
}

static _CSID
WhichCS(s)
    char       *s;
{
    _CSID      	n;

    if (!strncmp(s, "CS0", 3)) return CODESET0;
    if (!strncmp(s, "CS1", 3)) return CODESET1;
    if (!strncmp(s, "CS2", 3)) return CODESET2;
    if (!strncmp(s, "CS3", 3)) return CODESET3;
    if (!strncmp(s, "CS4", 3)) return CODESET4;
    if (!strncmp(s, "CS5", 3)) return CODESET5;
    if (!strncmp(s, "CS6", 3)) return CODESET6;
    if (!strncmp(s, "CS7", 3)) return CODESET7;
    if (!strncmp(s, "CS8", 3)) return CODESET8;
    if (!strncmp(s, "CS9", 3)) return CODESET9;
    if (!strncmp(s, "CSA", 3)) return CODESETA;
    if (!strncmp(s, "CSB", 3)) return CODESETB;
    if (!strncmp(s, "CSC", 3)) return CODESETC;
    if (!strncmp(s, "CSD", 3)) return CODESETD;
    if (!strncmp(s, "CSE", 3)) return CODESETE;
    if (!strncmp(s, "CSF", 3)) return CODESETF;
    if (!strncmp(s, "C0", 2)) return C0;
    if (!strncmp(s, "C1", 2)) return C1;
    if (!strncmp(s, "ND", 2)) return ND;
    if (!strncmp(s, "-", 1)) {
        s++;
        n = '0' - *s;
        return n;
    }
    return ND;
}

static Bool
LoadOneCharset(str, cset)
    char       *str;
    Charset    *cset;
{
    wchar	woffset;
    char       *esc;
    char       *nstr;
    extern Bool _XRegisterCharSet();

    if (nstr = index(str, ':')) *nstr = '\0';
    cset->cs_name = (char *) Xmalloc((unsigned)strlen(str) + 1);
    if (!cset->cs_name)
	return (False);
    strcpy(cset->cs_name, str);
    if (!(str = nstr))
        return False;
    str++;
    if (nstr = index(str, ':')) *nstr = '\0';
    cset->cs_GLorGR = (strcmp(str, "GL") == 0) ? GL : GR;
    if (!nstr) {
        /*
         * standard CharsetRegistry-CharsetEncoding registered by X,
         */
        if (_XcwNameGetAll(cset->cs_name, &esc, &woffset, cset->cs_GLorGR) == False)
            return False;

        cset->cs_des = strcpy(Xmalloc((unsigned)strlen(esc) + 1), esc);
	if (!cset->cs_des)
	    return (False);
        cset->cs_woff = woffset;
        /*
         * only 2 or 1 bytes font encoding in X.
         */
        cset->cs_len = (cset->cs_des[1] == '$') ? 2 : 1;
        return True;
    }
    /*
     * The form: CharsetRegistry-CharsetEncoding:woffset:escapeseq
     */
    if (!(str = nstr))
        return False;
    str++;
    if (nstr = index(str, ':')) *nstr = '\0';
    sscanf(str, "%x", &woffset);
    if (!(str = nstr))
        return False;
    str++;
    if (nstr = index(str, ':')) *nstr = '\0';
    cset->cs_des = escape(str);
    cset->cs_woff = woffset;
    if (cset->cs_des[1] == 0x25)
	cset->cs_len = cset->cs_des[3] - '0';
    else
	cset->cs_len = (cset->cs_des[1] == '$') ? 2 : 1;
    return _XRegisterCharSet(cset->cs_name, cset->cs_des, cset->cs_woff,
			     cset->cs_GLorGR, cset->cs_len);
}

static Fontset *
LoadFontList(fp)
    FILE       *fp;
{
    Fontset    *flist;
    Charset    *csets[MAXCHARSETS];
    char        buf[MAXLINEBUF];
    char       *s;
    int         cnt, i;

    cnt = 0;
    while ((s = fgetline(buf, MAXLINEBUF, fp)) != NULL) {
        if (!strncmp(s, END_LINE, 3))
            break;
        csets[cnt] = (Charset *) Xmalloc(sizeof(Charset));
	if (!csets[cnt]) {
	    for (cnt--; cnt >= 0; cnt--)
		Xfree((char *)csets[cnt]);
	    return ((Fontset *)NULL);
	}
        if (LoadOneCharset(buf, csets[cnt]) == True)
            cnt++;
        else {
            /* should return error. */  
	    for (cnt--; cnt >= 0; cnt--)
		Xfree((char *)csets[cnt]);
	    return ((Fontset *)NULL);
	}
    }               /* end of while */
    flist = (Fontset *) Xmalloc(sizeof(Fontset));
    if (!flist) {
	for (cnt--; cnt >= 0; cnt--)
	    Xfree((char *)csets[cnt]);
	return ((Fontset *)NULL);
    }
    flist->fs_num = cnt;
    flist->fs_cset = (Charset **) Xmalloc((unsigned)cnt * sizeof(Charset *));
    if (!flist->fs_cset) {
	Xfree((char *)flist);
	for (cnt--; cnt >= 0; cnt--)
	    Xfree((char *)csets[cnt]);
	return ((Fontset *)NULL);
    }
    for (i = 0; i < cnt; i++) {
      /* To avoid the bug of gcc 1.37 optimization,
       * temporary variable `temp' was introduced
       * by demizu on Feb 19, 1991 */
      register Charset *temp;
	temp = csets[i];
	flist->fs_cset[i] = temp;

    }
    return (flist);
}

static void
GetCTidToCSid(fontset)
    Fontset	*fontset;
{
    int i;
    for (i = 0; i < fontset->fs_num; i++) {
	fontset->fs_cset[i]->cs_id =
		_XcwNameGetGLorGRId(fontset->fs_cset[i]->cs_name,
				    fontset->fs_cset[i]->cs_GLorGR);
    }
}

/* Read/Load locale file */
struct _NLSAlias {
    char *lc_alias;
    char *lc_name;
};

static struct _NLSAlias *NLSAlias = NULL;

struct _NLSDir {
    char *dir;
    char *lc_name;
    char *fn;
};

static struct _NLSDir *NLSDir = NULL;

#ifndef lint
static int lock;
#endif

#define XREALLOC(p, size) ((p) ? Xrealloc(p, size) : Xmalloc(size))
static void
ReadNLSAlias(dir)
    char       	       *dir;
{
    char 		fname[PATH_MAX];
    char 		alias[PATH_MAX];
    char 		lang_name[PATH_MAX];
    char 		str[PATH_MAX];
    int 		count, i;
    struct _NLSAlias   *p, *ptr = NULL;
    static 		size = 0;
    FILE       	       *fp;


    sprintf(fname, "%s/nls.alias", dir);
    if ((fp = fopen(fname, "r")) == NULL)
	return;

    if (fgetline(str, PATH_MAX, fp) == NULL) {
	fclose(fp);
	return;
    }
    count = sscanf(str, "%d", &i);
    if (i < 1) {
	fclose(fp);
	return;
    }

    NLSAlias = (struct _NLSAlias *) XREALLOC((char *)NLSAlias,
			(unsigned)sizeof(struct _NLSAlias) * (size + i + 1));

    if (!NLSAlias) {
	fclose(fp);
	return;
    }

    ptr = NLSAlias + size;
    for (;;) {
	if (fgetline(str, PATH_MAX, fp) == NULL)
	    break;
	count = sscanf(str, "%s %s", alias, lang_name);
	if (count != 2)
	    break;
	for (p = NLSAlias; p != ptr; p++)
	    if (strcmp(alias, p->lc_alias) == 0)
		break;
	if (p != ptr)
	    continue;
	ptr->lc_alias = Xmalloc((unsigned)strlen(alias)+1);
	if (!ptr->lc_alias)
	    break;
	strcpy(ptr->lc_alias, alias);
	ptr->lc_name = Xmalloc((unsigned)strlen(lang_name)+1);
	if (!ptr->lc_name)
	    break;
	strcpy(ptr->lc_name, lang_name);
	ptr++;
	size++;
    }
    ptr->lc_alias = NULL;
    fclose(fp);
}

static void
ReadNLSDir(dir)
    char       	       *dir;
{
    char 		fname[PATH_MAX];
    char 		file_name[PATH_MAX];
    char 		lang_name[PATH_MAX];
    char 		str[PATH_MAX];
    char       	       *tmp;
    int 		count, i;
    struct _NLSDir     *p, *ptr = NULL;
    static 		size = 0;
    FILE	       *fp;

    sprintf(fname, "%s/nls.dir", dir);
    if ((fp = fopen(fname, "r")) == NULL)
	return;

    if (fgetline(str, PATH_MAX, fp) == NULL) {
	fclose(fp);
	return;
    }
    /* read number of locales */
    count = sscanf(str, "%d", &i);
    if (i < 1) {
	fclose(fp);
	return;
    }

    NLSDir = (struct _NLSDir *) XREALLOC((char *)NLSDir,
			(unsigned)sizeof(struct _NLSDir) * (size + i + 1));

    if (!NLSDir) {
	fclose(fp);
	return;
    }

    ptr = NLSDir + size;
    tmp = ptr->dir = Xmalloc((unsigned)strlen(dir) + 1);
    if (!ptr->dir) {
	fclose(fp);
	return;
    }
    strcpy(ptr->dir, dir);
    for (;;) {
	if (fgetline(str, PATH_MAX, fp) == NULL)
	    break;
	count = sscanf(str, "%s %s", file_name, lang_name);
	if(count == 1)
	    strcpy(lang_name, file_name);
	else if (count < 1)
	    break;
	for (p = NLSDir; p != ptr; p++)
	    if (strcmp(lang_name, p->lc_name) == 0)
		break;
	if (p != ptr)
	    continue;
	ptr->fn = Xmalloc((unsigned)strlen(file_name)+1);
	if (!ptr->fn)
	    break;
	strcpy(ptr->fn, file_name);
	ptr->lc_name = Xmalloc((unsigned)strlen(lang_name)+1);
	if (!ptr->lc_name)
	    break;
	strcpy(ptr->lc_name, lang_name);
	ptr->dir = tmp;
	ptr++;
	size++;
    }
    ptr->dir = NULL;
    fclose(fp);
    fp = NULL;
}


static void
ReadNLS()
{
    char 		nlspath[PATH_MAX];
    char 	       *path;
    char	       *dir;
    char 	       *env;

    LockMutex(&lock);
    if (NLSDir != NULL) {
	UnlockMutex(&lock);
	return;
    }
    if ((env = getenv("XNLSPATH")) == NULL) {
	env = XNLSPATHDEFAULT;
    }
    path = nlspath;
    strcpy(path, env);

    while (1) {
	if (path == NULL) {
	    UnlockMutex(&lock);
	    return;
	}
	dir = path;
	if ((path = index(dir, ':')) != NULL) {
	    *path++ = '\0';
	}

	ReadNLSDir(dir);
	ReadNLSAlias(dir);
    }
    UnlockMutex(&lock);
}

static void
_XlcFindNLSDir(lc_name, fname)
    char       	       *lc_name;
    char       	       *fname;
{
    struct _NLSDir     *ptr;

    ReadNLS();
    ptr = NLSDir;
    if (!ptr) {
	*fname = 0;
	return;
    }
    while (1) {
	if (ptr->dir == NULL) {
	    *fname = 0;
	    break;
	}
	if (strcmp(lc_name, ptr->lc_name) == 0) {
	   sprintf(fname, "%s/%s", ptr->dir, ptr->fn);
	   break;
	}
	ptr++;
    }
}

static char *lastBadLCName;

static XLocaleDB *
_XlcGetLocaleDB(lc_name)
    char	*lc_name;	/* locale name */
{
    XLocaleDB	  *xlc_db;
    char	  fn[PATH_MAX];
    int		  err;

    LockMutex(&lock_tbl);
    for (xlc_db = _Xlctbl_; xlc_db; xlc_db = xlc_db->next) {
	if (!strcmp(lc_name, xlc_db->lc_name)) {
	    UnlockMutex(&lock_tbl);
	    return xlc_db;
	}
    }
    _XlcFindNLSDir(lc_name, fn);
    if (*fn) {
	xlc_db = _XlcLoadLocale(fn, &err);
	if (xlc_db) {
	    xlc_db->next = _Xlctbl_;
	    _Xlctbl_ = xlc_db;
	} else if ((err != BadAccess || strcmp(lc_name, "C")) &&
		   (!lastBadLCName || strcmp(lc_name, lastBadLCName))) {
	    if (err == BadAccess)
		fprintf(stderr, "Xlib: missing locale file: %s\n", fn);
	    else if (err == BadAlloc)
		fprintf(stderr, "Xlib: failed to load locale file: %s\n", fn);
	    else
		fprintf(stderr, "Xlib: bad format locale file: %s\n", fn);
	    if (lastBadLCName)
		Xfree(lastBadLCName);
	    if (lastBadLCName = Xmalloc(strlen(lc_name) + 1))
		strcpy(lastBadLCName, lc_name);
	}
    }
    UnlockMutex(&lock_tbl);
    return (xlc_db);
}

static char *
_XlcResolveName(lc_name)
    char	        *lc_name;
{
    struct _NLSAlias    *ptr;

    ReadNLS();
    if (NLSAlias)
	for (ptr = NLSAlias; ptr->lc_alias != NULL ; ptr++) {
	    if (!strcmp(lc_name, ptr->lc_alias)) {
		return (ptr->lc_name);
	    }
	}
    return(lc_name);
}

/* list up supported locale name */
/* need to call Xfree(list) by caller */
int
_XlcListLocale(list)
    char 	      **list[];
{
    struct _NLSDir     *ptr;
    char 	      **p;
    int			i;

    ReadNLS();
    for (i = 0, ptr = NLSDir; ptr->dir != NULL ; ptr++, i++) ;
    p = *list = (char **) Xmalloc((unsigned)sizeof(char *) * (i + 1));
    if (!p)
	return (0);
    for (ptr = NLSDir; ptr->dir != NULL ; ptr++)
	*p++ = ptr->lc_name;
    *p = NULL;
    return(i);
}

XLocale
_XlcMakeLocale(lc_name)
    char       *lc_name;	/* locale name */
{
    char       *p;
    char       *lc_alias;
    char	lang[256];
    XLocaleDB  *xlc_db;
    XLocale	xlc;

    /* extract locale name */
    lc_alias = _XlcResolveName(lc_name);

    p = index(lc_alias, '@');
    if (p) {
	strncpy(lang, lc_alias, p - lc_alias);
	lang[p - lc_alias] = '\0';
	lc_alias = lang;
    }

    xlc_db = _XlcGetLocaleDB(lc_alias);
    if (!xlc_db)
	return NULL;
    xlc = (XLocale)Xmalloc(sizeof(XLocaleRec));
    if (!xlc)
	return NULL;

    xlc->xlc_db = xlc_db;
    xlc->lc_lang = xlc_db->lc_name;
    _Xctinit(xlc);
    _Xmbinit(xlc);

    return xlc;
}

XLocale
_XlcDupLocale(xlocale)
    XLocale	xlocale;
{
    XLocale	new;

    if ((new = (XLocale) Xmalloc(sizeof(XLocaleRec))) == NULL) {
	return ((XLocale)NULL);
    }
    new->xlc_db = xlocale->xlc_db;
    new->lc_lang = xlocale->lc_lang;
    _Xctinit(new);
    _Xmbinit(new);

    return new;
}

XLocale
_XFallBackConvert()
{
    static XLocale xlc_xlc;
    char *osname = setlocale(LC_CTYPE, (char *)NULL);
    char *name;
    XLocale xlc;
#if !defined(X_NOT_STDC_ENV) && !defined(X_LOCALE)
    char siname[256];
    char *_XlcMapOSLocaleName();
#endif

    if (xlc_xlc && !strcmp(xlc_xlc->lc_lang, osname))
	return xlc_xlc;
#if !defined(X_NOT_STDC_ENV) && !defined(X_LOCALE)
    name = _XlcMapOSLocaleName(osname, siname);
#else
    name = osname;
#endif
    xlc = _XlcMakeLocale(name);
    if (!xlc)
	return NULL;
    name = Xmalloc(strlen(osname) + 1);
    if (!name) {
	_XlcFreeLocale(xlc);
	return NULL;
    }
    strcpy(name, osname);
    xlc->lc_lang = name;
    if (xlc_xlc)
	_XlcFreeLocale(xlc_xlc);
    xlc_xlc = xlc;
    return xlc;
}

static void
_Xsimbinit(state)
    XPointer state;
{
    XLocale xlc = (XLocale)state;

    _Xmbinit(xlc);
}

/*ARGSUSED*/
static void
_Xsimbfinish(state)
    XPointer state;
{
}

static char
_Xsimbchar(state, str, lenp)
    XPointer state;
    char *str;
    int *lenp;
{
    _CSID csid;
    char ret = 'a';
    int cscode;
    int dlen;

    csid = _Xmbcsid((XLocale)state, str);
    dlen = _Xmbdlen((XLocale)state, str);
    if (dlen > 0) {
	for (*lenp = 0; ; ) {
	    str += dlen;
	    *lenp += dlen;
	    dlen = _Xmbdlen((XLocale)state, str);
	    if (dlen == 0)  return (ret);
	}
    }
    csid = _Xmbcsid((XLocale)state, str);
    if (csid == C0) {
	ret = *str;
    } else if (csid == CODESET0) {
	_Xmbctocsc((XLocale)state, str, &cscode);
	if (cscode < 0x7f)
	    ret = (char)cscode;
    }
    *lenp = _Xmblen((XLocale)state);
    return (ret);
}

static char *
_Xsilcname(state)
    XPointer state;
{
    return ((XLocale)state)->lc_lang;
}

static void _Xsidestroy(state)
    XPointer state;
{
    _XlcFreeLocale((XLocale)state);
}

static RConst XrmMethodsRec mb_methods = {
    _Xsimbinit,
    _Xsimbchar,
    _Xsimbfinish,
    _Xsilcname,
    _Xsidestroy
};

XrmMethods
_XrmInitParseInfo(state)
    XPointer *state;
{
    XLocale xlc = _XFallBackConvert();

    if (xlc)
	xlc = _XlcDupLocale(xlc);
    if (!xlc)
	return NULL;
    *state = (XPointer)xlc;
    return (XrmMethods)&mb_methods;
}
