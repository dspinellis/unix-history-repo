/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)history.c	1.1 */

/*
 *   History file manipulation routines
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 */


/*
 * Each command in the history file starts on an even byte is null terminated.
 * The first byte must contain the special character H_UNDO and the second
 * byte is the version number.  The sequence H_UNDO 0, following a command,
 * nullifies the previous command. A six byte sequence starting with
 * H_CMDNO is used to store the command number so that it is not necessary
 * to read the file from beginning to end to get to the last block of
 * commands.  This format of this sequence is different in version 1
 * then in version 0.  Version 1 allows commands to use the full 8 bit
 * character set.  It can understand version 0 format files.
 */


#ifdef KSHELL
#include	"defs.h"
#include	"io.h"
#include	"flags.h"
#include	"name.h"
#include	"shtype.h"
#include	"stak.h"
#include	"brkincr.h"
#include	"builtins.h"
#else
#include	<stdio.h>
#include	<setjmp.h>
#include	<signal.h>
#include	<ctype.h>
#endif	/* KSHELL */

#include	"history.h"
#ifdef MULTIBYTE
#include	"national.h"
#endif /* MULTIBYTE */

int	hist_open();
void	hist_close();
long	hist_list();
void	hist_flush();
void	hist_cancel();
void	hist_eof();
histloc	hist_find();
#ifdef ESH
histloc	hist_locate();
#endif	/* ESH */
long	hist_position();
#ifdef KSHELL
void	hist_subst();
#endif	/* KSHELL */

#ifdef KSHELL
extern char	*valup();
extern long	aeval();
extern FILE	*chkrdwr();
extern FILE	*create();
extern void	failed();
extern void	p_str();
#else
#define frenumber		hist_rename
#define tmp_open(s)	tmpfile()
#define p_str(s,c)	(fputs(s,stderr),putc(c,stderr))
#define closefd(f)	fclose(f)
#define	aeval(str)	atoi(str)
#define TMPSIZ	20
#define NL	'\n'
#define output	stderr
struct fixcmd *fc_fix;
extern char	*getenv();
extern FILE	*hist_rename();
char login_sh = 0;
MSG histfname = "./history";
#define unknown "unknown"
#endif	/* KSHELL */
extern char	*substitute();
extern FILE	*tmp_open();
extern long	lseek();
extern char	*malloc();
extern char	*movstr();
extern void	free();

static int fixmask;
static void hist_trim();
static int hist_nearend();
static int hist_check();
static int hist_version;
static int heof;


/*
 * open the history file
 * if HISTNAME is not given and userid==0 then no history file.
 * if login_sh and HISTFILE is longer than HISTMAX bytes then it is
 * cleaned up.
 */
int  hist_open()
{
	register FILE *fd;
	register struct fixcmd *fp;
	register char *histname;
	char fname[TMPSIZ];
	char hname[256];
	int maxlines;
	register char *cp;
	register long hsize = 0;
	int his_start;

	if(fc_fix)
		return(0);
	histname = valup(HISTFILE);
	if(histname==NULL)
	{
#ifdef KSHELL
		if(userid==0 && login_sh)
			return(-1);
#endif	/* KSHELL */
		cp = movstr(valup(HOME),hname);
		movstr(histfname,cp);
		histname = hname;
	}
	*fname = 0;
retry:
	/* first try to open the current file */
#ifdef KSHELL
	if((fd=fdopen(open(histname,012),"a+"))==NULL)
	{
		/* if you can't then try to create it */
		if(fd=create(histname))
		{
			fd = chkrdwr(histname,fd);
			chmod(histname,0600);
		}
	}
	else
		hsize=lseek(fileno(fd),0L,2);
#else
	if(fd=fopen(histname,"a+"))
	{
		chmod(histname,0600);
		hsize=lseek(fileno(fd),0L,2);
	}
#endif	/* KSHELL */
	/* make sure that file has history file format */
	if(hsize && hist_check(fd))
	{
		fclose(fd);
		unlink(histname);
		hsize = 0;
		goto retry;
	}
	if(fd == NULL)
		fd = tmp_open(fname);
	if(fd==NULL)
		return(-1);
	fd = frenumber(fd,FCIO);
	if(cp=valup(HISTSIZE))
		maxlines = (unsigned)aeval(cp);
	else
		maxlines = HIS_DFLT;
	for(fixmask=16;fixmask <= maxlines; fixmask <<=1 );
	if((fp=(struct fixcmd*)malloc(sizeof(struct fixcmd)+ (--fixmask)*sizeof(long)))==NULL)
	{
		fclose(fd);
		return(-1);
	}
	fc_fix = fp;
	fp->fixfd = fd;
	fp->fixmax = maxlines;
	setbuf(fd,malloc(BUFSIZ));
	fp->fixind = 1;
	fp->fixline = 0;
	fp->fixcmds[1] = 2;
	fp->fixcnt = 2;
	if(hsize==0)
	{
		/* put special characters at front of file */
		putc(H_UNDO,fd);
		putc(H_VERSION,fd);
		fflush(fd);
	}
	/* initialize history list */
	if(hsize)
	{
		int nlines = maxlines;
		long size = hsize - (HISMAX/4);
		do
		{
			size -= ((HISMAX/4) + nlines*HISLINE);
			his_start = fp->fixind = hist_nearend(fd,size);
			hist_eof();
			nlines = maxlines - (fp->fixind-his_start);
		}
		while(his_start >1 && nlines>0);
	}
	if(*fname)
		unlink(fname);
	if(login_sh && his_start>1 && hsize > HISMAX)
	{
		FILE *fdo;
		if((fdo=fdopen(open(histname,0),"r"))==NULL)
			return(0);
		unlink(histname);
		hist_trim(fdo,fp->fixind-maxlines);
	}
	return(0);
}

/*
 * check history file format to see if it begins with special byte
 */

static int hist_check(fd)
register FILE *fd;
{
	setbuf(fd,NULL);
	fseek(fd,0L,0);
	if(getc(fd) != H_UNDO)
		return(1);
	hist_version = getc(fd);
	return(0);
}

/*
 * Copy the last <n> commands to a new file and make this the history file
 */

static void hist_trim(fdo,n)
register FILE *fdo;
{
	register FILE *fd;
	register int c;
	register struct fixcmd *fp = fc_fix;
	struct fixcmd *fsave;
	/* use the old history I/O buffer for fdo */
	setbuf(fdo,fp->fixfd->_base);
	setbuf(fp->fixfd,NULL);
	fc_fix = 0;
	hist_open();
	if(fc_fix==0)
		return;
	fsave = fc_fix;
	fd = fc_fix->fixfd;
	do
	{
		fc_fix = fp;
		fseek(fdo,hist_position(++n),0);
		fc_fix = fsave;
		while((c=getc(fdo))!=EOF && c)
		{
			putc(c,fd);
		}
#ifdef KSHELL
		states |= FIXFLG;
#endif	/* KSHELL */
		hist_flush();
	}
	while(c!=EOF);
	fclose(fdo);
	free((char*)fdo->_base);
	free((char*)fp);
}

/*
 * position history file at size and find next command number 
 */

static int hist_nearend(fd,size)
register FILE *fd;
long size;
{
	register int n = 0;
	register int state = -1;
	register int c;
	if(size <=0)
		goto begin;
	fseek(fd,size,0);
	/* skip to numbered command and return the number */
	/* numbering commands occur after a null and begin with H_CMDNO */
	while((c=getc(fd))!=EOF)
	{
		if(state==5)
		{
			return(n);
		}
		else if(state>0)
		{
			if(state==1)
			{
				/* see if H_CMDNO is followed by 0 */
				if(hist_version && c)
				{
					n += 2;
					state = -1;
					continue;
				}
				n = 0;
			}
			if(hist_version)
				n = (n<<8) + c;
			else if(state < 3)
				n = (n<<7) + (c&0177);
			state++;
		}
		else if(state==0 && c==H_CMDNO)
		{
			fc_fix->fixcnt = size + n + 6;
			state = 1;
		}
		else
		{
			state = (c==0?0:-1);
			n++;
		}
	}
begin:
	fseek(fd,2L,0);
	fc_fix->fixcnt = 2;
	return(1);
}

/*
 * This routine unlinks the history file if the file is a temp file
 */

void hist_close()
{
	if(fc_fix)
		fclose(fc_fix->fixfd);
}

/*
 * This routine reads the history file from the present position
 * to the end-of-file and puts the information in the in-core
 * history table
 * Note that H_CMDNO is only recognized at the beginning of a command
 * and that H_UNDO as the first character of a command is skipped
 * unless it is followed by 0.  If followed by 0 then it cancels
 * the previous command.
 */

void hist_eof()
{
	register struct fixcmd *fp = fc_fix;
	register int c;
	register int incr = 0;
	register int oldc = 0;
	register long count = fp->fixcnt;
	int skip = 0;
	heof++;		/* don't add line number markers */
	fseek(fp->fixfd,count,0);
	while((c=getc(fp->fixfd))!=EOF)
	{
		count++;
		if(skip-- > 0)
		{
			oldc = 0;
			continue;
		}
		if(c == 0)
		{
			if(oldc==H_CMDNO && incr==0)
				skip = 3;
			fp->fixind += incr;
			fp->fixcmds[fp->fixind&fixmask] = count;
			incr = 0;
		}
		else if(oldc == 0)
		{
			if(c == H_CMDNO)
			{
				/* old format history file */
				if(hist_version==0)
					skip = 4;
				incr = 0;
			}
			else if(c==H_UNDO)
				incr = -1;
		}
		else
			incr = 1;
		oldc = c;
	}
	fp->fixline = 0;
	fp->fixcnt = count;
	heof = 0;
}

/*
 * This routine will cause the previous command to be cancelled
 */

void hist_cancel()
{
	register struct fixcmd *fp = fc_fix;
	register FILE *fd;
	register int c;
	if(fp==NULL)
		return;
	fd = fp->fixfd;
	putc(H_UNDO,fd);
	putc(0,fd);
	fflush(fd);
	fp->fixcnt += 2;
	c = (--fp->fixind)&fixmask;
	fp->fixcmds[c] = fp->fixcnt;
}

/*
 * This routine adds one or two null bytes and flushes the history buffer
 */

void hist_flush()
{
	register struct fixcmd *fp = fc_fix;
	register FILE *fd;
	register int c;
	if(fp==NULL)
		return;
#ifdef KSHELL
	if((states&FIXFLG) == 0)
		return;
	states &= ~FIXFLG;
#endif	/* KSHELL */
	fd = fp->fixfd;
	fp->fixline = 0;
	/* remove whitespace from end of commands */
	while(--fd->_ptr >= fd->_base)
	{
		if((c= *fd->_ptr)!=NL && !isspace(c))
			break;
	}
	fd->_cnt = ++fd->_ptr - fd->_base;
	if(fd->_cnt<=0)
	{
		fp->fixind--;
		goto set_count;
	}
	putc(NL,fd);
	putc('\0',fd);
	fflush(fd);
set_count:
	fp->fixcnt = lseek(fileno(fd),0L,2);
	/* start each command on an even byte boundary */
	if(fp->fixcnt&01)
	{
		fp->fixcnt++;
		putc('\0',fd);
		fflush(fd);
	}
	c = (++fp->fixind)&fixmask;
	fp->fixcmds[c] = fp->fixcnt;
	if((c = fp->fixcmds[c])> (HISMAX/4) && !heof)
	{
		/* put line number in file */
		fp->fixcnt += 6;
		putc(H_CMDNO,fd);
		putc(0,fd);
		c = (fp->fixind>>16);
		putc(c,fd);
		c = (fp->fixind>>8);
		putc(c,fd);
		c = fp->fixind;
		putc(c,fd);
		putc(0,fd);
		fflush(fd);
		fp->fixcmds[c&fixmask] = fp->fixcnt;
	}
}

/*
 * return byte offset in history file for command <n>
 */

long hist_position(n)
int n;
{
	register struct fixcmd *fp = fc_fix;
	return(fp->fixcmds[n&fixmask]);
}

/*
 * write the command starting at offset <offset> onto file <fd>.
 * listing stops when character <last> is encountered or end-of-string.
 * each new-line character is replaced with string <nl>.
 */

long hist_list(offset,last,nl)
long offset;
int last;
char *nl;
{
	register int oldc;
	register FILE *fd;
	register int c;
	if(offset<0)
	{
		p_str(unknown,NL);
		return(-1);
	}
	fd = fc_fix->fixfd;
	fseek(fd,offset,0);
	oldc=getc(fd);
	for(offset++;oldc && oldc!=last;oldc=c,offset++)
	{
		if((c = getc(fd)) == EOF)
			return(offset);
		if(oldc=='\n')
		{
			if(c)
			{
				fputs(nl,output);
				continue;
			}
			/* don't print trailing newline for job control */
			else if(last=='&')
				return(offset);
		}
		putc(oldc,output);
	}
	return(offset);
}
		 
/*
 * find index for last line with given string
 * If flag==0 then line must begin with string
 * direction < 1 for backwards search
*/

histloc hist_find(string,index1,flag,direction)
char *string;
int index1;
int direction;
{
	register struct fixcmd *fp = fc_fix;
	register char *cp;
	register int c;
	long offset;
	int count;
	int index2;
	histloc location;
#ifdef MULTIBYTE
	int nbytes = 0;
#endif /* MULTIBYTE */
	location.his_command = -1;
	if(fp==NULL)
		return(location);
	index2 = fp->fixind;
	if(direction<0)
	{
		index2 -= fp->fixmax;
		if(index2<1)
			index2 = 1;
		if(index1 <= index2)
			return(location);
	}
	else if(index1 >= index2)
		return(location);
	while(index1!=index2)
	{
		direction>0?++index1:--index1;
		offset = hist_position(index1);
		location.his_line = 0;
#ifdef KSHELL
		/* allow a search to be aborted */
		if(trapnote&SIGSET)
			exitsh(SIGFAIL);
#endif /* KSHELL */
		do
		{
			if(offset>=0)
			{
				fseek(fp->fixfd,offset,0);
				count = offset;
			}
			offset = -1;
			for(cp=string;*cp;cp++)
			{
				if((c=getc(fp->fixfd)) == EOF)
					break;
				if(c == 0)
					break;
				count++;
#ifdef MULTIBYTE
				/* always position at character boundary */
				if(--nbytes > 0)
				{
					if(cp==string)
					{
						cp--;
						continue;
					}
				}
				else
				{
					nbytes = echarset(c);
					nbytes = in_csize(nbytes) + (nbytes>=2);
				}
#endif /* MULTIBYTE */
				if(c == '\n')
					location.his_line++;
				/* save earliest possible matching character */
				if(flag && c == *string && offset<0)
					offset = count;
				if(*cp != c )
					break;
			}
			if(*cp==0)
				/* match found */
			{
				location.his_command = index1;
				return(location);
			}
		}
		while(flag && c && c != EOF);
	}
	fseek(fp->fixfd,0L,2);
	return(location);
}

#if ESH || VSH
/*
 * copy command <command> from history file to s1
 * at most MAXLINE characters copied
 * if s1==0 the number of lines for the command is returned
 * line=linenumber  for emacs copy and only this line of command will be copied
 * line < 0 for full command copy
 * -1 returned if there is no history file
 */

int hist_copy(s1,command,line)
register char *s1;
{
	register int c;
	register struct fixcmd *fp = fc_fix;
	register int count = 0;
	register char *s1max = s1+MAXLINE;
	long offset;
	if(fp==NULL)
		return(-1);
	offset =  hist_position(command);
	fseek(fp->fixfd,offset,0);
	while ((c = getc(fp->fixfd)) && c!=EOF)
	{
		if(c=='\n')
		{
			if(count++ ==line)
				break;
			else if(line >= 0)	
				continue;
		}
		if(s1 && (line<0 || line==count))
		{
			if(s1 >= s1max)
			{
				*--s1 = 0;
				break;
			}
			*s1++ = c;
		}
			
	}
	if(s1==0)
		return(count);
	if((c= *(s1-1)) == '\n')
		s1--;
	*s1 = '\0';
	fseek(fp->fixfd,0L,2);
	return(count);
}

/*
 * return word number <word> from command number <command>
 */

char *hist_word(s1,word)
char *s1;
{
	register int c;
	register char *cp = s1;
	register int flag = 0;
	if(fc_fix==0)
#ifdef KSHELL
		return(lastarg);
#else
		return(NULL);
#endif /* KSHELL */
	hist_copy(s1,fc_fix->fixind-1,-1);
	for(;c = *cp;cp++)
	{
		c = isspace(c);
		if(c && flag)
		{
			*cp = 0;
			if(--word==0)
				break;
			flag = 0;
		}
		else if(c==0 && flag==0)
		{
			s1 = cp;
			flag++;
		}
	}
	*cp = 0;
	return(s1);
}

#endif	/* ESH */

#ifdef ESH
/*
 * given the current command and line number,
 * and number of lines back or foward,
 * compute the new command and line number.
 */

histloc hist_locate(command,line,lines)
register int command;
register int line;
int lines;
{
	histloc next;
	line += lines;
	if(fc_fix==NULL)
	{
		command = -1;
		goto done;
	}
	if(lines > 0)
	{
		register int count;
		while(command <= fc_fix->fixind)
		{
			count = hist_copy(NIL,command,-1);
			if(count > line)
				goto done;
			line -= count;
			command++;
		}
	}
	else
	{
		register int least = fc_fix->fixind-fc_fix->fixmax;
		while(1)
		{
			if(line >=0)
				goto done;
			if(--command < least)
				break;
			line += hist_copy(NIL,command,-1);
		}
		command = -1;
	}
	next.his_command = command;
	return(next);
done:
	next.his_line = line;
	next.his_command = command;
	return(next);
}
#endif	/* ESH */

#ifdef KSHELL

/*
 * given a file containing a command and a string of the form old=new,
 * execute the command with the string old replaced by new
 */
void hist_subst(command,fd,replace)
char *command;
FILE *fd;
char *replace;
{
	register char *new=replace;
	register char *sp = locstak();
	register int c;
	char *string;
	while(*++new != '='); /* skip to '=' */
	while ((c=getc(fd)) != EOF)
		*sp++ = c;
	string = endstak(sp);
	fclose(fd);
	*new++ =  0;
	if(substitute(string,replace,new,(sp=locstak())))
		endstak(sp+strlen(sp));
	else
		failed(command,badsub);
	*(new-1) =  '=';
	fputs(sp,fc_fix->fixfd);
	hist_flush();
	fputs(sp,output);
	execexp(sp,(FILE*)0);
}
#endif	/* KSHELL */
