/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/*
 * Miscellaneous routine needed for standalone library for edit modes
 */

/*
 * copy string a to string b and return pointer to end of string b
 */

#include	<stdio.h>
#ifdef BSD
#include	<sgtty.h>
#define DUPFLG	0100
#endif
#include	<setjmp.h>
#include	"history.h"
#include	"edit.h"
#undef read

#define badcreate	"cannot create"

extern char *strrchr();
extern char *getenv();
char opt_flag;
static unsigned char errbuf[BUFSIZ];
static int	editfd;

/*
 * read routine with edit modes
 */

int read(fd,buff,n)
char *buff;
{
	register int r;
	register int flag;
	register char *sp;
	static char beenhere;
	if(fd==editfd && beenhere==0)
	{
		beenhere++;
		hist_open();
		sp  = getenv("VISUAL");
		if(sp==NULL)
			sp = getenv("EDITOR");
		if(sp)
		{
			if(strrchr(sp,'/'))
				sp = strrchr(sp,'/')+1;
			if(strcmp(sp,"vi") == 0)
				opt_flag = EDITVI;
			else if(strcmp(sp,"emacs")==0)
				opt_flag = EMACS;
			else if(strcmp(sp,"gmacs")==0)
				opt_flag = GMACS;
		}
	}
	flag = (fd==editfd?opt_flag&EDITMASK:0);
	if(flag && (unsigned char*)stderr->_base != errbuf)
	{
		fflush(stderr);
		setbuf(stderr,errbuf);
	}
	switch(flag)
	{
		case EMACS:
		case GMACS:
			r = hread(fd,buff,n);
			break;

		case VIRAW:
		case EDITVI:
			r = vread(fd,buff,n);
			break;
		default:
			if((unsigned char*)stderr->_base == errbuf)
				fflush(stderr);
			r = syscall(3,fd,buff,n);
	}
	if(fd==editfd && fc_fix && (opt_flag&NOHIST)==0 && r>0)
	{
		/* write and flush history */
		int c = buff[r];
		buff[r] = 0;
		hist_eof();
		fputs(buff,fc_fix->fixfd);
		hist_flush();
		buff[r] = c;
	}
	return(r);
}


/*
 * enable edit mode <mode> on file number <fd>
 * the NOHIST bit can also be set to avoid writing the history file
 * <fd> cannot be file two
 */

int	set_edit(fd,mode)
{
	opt_flag = mode;
	if(editfd==2)
		return(-1);
	editfd = fd;
}

char *e_movstr(a,b)
register char *a,*b;
{
	while(*b++ = *a++);
	return(--b);
}

/*
 * print and error message and exit
 */

e_failed(name,message)
char *name,*message;
{
	fputs(name,stderr);
	fputs(" : ",stderr);
	fputs(message,stderr);
	putc('\n',stderr);
	exit(2);
}


/*
 * move the file number on stream fd to unit fb
 */

FILE *hist_rename(fd, fb)
register FILE *fd;
register int 	fb;
{
	register int fa = fileno(fd);
#ifdef BSD
	dup(fa|DUPFLG, fb);
	ioctl(fb, FIOCLEX, 0);
#else	/*	TS lacks two-arg dup, ioctl	*/
	if(fa >= 0)
	{
		close(fb);
		fcntl(fa,0,fb); /* normal dup */
		fcntl(fb,2,1);	/* autoclose for fb */
	}
#endif	/* BSD */
	fd->_file = fb;
	return(fd);
}

/*
 * print a prompt
 */
void pr_prompt(string)
register char *string;
{
	register int c;
#ifdef BSD
	int mode;
#include	<sys/ioctl.h>
	mode = LFLUSHO;
	ioctl(fileno(stderr),TIOCLBIC,&mode);
#endif	/* BSD */
	fflush(stderr);
	if((unsigned char*)stderr->_base != errbuf)
		setbuf(stderr,errbuf);
	while(c= *string++)
		putc(c,stderr);
}

#ifdef BSD
/*
 *	tmpfile - return a pointer to an update file that can be
 *		used for scratch. The file will automatically
 *		go away if the program using it terminates.
 */

extern FILE *fopen();
extern int unlink();
extern char *tmpnam();

FILE *
tmpfile()
{
	char	tfname[1024];
	register FILE	*p;

	tmpnam(tfname);
	if((p = fopen(tfname, "w+")) == NULL)
		return NULL;
	else
		unlink(tfname);
	return(p);
}
#endif	/* BSD */

