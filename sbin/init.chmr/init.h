/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christoph Robitschko.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Defaults
 */
#define RETRYTIME		5*60
#define INIT_M2S_TERMTO		10
#define INIT_M2S_KILLTO		30
#define DEF_CHECKTIME		5
#define DEF_CHECKSTATUS		1
#define ALLOC_ARGV		4
#define CALLOUT_MINFREE		5
#define CALLOUT_CHUNK		10


#ifndef TESTRUN
#   define DEBUG_LEVEL		0
#   define INIT_CONFIG		"/etc/init.conf"
#else /* TESTRUN */
#   define DEBUG_LEVEL		5
#   define INIT_CONFIG		"./init.conf"
#endif /* TESTRUN */


extern int	debug;
extern int	retrytime;
extern char	**ienviron;
extern sigset_t	block_set;


#define blocksig()	sigprocmask(SIG_BLOCK, &block_set, 0)
#define unblocksig()	sigprocmask(SIG_UNBLOCK, &block_set, 0)



/* internal representation of getty table */
typedef struct ttytab	{
	struct ttytab	*next;
	struct ttytab	*prev;
	char		*name;		/* device name */
	char		**argv;		/* argv for execve() */
	char		*type;		/* terminal type */
	int		intflags;	/* internal flags, see below */
	pid_t		pid;		/* PID of spawned process */
	int		failcount;	/* how often getty exited with error */
	time_t		starttime;	/* when it was started */
	}	ttytab_t;


/* Values for intflags: */
#define INIT_SEEN	0x001
#define INIT_CHANGED	0x002
#define INIT_NEW	0x004
#define INIT_FAILED	0x008	/* process exited with error code last time */
#define INIT_OPEN	0x010	/* Init has to do the open() */
#define INIT_NODEV	0x020	/* do not append device to argv */
#define INIT_DONTSPAWN	0x040	/* do not respawn a process on this line */
#define INIT_ARG0	0x080	/* don't pass command as argv[0] */
#define INIT_FAILSLEEP	0x100	/* getty is asleep before it is retried */


/* type field for callout table */
typedef enum	{
	CO_ENT2TAB,		/* retry multiuser() */
	CO_FORK,		/* retry do_getty(tt) */
	CO_GETTY,		/* retry do_getty(tt) */
	CO_MUL2SIN,		/* timeout in multi2single */
	} retr_t;

/* format of callout table */
typedef struct callout {
	struct callout	*next;
	unsigned int	sleept;
	retr_t		what;
	void		*arg;
	}	callout_t;
