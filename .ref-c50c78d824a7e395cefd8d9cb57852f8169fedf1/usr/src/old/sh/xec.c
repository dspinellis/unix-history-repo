#ifndef lint
static char sccsid[] = "@(#)xec.c	4.6 %G%";
#endif

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"
#include	"sym.h"

LOCAL INT	parent;

SYSTAB		commands;



/* ========	command execution	========*/


execute(argt, execflg, pf1, pf2)
	TREPTR		argt;
	INT		*pf1, *pf2;
{
	/* `stakbot' is preserved by this routine */
	REG TREPTR	t;
	STKPTR		sav=savstak();

	sigchk();

	IF (t=argt) ANDF execbrk==0
	THEN	REG INT		treeflgs;
		INT		oldexit, type;
		REG STRING	*com;

		treeflgs = t->tretyp; type = treeflgs&COMMSK;
		oldexit=exitval; exitval=0;

		SWITCH type IN

		case TCOM:
			BEGIN
			STRING		a1;
			INT		argn, internal;
			ARGPTR		schain=gchain;
			IOPTR		io=t->treio.treio;
			gchain=0;
			argn = getarg(t);
			com=scan(argn);
			a1=com[1]; gchain=schain;

			IF argn==0 ORF (internal=syslook(com[0],commands))
			THEN	setlist(t->comnod.comset, 0);
			FI

			IF argn ANDF (flags&noexec)==0
			THEN	/* print command if execpr */
				IF flags&execpr
				THEN	argn=0;	prs(execpmsg);
					WHILE com[argn]!=ENDARGS
					DO prs(com[argn++]); blank() OD
					newline();
				FI

				SWITCH internal IN

				case SYSDOT:
					IF a1
					THEN	REG INT		f;
	
						IF (f=pathopen(getpath(a1), a1)) < 0
						THEN failed(a1,notfound);
						ELSE execexp(0,f);
						FI
					FI
					break;
	
				case SYSTIMES:
					{
					L_INT	t[4]; times(t);
					prt(t[2]); blank(); prt(t[3]); newline();
					}
					break;
	
				case SYSEXIT:
					exitsh(a1?stoi(a1):oldexit);
	
				case SYSNULL:
					io=0;
					break;
	
				case SYSCONT:
					execbrk = -loopcnt; break;
	
				case SYSBREAK:
					IF (execbrk=loopcnt) ANDF a1
					THEN breakcnt=stoi(a1);
					FI
					break;
	
				case SYSTRAP:
					IF a1
					THEN	BOOL	clear;
						IF (clear=digit(*a1))==0
						THEN	++com;
						FI
						WHILE *++com
						DO INT	i;
						   IF (i=stoi(*com))>=MAXTRAP ORF i<MINTRAP
						   THEN	failed(*com,badtrap);
						   ELIF clear
						   THEN	clrsig(i);
						   ELSE	replace(&trapcom[i],a1);
							IF *a1
							THEN	getsig(i);
							ELSE	ignsig(i);
							FI
						   FI
						OD
					ELSE	/* print out current traps */
						INT		i;
	
						FOR i=0; i<MAXTRAP; i++
						DO IF trapcom[i]
						   THEN	prn(i); prs(colon); prs(trapcom[i]); newline();
						   FI
						OD
					FI
					break;
	
				case SYSEXEC:
					com++;
					initio(io); ioset=0; io=0;
					IF a1==0 THEN break FI
	
				case SYSLOGIN:
					flags |= forked;
					oldsigs(); execa(com); done();
	
				case SYSCD:
					IF flags&rshflg
					THEN	failed(com[0],restricted);
					ELIF (a1==0 ANDF (a1=homenod.namval)==0) ORF chdir(a1)<0
					THEN	failed(a1,baddir);
					FI
					break;
	
				case SYSSHFT:
					IF dolc<1
					THEN	error(badshift);
					ELSE	dolv++; dolc--;
					FI
					assnum(&dolladr, dolc);
					break;
	
				case SYSWAIT:
					await(-1);
					break;
	
				case SYSREAD:
					exitval=readvar(&com[1]);
					break;

/*
				case SYSTST:
					exitval=testcmd(com);
					break;
*/

				case SYSSET:
					IF a1
					THEN	INT	argc;
						argc = options(argn,com);
						IF argc>1
						THEN	setargs(com+argn-argc);
						FI
					ELIF t->comnod.comset==0
					THEN	/*scan name chain and print*/
						namscan(printnam);
					FI
					break;
	
				case SYSRDONLY:
					exitval=N_RDONLY;
				case SYSXPORT:
					IF exitval==0 THEN exitval=N_EXPORT; FI
	
					IF a1
					THEN	WHILE *++com
						DO attrib(lookup(*com), exitval) OD
					ELSE	namscan(printflg);
					FI
					exitval=0;
					break;
	
				case SYSEVAL:
					IF a1
					THEN	execexp(a1,&com[2]);
					FI
					break;

                                case SYSUMASK:
                                        if (a1) {
                                                int c, i;
                                                i = 0;
                                                while ((c = *a1++) >= '0' &&
                                                        c <= '7')
                                                        i = (i << 3) + c - '0';
                                                umask(i);
                                        } else {
                                                int i, j;
                                                umask(i = umask(0));
                                                prc('0');
                                                for (j = 6; j >= 0; j -= 3)
                                                        prc(((i>>j)&07) + '0');
                                                newline();
                                        }
                                        break;
	
				default:
					internal=builtin(argn,com);
	
				ENDSW

				IF internal
				THEN	IF io THEN error(illegal) FI
					chktrap();
					break;
				FI
			ELIF t->treio.treio==0
			THEN	break;
			FI
			END
	
		case TFORK:
			IF execflg ANDF (treeflgs&(FAMP|FPOU))==0
			THEN	parent=0;
			ELSE	WHILE (parent=fork()) == -1
				DO sigchk(); alarm(10); pause() OD
			FI

			IF parent
			THEN	/* This is the parent branch of fork;    */
				/* it may or may not wait for the child. */
				IF treeflgs&FPRS ANDF flags&ttyflg
				THEN	prn(parent); newline();
				FI
				IF treeflgs&FPCL THEN closepipe(pf1) FI
				IF (treeflgs&(FAMP|FPOU))==0
				THEN	await(parent);
				ELIF (treeflgs&FAMP)==0
				THEN	post(parent);
				ELSE	assnum(&pcsadr, parent);
				FI

				chktrap();
				break;


			ELSE	/* this is the forked branch (child) of execute */
				flags |= forked; iotemp=0;
				postclr();
				settmp();

				/* Turn off INTR and QUIT if `FINT'  */
				/* Reset ramaining signals to parent */
				/* except for those `lost' by trap   */
				oldsigs();
				IF treeflgs&FINT
				THEN	signal(INTR,1); signal(QUIT,1);
				FI

				/* pipe in or out */
				IF treeflgs&FPIN
				THEN	rename(pf1[INPIPE],0);
					close(pf1[OTPIPE]);
				FI
				IF treeflgs&FPOU
				THEN	rename(pf2[OTPIPE],1);
					close(pf2[INPIPE]);
				FI

				/* default std input for & */
				IF treeflgs&FINT ANDF ioset==0
				THEN	rename(chkopen(devnull),0);
				FI

				/* io redirection */
				initio(t->treio.treio);
				IF type!=TCOM
				THEN	execute(t->forknod.forktre,1);
				ELIF com[0]!=ENDARGS
				THEN	setlist(t->comnod.comset,N_EXPORT);
					execa(com);
				FI
				done();
			FI

		case TPAR:
			rename(dup(2),output);
			execute(t->parnod.partre,execflg);
			done();

		case TFIL:
			BEGIN
			   INT pv[2]; chkpipe(pv);
			   IF execute(t->lstnod.lstlef, 0, pf1, pv)==0
			   THEN	execute(t->lstnod.lstrit, execflg, pv, pf2);
			   ELSE	closepipe(pv);
			   FI
			END
			break;

		case TLST:
			execute(t->lstnod.lstlef,0);
			execute(t->lstnod.lstrit,execflg);
			break;

		case TAND:
			IF execute(t->lstnod.lstlef,0)==0
			THEN	execute(t->lstnod.lstrit,execflg);
			FI
			break;

		case TORF:
			IF execute(t->lstnod.lstlef,0)!=0
			THEN	execute(t->lstnod.lstrit,execflg);
			FI
			break;

		case TFOR:
			BEGIN
			   NAMPTR	n = lookup(t->fornod.fornam);
			   STRING	*args;
			   DOLPTR	argsav=0;

			   IF t->fornod.forlst==0
			   THEN    args=dolv+1;
				   argsav=useargs();
			   ELSE	   ARGPTR	schain=gchain;
				   gchain=0;
				   trim((args=scan(getarg(t->fornod.forlst)))[0]);
				   gchain=schain;
			   FI
			   loopcnt++;
			   WHILE *args!=ENDARGS ANDF execbrk==0
			   DO	assign(n,*args++);
				execute(t->fornod.fortre,0);
				IF execbrk<0 THEN execbrk=0 FI
			   OD
			   IF breakcnt THEN breakcnt-- FI
			   execbrk=breakcnt; loopcnt--;
			   argfor=freeargs(argsav);
			END
			break;

		case TWH:
		case TUN:
			BEGIN
			   INT		i=0, saveflg;

			   saveflg = flags&errflg;
			   loopcnt++;
			   WHILE execbrk==0
			   DO flags &= ~errflg;
			      i=execute(t->whnod.whtre,0);
			      flags |= saveflg;
			      IF (i==0)!=(type==TWH) THEN break FI
			      i=execute(t->whnod.dotre,0);
			      IF execbrk<0 THEN execbrk=0 FI
			   OD
			   IF breakcnt THEN breakcnt-- FI
			   execbrk=breakcnt; loopcnt--; exitval=i;
			END
			break;

		case TIF:
			BEGIN
			   INT		i, saveflg;

			   saveflg = flags&errflg;
			   flags &= ~errflg;
			   i=execute(t->ifnod.iftre,0);
			   flags |= saveflg;
			   IF i==0
			   THEN	execute(t->ifnod.thtre,execflg);
			   ELSE	execute(t->ifnod.eltre,execflg);
			   FI
			END
			break;

		case TSW:
			BEGIN
			   REG STRING	r = mactrim(t->swnod.swarg);
			   REG REGPTR	eg = t->swnod.swlst;
			   WHILE eg
			   DO	ARGPTR		rex=eg->regptr;
				WHILE rex
				DO	REG STRING	s;
					IF gmatch(r,s=macro(rex->argval)) ORF (trim(s), eq(r,s))
					THEN	execute(eg->regcom,0);
						eg=0; break;
					ELSE	rex=rex->argnxt;
					FI
				OD
				IF eg THEN eg=eg->regnxt FI
			   OD
			END
			break;
		ENDSW
		exitset();
	FI

	sigchk();
	tdystak(sav);
	return(exitval);
}


execexp(s,f)
	STRING		s;
	UFD		f;
{
	FILEBLK		fb;
	push(&fb);
	IF s
	THEN	estabf(s); fb.feval=f;
	ELIF f>=0
	THEN	initf(f);
	FI
	execute(cmd(NL, NLFLG|MTFLG),0);
	pop();
}
