/* $Header: /usr/src/games/warp/RCS/score.c,v 1.1 87/07/03 02:13:26 games Exp $ */

/* $Log:	score.c,v $
 * Revision 7.0.1.2a  87/07/03  02:13:26  games
 * Fixed numerous long vs. int bugs in printfs, etc.
 * 
 * Revision 7.0.1.2  86/10/20  12:06:56  lwall
 * Made all exits reset tty.
 * 
 * Revision 7.0.1.1  86/10/16  10:52:47  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:13:14  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "intrp.h"
#include "object.h"
#include "play.h"
#include "sig.h"
#include "term.h"
#include "us.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "score.h"

void
score_init()
{
    Reg1 char *s;
    Reg2 int i;
    FILE *savfil;

    if (stat(SAVEDIR,&filestat)) {
	printf("Cannot access %s\r\n",SAVEDIR);
	finalize(1);
    }
    if (filestat.st_uid != geteuid()) {
	printf("Warp will not run right without being setuid.\r\n");
	finalize(1);
    }
    if ((filestat.st_mode & 0605) != 0605) {
	printf("%s is not protected correctly (must be u+rw o+rx).\r\n",SAVEDIR);
	finalize(1);
    }
    
#ifdef SCOREFULL
    interp(longlognam, sizeof longlognam, "%N");
    for (i=strlen(longlognam); i<24; i++)
	longlognam[i] = ' ';	/* make sure it is 24 long for strncmp */
    longlognam[24] = '\0';
#else
    interp(longlognam, sizeof longlognam, "%L");
    for (i=strlen(longlognam); i<8; i++)
	longlognam[i] = ' ';	/* make sure it is 8 long for strncmp */
    longlognam[8] = '\0';
#endif
    
    if (scorespec)
	wscore();

    Sprintf(savefilename, "save.%s", logname);

    savfil = experimenting ? NULL : fopen(savefilename,"r");
    if (savfil != NULL && fgets(spbuf,100,savfil) != NULL) {
	char tmpbuf[80];

	spbuf[strlen(spbuf)-1] = '\0';
	if (fgets(tmpbuf,80,savfil) != NULL) {
	    int processnum;

	    tmpbuf[strlen(tmpbuf)-1] = '\0';
	    printf("You seem to have left a game %s.\r\n",tmpbuf+9);
	    s = index(tmpbuf+9, ',');
	    *s = '\0';
	    processnum = atoi(s+11);
	    if (kill(processnum, SIGINT)) {
					/* does process not exist? */
					/* (warp ignores SIGINT) */
		printf("\r\n\
That process does not seem to exist anymore, so you'll have to start the\r\n");
		printf(
"last wave over.\r\n\n");
		printf(
"                      [type anything to continue]");
		Fflush(stdout);
		eat_typeahead();
		getcmd(tmpbuf);
		if (*tmpbuf == INTRCH)
		    finalize(0);
		printf("\r\n");
	    }
	    else {
		if (strcmp(term+8,tmpbuf+23)) {
		    printf(
"That is not your current terminal--you are on %s.\r\n", term+5);
		    printf("\r\nYour options:\r\n");
		    printf("   1) Exit and find the terminal it's running on\r\n");
		}
		else {
		    printf("\r\nYour options:\r\n");
		    printf("   1) Exit and try to foreground it\r\n");
		}
		printf("   2) Let me terminate the other game\r\n\n");
		printf("What do you want to do? ");
		Fflush(stdout);
		eat_typeahead();
		getcmd(tmpbuf);
		printf("\r\n");
		if (*tmpbuf == INTRCH)
		    finalize(0);
		if (*tmpbuf == '1') {
		    printf(
"If you don't succeed, come back and do option 2 instead.  Good luck.\r\n");
		    finalize(0);
		}
		printf(
"Ok, hang on a few moments \r\n");
		Fclose(savfil);
		if (kill(processnum, SIGQUIT)) {
		    printf("Unable to kill process #%d!\r\n",processnum);
		    roundsleep(2);
		}
		else {
#ifdef SIGCONT
		    kill(processnum, SIGCONT);
#endif
		    for (i=15; i; --i) {
			sleep(1);
			if (kill(processnum,SIGINT))
					/* does process not exist? */
					/* (warp ignores SIGINT) */
			    break;
		    }
		    didkill++;
		}
		savfil = fopen(savefilename,"r");
		if (savfil != NULL) {
		    Fgets(spbuf,100,savfil);
		}
	    }
	}
    }
    else
	savfil = NULL;
    if (savfil == NULL) {
	totalscore = smarts = cumsmarts = wave = 0;
	numents = 5;
	numbases = 3;
    }
    else {
	totalscore = atol(spbuf+9);
	smarts = atoi(spbuf+20);
	cumsmarts = atoi(spbuf+24);
	numents = atoi(spbuf+30);
	numbases = atoi(spbuf+33);
	wave = atoi(spbuf+36);
	apolspec = (spbuf[40] == 'a');
	beginner   = (spbuf[41] == 'b');
	crushspec  = (spbuf[42] == 'c');
	gornspec   = (spbuf[43] == 'g');
	massacre   = (spbuf[44] == 'm');
	romspec    = (spbuf[45] == 'r');
	tholspec   = (spbuf[46] == 't');
	lowspeed   = (spbuf[47] == 'l') || lowspeed;
	amoebaspec = (spbuf[48] == '&');
	Fclose(savfil);
    }

    if (!ismarts) {
	ismarts = 1;
	clear();
	page(NEWSFILE,FALSE);
	if (smarts) {
	    printf("\r\nSaved game: SCORE DIFF CUMDIFF ENTERPRISES BASES WAVE");
	    printf("\r\n          %7ld  %2d   %4d        %1d        %1d   %3d",
		totalscore,smarts,cumsmarts,numents,numbases,wave);
	}
	printf("\r\nWould you like instructions? ");
	Fflush(stdout);
	eat_typeahead();
	getcmd(buf);
	printf("\r\n");
	if (*buf == INTRCH)
	    finalize(0);
	if (*buf == 'Y' || *buf == 'y') {
	    page(HELPFILE,FALSE);
	    printf("\r\nWould you like to play easy games for a while? ");
	    Fflush(stdout);
	    eat_typeahead();
	    getcmd(buf);
	    printf("\r\n");
	    if (*buf == 'Y' || *buf == 'y') {
		beginner = TRUE;
		lowspeed = TRUE;
	    }
	}
    }
    if (!smarts)
	smarts = ismarts;
}

void
wscore()
{
    clear();
    printf("                             TOP WARPISTS\r\n\n");
    printf("RANK  WHO                     AKA        SCORE DIFF  CUMDIFF  WHEN\r\n");
    page(SCOREBOARD,TRUE);
    printf("                     [Type anything to continue]");
    Fflush(stdout);
    getcmd(spbuf);
    if (*spbuf == INTRCH)
	finalize(0);
    clear();
    printf("                        TOP LOW-SPEED WARPISTS\r\n\n");
    printf("RANK  WHO                     AKA        SCORE DIFF  CUMDIFF  WHEN\r\n");
    page(LSCOREBOARD,TRUE);
    printf("                     [Type anything to continue]");
    Fflush(stdout);
    getcmd(spbuf);
    if (*spbuf == INTRCH)
	finalize(0);
    clear();
    printf("                          TOP FUNNY WARPISTS\r\n\n");
    printf("RANK  WHO                     AKA        SCORE DIFF  CUMDIFF  WHEN\r\n");
    page(FSCOREBOARD,TRUE);
    printf("                     [Type anything to continue]");
    Fflush(stdout);
    getcmd(spbuf);
    if (*spbuf == INTRCH)
	finalize(0);
    clear();
    printf("          GAMES SAVED OR IN PROGRESS\r\n\n");
    printf("WHO           SCORE  DF   CDF  E  B  WV  FLAGS\r\n");
    resetty();
    Sprintf(spbuf,"/bin/cat %ssave.*",SAVEDIR);
#ifndef lint
    execl("/bin/sh", "sh", "-c", spbuf, 0);
#endif
    finalize(1);
}


void
display_status()
{
    Reg1 int tmp;
    static char *status_names[] = {"Impl", "Warp", "Base", "****" };

    if (oldstatus != status) {
	Sprintf(spbuf,"%-4s",status_names[status]);
	mvaddstr(0,0, spbuf);
	oldstatus = status;
    }
    if (ent) {
	if (ent->energy != oldeenergy) {
	    oldeenergy = ent->energy;
	    Sprintf(spbuf,"%4ld",oldeenergy);
	    mvaddstr(0,8, spbuf);
	}
	if (etorp != oldetorp) {
	    Sprintf(spbuf,"%2d",etorp);
	    mvaddstr(0,13, spbuf);
	    oldetorp = etorp;
	}
    }
    else {
	if (etorp >= 0) {
	    etorp = -1;
	    mvaddstr(0,8,"*******");
	    damage = 0;
	}
    }
    if (base) {
	if (base->energy != oldbenergy) {
	    oldbenergy = base->energy;
	    Sprintf(spbuf,"%5ld",oldbenergy);
	    mvaddstr(0,19, spbuf);
	}
	if (btorp != oldbtorp) {
	    Sprintf(spbuf,"%3d",btorp);
	    mvaddstr(0,25, spbuf);
	    oldbtorp = btorp;
	}
    }
    else {
	if (btorp >= 0) {
	    btorp = -1;
	    mvaddstr(0,19,"*********");
	}
    }
    if (damage) {
	if (!olddamage)
	    mvaddstr(0,42,"*** ");
	if (damage > 1 || !damflag[dam]) {
	    do {
		if (++dam == MAXDAMAGE)
		    dam = 0;
	    } while (!damflag[dam]);
	}
	if (!--damflag[dam]) {
	    olddamage = damage;
	    damage--;
	    Sprintf(spbuf,"%s OK ***       ",dammess[dam]);
	    spbuf[15] = '\0';
	    mvaddstr(0,46,spbuf);
	}
	else if (dam == NOSHIELDS) {
	    olddamage = damage;
	    tmp = (34 - damflag[dam]) * 3 - rand_mod(3);
	    if (tmp < 0)
		tmp = 0;
	    Sprintf(spbuf,"%d%% %s ***       ",tmp,dammess[dam]);
	    spbuf[15] = '\0';
	    mvaddstr(0,46,spbuf);
	}
	else if (dam != lastdam || !olddamage) {
	    olddamage = damage;
	    Sprintf(spbuf,"NO %s ***       ",dammess[dam]);
	    spbuf[15] = '\0';
	    mvaddstr(0,46,spbuf);
	}
	if (status < 2) {
	    if (dam == NOIMPULSE && !entmode)
		status = entmode = 1;
	    if (dam == NOWARP && entmode)
		status = entmode = 0;
	}
	tmp = damflag[dam] * damage;
	Sprintf(spbuf,"%3d.%1d ETR",tmp/10,tmp%10);
	mvaddstr(0,69,spbuf);
	lastdam = dam;
    }
    else {
	if (olddamage) {
	    Sprintf(spbuf,"Stars: %-3d Stardate",numstars);
	    mvaddstr(0,42,spbuf);
	    lastdam = -1;
	    olddamage = 0;
	    oldcurscore = -1;
	}
	else if (numstars != oldstrs) {
	    Sprintf(spbuf,"%-3d",numstars);
	    mvaddstr(0,49, spbuf);
	}
	oldstrs = numstars;
    }
    if (numenemies != oldenemies) {
	Sprintf(spbuf,"%-3d",numenemies);
	mvaddstr(0,38, spbuf);
	oldenemies = numenemies;
    }
    if (tmp = timer%10) {
	Sprintf(spbuf,"%1d",tmp);
	mvaddstr(0,67, spbuf);
    }
    else {
	Sprintf(spbuf,"%5d.%1d",timer/10+smarts*100,tmp);
	mvaddstr(0,61, spbuf);
    }
    if ((!damage || !damflag[dam]) && curscore != oldcurscore) {
	Sprintf(spbuf,"%9ld",curscore);
	mvaddstr(0,69, spbuf);
	oldcurscore = curscore;
    }
}

void
wavescore()
{
    double power, effectscore, starscore, pi_over_2;
    long bonuses;
    long tmp;
    FILE *mapfp;
    int row;
    double pow();
#ifndef lint
    double atan2();
#endif

    clear();
    if (curscore > possiblescore)
	curscore = possiblescore;
    pi_over_2 = 3.14159265 / 2.0;
    power = pow((double)inumenemies+     /* total number of enemies */
			inumroms*2+      /* count roms 3 times */
			inumgorns+       /* count gorns 2 times */
			inumthols+       /* count thols 2 times */
			inumapollos*4+   /* count apollo 5 times */
			inumcrushes*3+   /* count crushers 4 times */
			inumamoebas*5	 /* count amoebas 6 times */
	    , 0.50) *                    /* skew it a little */
	    (double)smarts;              /* average energy and intelligence */
    if (inumstars < 350 && inumenemies > 5)
	    power += (350.0 - (double)inumstars) * ((double)inumenemies - 5.0);
    if (inumstars > 850 && inumenemies > 2)
	    power += ((double)inumstars - 850.0) * ((double)inumenemies - 2.0);
#ifndef lint
    effectscore = ((double)curscore / possiblescore) *
	atan2(power, (double) timer + 1.0) / pi_over_2;
#else
    effectscore = pi_over_2;
#endif
    if (inumstars)
	starscore = (double) numstars / (double) inumstars;
    else
	starscore = 1.0;
    wave++;
    Sprintf(spbuf,"Wave = %d, Difficulty = %d, cumulative difficulty = %d",
	 wave, smarts, cumsmarts);
    mvaddstr(1, 13+(smarts<10), spbuf);
    mvaddstr( 4, 68, " BONUS");
    Sprintf(spbuf,"Efficiency rating:       %1.8f (diff=%0.2f,time=%d)",
	 effectscore, power, timer + 1);
    mvaddstr( 5,5, spbuf);
    if (effectscore < 0.8)
	bonuses = tmp = 0;
    else
	bonuses = tmp = (long) ((effectscore-0.8) * smarts * 1000);
    Sprintf(spbuf, "%6ld", tmp);
    mvaddstr( 5, 68, spbuf);
    Sprintf(spbuf,"Star save ratio:         %1.8f (%d/%d)",
	starscore, numstars, inumstars);
    mvaddstr( 6,5, spbuf);
#ifndef lint
    bonuses += tmp = (long) (((double)curscore / possiblescore) *
	(starscore*starscore) * smarts * 20);
#endif
    Sprintf(spbuf, "%6ld", tmp);
    mvaddstr( 6, 68, spbuf);
    row = 7;
    if (inuminhab != numinhab) {
	Sprintf(spbuf, "Inhabited stars depopulated:  %5d", inuminhab-numinhab);
	mvaddstr(row,5, spbuf);
	bonuses += tmp = (long) (inuminhab-numinhab) * -500;
	Sprintf(spbuf, "%6ld", tmp);
	mvaddstr(row, 68, spbuf);
	row++;
    }
    if (inumfriends != numfriends) {
	Sprintf(spbuf, "Friendly craft destroyed:     %5d",
	    inumfriends-numfriends);
	mvaddstr(row,5, spbuf);
	bonuses += tmp = (long) (inumfriends-numfriends) * -250;
	Sprintf(spbuf, "%6ld", tmp);
	mvaddstr(row, 68, spbuf);
	row++;
    }
    if (deadmudds) {
	mvaddstr(row,5,"For destroying Harry Mudd:");
	bonuses += tmp = (long) rand_mod(deadmudds * 20 + 1) - deadmudds*10;
	Sprintf(spbuf, "%6ld", tmp);
	mvaddstr(row, 68, spbuf);
	row++;
    }
    if (bombed_out) {
	mvaddstr(row,5, "For running away from reality:");
	bonuses += tmp = (long) -possiblescore/2;
	Sprintf(spbuf, "%6ld", tmp);
	mvaddstr(row, 68,  spbuf);
	row++;
    }
    if (row < 9)
	row++;
    Sprintf(spbuf, "Enterprise: %-9s%5d remaining",
	!ient?"":ent?"saved":"destroyed", numents);
    mvaddstr(row,5, spbuf);
    bonuses += tmp = ent && !bombed_out ? (smarts+1)*15 : 0;
    Sprintf(spbuf, "%6ld", tmp);
    mvaddstr(row, 68, spbuf);
    row++;
    Sprintf(spbuf, "Base: %-9s      %5d remaining",
	!ibase?"":base?"saved":"destroyed", numbases);
    mvaddstr(row,5, spbuf);
    bonuses += tmp = base && !bombed_out ? (smarts+1)*10 : 0;
    Sprintf(spbuf, "%6ld", tmp);
    mvaddstr(row, 68,  spbuf);
    if (beginner) {
	mvaddstr(13+(row>11),19, "(Special games count only a tenth as much)");
	curscore /= 10;
	bonuses /= 10;
    }
    Sprintf(spbuf, "Previous point total:%10ld",lastscore);
    mvaddstr(15,24, spbuf);
    Sprintf(spbuf, "Points this round:   %10ld",curscore);
    mvaddstr(16,24, spbuf);
    Sprintf(spbuf, "Bonuses:             %10ld",bonuses);
    mvaddstr(17,24, spbuf);
    totalscore = lastscore + curscore + bonuses;
    Sprintf(spbuf, "New point total:     %10ld",totalscore);
    mvaddstr(18,24, spbuf);
    if (lastscore / ENTBOUNDARY < totalscore / ENTBOUNDARY) {
	mvaddstr(row-1,42,"+ 1 new");
	numents++;
    }
    else if (numents>0 &&
	lastscore / ENTBOUNDARY > totalscore / ENTBOUNDARY) {
	mvaddstr(row-1,42,"- 1 obsolete");
	numents--;
    }
    if (lastscore / BASEBOUNDARY < totalscore / BASEBOUNDARY) {
	mvaddstr(row,42,"+ 1 new");
	numbases++;
    }
    else if (numbases>0 &&
	lastscore / BASEBOUNDARY > totalscore / BASEBOUNDARY) {
	mvaddstr(row,42,"- 1 obsolete");
	numbases--;
    }
    if (starscore < 0.8 && inumstars > 200 && numstars > 50) {
	Sprintf(spbuf, "smap.%d",rand_mod(MAPS-PERMMAPS)+PERMMAPS);
	if ((mapfp = fopen(spbuf,"w")) != NULL) {
	    Reg1 OBJECT *obj;

	    fprintf(mapfp,"%d\n",numstars);
	    for (obj = root.next; obj != &root; obj = obj->next) {
		if (obj->type == Star) {
		    fprintf(mapfp,"%d %d\n",obj->posy,obj->posx);
		}
	    }
	    Fclose(mapfp);
	}
    }
}

void
score()
{
    char tmp, *retval, cdate[30];
    Reg1 FILE *logfd;
    Reg2 FILE *outfd;
    Reg3 int i;
    long nowtime, time();
    char *scoreboard;

    for (i=0; link(LOGFILE, LOCKFILE) == -1 && i<10; i++)
	sleep(1);
    nowtime = time((long *)0);
    strcpy(cdate,ctime(&nowtime));
    if ((logfd = fopen(LOGFILE,"a")) != NULL) {
	fprintf(logfd,
	    "%-24s%-9s%7ld%c%2d %4d %s",
	    realname, logname, totalscore, c,smarts, cumsmarts, cdate);
	Fclose(logfd);
    }
    strcpy(cdate+11,cdate+20);
    if (beginner)
	scoreboard = FSCOREBOARD;
    else if (lowspeed)
	scoreboard = LSCOREBOARD;
    else
	scoreboard = SCOREBOARD;
    if (eaccess(scoreboard,0)) {
	if ((logfd = fopen(scoreboard,"w")) != NULL)
	    Fclose(logfd);
    }
    if ((logfd = fopen(scoreboard,"r")) != NULL &&
	(outfd = fopen(TMPSCOREBOARD,"w")) != NULL) {
	for (i=0; i<20; i++) {
	    if ((retval = fgets(buf, 100, logfd)) == NULL)
		break;
	    if (atol(buf+32) < totalscore)
		break;
	    if (strnEQ(buf+COMPOFF,COMPNAME,COMPLEN)) {
		i = 100;
		break;
	    }
	    fprintf(outfd, "%s", buf);
	}
	if (i == 100) {
	    mvaddstr(20,21, "You did not better your previous score");
	    Fclose(outfd);
	    unlink(TMPSCOREBOARD);
	}
	else if (i < 20) {
	    fprintf(outfd, "%-24s%-8s%8ld%c %2d    %4d    %s",
		realname, logname, totalscore, c,smarts, cumsmarts, cdate);
	    i++;
	    Sprintf(spbuf, "    Congratulations--you've placed %d%s",
	      i, i==1?"st":(i==2?"nd":(i==3?"rd":"th")));
	    if (retval != NULL) {
		if (strnNE(buf+COMPOFF,COMPNAME,COMPLEN)) {
		    fprintf(outfd, "%s", buf);
		    i++;
		}
		else
		    strcpy(spbuf,"Congratulations--you've bettered your score");
		while (i<20) {
		    if (fgets(buf, 100, logfd) == NULL)
			break;
		    if (strnNE(buf+COMPOFF,COMPNAME,COMPLEN)) {
			fprintf(outfd, "%s", buf);
			i++;
		    }
		}
	    }
	    mvaddstr(20,19, spbuf);
	    Fclose(logfd);
	    Fclose(outfd);
	    while (unlink(scoreboard) == 0)
		;
	    link(TMPSCOREBOARD,scoreboard);
	    unlink(TMPSCOREBOARD);
	    logfd = fopen(scoreboard,"r");
	}
	else {
	    mvaddstr(20,22,"You did not place within the top 20");
	    Fclose(outfd);
	}
    }
    else {
	Sprintf(spbuf,"(Cannot access %s file, error %d)",
	    (logfd==NULL?"log":"tmp"),errno);
	mvaddstr(20,22,spbuf);
    }
    move(23,0,0);
    erase_eol();
    mvaddstr(23,11,
	"[Hit space for scoreboard, 'r' for new game, 'q' to quit]");
    unlink(LOCKFILE);
    Fflush(stdout);
    eat_typeahead();
    do {
	getcmd(&tmp);
    } while (tmp != INTRCH && tmp != BREAKCH && !index(" rqQ",tmp));
    if (index("qQr",tmp)) {
	justonemoretime = (tmp == 'r');
	if (logfd != NULL)
	    Fclose(logfd);
    }
    else {
	clear();
	if (logfd != NULL) {
	    fseek(logfd, 0L, 0);
	    if (beginner)
		mvaddstr(0,31,"TOP FUNNY WARPISTS");
	    else if (lowspeed)
		mvaddstr(0,29,"TOP LOW-SPEED WARPISTS");
	    else
		mvaddstr(0,33,"TOP WARPISTS");
	    mvaddstr(2,0,"RANK  WHO                     AKA        SCORE DIFF  CUMDIFF  WHEN");
	    for (i=1; i<=20; i++) {
		if (fgets(buf, 100, logfd) == NULL)
		    break;
		buf[strlen(buf)-1] = '\0';
		Sprintf(spbuf, " %2d   %s", i, buf);
		mvaddstr(i+2,0, spbuf);
	    }
	    Fclose(logfd);
	}
	roundsleep(1);
	mvaddstr(23,25,"Would you like to play again?");
	eat_typeahead();
	do {
	    getcmd(&tmp);
	} while (tmp != INTRCH && tmp != BREAKCH && !index("nNyY \n\r",tmp));
	if (tmp == 'n' || tmp == 'N' || tmp == INTRCH || tmp == BREAKCH)
	    justonemoretime = FALSE;
    }

    smarts = ismarts;
    totalscore = cumsmarts = wave = 0;
    numents = 5;
    numbases = 3;
    apolspec = FALSE;
    beginner   = FALSE;
    crushspec  = FALSE;
    gornspec   = FALSE;
    massacre   = (ismarts >= 40);
    romspec    = FALSE;
    tholspec   = FALSE;
}

void
save_game()
{
    FILE *savfil;

    if (experimenting)
	return;
    if ((savfil = fopen(savefilename,"w")) == NULL) {
	resetty();
	printf("Cannot save game\r\n");
	finalize(1);
    }
    fprintf(savfil, "%-8s %10ld, %2d,%5d,%2d,%2d,%3d %c%c%c%c%c%c%c%c\n",
	logname, totalscore, smarts, cumsmarts, numents, numbases, wave,
	apolspec ? 'a' : ' ',
	beginner   ? 'b' : ' ',
	crushspec  ? 'c' : ' ',
	gornspec   ? 'g' : ' ',
	massacre   ? 'm' : ' ',
	romspec    ? 'r' : ' ',
	tholspec   ? 't' : ' ',
	lowspeed   ? 'l' : ' ',
	amoebaspec ? '&' : ' '
    );
    Fclose(savfil);
    resetty();
    if (panic)
	finalize(0);
    clear();
    finalize(0);
}
