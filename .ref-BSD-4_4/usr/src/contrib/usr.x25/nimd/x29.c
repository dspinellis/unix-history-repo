/*
 * X.29 specific code for
 * Network Interface Machine server
 *
 * Frank Pronk
 * Copyright (c) 1984
 */

#include <sys/types.h>
#include "../h/x29.h"

#include "nim.h"

short 	pnums[NX29_PARMS] = {
	1,2,3,4,5,6,7,8,9,10,11,12,	/* 1978+1980 params */
	13, 14, 15, 16, 17, 18		/* 1980 only */
};

char chartab[] = {	/* EVEN parity and forwarding code table */
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
};

char	profiles[NPROFILES+1][NX29_PARMS] = {
/*    1  2    3   4  5  6   7  8  9 10 11 12/13 14 15   16   17   18 */
    { 1, 1, 126,  0, 0, 1, 21, 0, 2, 0, 0, 1, 4, 0, 1,   8,  21,  18 },
    { 1, 0, 126,  0, 1, 1,  2, 0, 0, 0, 0, 1, 0, 0, 0,   0,   0,   0 },
    { 1, 0,   2,  0, 0, 1, 21, 0, 2, 0, 0, 1, 4, 0, 1,   8,  21,  18 },
    { 0, 0,   0, 20, 0, 0,  2, 0, 0, 0, 0, 0, 0, 0, 0,   0,   0,   0 },
    { 1, 0,   2,  0, 0, 1, 21, 0, 2, 0, 0, 0, 0, 0, 0,   0,   0,   0 },
    { 1, 0,   2,  0, 0, 1,  2, 0, 2, 0, 1, 0, 0, 0, 0,   0,   0,   0 },
    { 0, 0,   0,  4, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0,   0,   0,   0 }
};

/*
 * Initialize user's profile
 */

InitProfile(profile)
{
	register int i;

	for(i = 0; i < 128; i++)
		CurrentX29Parms[i] = INVALID;
	for(i = 0; i < NetInfo.n_nparms; i++)
		CurrentX29Parms[pnums[i]] = profiles[profile][i];
	SetSpecialChars();
	CurrentProfile = profile;
}

SetSpecialChars()
{
	register int c, code;

	for(c = 0; c < 128; c++)
		chartab[c] &= 0200;

	code = CurrentX29Parms[X29_FORWARDING_SIGNAL_CODE];
	if (code & 01) {	/* A-Z, a-z, 0-9 */
		for (c = 'A'; c <= 'Z'; c++)
			chartab[c] |= C_FORWARD;
		for (c = 'a'; c <= 'z'; c++)
			chartab[c] |= C_FORWARD;
		for (c = '0'; c <= '9'; c++)
			chartab[c] |= C_FORWARD;
	}
	if (code & 02)		/* CR */
		chartab['\r'] |= C_FORWARD;
	if (code & 04) {	/* ESC, BEL, ENQ, ACK */
		chartab[033] |= C_FORWARD;
		chartab[07] |= C_FORWARD;
		chartab[05] |= C_FORWARD;
		chartab[06] |= C_FORWARD;
	}
	if (code & 8) {		/* DEL, CAN, DC2 */
		chartab[0177] |= C_FORWARD;
		chartab[030] |= C_FORWARD;
		chartab[022] |= C_FORWARD;
	}
	if (code & 16) {	/* ETX, EOT */
		chartab[03] |= C_FORWARD;
		chartab[04] |= C_FORWARD;
	}
	if (code & 32) {	/* HT, LF, VT, FF */
		chartab[011] |= C_FORWARD;
		chartab[012] |= C_FORWARD;
		chartab[013] |= C_FORWARD;
		chartab[014] |= C_FORWARD;
	}
	if (code & 64) {	/* control codes not in the above list */
		chartab[01] |= C_FORWARD;
		chartab[02] |= C_FORWARD;
		chartab[010] |= C_FORWARD;
		chartab[016] |= C_FORWARD;
		chartab[017] |= C_FORWARD;
		chartab[020] |= C_FORWARD;
		chartab[021] |= C_FORWARD;
		chartab[023] |= C_FORWARD;
		chartab[024] |= C_FORWARD;
		chartab[025] |= C_FORWARD;
		chartab[026] |= C_FORWARD;
		chartab[027] |= C_FORWARD;
		chartab[031] |= C_FORWARD;
		chartab[032] |= C_FORWARD;
		chartab[034] |= C_FORWARD;
		chartab[035] |= C_FORWARD;
		chartab[036] |= C_FORWARD;
		chartab[037] |= C_FORWARD;
	}

	if ((c = CurrentX29Parms[X29_ESCAPE_TO_CMD_CODE]) > 0)
		chartab[c==1 ? ('P' & 037) : c] |= C_ESCAPE;

	if (CurrentX29Parms[X29_EDITING]) {
		if ((c = CurrentX29Parms[X29_CHARACTER_DELETE]) > 0)
			chartab[c] |= C_ERASE;
		if ((c = CurrentX29Parms[X29_LINE_DELETE]) > 0)
			chartab[c] |= C_KILL;
		if ((c = CurrentX29Parms[X29_LINE_DISPLAY]) > 0)
			chartab[c] |= C_DISPLAY;
	}
}

X29ControlMessage (bp, len)
register struct x29packet *bp;
{

	ForwardPacket ();
	switch (bp->p_x29code) {
	case X29_SET_PARMS:
		if (SetNIM (bp, len))
			ReadNIM (bp, len);
		break;

	case X29_READ_PARMS:
		ReadNIM (bp, len);
		break;

	case X29_SET_AND_READ_PARMS:
		(void) SetNIM (bp, len);
		ReadNIM (bp, len);
		break;

	case X29_INVITATION_TO_CLEAR:
		ExitDataState ("remote directive");
		break;

	case X29_INDICATION_OF_BREAK:
		break;

	case X29_ERROR:
		log ("x.29 error indication from remote host");
		LogPacket ((char *)bp, len);
		break;
		
	default:
		X29Error ("Unknown X.29 request", bp, len, 1);
	}
}

/*
 * Bad news - we received an invalid or an
 * unknown packet.  Send an error indication
 * to the remote host and dump the contents
 * of the packet in the log file.
 */

X29Error (why, bp, len, code)
struct x29packet *bp;
char *why;
{
	register struct x29packet *pp;
	char errmsg[4];

	log ("x.29 error: %s: packet contents:", why);
	LogPacket ((char *)bp, len);
	pp = (struct x29packet *)errmsg;
	pp->p_x29flag = Q_BIT;
	pp->p_x29code = X29_ERROR;
	pp->p_x29errno = code;
	pp->p_x29mtype = bp->p_x29code;
	ToNet (pp, sizeof (errmsg));
}

SetX29Parm (pnum, value)
u_char pnum, value;
{
	register int special = 0;

	if (pnum >= 128 || CurrentX29Parms[pnum] < 0)
		return (1);
	switch (pnum) {
	case X29_ESCAPE_TO_CMD_CODE:
		if (value > 1 && value != 033 && (value < 32 || value > 126))
			return (1);
		special++;
		break;

	case X29_ECHO_CODE:
	case X29_DISCARD_OUTPUT_CODE:
	case X29_XON_XOFF_CODE:
		if (value > 1)
			return (1);
		break;

	case X29_AUX_DEV_CONTROL_CODE:
		if (value > 1)
			return (1);
		OutputBlocked = 0;
		break;

	case X29_RECEIVE_NET_MSGS_CODE:
		if (value > 1 && value != 5)
			return (1);
		break;

	case X29_FORWARDING_SIGNAL_CODE:
		if (value > 127)
			return (1);
		special++;
		break;

	case X29_IDLE_TIMER_CODE:
		break;

	case X29_BREAK_PROCEDURE_CODE:
		if (value != 0 && value != 1 && value != 2 && value != 4 &&
		    value != 5 && value != 8 && value != 16 && value != 21)
			return (1);
		break;

	case X29_PADDING_CODE:
		if (value > 7)
			return (1);
		break;

	case X29_LINE_FOLDING_CODE:
		break;

	case X29_TRANSMISSION_SPEED_CODE:
	/*
	 * Trying to change the line speed is illegal.
	 * This seems to be a defect in many PADs.
	 * Rather than log an error and send a complaint back
	 * to the remote PAD, we will quietly ignore the problem.
	 */
/*		return (1);	/* read-only variable */
		return (0);

	/* Parameters specific to 1980 CCITT recommendation */

	case X29_LF_AFTER_CR:
		if (value > 1 && (value < 4 || value > 7))
			return (1);
		break;

	case X29_PADDING_AFTER_LF:
		if (value > 7)
			return (1);
		break;

	case X29_EDITING:
		if (value > 1)
			return (1);
		special++;
		break;

	case X29_CHARACTER_DELETE:
	case X29_LINE_DELETE:
	case X29_LINE_DISPLAY:
		if (value > 127)
			return (1);
		special++;
		break;

	default:
		return (1);
	}
	CurrentX29Parms[pnum] = value;
	if (special)
		SetSpecialChars ();
	return (0);
}

SetNIM (bp, len)
register struct x29packet *bp;
{
	register int nparms;

	nparms = (struct x29param *)((char *)bp + len) - bp->p_x29param;
	if ((char *)&bp->p_x29param[nparms] != (char *)bp + len) {
		X29Error ("incomplete set parameter request", bp, len, 3);
		return (0);
	}

	/*
	 * if no parameters then reset to subscription profile
	 */

	if (nparms == 0) {
		register int i;

		nparms = NetInfo.n_nparms;
		for (i = 0; i < nparms; i++)
			(void) SetX29Parm (pnums[i], profiles[CurrentProfile][i]);
			return (0);
	} else {
		register struct x29param *xp;
		register int errors = 0;

		for (xp = bp->p_x29param; xp < &bp->p_x29param[nparms]; xp++) {

			/*
			 * Stop processing parameters if we find a national
			 * parameter marker.  We don't support any network
			 * specific private parameters.  Rather than logging
			 * an error and bouncing back a complaint to the
			 * remote PAD, we will just give up.
			 */

			if (xp->x29_pnum == X29_NATIONAL_PARAMETER_MARKER)
				break;

			/*
			 * if attempt to set illegal parameter or legal
			 * parameter to illegal value then mark parameter
			 * as invalid.
			 */

			if (SetX29Parm(xp->x29_pnum, xp->x29_value)) {
				if (errors == 0) {
					log ("x.29 error: invalid parameters in set parameter request:");
					LogPacket ((char *)bp, len);
				}
				xp->x29_pnum |= 0200;
				errors++;
			}
		}
		return (errors);
	}
}

ReadNIM (bp, len)
register struct x29packet *bp;
{
	register int nparms;

	nparms = (struct x29param *)((char *)bp + len) - bp->p_x29param;
	if ((char *)&bp->p_x29param[nparms] != (char *)bp + len) {
		X29Error ("incomplete read parameter request", bp, len, 3);
		return;
	}

	/*
	 * if no parameters specified then send
	 * all parameters.
	 */

	if (nparms == 0) {
		register int i;
		register struct x29packet *pp;
		char reply[128];

		pp = (struct x29packet *)reply;
		pp->p_x29flag = Q_BIT;
		pp->p_x29code = X29_PARAMETER_INDICATION;

		nparms = NetInfo.n_nparms;
		for (i = 0; i < nparms; i++) {
			pp->p_x29param[i].x29_pnum = pnums[i];
			pp->p_x29param[i].x29_value = CurrentX29Parms[pnums[i]];
		}
		ToNet (pp, (char *)&pp->p_x29param[nparms] - (char *)pp);
	} else {
		register int pnum, errors = 0;
		register struct x29param *xp;

		/*
		 * Scribble over the original Read Parameter request
		 * replacing the message code and filling in the values
		 * for the requested parameters.
		 */

		bp->p_x29code = X29_PARAMETER_INDICATION;
		for (xp = bp->p_x29param; xp < &bp->p_x29param[nparms]; xp++) {
			pnum = xp->x29_pnum;

			/*
			 * Parameters private to specific networks generate
			 * lots of errors because we don't support any of them.
			 * We'll set the errors flag to avoid filling our
			 * log file with messages related to this limitation.
			 */

			if (pnum == X29_NATIONAL_PARAMETER_MARKER)
				errors++;
			/*
			 * If the requested parameter is invalid and has
			 * not already been marked as invalid (by SetNIM()
			 * in a set and read parameter request) then mark
			 * the parameter as invalid and set its value to 0.
			 */

			if ((pnum & 0200) == 0 && CurrentX29Parms[pnum] < 0) {
				if (errors == 0) {
					log ("x.29 error: invalid parameters in read parameter request:");
					LogPacket ((char *)bp, len);
				}
				pnum |= 0200;
				errors++;
			}
			if (pnum & 0200)
				xp->x29_value = 0;
			else
				xp->x29_value = CurrentX29Parms[pnum];
		}
		ToNet (bp, len);
	}
}

Break (code)
{
	register struct x29packet *bp;
	char brkmsg[4];

	bp = (struct x29packet *)brkmsg;
	switch (code) {
	case 1:		/* interrupt */
		SendX25Interrupt ();
		ForwardPacket ();
		return;

	case 2:		/* reset */
		ResetBufs ();
		return;

	case 4:		/* send indication of break */
		ForwardPacket();
		bp->p_x29flag = Q_BIT;
		bp->p_x29code = X29_INDICATION_OF_BREAK;
		ToNet (bp, 2);
		return;

	case 5:		/* send interrupt and indication of break */
		SendX25Interrupt ();
		ForwardPacket();
		bp->p_x29flag = Q_BIT;
		bp->p_x29code = X29_INDICATION_OF_BREAK;
		ToNet (bp, 2);
		return;

	case 8:		/* enter command state */
		EnterCommandState ();
		return;

	case 16:	/* discard output */
		ForwardPacket();
		bp->p_x29flag = Q_BIT;
		bp->p_x29code = X29_PARAMETER_INDICATION;
		bp->p_x29param[0].x29_pnum = X29_DISCARD_OUTPUT_CODE;
		bp->p_x29param[0].x29_value = 1;
		ToNet (bp, 4);
		CurrentX29Parms[X29_DISCARD_OUTPUT_CODE] = 1;
		return;

	case 21:	/* INT + BR indication + discard output */
		SendX25Interrupt ();
		ForwardPacket ();
		bp->p_x29flag = Q_BIT;
		bp->p_x29code = X29_INDICATION_OF_BREAK;
		bp->p_x29param[0].x29_pnum = X29_DISCARD_OUTPUT_CODE;
		bp->p_x29param[0].x29_value = 1;
		ToNet (bp, 4);
		CurrentX29Parms[X29_DISCARD_OUTPUT_CODE] = 1;
		return;
	}
}

/*
 * Change data pointed to by 'start' to be
 * even parity.  Note that with the 1980 standard
 * there is no way to force an 8 bit data path,
 * but by convention, we do not set parity iff
 * data forwarding occurs only on timer expiry.
 */

AddParity(start, len)
register char *start;
{
	register char *end;

	if(CurrentX29Parms[X29_FORWARDING_SIGNAL_CODE] == 0)
		return;
	end = start + len;
	while(start < end) {
		*start &= 0177;
		*start |= chartab[*start] & 0200;
		start++;
	}
}
