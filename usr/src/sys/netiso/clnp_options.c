/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* $Header: /var/src/sys/netiso/RCS/clnp_options.c,v 5.1 89/02/09 16:20:37 hagens Exp $ */
/* $Source: /var/src/sys/netiso/RCS/clnp_options.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /var/src/sys/netiso/RCS/clnp_options.c,v 5.1 89/02/09 16:20:37 hagens Exp $";
#endif lint

#ifdef ISO

#include "../h/types.h"
#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/domain.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"

#include "iso.h"
#include "clnp.h"
#include "clnp_stat.h"
#include "argo_debug.h"

/*
 * FUNCTION:		clnp_update_srcrt
 *
 * PURPOSE:			Process src rt option accompanying a clnp datagram.
 *						- bump src route ptr if src routing and
 *							we appear current in src route list.
 *
 * RETURNS:			none
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			If source routing has been terminated, do nothing.
 */
clnp_update_srcrt(options, oidx)
struct mbuf			*options;	/* ptr to options mbuf */
struct clnp_optidx	*oidx;		/* ptr to option index */
{
	u_char			len;	/* length of current address */
	struct iso_addr	isoa;	/* copy current address into here */

	if (CLNPSRCRT_TERM(oidx, options)) {
		IFDEBUG(D_OPTIONS)
			printf("clnp_update_srcrt: src rt terminated\n");
		ENDDEBUG
		return;
	}

	len = CLNPSRCRT_CLEN(oidx, options);
	bcopy(CLNPSRCRT_CADDR(oidx, options), (caddr_t)&isoa, len);
	isoa.isoa_len = len;
		
	IFDEBUG(D_OPTIONS)
		printf("clnp_update_srcrt: current src rt: %s\n", 
			clnp_iso_addrp(&isoa));
	ENDDEBUG

	if (clnp_ours(&isoa)) {
		IFDEBUG(D_OPTIONS)
			printf("clnp_update_srcrt: updating src rt\n");
		ENDDEBUG

		/* update pointer to next src route */
		len++;	/* count length byte too! */
		CLNPSRCRT_OFF(oidx, options) += len;
	}
}

/*
 * FUNCTION:		clnp_dooptions
 *
 * PURPOSE:			Process options accompanying a clnp datagram.
 *					Processing includes
 *						- log our address if recording route
 *
 * RETURNS:			none
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
clnp_dooptions(options, oidx, ifp, isoa)
struct mbuf			*options;	/* ptr to options mbuf */
struct clnp_optidx	*oidx;		/* ptr to option index */
struct ifnet		*ifp;		/* ptr to interface pkt is leaving on */
struct iso_addr		*isoa;		/* ptr to our address for this ifp */
{
	/*
	 *	If record route is specified, move all
	 *	existing records over, and insert the address of
	 *	interface passed
	 */
	if (oidx->cni_recrtp) {
		char 	*opt;			/* ptr to beginning of recrt option */
		u_char	off;			/* offset from opt of first free byte */
		char	*rec_start;		/* beginning of first record rt option */

		opt = CLNP_OFFTOOPT(options, oidx->cni_recrtp);
		off = *(opt + 1);
		rec_start = opt + 2;

		IFDEBUG(D_OPTIONS)
			printf("clnp_dooptions: record route: option x%x for %d bytes\n",
				opt, oidx->cni_recrt_len);
			printf("\tfree slot offset x%x\n", off);
			printf("clnp_dooptions: recording %s\n", clnp_iso_addrp(isoa));
			printf("clnp_dooptions: option dump:\n");
			dump_buf(opt, oidx->cni_recrt_len);
		ENDDEBUG

		/* proceed only if recording has not been terminated */
		if (off != 0xff) {
			/* 
			 *	if there is insufficient room to store the next address,
			 *	then terminate recording. Plus 1 on isoa_len is for the
			 *	length byte itself
			 */
			if (oidx->cni_recrt_len - off < isoa->isoa_len+1) {
				*(opt + 1) = 0xff;	/* terminate recording */
			} else {
				int new_addrlen = isoa->isoa_len + 1;
				IFDEBUG(D_OPTIONS)
					printf("clnp_dooptions: clnp_ypocb(x%x, x%x, %d)\n",
						rec_start, rec_start + new_addrlen, off - 3);
				ENDDEBUG
						
				/* move existing records over */
				clnp_ypocb(rec_start, rec_start + new_addrlen, off - 3);

				IFDEBUG(D_OPTIONS)
					printf("clnp_dooptions: new addr at x%x for %d\n",
						rec_start, new_addrlen);
				ENDDEBUG

				/* add new record */
				*rec_start = isoa->isoa_len;
				bcopy((caddr_t)isoa, rec_start + 1, isoa->isoa_len);

				/* update offset field */
				*(opt + 1) = off + new_addrlen;

				IFDEBUG(D_OPTIONS)
					printf("clnp_dooptions: new option dump:\n");
					dump_buf(opt, oidx->cni_recrt_len);
				ENDDEBUG
			}
		}
	}
}

/*
 * FUNCTION:		clnp_set_opts
 *
 * PURPOSE:			Check the data mbuf passed for option sanity. If it is
 *					ok, then set the options ptr to address the data mbuf.
 *					If an options mbuf exists, free it. This implies that
 *					any old options will be lost. If data is NULL, simply
 *					free any old options.
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
clnp_set_opts(options, data)
struct mbuf	**options;	/* target for option information */
struct mbuf	**data;		/* source of option information */
{
	int					error = 0;	/* error return value */
	struct clnp_optidx	dummy;		/* dummy index - not used */

	/*
	 *	remove any existing options
	 */
	if (*options != NULL) {
		m_freem(*options);
		*options = NULL;
	}

	if (*data != NULL) {
		/*
		 *	Insure that the options are reasonable.
		 *
		 *	Also, we do not support security, priority, or QOS
		 *	nor do we allow one to send an ER option
		 */
		if ((clnp_opt_sanity(*data, mtod(*data, caddr_t), (*data)->m_len, 
			&dummy) != 0) ||
				(dummy.cni_securep) ||
				(dummy.cni_priorp) ||
				(dummy.cni_qos_formatp) ||
				(dummy.cni_er_reason != ER_INVALREAS)) {
			error = EINVAL;
		} else {
			*options = *data;
			*data = NULL;	/* so caller won't free mbuf @ *data */
		}
	}
	return error;
}

/*
 * FUNCTION:		clnp_opt_sanity
 *
 * PURPOSE:			Check the options (beginning at opts for len bytes) for
 *					sanity. In addition, fill in the option index structure 
 *					in with information about each option discovered.
 *
 * RETURNS:			success (options check out) - 0
 *					failure - an ER pdu error code describing failure
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Each pointer field of the option index is filled in with
 *					the offset from the beginning of the mbuf data, not the
 *					actual address.
 */
clnp_opt_sanity(m, opts, len, oidx)
struct mbuf 		*m;		/* mbuf options reside in */
caddr_t				opts;	/* ptr to buffer containing options */
int					len;	/* length of buffer */
struct clnp_optidx	*oidx;	/* RETURN: filled in with option idx info */
{
	u_char	opcode;			/* code of particular option */
	u_char	oplen;			/* length of a particular option */
	caddr_t	opts_end;		/* ptr to end of options */
	u_char	pad = 0, secure = 0, srcrt = 0, recrt = 0, qos = 0, prior = 0;
							/* flags for catching duplicate options */
	
	IFDEBUG(D_OPTIONS)
		printf("clnp_opt_sanity: checking %d bytes of data:\n", len);
		dump_buf(opts, len);
	ENDDEBUG

	/* clear option index field if passed */
	bzero((caddr_t)oidx, sizeof(struct clnp_optidx));

	/*
	 *	We need to indicate whether the ER option is present. This is done
	 *	by overloading the er_reason field to also indicate presense of
	 *	the option along with the option value. I would like ER_INVALREAS
	 *	to have value 0, but alas, 0 is a valid er reason...
	 */
	oidx->cni_er_reason = ER_INVALREAS;

	opts_end = opts + len;
	while (opts < opts_end) {
		/* must have at least 2 bytes per option (opcode and len) */
		if (opts + 2 > opts_end)
			return(GEN_INCOMPLETE);
		
		opcode = *opts++;
		oplen = *opts++;
		IFDEBUG(D_OPTIONS)
			printf("clnp_opt_sanity: opcode is %x and oplen %d\n",
				opcode, oplen);
			printf("clnp_opt_sanity: clnpoval_SRCRT is %x\n", CLNPOVAL_SRCRT);

				switch (opcode) {
					case CLNPOVAL_PAD: {
						printf("CLNPOVAL_PAD\n");
					} break;
					case CLNPOVAL_SECURE: {
						printf("CLNPOVAL_SECURE\n");
					} break;
					case CLNPOVAL_SRCRT: {
							printf("CLNPOVAL_SRCRT\n");
					} break;
					case CLNPOVAL_RECRT: {
						printf("CLNPOVAL_RECRT\n");
					} break;
					case CLNPOVAL_QOS: {
						printf("CLNPOVAL_QOS\n");
					} break;
					case CLNPOVAL_PRIOR: {
						printf("CLNPOVAL_PRIOR\n");
					} break;
					case CLNPOVAL_ERREAS: {
						printf("CLNPOVAL_ERREAS\n");
					} break;
					default:
						printf("UKNOWN option %x\n", opcode);
				}
		ENDDEBUG

		/* don't allow crazy length values */
		if (opts + oplen > opts_end)
			return(GEN_INCOMPLETE);

		switch (opcode) {
			case CLNPOVAL_PAD: {
				/*
				 *	Padding: increment pointer by length of padding
				 */
				if (pad++)						/* duplicate ? */
					return(GEN_DUPOPT);
				opts += oplen;
			} break;

			case CLNPOVAL_SECURE: {
				u_char	format = *opts;

				if (secure++)					/* duplicate ? */
					return(GEN_DUPOPT);
				/*
				 *	Security: high 2 bits of first octet indicate format
				 *	(00 in high bits is reserved).
				 *	Remaining bits must be 0. Remaining octets indicate
				 *	actual security
				 */
				if (((format & 0x3f) > 0) ||	/* low 6 bits set ? */
					((format & 0xc0) == 0))		/* high 2 bits zero ? */
					return(GEN_HDRSYNTAX);

				oidx->cni_securep = CLNP_OPTTOOFF(m, opts);
				oidx->cni_secure_len = oplen;
				opts += oplen;
			} break;

			case CLNPOVAL_SRCRT: {
				u_char	type, offset;	/* type of rt, offset of start */
				caddr_t	route_end;		/* address of end of route option */

				IFDEBUG(D_OPTIONS)
					printf("clnp_opt_sanity: SRC RT\n");
				ENDDEBUG

				if (srcrt++)					/* duplicate ? */
					return(GEN_DUPOPT);
				/* 
				 *	source route: There must be 2 bytes following the length
				 *	field: type and offset. The type must be either
				 *	partial route or complete route. The offset field must
				 *	be within the option. A single exception is made, however.
				 *	The offset may be 1 greater than the length. This case 
				 *	occurs when the last source route record is consumed. 
				 *	In this case, we ignore the source route option.
				 *	RAH? You should be able to set offset to 'ff' like in record
				 *	route!
				 *	Following this is a series of address fields. 
				 *	Each address field is composed of a (length, address) pair.
				 *	Insure that the offset and each address length is reasonable
				 */
				route_end = opts + oplen;

				if (opts + 2 > route_end)
					return(SRCRT_SYNTAX);

				type = *opts;
				offset = *(opts+1);


				/* type must be partial or complete */
				if (!((type == CLNPOVAL_PARTRT) || (type == CLNPOVAL_COMPRT)))
					return(SRCRT_SYNTAX);
				
				oidx->cni_srcrt_s = CLNP_OPTTOOFF(m, opts);
				oidx->cni_srcrt_len = oplen;

				opts += offset-1;	/*set opts to first addr in rt */

				/* 
				 *	Offset must be reasonable:
				 *	less than end of options, or equal to end of options
				 */
				if (opts >= route_end) {
					if (opts == route_end) {
						IFDEBUG(D_OPTIONS)
							printf("clnp_opt_sanity: end of src route info\n");
						ENDDEBUG
						break;
					} else 
						return(SRCRT_SYNTAX);
				}

				while (opts < route_end) {
					u_char	addrlen = *opts++;
					if (opts + addrlen > route_end)
						return(SRCRT_SYNTAX);
					opts += addrlen;
				}
			} break;
			case CLNPOVAL_RECRT: {
				u_char	type, offset;	/* type of rt, offset of start */
				caddr_t	record_end;		/* address of end of record option */

				if (recrt++)					/* duplicate ? */
					return(GEN_DUPOPT);
				/*
				 *	record route: after the length field, expect a
				 *	type and offset. Type must be partial or complete.
				 *	Offset indicates where to start recording. Insure it
				 *	is within the option. All ones for offset means
				 *	recording is terminated.
				 */
				record_end = opts + oplen;

				oidx->cni_recrtp = CLNP_OPTTOOFF(m, opts);
				oidx->cni_recrt_len = oplen;

				if (opts + 2 > record_end)
					return(GEN_INCOMPLETE);

				type = *opts;
				offset = *(opts+1);

				/* type must be partial or complete */
				if (!((type == CLNPOVAL_PARTRT) || (type == CLNPOVAL_COMPRT)))
					return(GEN_HDRSYNTAX);
				
				/* offset must be reasonable */
				if ((offset < 0xff) && (opts + offset > record_end))
					return(GEN_HDRSYNTAX);
				opts += oplen;
			} break;
			case CLNPOVAL_QOS: {
				u_char	format = *opts;

				if (qos++)					/* duplicate ? */
					return(GEN_DUPOPT);
				/*
				 *	qos: high 2 bits of first octet indicate format
				 *	(00 in high bits is reserved).
				 *	Remaining bits must be 0 (unless format indicates
				 *	globally unique qos, in which case remaining bits indicate
				 *	qos (except bit 6 which is reserved)).  Otherwise,
				 *	remaining octets indicate actual qos.
				 */
				if (((format & 0xc0) == 0) ||	/* high 2 bits zero ? */
					(((format & 0xc0) != CLNPOVAL_GLOBAL) && 
						((format & 0x3f) > 0))) /* not global,low bits used ? */
					return(GEN_HDRSYNTAX);
				
				oidx->cni_qos_formatp = CLNP_OPTTOOFF(m, opts);
				oidx->cni_qos_len = oplen;

				opts += oplen;
			} break;

			case CLNPOVAL_PRIOR: {
				if (prior++)				/* duplicate ? */
					return(GEN_DUPOPT);
				/*
				 *	priority: value must be one byte long
				 */
				if (oplen != 1)
					return(GEN_HDRSYNTAX);
				
				oidx->cni_priorp = CLNP_OPTTOOFF(m, opts);

				opts += oplen;
			} break;

			case CLNPOVAL_ERREAS: {
				/*
				 *	er reason: value must be two bytes long
				 */
				if (oplen != 2)
					return(GEN_HDRSYNTAX);

				oidx->cni_er_reason = *opts;

				opts += oplen;
			} break;

			default: {
				IFDEBUG(D_OPTIONS)
					printf("clnp_opt_sanity: UNKNOWN OPTION 0x%x\n", opcode);
				ENDDEBUG
				return(DISC_UNSUPPOPT);
			}
		}
	}
		IFDEBUG(D_OPTIONS)
			printf("clnp_opt_sanity: return(0)\n", opcode);
		ENDDEBUG
	return(0);
}
#endif	ISO
