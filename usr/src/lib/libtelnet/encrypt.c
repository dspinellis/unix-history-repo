/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)encrypt.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Copyright (C) 1990 by the Massachusetts Institute of Technology
 *
 * Export of this software from the United States of America is assumed
 * to require a specific license from the United States Government.
 * It is the responsibility of any person or organization contemplating
 * export to obtain such a license before exporting.
 *
 * WITHIN THAT CONSTRAINT, permission to use, copy, modify, and
 * distribute this software and its documentation for any purpose and
 * without fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of M.I.T. not be used in advertising or publicity pertaining
 * to distribution of the software without specific, written prior
 * permission.  M.I.T. makes no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 */

#if	defined(ENCRYPT)

#define	ENCRYPT_NAMES
#include <arpa/telnet.h>

#include "encrypt.h"
#include "misc.h"

#ifdef	__STDC__
#include <stdlib.h>
#endif
#ifdef	NO_STRING_H
#include <strings.h>
#else
#include <string.h>
#endif

/*
 * These functions pointers point to the current routines
 * for encrypting and decrypting data.
 */
void	(*encrypt_output) P((unsigned char *, int));
int	(*decrypt_input) P((int));

int encrypt_debug_mode = 0;
static int decrypt_mode = 0;
static int encrypt_mode = 0;
static int encrypt_verbose = 0;
static int autoencrypt = 0;
static int autodecrypt = 0;
static int havesessionkey = 0;
static int encrypt_mark = 0;
static int decrypt_mark = 0;
static int Server = 0;
static char *Name = "Noname";

#define	typemask(x)	((x) > 0 ? 1 << ((x)-1) : 0)

static long i_support_encrypt = typemask(ENCTYPE_KRBDES);
static long i_support_decrypt = typemask(ENCTYPE_KRBDES);
static long remote_supports_encrypt = 0;
static long remote_supports_decrypt = 0;

static Encryptions encryptions[] = {
#if	defined(KRBDES_ENCRYPT)
    { "KRBDES",		ENCTYPE_KRBDES,
			krbdes_encrypt,	
			krbdes_decrypt,
			krbdes_init,
			krbdes_start,
			krbdes_is,
			krbdes_reply,
			krbdes_session,
			krbdes_printsub },
#endif
    { 0, },
};

static unsigned char str_send[64] = { IAC, SB, TELOPT_ENCRYPT,
					 ENCRYPT_SUPPORT, };
static unsigned char str_suplen = 0;
static unsigned char str_start[] = { IAC, SB, TELOPT_ENCRYPT, 0, IAC, SE };
static unsigned char str_end[] = { IAC, SB, TELOPT_ENCRYPT, 0, IAC, SE };

	Encryptions *
findencryption(type)
	int type;
{
	Encryptions *ep = encryptions;

	if (!(i_support_encrypt & remote_supports_decrypt & typemask(type)))
		return(0);
	while (ep->type && ep->type != type)
		++ep;
	return(ep->type ? ep : 0);
}

	Encryptions *
finddecryption(type)
	int type;
{
	Encryptions *ep = encryptions;

	if (!(i_support_decrypt & remote_supports_encrypt & typemask(type)))
		return(0);
	while (ep->type && ep->type != type)
		++ep;
	return(ep->type ? ep : 0);
}

	void
encrypt_init(name, server)
	char *name;
	int server;
{
	Encryptions *ep = encryptions;

	Name = name;
	Server = server;
	i_support_encrypt = i_support_decrypt = 0;
	remote_supports_encrypt = remote_supports_decrypt = 0;
	encrypt_mode = 0;
	decrypt_mode = 0;
	encrypt_output = 0;
	decrypt_input = 0;
#ifdef notdef
	encrypt_verbose = !server;
#endif

	str_suplen = 4;

	while (ep->type) {
		if (encrypt_debug_mode)
			printf(">>>%s: I will support %s\r\n",
				Name, ENCTYPE_NAME(ep->type));
		i_support_encrypt |= typemask(ep->type);
		i_support_decrypt |= typemask(ep->type);
		if ((str_send[str_suplen++] = ep->type) == IAC)
			str_send[str_suplen++] = IAC;
		if (ep->init)
			(*ep->init)(Server);
		++ep;
	}
	str_send[str_suplen++] = IAC;
	str_send[str_suplen++] = SE;
}

	void
encrypt_list_types()
{
	Encryptions *ep = encryptions;

	printf("Valid encryption types:\n");
	while (ep->type) {
		printf("\t%s\n\n", ENCTYPE_NAME(ep->type));
		++ep;
	}
}

	int
EncryptEnable(type, mode)
	char *type, *mode;
{
	if (isprefix(type, "help") || isprefix(type, "?")) {
		printf("Usage: encrypt enable <type> [input|output]\n");
		encrypt_list_types();
		return(0);
	}
	if (EncryptType(type, mode))
		return(EncryptStart(mode));
	return(0);
}

	int
EncryptType(type, mode)
	char *type;
	char *mode;
{
	register Encryptions *ep;

	if (isprefix(type, "help") || isprefix(type, "?")) {
		printf("Usage: encrypt type <type> [input|output]\n");
		encrypt_list_types();
		return(0);
	}

	ep = (Encryptions *)genget(type, encryptions, sizeof(Encryptions));

	if (ep == 0) {
		printf("%s: invalid encryption type\n", type);
		return(0);
	}
	if (Ambiguous(ep)) {
		printf("Ambiguous type '%s'\n", type);
		return(0);
	}

	if (mode) {
		if (isprefix(mode, "input"))
			decrypt_mode = ep->type;
		else if (isprefix(mode, "output"))
			encrypt_mode = ep->type;
		else {
			printf("%s: invalid encryption mode\n", mode);
			return(0);
		}
	} else
		decrypt_mode = encrypt_mode = ep->type;
	return(1);
}

	int
EncryptStart(mode)
	char *mode;
{
	register int ret = 0;
	if (mode) {
		if (isprefix(mode, "input"))
			return(EncryptStartInput());
		if (isprefix(mode, "output"))
			return(EncryptStartOutput());
		if (isprefix(mode, "help") || isprefix(mode, "?")) {
			printf("Usage: encrypt start [input|output]\n");
			return(0);
		}
		printf("%s: invalid encryption mode 'encrypt start ?' for help\n", mode);
		return(0);
	}
	ret += EncryptStartInput();
	ret += EncryptStartOutput();
	return(ret);
}

	int
EncryptStartInput()
{
	if (decrypt_mode) {
		encrypt_send_request_start();
		return(1);
	}
	printf("No previous decryption mode, decryption not enabled\r\n");
	return(0);
}

	int
EncryptStartOutput()
{
	if (encrypt_mode) {
		encrypt_start_output(encrypt_mode);
		return(1);
	}
	printf("No previous encryption mode, encryption not enabled\r\n");
	return(0);
}

	int
EncryptStop(mode)
	char *mode;
{
	int ret = 0;
	if (mode) {
		if (isprefix(mode, "input"))
			return(EncryptStopInput());
		if (isprefix(mode, "output"))
			return(EncryptStopOutput());
		if (isprefix(mode, "help") || isprefix(mode, "?")) {
			printf("Usage: encrypt stop [input|output]\n");
			return(0);
		}
		printf("%s: invalid encryption mode 'encrypt stop ?' for help\n", mode);
		return(0);
	}
	ret += EncryptStopInput();
	ret += EncryptStopOutput();
	return(ret);
}

	int
EncryptStopInput()
{
	encrypt_send_request_end();
	return(1);
}

	int
EncryptStopOutput()
{
	encrypt_send_end();
	return(1);
}

	void
encrypt_display()
{
	if (encrypt_output)
		printf("Currently encrypting output with %s\r\n",
			ENCTYPE_NAME(encrypt_mode));
	if (decrypt_input)
		printf("Currently decrypting input with %s\r\n",
			ENCTYPE_NAME(decrypt_mode));
}

	int
EncryptStatus()
{
	if (encrypt_output)
		printf("Currently encrypting output with %s\r\n",
			ENCTYPE_NAME(encrypt_mode));
	else if (encrypt_mode) {
		printf("Currently output is clear text.\r\n");
		printf("Last encryption mode was %s\r\n",
			ENCTYPE_NAME(encrypt_mode));
	}
	if (decrypt_input) {
		printf("Currently decrypting input with %s\r\n",
			ENCTYPE_NAME(decrypt_mode));
	} else if (decrypt_mode) {
		printf("Currently input is clear text.\r\n");
		printf("Last decryption mode was %s\r\n",
			ENCTYPE_NAME(decrypt_mode));
	}
	return 1;
}

	void
encrypt_send_support()
{
	if (str_suplen) {
		/*
		 * If the user has requested that decryption start
		 * immediatly, then send a "REQUEST START" before
		 * we negotiate the type.
		 */
		if (!Server && autodecrypt)
			encrypt_send_request_start();
		net_write(str_send, str_suplen);
		printsub('>', &str_send[2], str_suplen - 2);
		str_suplen = 0;
	}
}

	int
EncryptTogDebug()
{
	encrypt_debug_mode ^= 1;
	printf("Encryption debugging %s\r\n",
		encrypt_debug_mode ? "enabled" : "disabled");
	return(1);
}

	int
EncryptTogVerbose()
{
	encrypt_verbose ^= 1;
	printf("Encryption %s verbose\r\n",
		encrypt_verbose ? "is" : "is not");
	return(1);
}

	int
EncryptTogAuto()
{
	autoencrypt ^= 1;
	autodecrypt ^= 1;
	printf("Automatic encryption of data is %s\r\n",
		autoencrypt ? "enabled" : "disabled");
	return(1);
}


/*
 * Called when ENCRYPT SUPPORT is received.
 */
	void
encrypt_support(typelist, cnt)
	unsigned char *typelist;
	int cnt;
{
	register int type, use_type = 0;
	Encryptions *ep;

	/*
	 * Forget anything the other side has previously told us.
	 */
	remote_supports_decrypt = 0;

	while (cnt-- > 0) {
		type = *typelist++;
		if (encrypt_debug_mode)
			printf(">>>%s: He is supporting %s (%d)\r\n",
				Name,
				ENCTYPE_NAME(type), type);
		if ((type < ENCTYPE_CNT) &&
		    (i_support_encrypt & typemask(type))) {
			remote_supports_decrypt |= typemask(type);
			if (use_type == 0)
				use_type = type;
		}
	}
	if (use_type) {
		ep = findencryption(use_type);
		if (!ep)
			return;
		type = ep->start ? (*ep->start)(DIR_ENCRYPT, Server) : 0;
		if (encrypt_debug_mode)
			printf(">>>%s: (*ep->start)() returned %d\r\n",
					Name, type);
		if (type < 0)
			return;
		encrypt_mode = type;
		if (type == 0)
			encrypt_start_output(use_type);
	}
}

	void
encrypt_is(data, cnt)
	unsigned char *data;
	int cnt;
{
	Encryptions *ep;
	register int type, ret;

	if (--cnt < 0)
		return;
	type = *data++;
	if (type < ENCTYPE_CNT)
		remote_supports_encrypt |= typemask(type);
	if (!(ep = finddecryption(type))) {
		if (encrypt_debug_mode)
			printf(">>>%s: Can't find type %s (%d) for initial negotiation\r\n",
				Name,
				ENCTYPE_NAME(data[-1]), data[1]);
		return;
	}
	if (!ep->is) {
		if (encrypt_debug_mode)
			printf(">>>%s: No initial negotiation needed for type %s (%d)\r\n",
				Name,
				ENCTYPE_NAME(type), type);
		ret = 0;
	} else {
		ret = (*ep->is)(data, cnt);
/*@*/		if (encrypt_debug_mode)
/*@*/			printf("(*ep->is)(%x, %d) returned %s(%d)\n", data, cnt,
/*@*/				(ret < 0) ? "FAIL " :
/*@*/				(ret == 0) ? "SUCCESS " : "MORE_TO_DO ", ret);
	}
	if (ret < 0) {
		autodecrypt = 0;
	} else {
		decrypt_mode = type;
		if (ret == 0 && autodecrypt)
			encrypt_send_request_start();
	}
}

	void
encrypt_reply(data, cnt)
	unsigned char *data;
	int cnt;
{
	Encryptions *ep;
	register int ret, type;

	if (--cnt < 0)
		return;
	type = *data++;
	if (!(ep = findencryption(type))) {
		if (encrypt_debug_mode)
			printf(">>>%s: Can't find type %s (%d) for initial negotiation\r\n",
				Name,
				ENCTYPE_NAME(data[-1]), data[1]);
		return;
	}
	if (!ep->reply) {
		if (encrypt_debug_mode)
			printf(">>>%s: No initial negotiation needed for type %s (%d)\r\n",
				Name,
				ENCTYPE_NAME(data[-1]), data[1]);
		ret = 0;
	} else {
		ret = (*ep->reply)(data, cnt);
/*@*/		if (encrypt_debug_mode)
/*@*/			printf("(*ep->reply)(%x, %d) returned %s(%d)\n",
/*@*/				data, cnt,
/*@*/				(ret < 0) ? "FAIL " :
/*@*/				(ret == 0) ? "SUCCESS " : "MORE_TO_DO ", ret);
	}
	if (encrypt_debug_mode)
		printf(">>>%s: encrypt_reply returned %d\n", Name, ret);
	if (ret < 0) {
		autoencrypt = 0;
	} else {
		encrypt_mode = type;
		if (ret == 0 && autoencrypt)
			encrypt_start_output(type);
	}
}

/*
 * Called when a ENCRYPT START command is received.
 */
	void
encrypt_start()
{
	Encryptions *ep;

	if (!decrypt_mode) {
		/*
		 * Something is wrong.  We should not get a START
		 * command without having already picked our
		 * decryption scheme.  Send a REQUEST-END to
		 * attempt to clear the channel...
		 */
		printf("%s: Warning, Cannot decrypt input stream!!!\r\n", Name);
		encrypt_send_request_end();
		return;
	}

	if (ep = finddecryption(decrypt_mode)) {
		decrypt_input = ep->input;
		if (encrypt_verbose)
			printf("[ Input is now decrypted with type %s ]\r\n",
				ENCTYPE_NAME(decrypt_mode));
		if (encrypt_debug_mode)
			printf(">>>%s: Start to decrypt input with type %s\r\n",
				Name, ENCTYPE_NAME(decrypt_mode));
	} else {
		printf("%s: Warning, Cannot decrypt type %s (%d)!!!\r\n",
				Name, ENCTYPE_NAME(decrypt_mode), decrypt_mode);
		encrypt_send_request_end();
	}
}

	void
encrypt_session_key(key, server)
	Session_Key *key;
	int server;
{
	Encryptions *ep = encryptions;

	havesessionkey = 1;

	while (ep->type) {
		if (ep->session)
			(*ep->session)(key, server);
		if (!encrypt_output && autoencrypt && !server)
			encrypt_start_output(ep->type);
		if (!decrypt_input && autodecrypt && !server)
			encrypt_send_request_start();
		++ep;
	}
}

/*
 * Called when ENCRYPT END is received.
 */
	void
encrypt_end()
{
	decrypt_input = 0;
	if (encrypt_debug_mode)
		printf(">>>%s: Input is back to clear text\r\n", Name);
	if (encrypt_verbose)
		printf("[ Input is now clear text ]\r\n");
}

/*
 * Called when ENCRYPT REQUEST-END is received.
 */
	void
encrypt_request_end()
{
	encrypt_send_end();
}

/*
 * Called when ENCRYPT REQUEST-START is received.  If we receive
 * this before a type is picked, then that indicates that the
 * other side wants us to start encrypting data as soon as we
 * can. 
 */
	void
encrypt_request_start()
{
	if (!encrypt_mode && Server) {
		autoencrypt = 1;
		return;
	}
	encrypt_start_output(encrypt_mode);
}

	void
encrypt_auto()
{
	autoencrypt = 1;
	autodecrypt = 1;
}

	void
encrypt_start_output(type)
	int type;
{
	Encryptions *ep;
	register int ret;

	if (!(ep = findencryption(type))) {
		if (encrypt_debug_mode) {
			printf(">>>%s: Marking type %s for later encryption use\r\n",
				Name,
				ENCTYPE_NAME(type));
		}
		encrypt_mark |= typemask(type);
		return;
	}
	if (ep->start) {
		ret = (*ep->start)(DIR_ENCRYPT, Server);
		if (ret) {
			if (encrypt_debug_mode) {
				if (ret < 0)
					printf(">>>%s: Start failed for %s\r\n",
						Name, ENCTYPE_NAME(type));
				else
					printf(">>>%s: Start: initial negotiation in progress%s\r\n",
						Name, ENCTYPE_NAME(type));
			}

			return;
		}
	}
	str_start[3] = ENCRYPT_START;
	net_write(str_start, sizeof(str_start));
	net_encrypt();
	printsub('>', &str_start[2], sizeof(str_start) - 2);
	/*
	 * If we are already encrypting in some mode, then
	 * encrypt the ring (which includes our request) in
	 * the old mode, mark it all as "clear text" and then
	 * switch to the new mode.
	 */
	encrypt_output = ep->output;
	encrypt_mode = type;
	if (encrypt_debug_mode)
		printf(">>>%s: Started to encrypt output with type %s\r\n",
			Name, ENCTYPE_NAME(type));
	if (encrypt_verbose)
		printf("[ Output is now encrypted with type %s ]\r\n",
			ENCTYPE_NAME(type));
}

	void
encrypt_send_end()
{
	if (!encrypt_output)
		return;

	str_end[3] = ENCRYPT_END;
	net_write(str_end, sizeof(str_end));
	net_encrypt();
	printsub('>', &str_end[2], sizeof(str_end) - 2);
	/*
	 * Encrypt the output buffer now because it will not be done by
	 * netflush...
	 */
	encrypt_output = 0;
	if (encrypt_debug_mode)
		printf(">>>%s: Output is back to clear text\r\n", Name);
	if (encrypt_verbose)
		printf("[ Output is now clear text ]\r\n");
}

	void
encrypt_send_request_start()
{
#ifdef notdef
	Encryptions *ep;

	if (!(ep = findencryption(type))) {
		if (encrypt_debug_mode) {
			printf(">>>%s: Marking type %s for later decryption use\r\n",
				Name,
				ENCTYPE_NAME(type));
		}
		decrypt_mark |= typemask(type);
		return;
	}

	if (ep->start && (*ep->start)(DIR_DECRYPT, Server)) {
		if (encrypt_debug_mode) {
			printf(">>>%s: Request failed for %s\r\n",
				Name,
				ENCTYPE_NAME(type));
		}
		return;
	}
#endif

	str_start[3] = ENCRYPT_REQSTART;
	net_write(str_start, sizeof(str_start));
	printsub('>', &str_start[2], sizeof(str_start) - 2);
	if (encrypt_debug_mode)
		printf(">>>%s: Request input to be encrypted\r\n", Name);
}

	void
encrypt_send_request_end()
{
	str_end[3] = ENCRYPT_REQEND;
	net_write(str_end, sizeof(str_end));
	printsub('>', &str_end[2], sizeof(str_end) - 2);

	if (encrypt_debug_mode)
		printf(">>>%s: Request input to be clear text\r\n", Name);
}

	void
encrypt_wait()
{
	register int encrypt, decrypt;
	if (encrypt_debug_mode)
		printf(">>>%s: in encrypt_wait\r\n", Name);
	if (!havesessionkey || !(i_support_encrypt & remote_supports_decrypt))
		return;
	while (autoencrypt && !encrypt_output)
		if (telnet_spin())
			return;
}

	void
encrypt_debug(mode)
	int mode;
{
	encrypt_debug_mode = mode;
}

	void
encrypt_gen_printsub(data, cnt, buf, buflen)
	unsigned char *data, *buf;
	int cnt, buflen;
{
	char tbuf[16], *cp;

	cnt -= 2;
	data += 2;
	buf[buflen-1] = '\0';
	buf[buflen-2] = '*';
	buflen -= 2;;
	for (; cnt > 0; cnt--, data++) {
		sprintf(tbuf, " %d", *data);
		for (cp = tbuf; *cp && buflen > 0; --buflen)
			*buf++ = *cp++;
		if (buflen <= 0)
			return;
	}
	*buf = '\0';
}

	void
encrypt_printsub(data, cnt, buf, buflen)
	unsigned char *data, *buf;
	int cnt, buflen;
{
	Encryptions *ep;
	register int type = data[1];

	for (ep = encryptions; ep->type && ep->type != type; ep++)
		;

	if (ep->printsub)
		(*ep->printsub)(data, cnt, buf, buflen);
	else
		encrypt_gen_printsub(data, cnt, buf, buflen);
}
#endif
