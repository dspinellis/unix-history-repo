/*
 *    $Source: /mit/krb5/.cvsroot/src/appl/bsd/forward.c,v $
 *    $Id: forward.c,v 1.4 1993/10/15 16:41:29 tytso Exp $
 */

#ifndef lint
static char *rcsid_forward_c =
  "$Id: forward.c,v 1.4 1993/10/15 16:41:29 tytso Exp $";
#endif /* lint */
#define LIBC_SCCS

/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)forward.c	8.1 (Berkeley) %G%";
#endif /* not lint */


/* General-purpose forwarding routines. These routines may be put into */
/* libkrb5.a to allow widespread use */ 

#if defined(KRB5) && defined(FORWARD)
#include <stdio.h>
#include <pwd.h>
#include <netdb.h>

#include <krb5/krb5.h>
#include <krb5/asn1.h>
#include <krb5/crc-32.h>
#include <krb5/los-proto.h>
#include <krb5/ext-proto.h>

#define KRB5_DEFAULT_LIFE 60*60*8   /* 8 hours */
/* helper function: convert flags to necessary KDC options */
#define flags2options(flags) (flags & KDC_TKT_COMMON_MASK)



/* Get a TGT for use at the remote host */
krb5_error_code
get_for_creds(etype, sumtype, rhost, client, enc_key, forwardable, outbuf)
     const krb5_enctype etype;
     const krb5_cksumtype sumtype;
     char *rhost;
     krb5_principal client;
     krb5_keyblock *enc_key;
     int forwardable;      /* Should forwarded TGT also be forwardable? */
     krb5_data *outbuf;
{
    struct hostent *hp;
    krb5_address **addrs;
    krb5_error_code retval;
    krb5_data *scratch;
    krb5_kdc_rep *dec_rep;
    krb5_error *err_reply;
    krb5_response tgsrep;
    krb5_creds creds, tgt;
    krb5_ccache cc;
    krb5_flags kdcoptions;
    krb5_timestamp now;
    char *remote_host;
    char **hrealms;
    int i;

    if (!rhost || !(hp = gethostbyname(rhost)))
      return KRB5_ERR_BAD_HOSTNAME;

    remote_host = (char *) malloc(strlen(hp->h_name)+1);
    if (!remote_host)
      return ENOMEM;
    strcpy(remote_host, hp->h_name);

    if (retval = krb5_get_host_realm(remote_host, &hrealms)) {
	free(remote_host);
	return retval;
    }
    if (!hrealms[0]) {
	free(remote_host);
	krb5_xfree(hrealms);
	return KRB5_ERR_HOST_REALM_UNKNOWN;
    }

    /* Count elements */
    for(i=0; hp->h_addr_list[i]; i++);

    addrs = (krb5_address **) malloc ((i+1)*sizeof(*addrs));
    if (!addrs)
      return ENOMEM;
    
    for(i=0; hp->h_addr_list[i]; i++) {
	addrs[i] = (krb5_address *) malloc(sizeof(krb5_address));
	if (addrs[i]) {
	    addrs[i]->addrtype = hp->h_addrtype;
	    addrs[i]->length   = hp->h_length;
	    addrs[i]->contents = (unsigned char *)malloc(addrs[i]->length);
	    if (!addrs[i]->contents) {
		krb5_free_addresses(addrs);
		return ENOMEM;
	    }
	    else
	      memcpy ((char *)addrs[i]->contents, hp->h_addr_list[i],
		      addrs[i]->length);
	}
	else {
	    return ENOMEM;
	}
    }
    addrs[i] = 0;

    memset((char *)&creds, 0, sizeof(creds));
    if (retval = krb5_copy_principal(client, &creds.client))
      return retval;
    
    if (retval = krb5_build_principal_ext(&creds.server,
					  strlen(hrealms[0]),
					  hrealms[0],
					  KRB5_TGS_NAME_SIZE,
					  KRB5_TGS_NAME,
					  client->realm.length,
					  client->realm.data,
					  0))
      return retval;
	
    creds.times.starttime = 0;
    if (retval = krb5_timeofday(&now)) {
	return retval;
    }
    creds.times.endtime = now + KRB5_DEFAULT_LIFE;
    creds.times.renew_till = 0;
    
    if (retval = krb5_cc_default(&cc)) {
	return retval;
    }

    /* fetch tgt directly from cache */
    if (retval = krb5_cc_retrieve_cred (cc,
					KRB5_TC_MATCH_SRV_NAMEONLY,
					&creds,
					&tgt)) {
	return retval;
    }

    /* tgt->client must be equal to creds.client */
    if (!krb5_principal_compare(tgt.client, creds.client))
	return KRB5_PRINC_NOMATCH;

    if (!tgt.ticket.length)
	return(KRB5_NO_TKT_SUPPLIED);

    kdcoptions = flags2options(tgt.ticket_flags)|KDC_OPT_FORWARDED;

    if (!forwardable) /* Reset KDC_OPT_FORWARDABLE */
      kdcoptions &= ~(KDC_OPT_FORWARDABLE);

    if (retval = krb5_send_tgs(kdcoptions, &creds.times, etype, sumtype,
			       creds.server,
			       addrs,
			       creds.authdata,
			       0,		/* no padata */
			       0,		/* no second ticket */
			       &tgt, &tgsrep))
	return retval;

#undef cleanup
#define cleanup() free(tgsrep.response.data)

    switch (tgsrep.message_type) {
    case KRB5_TGS_REP:
	break;
    case KRB5_ERROR:
    default:
	if (!krb5_is_krb_error(&tgsrep.response)) {
	    retval = KRB5KRB_AP_ERR_MSG_TYPE;
	} else
	    retval = decode_krb5_error(&tgsrep.response, &err_reply);
	if (retval) {
	    cleanup();
	    return retval;		/* neither proper reply nor error! */
	}

	retval = err_reply->error + ERROR_TABLE_BASE_krb5;

	krb5_free_error(err_reply);
	cleanup();
	return retval;
    }
    retval = krb5_decode_kdc_rep(&tgsrep.response,
				 &tgt.keyblock,
				 etype, /* enctype */
				 &dec_rep);
    
    cleanup();
    if (retval)
	return retval;
#undef cleanup
#define cleanup() {\
	memset((char *)dec_rep->enc_part2->session->contents, 0,\
	      dec_rep->enc_part2->session->length);\
		  krb5_free_kdc_rep(dec_rep); }

    if (dec_rep->msg_type != KRB5_TGS_REP) {
	retval = KRB5KRB_AP_ERR_MSG_TYPE;
	cleanup();
	return retval;
    }
    
    /* now it's decrypted and ready for prime time */

    if (!krb5_principal_compare(dec_rep->client, tgt.client)) {
	cleanup();
	return KRB5_KDCREP_MODIFIED;
    }

    if (retval = mk_cred(dec_rep, 
			 etype, 
			 enc_key,
			 0,
			 0, 
			 outbuf))
      return retval;

    krb5_free_kdc_rep(dec_rep);

    return retval;
#undef cleanup
}



/* Create asn.1 encoded KRB-CRED message from the kdc reply. */
krb5_error_code
mk_cred(dec_rep, etype, key, sender_addr, recv_addr, outbuf)
krb5_kdc_rep *dec_rep;
krb5_enctype etype;
krb5_keyblock *key;
krb5_address *sender_addr;
krb5_address *recv_addr;
krb5_data *outbuf;
{
    krb5_error_code retval;
    krb5_encrypt_block eblock;
    krb5_cred ret_cred;
    krb5_cred_enc_part cred_enc_part;
    krb5_data *scratch;

    if (!valid_etype(etype))
      return KRB5_PROG_ETYPE_NOSUPP;

    ret_cred.tickets = (krb5_ticket **) calloc(2, sizeof(*ret_cred.tickets));
    if (!ret_cred.tickets)
      return ENOMEM;
    ret_cred.tickets[0] = dec_rep->ticket;
    ret_cred.tickets[1] = 0;

    ret_cred.enc_part.etype = etype; 
    ret_cred.enc_part.kvno = 0;

    cred_enc_part.ticket_info = (krb5_cred_info **) 
      calloc(2, sizeof(*cred_enc_part.ticket_info));
    if (!cred_enc_part.ticket_info) {
	krb5_free_tickets(ret_cred.tickets);
	return ENOMEM;
    }
    cred_enc_part.ticket_info[0] = (krb5_cred_info *) 
      malloc(sizeof(*cred_enc_part.ticket_info[0]));
    if (!cred_enc_part.ticket_info[0]) {
	krb5_free_tickets(ret_cred.tickets);
	krb5_free_cred_enc_part(cred_enc_part);
	return ENOMEM;
    }
    cred_enc_part.nonce = 0;

    if (retval = krb5_us_timeofday(&cred_enc_part.timestamp,
				   &cred_enc_part.usec))
      return retval;

    cred_enc_part.s_address = (krb5_address *)sender_addr;
    cred_enc_part.r_address = (krb5_address *)recv_addr;

    cred_enc_part.ticket_info[0]->session = dec_rep->enc_part2->session;
    cred_enc_part.ticket_info[0]->client = dec_rep->client;
    cred_enc_part.ticket_info[0]->server = dec_rep->enc_part2->server;
    cred_enc_part.ticket_info[0]->flags  = dec_rep->enc_part2->flags;
    cred_enc_part.ticket_info[0]->times  = dec_rep->enc_part2->times;
    cred_enc_part.ticket_info[0]->caddrs = dec_rep->enc_part2->caddrs;

    cred_enc_part.ticket_info[1] = 0;

    /* start by encoding to-be-encrypted part of the message */

    if (retval = encode_krb5_enc_cred_part(&cred_enc_part, &scratch))
      return retval;

#define cleanup_scratch() { (void) memset(scratch->data, 0, scratch->length); krb5_free_data(scratch); }

    /* put together an eblock for this encryption */

    krb5_use_cstype(&eblock, etype);
    ret_cred.enc_part.ciphertext.length = krb5_encrypt_size(scratch->length,
						eblock.crypto_entry);
    /* add padding area, and zero it */
    if (!(scratch->data = realloc(scratch->data,
				  ret_cred.enc_part.ciphertext.length))) {
	/* may destroy scratch->data */
	krb5_xfree(scratch);
	return ENOMEM;
    }
    memset(scratch->data + scratch->length, 0,
	  ret_cred.enc_part.ciphertext.length - scratch->length);
    if (!(ret_cred.enc_part.ciphertext.data =
	  malloc(ret_cred.enc_part.ciphertext.length))) {
        retval = ENOMEM;
        goto clean_scratch;
    }

#define cleanup_encpart() {\
	(void) memset(ret_cred.enc_part.ciphertext.data, 0, \
	     ret_cred.enc_part.ciphertext.length); \
	free(ret_cred.enc_part.ciphertext.data); \
	ret_cred.enc_part.ciphertext.length = 0; \
	ret_cred.enc_part.ciphertext.data = 0;}

    /* do any necessary key pre-processing */
    if (retval = krb5_process_key(&eblock, key)) {
        goto clean_encpart;
    }

#define cleanup_prockey() {(void) krb5_finish_key(&eblock);}

    /* call the encryption routine */
    if (retval = krb5_encrypt((krb5_pointer) scratch->data,
			      (krb5_pointer)
			      ret_cred.enc_part.ciphertext.data, 
			      scratch->length, &eblock,
			      0)) {
        goto clean_prockey;
    }
    
    /* private message is now assembled-- do some cleanup */
    cleanup_scratch();

    if (retval = krb5_finish_key(&eblock)) {
        cleanup_encpart();
        return retval;
    }
    /* encode private message */
    if (retval = encode_krb5_cred(&ret_cred, &scratch))  {
        cleanup_encpart();
	return retval;
    }

    cleanup_encpart();

    *outbuf = *scratch;
    krb5_xfree(scratch);
    return 0;

 clean_prockey:
    cleanup_prockey();
 clean_encpart:
    cleanup_encpart();
 clean_scratch:
    cleanup_scratch();

    return retval;
#undef cleanup_prockey
#undef cleanup_encpart
#undef cleanup_scratch
}



/* Decode, decrypt and store the forwarded creds in the local ccache. */
krb5_error_code
rd_and_store_for_creds(inbuf, ticket, lusername)
     krb5_data *inbuf;
     krb5_ticket *ticket;
     char *lusername;
{
    krb5_encrypt_block eblock;
    krb5_creds creds;
    krb5_error_code retval;
    char ccname[35];
    krb5_ccache ccache = NULL;
    struct passwd *pwd;

    if (retval = rd_cred(inbuf, ticket->enc_part2->session, 
			 &creds, 0, 0)) {
	return(retval);
    }
    
    if (!(pwd = (struct passwd *) getpwnam(lusername))) {
	return -1;
    }

    sprintf(ccname, "FILE:/tmp/krb5cc_%d", pwd->pw_uid);

    if (retval = krb5_cc_resolve(ccname, &ccache)) {
	return(retval);
    }

    if (retval = krb5_cc_initialize(ccache,
				    ticket->enc_part2->client)) {
	return(retval);
    }

    if (retval = krb5_cc_store_cred(ccache, &creds)) {
	return(retval);
    }

    if (retval = chown(ccname+5, pwd->pw_uid, -1)) {
	return(retval);
    }

    return retval;
}



extern krb5_deltat krb5_clockskew;   
#define in_clock_skew(date) (abs((date)-currenttime) < krb5_clockskew)

/* Decode the KRB-CRED message, and return creds */
krb5_error_code
rd_cred(inbuf, key, creds, sender_addr, recv_addr)
const krb5_data *inbuf;
const krb5_keyblock *key;
krb5_creds *creds;                /* Filled in */
const krb5_address *sender_addr;  /* optional */
const krb5_address *recv_addr;    /* optional */
{
    krb5_error_code retval;
    krb5_encrypt_block eblock;
    krb5_cred *credmsg;
    krb5_cred_enc_part *credmsg_enc_part;
    krb5_data *scratch;
    krb5_timestamp currenttime;

    if (!krb5_is_krb_cred(inbuf))
	return KRB5KRB_AP_ERR_MSG_TYPE;
    
    /* decode private message */
    if (retval = decode_krb5_cred(inbuf, &credmsg))  {
	return retval;
    }
    
#define cleanup_credmsg() {(void)krb5_xfree(credmsg->enc_part.ciphertext.data); (void)krb5_xfree(credmsg);}

    if (!(scratch = (krb5_data *) malloc(sizeof(*scratch)))) {
	cleanup_credmsg();
	return ENOMEM;
    }

#define cleanup_scratch() {(void)memset(scratch->data, 0, scratch->length); (void)krb5_xfree(scratch->data);}

    if (retval = encode_krb5_ticket(credmsg->tickets[0], &scratch)) {
	cleanup_credmsg();
	cleanup_scratch();
	return(retval);
    }

    creds->ticket = *scratch;
    if (!(creds->ticket.data = malloc(scratch->length))) {
	krb5_xfree(creds->ticket.data);
	return ENOMEM;
    }
    memcpy((char *)creds->ticket.data, (char *) scratch->data, scratch->length);

    cleanup_scratch();

    if (!valid_etype(credmsg->enc_part.etype)) {
	cleanup_credmsg();
	return KRB5_PROG_ETYPE_NOSUPP;
    }

    /* put together an eblock for this decryption */

    krb5_use_cstype(&eblock, credmsg->enc_part.etype);
    scratch->length = credmsg->enc_part.ciphertext.length;
    
    if (!(scratch->data = malloc(scratch->length))) {
	cleanup_credmsg();
        return ENOMEM;
    }

    /* do any necessary key pre-processing */
    if (retval = krb5_process_key(&eblock, key)) {
        cleanup_credmsg();
	cleanup_scratch();
	return retval;
    }
    
#define cleanup_prockey() {(void) krb5_finish_key(&eblock);}
    
    /* call the decryption routine */
    if (retval = krb5_decrypt((krb5_pointer) credmsg->enc_part.ciphertext.data,
			      (krb5_pointer) scratch->data,
			      scratch->length, &eblock,
			      0)) {
	cleanup_credmsg();
	cleanup_scratch();
        cleanup_prockey();
	return retval;
    }

    /* cred message is now decrypted -- do some cleanup */

    cleanup_credmsg();

    if (retval = krb5_finish_key(&eblock)) {
        cleanup_scratch();
        return retval;
    }

    /*  now decode the decrypted stuff */
    if (retval = decode_krb5_enc_cred_part(scratch, &credmsg_enc_part)) {
	cleanup_scratch();
	return retval;
    }
    cleanup_scratch();

#define cleanup_mesg() {(void)krb5_xfree(credmsg_enc_part);}

    if (retval = krb5_timeofday(&currenttime)) {
	cleanup_mesg();
	return retval;
    }
    if (!in_clock_skew(credmsg_enc_part->timestamp)) {
	cleanup_mesg();  
	return KRB5KRB_AP_ERR_SKEW;
    }

    if (sender_addr && credmsg_enc_part->s_address &&
	!krb5_address_compare(sender_addr, 
			      credmsg_enc_part->s_address)) {
	cleanup_mesg();
	return KRB5KRB_AP_ERR_BADADDR;
    }
    if (recv_addr && credmsg_enc_part->r_address &&
	!krb5_address_compare(recv_addr, 
			      credmsg_enc_part->r_address)) {
	cleanup_mesg();
	return KRB5KRB_AP_ERR_BADADDR;
    }	    

    if (credmsg_enc_part->r_address) {
	krb5_address **our_addrs;
	
	if (retval = krb5_os_localaddr(&our_addrs)) {
	    cleanup_mesg();
	    return retval;
	}
	if (!krb5_address_search(credmsg_enc_part->r_address, 
				 our_addrs)) {
	    krb5_free_addresses(our_addrs);
	    cleanup_mesg();
	    return KRB5KRB_AP_ERR_BADADDR;
	}
	krb5_free_addresses(our_addrs);
    }

    if (retval = krb5_copy_principal(credmsg_enc_part->ticket_info[0]->client,
				     &creds->client)) {
	return(retval);
    }

    if (retval = krb5_copy_principal(credmsg_enc_part->ticket_info[0]->server,
				     &creds->server)) {
	return(retval);
    }  

    if (retval =
	krb5_copy_keyblock_contents(credmsg_enc_part->ticket_info[0]->session, 
				    &creds->keyblock)) {
	return(retval);
    }

#undef clean
#define clean() {\
	memset((char *)creds->keyblock.contents, 0, creds->keyblock.length);}

    creds->times = credmsg_enc_part->ticket_info[0]->times;
    creds->is_skey = FALSE;
    creds->ticket_flags = credmsg_enc_part->ticket_info[0]->flags;

    if (retval = krb5_copy_addresses(credmsg_enc_part->ticket_info[0]->caddrs,
				     &creds->addresses)) {
	clean();
	return(retval);
    }

    creds->second_ticket.length = 0;

    creds->authdata = 0;

    cleanup_mesg();
    return 0;
#undef clean
#undef cleanup_credmsg
#undef cleanup_scratch
#undef cleanup_prockey
#undef cleanup_mesg
}

#endif /* defined(KRB5) && defined(FORWARD) */
