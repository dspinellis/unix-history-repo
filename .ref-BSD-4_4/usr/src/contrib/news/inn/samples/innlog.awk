##  $Revision: 1.7 $
##  @(#) newsinfo.awk	falcon@cats.ucsc.edu (jon r. luini)	7/4/92
##  Modified by Robert Elz to understand more reports.
##  Modified by Rich $alz for consistent formatting.
##  Modified by Chris Schmidt to sort output.
BEGIN {
    unknowns = 0;
    batcher = 0;
    client_timeout = 0;
    ctlinnd_new = 0;
    ctlinnd_rmg = 0;
    ctlinnd_seen = 0;
    innd = 0;
    innd_bad_ihaves = 0;
    innd_bad_msgids = 0;
    innd_bad_sendmes = 0;
    innd_blockeds = 0;
    innd_change_groups = 0;
    innd_newgroups = 0;
    innd_pauses = 0;
    innd_points = 0;
    innd_reloads = 0;
    innd_restarts = 0;
    innd_rmgroups = 0;
    innd_shutdowns = 0;
    innd_throttles = 0;
    innxmit = 0;
    invalid = "~~~~~~~~~~~~~~~~~~~~~~~~"
    mthreads = 0;
    mthreads_added = 0;
    mthreads_expired = 0;
    mthreads_started = 0;
    mthreads_turned_off = 0;
    mthreads_turned_on = 0;
    nnrp = 0;
    nnrp_gethostfails = 0;
    nnrp_noperms = 0;
    nnrp_readers = 0;
    nnrp_unrecs = 0;
    nntplink = 0;
    rnews_bad = 0;
    rnews_bad_date = 0;
    rnews_bad_dist = 0;
    rnews_bad_ng = 0;
    rnews_host = 0;
    rnews_host = 0;
    rnews_rejects = 0;
    server_timeout = 0;

    ctlinnd["a"] = "addhist";
    ctlinnd["D"] = "allow";
    ctlinnd["b"] = "begin";
    ctlinnd["c"] = "cancel";
    ctlinnd["u"] = "changegroup";
    ctlinnd["d"] = "checkfile";
    ctlinnd["e"] = "drop";
    ctlinnd["f"] = "flush";
    ctlinnd["g"] = "flushlogs";
    ctlinnd["h"] = "go";
    ctlinnd["i"] = "hangup";
    ctlinnd["s"] = "mode";
    ctlinnd["j"] = "name";
    ctlinnd["k"] = "newgroup";
    ctlinnd["l"] = "param";
    ctlinnd["m"] = "pause";
    ctlinnd["v"] = "readers";
    ctlinnd["t"] = "refile";
    ctlinnd["C"] = "reject";
    ctlinnd["o"] = "reload";
    ctlinnd["n"] = "renumber";
    ctlinnd["z"] = "reserve";
    ctlinnd["p"] = "rmgroup";
    ctlinnd["A"] = "send";
    ctlinnd["q"] = "shutdown";
    ctlinnd["B"] = "signal";
    ctlinnd["r"] = "throttle";
    ctlinnd["w"] = "trace";
    ctlinnd["x"] = "xabort";
    ctlinnd["y"] = "xexec";
}


##
##  SYSLOG
##
$5 == "last" && $6 == "message" && $7 == "repeated" {
    ## Skip.
    next;
}


##
##  NNRP
##
$5 ~ /nnrpd.*/ {
    host = $6;

    if ($7 == "connect") {
	nnrp_connect[host]++;
	nnrp = 1;
	next;
    }

    if ($7 == "exit") {
	nnrp_articles[host] += $9;
	nnrp_groups[host] += $11;
	next;
    }

    if ($7 == "posts") {
	nnrp_posts[host] += $9;
	nnrp_rejected[host] += $11;
	next;
    }

    if ($7 == "times") {
	nnrp_times_user[host] += $9;
	nnrp_times_sys[host] += $11;
	nnrp_times_elapsed[host] += $13;
	next;
    }

    if ($7 == "group") {
	nnrp_readers = 1;
	nnrp_group_request[$8]++;
	next;
    }

    if ($7 == "post") {
	##  Skip; handled in "posts" above.
	next;
    }

    if ($7 == "unrecognized") {
	nnrp_unrecs = 1;
	nnrp_unrec[host]++;
	next;
    }

    if ($7 == "no_permission") {
	nnrp_noperms = 1;
	nnrp_noperm[host]++;
	next;
    }

    if ($7 == "timeout") {
	client_timeout = 1;
	nnrp_timeout[host]++;
	next;
    }

    if ($6 == "gethostbyaddr:") {
	nnrp_gethostfails = 1;
	nnrp_gethostfail[$7]++;
	next;
    }

    if ($7 == "cant" && $8 == "gethostbyaddr") {
	##  Skip; handled in "gethostbyaddr:" above.
	next;
    }
}


##
## INND
##
$5 == "innd:" {
    innd = 1;
    if ( $6 ~ /^[a-zA-Z]:.*/ || $6 ~ /^[dgs]$/ ) {
	##  A ctlinnd command.
	##  Note that the last parameter in $6 may be continued in $7 etc
	n = split($6, ctl, ":");
	if (ctlinnd[ctl[1]] == "") {
	    ##  Unknown; update this script!
	    print;
	    next;
	}
	ctlinnd_type[ctl[1]]++;
	ctlinnd_seen++;

	if (ctl[1] == "k") {
	    ##  Newgroup.  Process it here so we can get the creator.
	    ctlinnd_new++;
	    ctlinnd_newgroups[ctl[2]] = ctl[4];
	    next;
	}
	if (ctl[1] == "p") {
	    ##  Rmgroup.  Process it here for symmetry.
	    ctlinnd_rmg++;
	    ctlinnd_rmgroups[ctl[2]] = 1;
	    next;
	}

	## Skip others for now.
	next;
    }

    if ( $6 ~ /.*:.*/ ) {
	n = split($6, path, ":");
	host = path[1];
    }
    else
	host = $6;

    if ($7 == "connected") {
	##  Account for the fact that innd says "local connected ##"
	##  and then "localhost:## closed"
	if (host == "local") {
	    host = "localhost";
	}
	innd_connect[host]++;
	next;
    }

    if ($7 == "closed") {
	innd_seconds[host] += $9;
	innd_accepted[host] += $11;
	innd_refused[host] += $13;
	innd_rejected[host] += $15;
	innd_chkp[host] = 0;
	if (cp_innd_chkp[host] == 1) {
	    cp_innd_chkp[host] = 0;
	    innd_points--;
	}
	next;
    }
    if ($7 == "checkpoint") {
	cp_innd_seconds[host] = $9;
	cp_innd_accepted[host] = $11;
	cp_innd_refused[host] = $13;
	cp_innd_rejected[host] = $15;
	if (cp_innd_chkp[host] == 0) {
	    cp_innd_chkp[host] = 1;
	    innd_points++;
	}
	next;
    }

    if ($6 ~ /\/.*/) {
	##  Skip; handled by "opened/spawned" below.
	next;
    }

    if ($7 == "flush") {
	innd_flush[host]++;
	next;
    }

    if ($7 == "timeout") {
	innd_timeout[host]++;
	next;
    }

    if ($7 == "opened" || $7 == "spawned") {
	n = split($8, blarg, ":");
	innd_feed[host] = blarg[n];
	next;
    }

    if ($7 == "newgroup") {
	innd_newgroups++;
	next;
    }

    if ($7 == "rmgroup") {
	innd_rmgroups++;
	next;
    }

    if ($7 == "paused") {
	innd_pauses++;
	next;
    }

    if ($7 == "throttled") {
	innd_throttles++;
	next;
    }

    if ($7 == "reload") {
	innd_reloads++;
	next;
    }

    if ($7 == "change_group") {
	innd_change_groups++;
	next;
    }

    if ($7 == "shutdown") {
	innd_shutdowns++;
	next;
    }

    if ($7 == "starting") {
	innd_restarts++;

	if (innd_points != 0) {
	    ##  Unfinished checkpoints from before; something crashed.
	    for (s in cp_innd_chkp) {
		if (cp_innd_chkp[s] == 0)
			continue;
		innd_seconds[s] += cp_innd_seconds[s];
		innd_accepted[s] += cp_innd_accepted[s];
		innd_refused[s] += cp_innd_refused[s];
		innd_rejected[s] += cp_innd_rejected[s];
		cp_innd_chkp[s] = 0;
	    }
	    innd_points = 0;
	}
	next;
    }

    if ($7 == "bad_ihave") {
	innd_bad_ihave[host]++;
	innd_bad_ihaves = 1;
	next;
    }

    if ($7 == "ihave_from_me") {
	next;
    }

    if ($7 == "bad_messageid") {
	innd_bad_msgid[host]++;
	innd_bad_msgids = 1;
	next;
    }

    if ($7 == "bad_sendme") {
	innd_bad_sendme[host]++;
	innd_bad_sendmes = 1;
	next;
    }

    if ($7 == "blocked" && $8 == "sleeping") {
	innd_blocked[host]++;
	innd_blockeds = 1;
	next;
    }

    if ($7 == "exit" && $8 == "0") {
	##  Program or process finished normally; don't care.
	next;
    }

    if ($7 == "wakeup" || ($7 == "cant" && ($8 == "write" || $8 == "read"))) {
	##  Skip; handled by "blocked/sleeping" above.
	next;
    }

    if ($7 == "inactive"	|| $7 == "descriptors"	\
     || $7 == "outgoing"	|| $7 == "running"	\
     || $7 == "lcsetup"		|| $7 == "ccsetup"	\
     || $7 == "rcsetup"		|| $7 == "readclose"	\
     || $7 == "flush_all"	\
    ) {
	##  Skip; for now.
	next;
    }
}


##
##  BATCHER
##
$5 ~ /batcher.*/ {
    host = $7;

    ## Since times lines are always followed by stats lines, we
    ## only do the batcher_site[host]++ and batcher=1 once
    if ($8 == "times") {
	batcher_times_user[host] += $10;
	batcher_times_sys[host] += $12;
	batcher_times_elapsed[host] += $14;
	batcher_site[host]++;
	batcher = 1;
	next;
    }

    if ($8 == "stats") {
	batcher_num[host] += $10;
	batcher_articles[host] += $12;
	batcher_bytes[host] += $14;
	next;
    }
}


##
##  INNXMIT
##
$5 ~ /innxmit.*/ {
    if ( $6 ~ /:/ ) {
	n = split($6, path, ":");
	host = path[1];
    }
    else
	host = $6;

    if ($7 == "ihave" && $8 == "failed") {
	## ihave failed occurs when the server rejects an article,
	## and requests it to be resent at the ihave stage - this
	## always indicates a server problem (all it has of the article
	## is its message-id) most commonly "out of space" - in that
	## case we don't really want to count this as an offered
	## article, so remember this happened so we can reduce the
	## offered count below.

	innxmit_ihfail[host] = 1;

	if ($9 == "436" && $11 == "NNTP" && $13 == "out" && $15 ~ /space/) {
	    innxmit_nospace[host]++;
	    next;
	}
    }

    ## Since stats lines are always followed by times lines we
    ## only do the innxmit_site[host]++ and innxmit=1 once
    if ($7 == "stats") {
	innxmit_offered[host] += $9 - innxmit_ihfail[host];
	innxmit_accepted[host] += $11;
	innxmit_rejected[host] += $13;
	innxmit_failed[host] += $15;
	innxmit_ihfail[host] = 0;
	innxmit_site[host]++;
	innxmit = 1;
	next;
    }

    if ($7 == "times") {
	innxmit_times_user[host] += $9;
	innxmit_times_sys[host] += $11;
	innxmit_times_elapsed[host] += $13;
	next;
    }

    if ($7 == "requeued") {
	r = $9;
	for (i = 10; i < NF; i++)
	    r = r " " $i;
	innxmit_reQ_host[host]++;
	innxmit_reQ_reason[r]++;
	next;
    }

    if ($7 == "connect" && $9 == "400" \
     && (($10 == "No" && $11 == "space") \
       || ($11 == "NNTP" && $13 == "out" && $15 ~ /space/))) {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_nospace[host]++;
	next;
    }

    if ($7 == "connect" && $9 == "400" && $10 == "loadav") {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_hiload[host]++;
	next;
    }

    if ($7 == "connect" && $9 == "400" && $0 ~ /[Ee][Xx][Pp][Ii][Rr]/) {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_expire[host]++;
	next;
    }

    if ($7 == "connect" && $9 == "400") {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_crefused[host]++;
	next;
    }

    if ($7 == "connect" && $8 == "failed") {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_cfail_host[host]++;

	##  Later, maybe.
	#r = $9;
	#for (i = 10; i < NF; i++)
	#    r = r " " $i;
	#innxmit_cfail_reason[r]++;
	next;
    }

    if ($7 == "authenticate" && $8 == "failed") {
	##  There is no startup marked for this.
	innxmit_site[host]++;
	innxmit_afail_host[host]++;

	##  Later, maybe.
	#r = $9;
	#for (i = 10; i < NF; i++)
	#	r = r " " $i;
	#innxmit_afail_reason[r]++;
	next;
    }
}

$5 ~ /rnews.*/ {
    if ($6 == "rejected") {
	if ($7 == "connection") {
	    rnews_rejects++;
	    i = 8;
	    if ($8 == "400")
		i++;
	    n = $i;
	    for (i++; i <= NF; i++)
		n = n " " $i;
	    rnews_r_reject[n]++;
	    next;
	}
	if ($7 == "437") {
	    rnews_bad++;
	    if ($8 == "Unwanted" && $9 == "newsgroup") {
		rnews_bad_ng++;
		rnews_bng[$10]++;
		next;
	    }
	    if ($8 == "Unwanted" && $9 == "distribution") {
		rnews_bad_dist++;
		rnews_bdist[$10]++;
		next;
	    }
	    if ($8 == "Bad" && $9 == "\"Date\"") {
		rnews_bad_date++;
		next;
	    }
	}
    }
    if ($6 == "offered") {
	rnews_hosts[$8]++;
	rnews_host = 1;
	next;
    }
}


##
##  NNTPLINK
##
$5 ~ /.*nntplink.*/ {
    if ( $6 ~ /:/ ) {
	n = split($6, path, ":");
	host = path[1];
    }
    else
	host = $6;

    if ($7 == "EOF") {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_eof[host]++;
	next;
    }

    if ($10 == "Broken" && $11 == "pipe") {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_bpipe[host]++;
	next;
    }

    if ($7 == "greeted" && $10 == "400" \
     && (($11 == "No" && $12 == "space") \
       || ($12 == "NNTP" && $14 == "out" && $16 ~ /space/))) {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_nospace[host]++;
	next;
    }

    if ($7 == "greeted" && $10 == "400" && $11 == "loadav") {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_hiload[host]++;
	next;
    }

    if ($7 == "greeted" && $10 == "400" && $0 ~ /[Ee][Xx][Pp][Ii][Rr]/) {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_expire[host]++;
	next;
    }

    if ($7 == "greeted" && $10 == "400") {
	##  Some other failure, or innd throttle for some local reason.
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_fail[host]++;
	next;
    }

    if ($7 == "socket():") {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_sockerr[host]++;
	next;
    }

    if ($7 == "connection" && $8 == "timed" && $9 == "out") {
	##  There is no startup marked for this.  Fake reason.
	nntplink_bpipe[host]++;
	nntplink_site[host]++;
	next;
    }

    if ($7 == "sent" && $8 == "authinfo" && $10 == "exiting") {
	##  There is no startup marked for this.
	nntplink_site[host]++;
	nntplink_auth[host]++;
    }

    if ($7 == "sent" && $8 == "IHAVE") {
	##  An "ihave failure":  site rejected the article after the
	##  IHAVE command; most likely it was out of space.  Don't
	##  count this as an offered article, so remember it.  Since
	##  nntplink keeps trying to send we could get many such
	##  failures in a single connection.  For counting purposes
	##  we pretend that there were several separate connections.
	nntplink_ihfail[host]++;

	if ($11 == "436" && $13 == "NNTP" && $15 == "out" && $17 ~ /space/) {
	    nntplink_fake_connects[host]++;
	    nntplink_nospace[host]++;
	    next;
	}
    }

    ## Since stats lines are always followed by xmit lines, we
    ## only do the nntplink_site[host]++ and nntplink=1 once
    if ($7 == "stats") {
	nntplink_offered[host] += $8 - nntplink_ihfail[host];
	nntplink_accepted[host] += $10;
	nntplink_rejected[host] += $12;
	nntplink_failed[host] += $14;
	nntplink_ihfail[host] = 0;
	if (nntplink_fake_connects[host]) {
	    nntplink_site[host] += nntplink_fake_connects[host];
	    nntplink_fake_connects[host] = 0;
	} else {
	    nntplink_site[host]++;
	}
	nntplink = 1;
	next;
    }

    if ($7 == "xmit") {
	nntplink_times_user[host] += $9;
	nntplink_times_sys[host] += $11;
	nntplink_times_elapsed[host] += $13;
	next;
    }

    if ($7 == "xfer") {
	##  We can compute this if we need it, but there isn't
	##  space in the output table for it
	#nntplink_offer_min[host] += $9;
	#nntplink_accept_min[host] += $11;
	next;
    }

    ##  503 is the code for timeouts.
    if ($11 == "503" || $12 == "Timeout") {
	nntplink_timeout[host]++;
	timeout = 1;
	next;
    }
}


##
##  NNTPD
##
$5 ~ /nntpd.*/ {
    if ( $6 ~ /.*:.*/ ) {
	n = split($6, path, ":");
	host = path[1];
    }
    else
	host = $6;

    if ($7 == "connect") {
	nntpd_connect[host]++;
	nntpd = 1;
	next;
    }

    if ($7 == "times") {
	nntpd_seconds[host] += $9 + $11;
	nntpd_elapsed[host] += $13;
	if (nntpd_connect[host] == 0)
	    nntpd_connect[host]++;
	nntpd_done[host]++;
	next;
    }

    if ($7 == "ihave_stats") {
	nntpd_accepted[host] += $9;
	nntpd_refused[host] += $11;
	nntpd_rejected[host] += $13;
	next;
    }

    if ($7 == "no" && $8 == "space") {
	nntpd_nospace[host]++;
	if (nntpd_connect[host] == 0)
	    nntpd_connect[host]++;
	nntpd_done[host]++;
	next;
    }

    if ($6 == "no" && $7 == "space")
	next;

    ##  Rest is likely to be reader stuff; skip for now.
}



##
##  MTHREADS
##
$5 ~ /mthreads.*/ {

    if ($6 == "Started" && $7 == "mthreads") {
	mthreads = 1;
	mthreads_started++;
	next;
    }

    if ($6 == "Processed" && $9 == "added" && $12 == "expired") {
	mthreads = 1;
	mthreads_added += $10;
	mthreads_expired += $13;
	next;
    }

    if ($6 == "Turned" && $8 == "groups" && $9 == "on.") {
	mthreads = 1;
	mthreads_turned_on += $7;
	next;
    }

    if ($6 == "Turned" && $8 == "groups" && $9 == "off.") {
	mthreads = 1;
	mthreads_turned_off += $7;
	next;
    }
}


##
##  UNKNOWN
##
{
    if (unknowns == 0) {
	printf("Unknown entries from news log file:\n");
	unknowns = 1;
    }
    print;
}


##
##  SUMMARIZE DATA
##  NOTE: the following are collected but not used right now:
##	innd_flush
##	innd_feed
##	innd_pauses
##	innd_throttles
##	innd_newgroups
##	innd_rmgroups
##	innd_reloads
##	innd_timeout
##	innd_change_groups
##
END {
    printf("\n");
    if (innd) {
	##  INND control statistics.
	if (ctlinnd_seen) {
	    printf("Control commands to INND\t(%d total)\n\n", ctlinnd_seen);
	    i = 0;
	    for (m in ctlinnd_type) {
		printf("\t%13s %4d", ctlinnd[m], ctlinnd_type[m]);
		if (++i == 3) {
		    i = 0;
		    printf("\n");
		}
	    }
	    if (i != 0)
		printf("\n");
	    printf("\n");

	    if (ctlinnd_new) {
		printf("Newsgroups created:\n");
		for (m in ctlinnd_newgroups)
		    printf("\t%-30.30s by %s\n", m, ctlinnd_newgroups[m]);
		printf("\n");
	    }

	    if (ctlinnd_rmg) {
		printf("Newsgroups removed:\n");
		for (m in ctlinnd_rmgroups)
		    printf("\t%s\n", m);
		printf("\n");
	    }
	}

	##  INND exchange statistics.
	printf("Articles received by server\n");
	printf("System            Connects  Offered   Took Refuse Rejct Accpt   Elapsed\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innd_connect)
		if (innd_connect[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
	    if (s == invalid)
		break;

	    ninnd_connect += innd_connect[s];
	    ninnd_accept += innd_accepted[s];
	    ninnd_refuse += innd_refused[s];
	    ninnd_reject += innd_rejected[s];
	    ninnd_ela += innd_seconds[s];
	    offered = innd_accepted[s] + innd_refused[s] + innd_rejected[s];
	    ninnd_offered += offered;
	    if (offered == 0)
		offered = 1;
	    percent_accpt = (innd_accepted[s] * 100) / offered;
	    e_hours      = innd_seconds[s] / 3600;
	    e_sec        = innd_seconds[s] % 3600;
	    e_min        = e_sec / 60;
	    e_sec        %= 60;

	    printf("%-20.20s %5d   %6d %6d %6d %5d  %3d%% %3d:%02d:%02d\n", \
		s, innd_connect[s], \
		innd_accepted[s] + innd_refused[s] + innd_rejected[s], \
		innd_accepted[s], innd_refused[s], innd_rejected[s], \
		percent_accpt, e_hours, e_min, e_sec);

            innd_connect[s] = -1;
	}

	e_hours      = ninnd_ela / 3600;
	e_sec        = ninnd_ela % 3600;
	e_min        = e_sec / 60;
	e_sec        %= 60;
	they_offered = ninnd_offered;
	if (they_offered == 0)
	    they_offered = 1;
	percent_accpt = (ninnd_accept * 100.0)/ (they_offered + 0.1);

	printf("\n%-20s %5d   %6d %6d %6d %5d  %3d%% %3d:%02d:%02d\n\n", \
	    "TOTALS", ninnd_connect, ninnd_offered, ninnd_accept, \
	    ninnd_refuse, ninnd_reject, percent_accpt, e_hours, e_min, \
	    e_sec);
    }

    ##  Miscellaneous innd statistics.
    if (innd_bad_msgids) {
	printf("Bad Message-ID's offered\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innd_bad_msgid)
		if (innd_bad_msgid[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
	    if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, innd_bad_msgid[s]);

	    innd_bad_msgid[s] = -1;
        }
	printf("\n");
    }
    if (innd_bad_ihaves) {
	printf("Bad ihave control messages received\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innd_bad_ihave)
	        if (innd_bad_ihave[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, innd_bad_ihave[s]);

	    innd_bad_ihave[s] = -1;
        }
	printf("\n");
    }
    if (innd_bad_sendmes) {
	printf("Ignored sendme control messages received\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innd_bad_sendme)
		if (innd_bad_sendme[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, innd_bad_sendme[s]);

	    innd_bad_sendme[s] = -1;
        }
	printf("\n");
    }
    if (innd_blockeds) {
	printf("Blocked server feeds\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innd_blocked)
		if (innd_blocked[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, innd_blocked[s]);

	    innd_blocked[s] = -1;
        }
	printf("\n");
    }

    ##  NNTPD statistics.
    if (nntpd) {
	printf("Articles received by NNTPD\n");
	printf("System            Connects NSpc Fail Offered  Took Refuse Rejct Accpt   Elapsed\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nntpd_connect)
		if (nntpd_connect[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    nnntpd_connect += nntpd_connect[s];
	    nnntpd_nospace += nntpd_nospace[s];
	    nnntpd_accept += nntpd_accepted[s];
	    nnntpd_refuse += nntpd_refused[s];
	    nnntpd_reject += nntpd_rejected[s];
	    nnntpd_ela += nntpd_elapsed[s];
	    nnntpd_done += nntpd_done[s];
	    offered = nntpd_accepted[s] + nntpd_refused[s] + nntpd_rejected[s];
	    nnntpd_offered += offered;
	    if (offered == 0)
		offered = 1;
	    percent_accpt = (nntpd_accepted[s] * 100) / offered;
	    e_hours      = nntpd_elapsed[s] / 3600;
	    e_sec        = nntpd_elapsed[s] % 3600;
	    e_min        = e_sec / 60;
	    e_sec        %= 60;

	    printf("%-20.20s %5d %4d %4d %6d %6d %6d %5d  %3d%% %3d:%02d:%02d\n", \
		s, nntpd_connect[s], nntpd_nospace[s], \
		nntpd_connect[s] - nntpd_done[s], \
		nntpd_accepted[s] + nntpd_refused[s] + nntpd_rejected[s], \
		nntpd_accepted[s], nntpd_refused[s], nntpd_rejected[s], \
		percent_accpt, e_hours, e_min, e_sec);

            nntpd_connect[s] = -1;
	}

	e_hours      = nnntpd_ela / 3600;
	e_sec        = nnntpd_ela % 3600;
	e_min        = e_sec / 60;
	e_sec        %= 60;
	they_offered = nnntpd_offered;
	if (they_offered == 0)
	    they_offered = 1;
	percent_accpt = (nnntpd_accept * 100.0)/ (they_offered + 0.1);

	printf("\n%-20s %5d %4d %4d %6d %6d %6d %5d  %3d%% %3d:%02d:%02d\n\n", \
	    "TOTALS", nnntpd_connect, nnntpd_nospace, \
	    nnntpd_connect - nnntpd_done, \
	    nnntpd_offered, nnntpd_accept, nnntpd_refuse, nnntpd_reject, \
	    percent_accpt, e_hours, e_min, e_sec);
    }

    ##  Innxmit statistics.
    if (innxmit) {
	printf("Articles sent by innxmit\n");
	printf("System                Offrd   Took   Toss  Fail  Pct   Elapsed       CPU  Pct\n");

	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innxmit_site)
		if (innxmit_offered[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    we_offered = innxmit_offered[s];
	    if (we_offered == 0)
		we_offered = 1;
	    they_take = (innxmit_accepted[s] * 100.0) / we_offered;
	    e_hours = innxmit_times_elapsed[s] / 3600;
	    e_sec   = innxmit_times_elapsed[s] % 3600;
	    e_min   = e_sec / 60;
	    e_sec   %= 60;
	    c_hours = (innxmit_times_user[s] + innxmit_times_sys[host]) / 3600;
	    c_sec   = (innxmit_times_user[s] + innxmit_times_sys[s]) % 3600;
	    c_min   = c_sec / 60;
	    c_sec   %= 60;
	    elapsed = innxmit_times_elapsed[s];
	    if (elapsed == 0)
		elapsed = 1;
	    pct = ((innxmit_times_user[s] + innxmit_times_sys[s]) * 100.0) / elapsed;

	    printf("%-20.20s %6d %6d %6d %5d %3d%% %3d:%02d:%02d %3d:%02d:%02d %3d%%\n", \
		s, innxmit_offered[s], innxmit_accepted[s], \
		innxmit_rejected[s], innxmit_failed[s], they_take, \
		e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);

	    ixmt        += innxmit_offered[s];
	    ixmt_accept += innxmit_accepted[s];
	    ixmt_reject += innxmit_rejected[s];
	    ixmt_failed += innxmit_failed[s];
	    ixmt_ela    += innxmit_times_elapsed[s];
	    ixmt_cpu    += innxmit_times_user[s] + innxmit_times_sys[s];

	    innxmit_offered[s] = -1;
	}

	we_offered = ixmt;
	if (we_offered == 0)
	    we_offered = 1;
	they_take = (ixmt_accept * 100) / we_offered;
	e_hours = ixmt_ela / 3600;
	e_sec   = ixmt_ela % 3600;
	e_min   = e_sec / 60;
	e_sec   %= 60;
	c_hours = ixmt_cpu / 3600;
	c_sec   = ixmt_cpu % 3600;
	c_min   = c_sec / 60;
	c_sec   %= 60;
	if (ixmt_ela == 0)
	    ixmt_ela = 1;
	pct = (ixmt_cpu * 100.0) / ixmt_ela;

	printf("\n%-20.20s %6d %6d %6d %5d %3d%% %3d:%02d:%02d %3d:%02d:%02d %3d%%\n\n", \
	    "TOTALS", ixmt, ixmt_accept, ixmt_reject, ixmt_failed, \
	    they_take, e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);

	printf("\n");
	printf("Transmission Connection Attempts         ------errors-------------------\n");
	printf("System               Conn   Ok Auth Load Space Expire Connct Other   Pct\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in innxmit_site)
		if (innxmit_site[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    tot = innxmit_site[s];
	    if (tot == 0)
	        tot = 1;
	    errs = innxmit_afail_host[s] + innxmit_hiload[s] + \
		innxmit_nospace[s] + innxmit_cfail_host[s] + \
		innxmit_expire[s] + innxmit_crefused[s];
	    ok = (innxmit_site[s] - errs);

	    printf("%-20.20s %4d %4d %4d %4d %5d  %5d  %5d %5d   %3d%%\n", \
		s, innxmit_site[s], ok, innxmit_afail_host[s], \
		innxmit_hiload[s], innxmit_nospace[s], innxmit_expire[s], \
		innxmit_cfail_host[s], innxmit_crefused[s], \
		(100.0 * ok / tot));

	    ict_tot += innxmit_site[s];
	    ict_ok  += ok;
	    ict_afail += innxmit_afail_host[s];
	    ict_hiload += innxmit_hiload[s];
	    ict_nospace += innxmit_nospace[s];
	    ict_expire += innxmit_expire[s];
	    ict_crefused += innxmit_crefused[s];
	    ict_cfail += innxmit_cfail_host[s];

	    innxmit_site[s] = -1;
	}
	tot = ict_tot;
	if (tot == 0)
	    tot = 1;
	errs = ict_afail + ict_nospace + ict_hiload + ict_cfail + ict_crefused;

	printf("\n%-20.20s %4d %4d %4d %4d %5d  %5d  %5d %5d   %3d%%\n\n", \
	    "TOTALS", ict_tot, ict_ok, ict_afail, ict_hiload, \
	    ict_nospace, ict_expire, ict_cfail, ict_crefused, \
	    (100.0 * ict_ok / tot));
    }

    ##  Nntplink statistics.
    if (nntplink) {
	printf("Articles sent by nntplink\n");
	printf("System                Offrd   Took   Toss  Fail  Pct   Elapsed       CPU  Pct\n");

	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nntplink_site)
		if (nntplink_offered[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    we_offered = nntplink_offered[s];
	    if (we_offered == 0)
		we_offered = 1;
	    they_take = (nntplink_accepted[s] * 100.0) / we_offered;
	    e_hours = nntplink_times_elapsed[s] / 3600;
	    e_sec   = nntplink_times_elapsed[s] % 3600;
	    e_min   = e_sec / 60;
	    e_sec   %= 60;
	    c_hours = (nntplink_times_user[s] + nntplink_times_sys[host]) / 3600;
	    c_sec   = (nntplink_times_user[s] + nntplink_times_sys[s]) % 3600;
	    c_min   = c_sec / 60;
	    c_sec   %= 60;
	    elapsed = nntplink_times_elapsed[s];
	    if (elapsed == 0)
		elapsed = 1;
	    pct = ((nntplink_times_user[s] + nntplink_times_sys[s]) * 100.0) / elapsed;

	    printf("%-20.20s %6d %6d %6d %5d %3d%% %3d:%02d:%02d %3d:%02d:%02d %3d%%\n", \
		s, nntplink_offered[s], nntplink_accepted[s], \
		nntplink_rejected[s], nntplink_failed[s], they_take, \
		e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);

	    nxmt        += nntplink_offered[s];
	    nxmt_accept += nntplink_accepted[s];
	    nxmt_reject += nntplink_rejected[s];
	    nxmt_failed += nntplink_failed[s];
	    nxmt_ela    += nntplink_times_elapsed[s];
	    nxmt_cpu    += nntplink_times_user[s] + nntplink_times_sys[s];

	    nntplink_offered[s] = -1;
	}

	we_offered = nxmt;
	if (we_offered == 0)
	    we_offered = 1;
	they_take = (nxmt_accept * 100) / we_offered;
	e_hours = nxmt_ela / 3600;
	e_sec   = nxmt_ela % 3600;
	e_min   = e_sec / 60;
	e_sec   %= 60;
	c_hours = nxmt_cpu / 3600;
	c_sec   = nxmt_cpu % 3600;
	c_min   = c_sec / 60;
	c_sec   %= 60;
	if (nxmt_ela == 0)
	    nxmt_ela = 1;
	pct = (nxmt_cpu * 100.0) / nxmt_ela;

	printf("\n%-20.20s %6d %6d %6d %5d %3d%% %3d:%02d:%02d %3d:%02d:%02d %3d%%\n\n", \
	    "TOTALS", nxmt, nxmt_accept, nxmt_reject, nxmt_failed, \
	    they_take, e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);

	printf("Transmission Connection Attempts         ------errors-------\n");
	printf("System               Conn   Ok EOF Sock Load Bpipe Space  Exp Auth Other  Pct\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nntplink_site)
		if (nntplink_site[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    tot = nntplink_site[s];
	    if (tot == 0)
		tot = 1;
	    errs = nntplink_eof[s] + nntplink_sockerr[s] + \
		 nntplink_hiload[s] + nntplink_bpipe[s] + \
		 nntplink_nospace[s] + nntplink_auth[s] + \
		 nntplink_expire[s] + nntplink_fail[s];
	    ok = (nntplink_site[s] - errs);

	    printf("%-20.20s %4d %4d %3d %4d %4d %5d %5d %4d %4d %5d  %3d%%\n", \
		s, nntplink_site[s], ok, nntplink_eof[s], \
		nntplink_sockerr[s], nntplink_hiload[s], \
		nntplink_bpipe[s], nntplink_nospace[s], \
		nntplink_expire[s], nntplink_auth[s], nntplink_fail[s], \
		(100.0 * ok / tot));

	    ct_tot += nntplink_site[s];
	    ct_ok  += ok;
	    ct_eof += nntplink_eof[s];
	    ct_sockerr += nntplink_sockerr[s];
	    ct_hiload += nntplink_hiload[s];
	    ct_bpipe += nntplink_bpipe[s];
	    ct_nospace += nntplink_nospace[s];
	    ct_auth += nntplink_auth[s];
	    ct_expire += nntplink_expire[s];
	    ct_fail += nntplink_fail[s];

	    nntplink_site[s] = -1;
	}
	tot = ct_tot;
	if (tot == 0)
	    tot = 1;

	printf("\n%-20.20s %4d %4d %3d %4d %4d %5d %5d %4d %4d %5d  %3d%%\n\n", \
	    "TOTALS", ct_tot, ct_ok, ct_eof, ct_sockerr, ct_hiload, \
	    ct_bpipe, ct_nospace, ct_expire, ct_auth, ct_fail, \
	    (100.0 * ct_ok / tot));
    }

    ##  Batcher statistics.
    if (batcher) {
	printf("UUCP batches created\n");
	printf("System                Offrd   Arts      Bytes   Elapsed       Cpu  Pct\n");

	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in batcher_site)
		if (batcher_site[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    e_hours = batcher_times_elapsed[s] / 3600;
	    e_sec   = batcher_times_elapsed[s] % 3600;
	    e_min   = e_sec / 60;
	    e_sec   %= 60;
	    c_hours = (batcher_times_user[s] + batcher_times_sys[s]) / 3600;
	    c_sec   = (batcher_times_user[s] + batcher_times_sys[s]) % 3600;
	    c_min   = c_sec / 60;
	    c_sec   %= 60;
	    elapsed = batcher_times_elapsed[s];
	    if (elapsed == 0)
		elapsed = 1;
	    pct = ((batcher_times_user[s] + batcher_times_sys[s]) * 100.0) / elapsed;

	    printf("%-20.20s %6d %6d %10d %3d:%02d:%02d %3d:%02d:%02d %3d%%\n",\
		s, batcher_num[s], batcher_articles[s], batcher_bytes[s], \
		e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);

	    nbatch          += batcher_num[s];
	    nbatch_articles += batcher_articles[s];
	    nbatch_bytes    += batcher_bytes[s];
	    nbatch_ela      += batcher_times_elapsed[s];
	    nbatch_cpu      += batcher_times_user[s] + batcher_times_sys[s];

	    batcher_site[s] = -1;
	}

	e_hours = nbatch_ela / 3600;
	e_sec   = nbatch_ela % 3600;
	e_min   = e_sec / 60;
	e_sec   %= 60;
	c_hours = nbatch_cpu / 3600;
	c_sec   = nbatch_cpu % 3600;
	c_min   = c_sec / 60;
	c_sec   %= 60;
	if (nbatch_ela == 0)
	    nbatch_ela = 1;
	pct = (nbatch_cpu * 100.0) / nbatch_ela;

	printf("\n%-20.20s %6d %6d %10d %3d:%02d:%02d %3d:%02d:%02d %3d%%\n\n",\
	    "TOTALS", nbatch, nbatch_articles, nbatch_bytes,\
	    e_hours, e_min, e_sec, c_hours, c_min, c_sec, pct);
    }

    ##  Rnews statistics.
    if (rnews_host) {
	printf("Rnews articles offered from:\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in rnews_hosts)
		if (rnews_hosts[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("\t%6d\t%s\n", rnews_hosts[s], s);

	    rnews_hosts[s] = -1;
        }
	printf("\n");
    }
    if (rnews_rejects) {
	printf("Rnews connections rejected %d times\n", rnews_rejects);
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in rnews_r_reject)
		if (rnews_r_reject[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("\t%6d\t%s\n", rnews_r_reject[s], s);

	    rnews_r_reject[s] = -1;
        }
	printf("\n");
    }
    if (rnews_bad) {
	printf("Rnews bad articles: (Total %d)\n", rnews_bad);
	if (rnews_bad_ng) {
	    printf("Bad newsgroup: %d\n", rnews_bad_ng);
	    for ( ; ; ) {
		s = invalid;
		for (sortindex in rnews_bng)
		    if (rnews_bng[sortindex] >= 0 && sortindex < s)
			s = sortindex;
                if (s == invalid)
		    break;

		printf("\t%5d: %s\n", rnews_bng[s], s);

		rnews_bng[s] = -1;
            }
	}
	if (rnews_bad_dist) {
	    printf("Bad distribution: %d\n", rnews_bad_dist);
	    for ( ; ; ) {
		s = invalid;
		for (sortindex in rnews_bdist)
		    if (rnews_bdist[sortindex] >= 0 && sortindex < s)
			s = sortindex;
                if (s == invalid)
		    break;

		printf("\t%5d: %s\n", rnews_bdist[s], s);

		rnews_bdist[s] = -1;
            }
	}
	if (rnews_bad_date) {
	    printf("Bad date: %d\n", rnews_bad_date);
	}
	printf("\n");
    }

    ##  NNRP statistics.
    if (nnrp) {
	printf("NNRP readership statistics\n");
	printf("System                Conn Articles Groups Post  Rej   Elapsed       CPU  Pct\n");

	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_connect)
		if (nnrp_connect[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	   ##  Report curious pokers elsewhere.
	    if (nnrp_groups[s] == 0	\
	     && nnrp_articles[s] == 0	\
	     && nnrp_posts[s] == 0) {
		nnrp_curious[s] += nnrp_connect[s];
		curious = 1;
		nnrp_connect[s] = -1;
		continue;
	    }

	    nconn += nnrp_connect[s];
	    nart += nnrp_articles[s];
	    ngrp += nnrp_groups[s];
	    npost += nnrp_posts[s];
	    nrej += nnrp_rejected[s];
	    ncpu += (nnrp_times_user[s] + nnrp_times_sys[s]);
	    nela += nnrp_times_elapsed[s];
	    e_hours      = nnrp_times_elapsed[s] / 3600;
	    e_sec        = nnrp_times_elapsed[s] % 3600;
	    e_min        = e_sec / 60;
	    e_sec        %= 60;
	    c_hours      = (nnrp_times_user[s] + nnrp_times_sys[s]) / 3600;
	    c_sec        = (nnrp_times_user[s] + nnrp_times_sys[s]) % 3600;
	    c_min        = c_sec / 60;
	    c_sec        %= 60;
	    elapsed = nnrp_times_elapsed[s];
	    if (elapsed == 0)
		elapsed = 1;
	    pct = ((nnrp_times_user[s] + nnrp_times_sys[s]) * 100.0) / elapsed;

	    printf("%-20.20s %5d %8d %6d %4d %4d %3d:%02d:%02d %3d:%02d:%02d %3d%%\n",\
		s, nnrp_connect[s], nnrp_articles[s], nnrp_groups[s],\
		nnrp_posts[s], nnrp_rejected[s], e_hours, e_min, e_sec,\
		c_hours, c_min, c_sec, pct);

            nnrp_connect[s] = -1;
	}

	e_hours      = nela / 3600;
	e_sec        = nela % 3600;
	e_min        = e_sec / 60;
	e_sec        %= 60;
	c_hours      = ncpu / 3600;
	c_sec        = ncpu % 3600;
	c_min        = c_sec / 60;
	c_sec        %= 60;
	if (nela == 0)
	    nela = 1;
	pct = (ncpu * 100.0) / nela;

	printf("\n%-20.20s %5d %8d %6d %4d %4d %3d:%02d:%02d %3d:%02d:%02d %3d%%\n\n",\
	    "TOTALS", nconn, nart, ngrp, npost, nrej, e_hours, e_min, \
	    e_sec, c_hours, c_min, c_sec, pct);
    }

    ##  Miscellaneous NNRP statistics.
    if (curious) {
	printf("Curious NNRP server explorers\n");
	printf("System                Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_curious)
		if (nnrp_curious[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nnrp_curious[s]);

	    nnrp_curious[s] = -1;
        }
	printf("\n");
    }
    if (nnrp_noperms) {
	printf("NNRP no permission clients\n");
	printf("System                Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_noperm)
		if (nnrp_noperm[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nnrp_noperm[s]);

	    nnrp_noperm[s] = -1;
        }
	printf("\n");
    }
    if (nnrp_unrecs) {
	printf("NNRP unrecognized commands\n");
	printf("System                Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_unrec)
		if (nnrp_unrec[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nnrp_unrec[s]);

	    nnrp_unrec[s] = -1;
        }
	printf("\n");
    }
    if (nnrp_gethostfails) {
	printf("NNRP gethostbyname failures\n");
	printf("IP                    Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_gethostfail)
		if (nnrp_gethostfail[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nnrp_gethostfail[s]);

	    nnrp_gethostfail[s] = -1;
        }
	printf("\n");
    }
    if (client_timeout) {
	printf("NNRP client timeouts\n");
	printf("System                Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_timeout)
		if (nnrp_timeout[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nnrp_timeout[s]);

	    nnrp_timeout[s] = -1;
        }
	printf("\n");
    }
    if (server_timeout) {
	printf("NNTPLINK remote server timeouts\n");
	printf("System                Conn\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nntplink_timeout)
		if (nntplink_timeout[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf("%-20.20s %5d\n", s, nntplink_timeout[s]);

	    nntplink_timeout[s] = -1;
        }
	printf("\n");
    }

    ##  MTHREADS statistics.
    if (mthreads) {
	printf("Mthreads:   Starts   Groups on  Groups off    Articles     Expired\n");
	printf("%18d%12d%12d%12d%12d\n", mthreads_started, mthreads_turned_on, \
		mthreads_turned_off, mthreads_added, mthreads_expired);
	printf("\n");
    }

    ##  Group readership statistics.
    if (nnrp_readers) {
	for (g in nnrp_group_request) {
	    x = length(g);
	    if (x > max)
		max = x;
	    i = index(g, ".");
	    if (i > 0)
		top = substr(g, 1, i - 1);
	    else
		top = g;
	    category[top] += nnrp_group_request[g];
	}
	fmt = sprintf("%%-%ds %%5d\n", max);

	printf("Newsgroup request counts (by category)\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in category)
		if (category[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf(fmt, s, category[s]);

	    category[s] = -1;
        }
	printf("\n");

	printf("Newsgroup request counts (by newsgroup)\n");
	for ( ; ; ) {
	    s = invalid;
	    for (sortindex in nnrp_group_request)
		if (nnrp_group_request[sortindex] >= 0 && sortindex < s)
		    s = sortindex;
            if (s == invalid)
		break;

	    printf(fmt, s, nnrp_group_request[s]);

	    nnrp_group_request[s] = -1;
        }
	printf("\n");
    }

    printf("\n");
}
