: From cbosg!mcnc!unc!smb Sat Feb 27 05:45:42 1982
: Date: 27-Feb-82 05:45:41-EST

: Heres a shell file that should be distributed with netnews.
: Its intended to be the program that replies to the mapping control
: message, it extracts info from the L.sys file, add extra names, and
: suppresses private ones.

: To use this, change the senduuname control message to call
: LIBDIR/euuname instead of uuname, and install this script in that location.
: the lists secret and extras are names you want deleted from added to
: your uuname output.

PATH=/usr/ucb:/bin:/usr/bin
export PATH
cd /usr/lib/news
trap "rm -f /tmp/nam$$; exit" 0 1 2
sort secret >/tmp/nam$$
echo Subject: edited uuname output enclosed
((cat extras; uuname) | sort | comm -23 - /tmp/nam$$) | uniq
