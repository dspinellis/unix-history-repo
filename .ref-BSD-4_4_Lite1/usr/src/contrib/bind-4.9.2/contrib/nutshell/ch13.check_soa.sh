#!/bin/sh
if test "$1" = ""
then
    echo usage: $0 domain
    exit 1
fi
DOMAIN=$1
#
# Use nslookup to discover the nameservers for this domain ($1).
# Use awk to grab the name server names from the nameserver lines.
# (The names are always in the last field).  Use sort -u to weed out
# duplicates; we don't actually care about collation.
#
SERVERS=`nslookup -type=ns $DOMAIN |\
                 awk '/nameserver/ {print $NF}' | sort -u`
if test "$SERVERS" = ""
then
    #
    # Didn't find any servers.  Just quit silently; nslookup will
    # have detected this error and printed a message.  That will 
    # suffice.
    #
    exit 1
fi
#
# Check each server's SOA serial number.  The output from 
# nslookup is saved in two tmp files: nso.$$ (standard output) 
# and nse.$$ (standard error).  These files are rewritten on
# every iteration.  Turn off defname and search since we
# should be dealing with fully qualified names.
#
# NOTE: this loop is rather long; don't be fooled.
#
for i in $SERVERS
do
  nslookup >/tmp/nso.$$ 2>/tmp/nse.$$ <<-EOF
    server $i
    set nosearch
    set nodefname
    set norecurse
    set q=soa
    $DOMAIN
EOF
  #
  # Does this response indicate that the current server ($i) is
  # authoritative?  The server is NOT authoritative if (a) the
  # response says so, or (b) the response tells you to find
  # authoritative info elsewhere.
  #
  if egrep "Non-authoritative|Authoritative answers can be" \
                                          /tmp/nso.$$ >/dev/null
  then
    echo $i is not authoritative for $DOMAIN
    continue
  fi
  #
  # We know the server is authoritative; extract the serial number.
  #
  SERIAL=`cat /tmp/nso.$$ | grep serial | sed -e "s/.*= //"`
  if test "$SERIAL" = ""
  then
    #
    # We get here if SERIAL is null.  In this case, there should
    # be an error message from nslookup; so cat the "standard 
    # error" file.
    #
    cat /tmp/nse.$$
  else
    #
    # Report the server's name and its serial number.
    #
    echo $i has serial number $SERIAL
  fi
done  # end of the "for" loop
#
# Delete the temporary files.
#
rm -f /tmp/nso.$$ /tmp/nse.$$
