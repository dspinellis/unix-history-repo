#
# doc3.awk
#
#      Distributed with:  Doc - Version 2.0 (8/22/90)
#      USC Information Sciences Institute
#
# Accepts as input, output from dig querying for NS records of a
# domain.  Expects input to include TTLs and to be all lower case.
#
# Prints list of machines which are nameserves for the domain.
# Also crunches following info into the 8 bit unix exit code:
#
#    o response was authoritative (or not)
#    o number of unique TTLs associated with NS records
#    o number of A records found for domain names found to be servers
#

BEGIN {aa=0; glue=0; ttl=0}

/flags:/ && /aa/ {aa++}

$4=="ns" {print $5; a[$5]=1; if (zttl[$2]++ == 0) ttl++}
$4=="a" {if (a[$1]++ == 1) glue++}

END {
  j = glue;
  j = j + 16 * ttl;

  if (aa)
     j = j - 127;

   exit(j)
}
