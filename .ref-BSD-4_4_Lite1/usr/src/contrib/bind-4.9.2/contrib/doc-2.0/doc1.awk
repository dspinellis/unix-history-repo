#
# doc1.awk
#
#      Distributed with:  Doc - Version 2.0 (8/22/90)
#      USC Information Sciences Institute
#
# Accepts as input, output from dig querying for SOA of a domain.
# Expects input to be lower case.
#
# Prints serial # of SOA (only one)
# Exit status indicates:
#    o response was authoritative (or not)
#    o number of SOA records found
#

BEGIN {aa=0; ss=0}

/flags:/ && /aa/ {aa++}

$2 == ";serial" {soa=$1; ss++}

END {
print soa

if (aa==0) 
  {ss=0-ss};
exit(ss);
}




