#
# doc4.awk
#
#      Distributed with:  Doc - Version 2.0 (8/22/90)
#      USC Information Sciences Institute
#
#
# Accepts as input, output from dig querying for NS records of a
# domain.  Expects input to include TTLs and to be all lower case.
# Also expect to get address of these nameservers in the Additional
# section.
#
# Ouput a list of internet dot-notion addresses of nameservers
# for domain. Only does so if domain name of server is in domain.
# i.e. vax.darpa.mil. is a nameserver for isi.edu., but we are
#      not interested in addresses not on isi networks
#
# Currently, if more than one address (of some server) are on the
# same network (subnets not recognized), only one is printed.
# Arguably for completeness, all address should be printed.
#
# Intended use: targets for queries to check for presence of
# in-addr.arpa mappings for domain.
#

BEGIN {n=0}

## Find domain being tested (will be NS record for it)

$4 == "ns" {
  ns = $1;
}

##
## Look at A records, presumably Additional answers
## for addresses of nameservers.
##

$4 == "a" && ns != "" {
    if (index($1,ns) > 0) {      ## is server name in domain ?
       split($5, dd, ".");
       if (dd[1] < 127) {
          ii = dd[1];
       } else if (dd[1] < 192) {
          ii = dd[1] dd[2];
       } else {
          ii = dd[1] dd[2] dd[3];
       }
#       print "DDT", ii, hnet[ii], $5
       if (hnet[ii] == "") {
          hnet[ii] = $5;
#          print "DDT:" , $0
          print $5;
        }
      }
}
