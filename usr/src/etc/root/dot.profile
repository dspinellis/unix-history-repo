PATH=/sbin:/usr/sbin:/bin:/usr/bin:
echo 'erase ^?, kill ^U, intr ^C'
stty crt erase  kill  intr 
export PATH
HOME=/root
export HOME TERM

umask 022
