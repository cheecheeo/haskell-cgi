#!/bin/sh

set -e

sed -i.orig -e 's/^name:  *cgi/name: cgi-compat/' -e 's/Network\.CGI,/Network.NewCGI,/' -e 's/Build-depends:\(.*\)/Build-depends:\1, fps/' cgi.cabal
sed -e 's/^module Network\.CGI/module Network.NewCGI/' < src/Network/CGI.hs > src/Network/NewCGI.hs
