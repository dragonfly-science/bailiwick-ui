FROM lnl7/nix:2.0

COPY default.nix /setup/
COPY deploy.nix /setup/
COPY bailiwick.cabal /setup/
COPY app/ /setup/app/
COPY src/ /setup/src/
COPY static/ /setup/static/

COPY nix.conf /etc/nix/nix.conf
 
RUN cd /setup/ && nix-build -j6 deploy.nix

