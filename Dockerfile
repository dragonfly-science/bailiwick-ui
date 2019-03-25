FROM lnl7/nix:2.1.2

RUN nix-env -i bash
RUN nix-env -i zip
RUN nix-env -i awscli
RUN nix-env -i gnutar
RUN nix-env -i gzip
RUN nix-env -i gnumake

COPY default.nix /setup/
COPY deploy.nix /setup/
COPY bailiwick.cabal /setup/
COPY app/ /setup/app/
COPY src/ /setup/src/
COPY static/ /setup/static/
COPY db/ /setup/db/

## Tell nix about locally served cache
COPY nix.conf /etc/nix/nix.conf

# Install a bunch of R libraries to allow creation of json data files
RUN nix-env -f /setup/db/default.nix -iA bailiwick-data

# Install the haskell libraries for compiling reflex and bailiwick to javascript
RUN cd /setup/ && nix-shell -j6 -A shells.ghcjs --run exit

## return to nix conf that will work on gorby
COPY nix-gorbachev.conf /etc/nix/nix.conf

ARG AWS_ACCESS_KEY_ID
ARG AWS_SECRET_ACCESS_KEY
ENV AWS_ACCESS_KEY_ID $AWS_ACCESS_KEY_ID
ENV AWS_SECRET_ACCESS_KEY $AWS_SECRET_ACCESS_KEY
