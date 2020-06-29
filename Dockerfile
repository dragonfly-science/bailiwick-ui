FROM lnl7/nix:2.3.3

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
RUN nix-env -f  /setup/db/default.nix -iA bailiwick-data

# Install the haskell libraries for compiling reflex and bailiwick to javascript
RUN cd /setup/ && nix-shell -j6 -A shells.ghcjs --run exit
RUN cd /setup/ && nix-shell -j6 -A shells.ghc --run exit

# Install the javascript dependencies
COPY javascript.nix /setup/
COPY node-env.nix /setup/
COPY node-packages.nix /setup/
COPY package.json /setup/
COPY package-lock.json /setup/
RUN cd /setup/ && nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz javascript.nix -j6 -A shell --run 'ln -s $NODE_PATH'

## return to nix conf that will work on gorby
COPY nix-gorbachev.conf /etc/nix/nix.conf

