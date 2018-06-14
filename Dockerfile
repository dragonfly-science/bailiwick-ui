FROM lnl7/nix:2.0

COPY default.nix /setup/
COPY deploy.nix /setup/
COPY bailiwick.cabal /setup/
COPY app/ /setup/app/
COPY src/ /setup/src/
COPY static/ /setup/static/

COPY nix.conf /etc/nix/nix.conf
 
RUN cd /setup/ && nix-build -j6 deploy.nix

RUN nix-env -i bash
RUN nix-env -i zip
RUN nix-env -i awscli

ARG AWS_ACCESS_KEY_ID
ARG AWS_SECRET_ACCESS_KEY
ENV AWS_ACCESS_KEY_ID $AWS_ACCESS_KEY_ID
ENV AWS_SECRET_ACCESS_KEY $AWS_SECRET_ACCESS_KEY