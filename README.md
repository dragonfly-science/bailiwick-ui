## Bailiwick

This is the frontend for the MBIE Regional Activity web tool.

It works by reading the JSON data and YAML config files at compile time to
produce a small javscript executable that can be shipped with the JSON data
files.

## Getting started

Open two shells. One is going to run the ghc shell, the other will run a ghcjs shell.

In the first shell, the ghc one:

```bash
$ nix-shell -A shells.ghc
$ cabal new-repl
.... # Lots of stuff...
*Bailiwick> :reload
```

That will compile the project, and start a web server running that you can
connect to.

And in the other shell:

```bash
$ nix-shell -A shells.ghcjs
$ cabal new-build --ghcjs
```

That will compile the javascript code ready to be served by the first process.

Now you open your browser at http://localhost:3701

Note, you also need to add a `127.0.0.1 jsaddle.locahost` to your `/etc/hosts` file

## Setting up the d3 based javascript in development mode

```bash
$ nix-shell javascript.nix -A shell
[nix-shell:bailiwick-ui]$ ln -sf $NODE_PATH
[nix-shell:bailiwick-ui]$ npm run develop
```


## Building docker image for gorbachev

There is `nix.conf` file that may need to be updated. It has a line to point at
the local store. To setup the local store:

```bash
$ nix-store --generate-binary-cache-key nix-cache.kahu.dragonfly.co.nz-1 nix-serve.sec nix-serve.pub
$ NIX_SECRET_KEY_FILE=nix-serve.sec nix-serve --host nix-cache.kahu.dragonfly.co.nz --port 8080
```

And in the `nix.conf` file you will need to update the lines that refer to the
cache before running `make docker`.

## Javascript generation

Currently the javascript is bundled via webpack (this will be moved to nix at
some point).

To run, first install all the npm dependencies.

```
npm i
```

Then to watch for changes & compie:

```
npm run develop
```

To build for production:

```
npm run build
```
