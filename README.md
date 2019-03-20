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
$ nix-shell -A shells.ghc
$ cabal new-build --ghcjs
```

That will compile the javascript code ready to be served by the first process.

Now you open your browser at http://localhost:3701

Note, you also need to add a `127.0.0.1 jsaddle.locahost` to your `/etc/hosts` file

