# haskup

*Cross platform Haskell toolchain installer*

[![Release](https://github.com/snoyberg/haskup/actions/workflows/release.yml/badge.svg)](https://github.com/snoyberg/haskup/actions/workflows/release.yml)

This package provides tooling that leverages the GHC toolchain installer logic present in Stack. Its primary purposes currently is to provide a Windows installer which provides 3 executables:

* `stack.exe`
* `cabal.exe`
* `haskup.exe`, which can install and run various GHC version

In the future, the goal is to have `haskup.exe` itself install and manage various Stack and Cabal versions.

## Sample usage

```
> haskup --ghc 8.8.3 ghci
> haskup runghc Main.hs
> haskup --ghc 8.6.5 exec cmd
```

Commands will automatically install GHC and `msys2` if necessary, and then run the command in question with a modified `PATH` that includes these directories.

## haskwrap

This package provides a work in progress executable called `haskwrap`. This is intended for use cases where you don't want to call `haskup` explicitly. Instead, if you install `haskwrap` to a location like `/usr/local/bin/ghc`, running that executable will automatically invoke the real `ghc` behind the scenes.

Again, this is a work in progress, and is not included in the Windows installer.

This tool intentionally does minimalistic command line parsing, to allow passthrough of arguments as easily as possible to underlying tools. Restrictions:

* This tool ignores all GHC RTS options (i.e., it sets `-rtsopts=ignoreAll`)
* The only location where `--ghc` is accepted is as the very first argument to the command
* The `exec` command is only recognized immediately following `--ghc VERSION` or, if that is absent, as the first argument

## Default GHC version

If you leave off the `--ghc VERSION`, currently the executable will default to using GHC 8.10.4. This is _not good behavior_. Instead, there should be a config file for the default GHC version, potentially with environment variable and local filepath overrides, and some intelligent way of setting that default value on first execution. Contributions welcome to make that happen!

## Maintainers welcome!

I (Michael Snoyman) wrote the first version of this to get it off the ground. I welcome other contributors and maintainers on board. If you're interested, please reach out to me on just about any channel, including the Haskell Foundation Slack or this issue tracker.
