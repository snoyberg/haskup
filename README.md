# haskwrap

*That's a wrap!*

This package provides an executable that leverages the GHC toolchain installer logic present in Stack. When run, this executable will ensure that an appropriate GHC version is installed, modify the `PATH` environment variables, and execute the correct command. As a simple example, if you have this executable installed as `haskwrap` and run `haskwrap --ghc 8.8.3 exec runghc Main.hs`, it will:

1. Install GHC 8.8.3 to a user-local directory, if not present.
2. Place the directory containing GHC's binaries on the PATH, as well as `msys2` binaries if running on Windows.
3. Execute `runghc Main.hs`

## Automatic command discovery

If you rename this executable to something like `ghc` or `runghc`, you do not need to provide the `exec runghc` bit. Instead, this executable will automatically run that command. In other words, you can install this executable to `/usr/bin/ghc`, and it will automatically shell out to a locally installed GHC for you. You can copy or symlink the same executable to `/usr/bin/runghc` and other paths as well for that matter.

## Default GHC version

If you leave off the `--ghc VERSION`, currently the executable will default to using GHC 8.10.4. This is _not good behavior_. Instead, there should be a config file for the default GHC version, potentially with environment variable and local filepath overrides, and some intelligent way of setting that default value on first execution. Contributions welcome to make that happen!

## Minimal command line

This tool intentionally does minimalistic command line parsing, to allow passthrough of arguments as easily as possible to underlying tools. Restrictions:

* This tool ignores all GHC RTS options (i.e., it sets `-rtsopts=ignoreAll`)
* The only location where `--ghc` is accepted is as the very first argument to the command
* The `exec` command is only recognized immediately following `--ghc VERSION` or, if that is absent, as the first argument

## Maintainers welcome!

I (Michael Snoyman) wrote the first version of this to get it off the ground. I welcome other contributors and maintainers on board. If you're interested, please reach out to me on just about any channel, including the Haskell Foundation Slack or this issue tracker.
