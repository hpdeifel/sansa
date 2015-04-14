# Sansa

`sansa` is a commandline frontend for the [aria2] download manager.

It let's you do the things you would expect from a download manager,
such as queuing files for download, pausing them or observing the
download progress from the command line.

## Installation

`sansa` is written in Haskell. To build Haskell programs, install the
[Haskell platform], which gives you the GHC compiler and the
cabal-install package manager. Then run the following commands in the
top-level source directory:

    runhaskell Setup.hs configure
    runhaskell Setup.hs build
    runhaskell Setup.hs install

This will install the binary into `~/.cabal/bin/`. See

    runhaskell Setup.hs configure --help

for options to change the install path.

## Usage

To use `sansa`, an `aria2` daemon has to be running. Start it with:

    aria2c --enable-rpc

See the excellent `aria2` man page for more options.

The basic command syntax of `sansa` is:

    sansa COMMAND

where COMMAND is one of one of the available subcommands. To get a
list of those, type `sansa --help`.

`sansa` is mainly documented trough it's own `--help` output, so be
sure to read it. Help for individual subcommands can be optained with
`sansa SUBCOMMAND --help`.

Instead of repeating the help output in this README, here are some
example invocations with their output:

## Examples

```
sansa add add http://cdimage.debian.org/cdimage/weekly-builds/amd64/iso-dvd/debian-testing-amd64-DVD-1.iso
```

```
sansa status
```

> #1 active
> Download: [###                 ] (17%)  648.84 MiB / 3.70 GiB
> Upload:   [                    ] (0%)   0.00 B / 3.70 GiB
> Speed     46.15 MiB/s Down, 0.00 B/s Up
> Files:
>   /tmp/debian-testing-amd64-DVD-1.iso


[aria2]: http://aria2.sourceforge.net/
[Haskell platform]: https://www.haskell.org/platform/
