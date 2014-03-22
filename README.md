# suns-search v1.0.0

The Suns protein search engine lets you perform fast all-atom searches on
protein structures

Note that the DeGrado lab hosts a public search engine which you can use for
free at `suns.degradolab.org`.  The Suns `PyMOL` plugin and the `suns-cmd`
command line tool both will send searches to this public server by default and
if you are satisfied with that server you do not need to build your own server
using this source package.

The main reasons you might want to build this package and host your own search
engine are:

* You are a company that wants to host its own search service on a private
  intranet so that the search queries never leave your network

* You want to customize the searchable motifs or protein structures

* The server at `suns.degradolab.org` is unavailable

This package does **not** include either the Suns `PyMOL` plugin, nor the
`suns-cmd` command line search tool.  Those are provided separately at:

* `PyMOL` client: <http://degradolab.org/suns/>
* `suns-cmd`: <https://github.com/Gabriel439/suns-cmd>

## Overview

The search engine has two components:

* The search engine server (what this source package provides)
* A message queue that forwards client requests to servers and forwards server
  responses to clients

The **message queue** is what both clients and servers connect to, so you must
install that on a computer that is accessible to both of them.  That means that
if you are setting up a custom search engine and you want your `PyMOL` plugin or
`suns-cmd` program to talk to the search engine, you must point it to the
address of the **message queue**, not the search engine.

Note that you can connect multiple search engines to the message queue to handle
a higher search request load.  Similarly, multiple clients can connect to the
same message queue without interfering with each other's requests, as long as
there are a sufficient number of search engines to handle the request volume.

## Requirements

* `lapack`, `gsl`, `ncurses` installations with header files
* The Haskell Platform
* A properly configured `rabbitmq-server` installation (see below)

Debian is the easiest system to set up these dependencies:

    # aptitude install liblapack-dev \
    >                  libgsl0-dev \
    >                  ncurses-dev \
    >                  haskell-platform \
    >                  rabbitmq-server

## Installation

    $ cabal update                 # Required after Haskell Platform install
    $ cabal install cabal-install  # Get latest `cabal` (If using an old Haskell Platform)
    $ ~/.cabal/bin/cabal install   # Install `suns-search` using updated `cabal`

This produces three executables in your `~/.cabal/bin/` directory:

* `suns-admin`: initially configures the message queue
* `suns-index`: builds a search index from a set of motifs and structures
* `suns-server`: the search engine which handles search requests

Then, install `rabbitmq-server` and configure it by running the
`sh/suns-configure.sh` script with root privileges.  This does three things:

* Creates a `suns-vhost` virtual host on the rabbitmq message queue
* Creates three users:
    * `suns-admin`: user with full access rights to configure `suns-vhost`
    * `suns-server`: user with permission to handle search requests
    * `suns-client`: user with permission to add requests and receive results
* Configures user permissions

The `sh/suns-configure.sh` script will prompt you for two passwords:

* The admin password that the `suns-admin` program uses
* The server password that the `suns-server` program uses

Then run `suns-admin`, entering the admin password.  This configures the
necessary exchanges and queues that the client and search engine need:

    ~/.cabal/bin/suns-admin

For alternative options (particularly if the message queue is not located on the
same computer), type:

    ~/.cabal/bin/suns-admin --help

Next, you must build a search index using the `suns-index` program.  This
requires two inputs:

* A `motif` directory containing the desired motifs
* A `pdb` directory containing all the protein structures you want to search

A default `motif` directory is provided with a reasonable default set of motifs.
Each sub-directory corresponds to a searchable motif, and the PDB files
contained within these sub-directories correspond to specific instances of each
motif type.

You need at least one PDB file per residue type that the motif matches.  For
example, the `peptide_bond` motif has one file per residue, since it matches all
twenty residue types.  Each match must have the same number of atoms and the
atom order between matches must be consistent.

Also note that motifs are indexed by bonds, not by atoms.  That means that you
cannot define a single-atom motif and it also means that any non-bonded atoms
will be ignored.

Note that if you are using the `PyMOL` plugin and choose to customize the
`motif` directory, you will also need to rebuild the `PyMOL` plugin to recognize
the new motifs you have chosen.  This is because the `PyMOL` plugin uses the
`motif` definitions to automatically expand selections to motifs.  See the
`PyMOL` plugin build instructions for details on how to customize its behavior.

If you are using the `suns-cmd` command line tool to search the database you do
not need to rebuild it in response to changes to the `motif` directory.

The `pdb` directory is not provided by default and you must populate this with
your desired database of structures:

    $ mkdir pdb
    $ # ... now copy as many structures as you want to this directory

Note that `suns-server` requires approximately 1 GB of working memory per 270
chains, so factor that into how many structures you want to be able to search.

Once you have set up a `pdb` directory you can build your index:

    $ mkdir index
    $ ~/.cabal/bin/suns-index

This builds the index and stores it within the `index` directory.

For alternative options (mainly to change the default directories), type:

    $ ~/.cabal/bin/suns-index --help

Once you've done that you are ready to run the search engine:

    $ ~/.cabal/bin/suns-server --timeout 10000  # 10000 ms timeout

The timeout option is the most important one, which allows you to limit the
duration of search requests.  If you omit it, the server will not impose any
time limit on requests and process them until they complete.

For alternative options (like connecting to a message queue on a different
computer), type:

    $ ~/.cabal/bin/suns-server --help

The most important part is ensuring that the server connects to the message
queue you just configured.  If the message queue is located on a different
computer use the `-n` option to specify its address.

Once the search engine is running and connected to the message queue you can
begin running searches.  Just point your `PyMOL` plugin or `suns-cmd` tool to
the message queue's address and you can begin searching for protein structures.

## License (GPLv2)

Copyright 2013 Gabriel Gonzalez

This file is part of the Suns Search Engine

The Suns Search Engine is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 2 of the License, or (at your option) any
later version.

The Suns Search Engine is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with the
Suns Search Engine.  If not, see <http://www.gnu.org/licenses/>.
