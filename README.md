# suns-search v1.0.0

The Suns protein search engine lets you perform fast all-atom searches on
protein structures

Note that the DeGrado lab hosts a public search engine which you can use for
free at `suns.degradolab.org`.  The Suns `PyMOL` plugin and the `suns-cmd`
command line tool both will send searches to this public server by default and
if you are satisfied with that server you do not need to deploy your own server
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

## Requirements

In order to deploy your own server, you must install the `nix` package manager,
which you can find here:

* [https://nixos.org/nix/](https://nixos.org/nix/)

Then you must install the `nixops` deploy tool by running:

```bash
$ nix-env --install --attr nixops --file ./release.nix
```

## Specify PDB files to index

This repository comes with a `pdb/` subdirectory with one example PDB file
inside (`2GBP.pdb`).  Before deploying the server you must populate this
directory with your desired set of PDB structures that you would like the SUNS
server to index and make searchable

Note that `suns-server` requires approximately 1 GB of working memory per 270
chains, so factor that into how many structures you want to be able to search.

If you're not sure what to use, try one of the PISCES datasets curated by the
Dunbrack lab:

* [http://dunbrack.fccc.edu/PISCES.php](http://dunbrack.fccc.edu/PISCES.php)

## Specify motifs to index

You can optionally change the set of motifs that Suns indexes, too.  If you
don't want to do that then skip this section.

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

## Deployment

This is set up so that you can use `nixops` to deploy the server.  `nixops`
supports several deployment options, including (but not limited to):

* Deployment to an AWS EC2 instance
* Deployment to a VirtualBox virtual machine
* Deployment to a `libvirtd` virtual machine running on NixOS

The following sections explain how to deploy to the above types of machines

Note that `nixops` also supports deploying to an existing machine running NixOS,
but that's not covered in this manual

Also, if you have any issues with the following instructions, you might want to
check out the NixOps manual:

* [https://nixos.org/nixops/manual/](https://nixos.org/nixops/manual/)

The manual also includes instructions for additional ways that you can customize
your deployment beyond the defaults configured in this repository

## Deploy to an AWS EC2 instance

You will need to create an AWS account with programmatic access by following
the instructions here:

* [http://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html](http://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html)

... and then saving your credentials to `~/.aws/credentials`, like so:

```
[default]
aws_access_key_id     = AKIA...
aws_secret_access_key = ...
```

Make sure that this `~/.aws/credentials` file is not world-readable!

Modify the `nix/ec2.nix` to specify the AWS region and instance size you want
to use.  The deployment uses `t2.micro` by default, which is eligible for 750
hours of [free tier usage](https://aws.amazon.com/free/faqs/).

Once you are done, you can run:

```bash
$ nixops create --deployment suns nix/ec2.nix nix/logical.nix
$ nix/deploy.sh
```

## Deploy to a VirtualBox virtual machine

You must first install VirtualBox from:

* [https://www.virtualbox.org/wiki/Downloads](https://www.virtualbox.org/wiki/Downloads)

Modify `nix/vbox.nix` to configure how much memory (in MB) that you wish to
provide the virtual machine and then run:

```bash
$ nixops create --deployment suns nix/vbox.nix nix/logical.nix
$ nix/deploy.sh
```

## Deploy to a `libvirtd` virtual machine on NixOS

Follow the instructions here to set up `libvirtd` on your NixOS machine:

* [https://nixos.org/nixops/manual/#idm140737318336624](https://nixos.org/nixops/manual/#idm140737318336624)

Modify `nix/virtd.nix` to configure how much memory (in MB) that you wish to
provide the virtual machine and then run:

```bash
$ nixops create --deployment suns nix/virtd.nix nix/logical.nix
$ nix/deploy.sh
```

## Now what?

Once the deploy completes the server is ready to go.  You can obtain the server
IP address by running:

```bash
$ nixops info --deployment suns
```

... and once you point your `PyMOL` SUNS plugin to that IP address you can begin
searching for protein structures.

You can also log into the server by running:

```bash
$ nixops ssh --deployment suns machine
```

... and you can turn off the machine by running:

```bash
$ nixops stop --deployment suns
```

... and you can also destroy all traces of the instance or virtual machine by
running:

```bash
$ nixops destroy --deployment suns
$ nixops delete  --deployment suns
```

## License (GPLv2)

Copyright 2013-2017 Gabriel Gonzalez

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
