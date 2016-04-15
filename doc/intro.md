# Introduction to uplift

Uplift has the primary concern of being a provisioning utility to setup a remote system.


## Namespaces


### config

The config namespace contains functions to read config files (including python style)
config files, edit them, and write out a new config file

This is useful, as we often need to edit or create repo files, which are actually python
style config files.  This library can be used for that purpose.

### core

The core library mostly deals with yum package helpers, but it also contains generically
useful functions.  This namespace should only handle functions which are broad in nature


### deploy

This is a very rhsm-qe specific namespace, and contains the functionality to provision
all the components necessary for GUI testing.

TODO:  Turn this into a PheiModule that can be installed by a Pheidippides controller