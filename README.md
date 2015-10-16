# uplift

A Clojure library to help deploy and provision virtual machines

# What is uplift for?

Currently uplift is just a pile of libraries to help provision a system for testing subscription
manager and candlepin.  It is focused on developer/ad-hoc testing so that tests can become semi-
automated.

- Helper Tools
  - ~~Create repo files for various systems~~
  - checksum calculation (low-priority)
  - create activation key
  - candlepin API wrappers
  - Look into ability to call with SSH and grabbing stdout in separate thread
  - /var/log/rhsm/rhsm.log monitor
- Start experimenting with selenium
  - automate the ISO download checksum
- Improved way to do development and one-off tests
  - Use Cases:
    - Make it easier for people not familiar with clojure to run a test
    - Make it easier to test development code (nightlies)
	- ~~Improve runtestng (run tests from the repl)~~
  - Create an agent that uses messaging system
    - low-level binary message exchange and edn
  - Figure out a way to separately compile the java classes into a jar
  - Make it easy for others to kick off a test (not through jenkins)
    - ~~Be able to select a test within an xml suite file~~
    - web interface to setup automation.properties
    - auto-create a devel candlepin VM
    - auto-create a devel client VM through web interface
  - ~~Run tests in a clojure environment (via lein or boot)~~
    - How to embed a nrepl for debugging?
  - Spin up a KVM instance quickly
    - Setup networking
    - Get IP address of VM programmatically (via serial console)
    - Provide a list of URLs for network installs

Other possibilities
  - JNI/JNA/JNR bindings for at-spi2-core (libatspi)?

## Design

There are essentially 2 parts to uplift:

1. A bootstrapping part where a VM

## Usage

TODO:

The most important thing for now is to create a user.edn file somewhere on your system and note
the location in the uplift/resources/properties.edn file.  This is where sensitive information
such as passwords or URL's for files can be specified.

## License

Copyright © 2015 FIXME

Distributed under the Apache2 Public License

