# uplift

A Clojure library to help deploy and provision virtual machines

# What is uplift for?

Currently uplift is just a pile of libraries to help provision a system for testing subscription
manager and candlepin.  It is focused on developer/ad-hoc testing so that tests can become semi-
automated.

## Roadmap

- Helper Tools
  - ~~Create repo files for various systems~~
  - Dependency checker and installer
  - checksum calculation (low-priority)
  - create activation key
  - candlepin API wrappers
  - Look into ability to call with SSH/ProcessBuilder and grabbing stdout as it runs
    - The clojure commons exec appears to have this, and the ssh might too
    - Need to pump the stdout back across a websocket or socket if remote
  - /var/log/rhsm/rhsm.log monitor
    - Mechanism to write logic to send event when something of interest is seen
- Create an agent that uses messaging system
  - low-level binary message exchange and edn
  - Transport over TLS encrypted socket
- Improved way to do development and one-off tests
  - Use Cases:
    - Make it easier for people not familiar with clojure to run a test
    - Make it easier to test development code (nightlies)
	- ~~Improve runtestng (run tests from the repl)~~
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
  - Java dbus hooks to notify remote user of dbus events?
  - Packaged in a way that can be AOT compiled so it can be used by other JVM languages

## Design

There are essentially 2 parts to uplift:

1. A bootstrapping part where a VM is provisioned with the bare minimum
2. Starting a service agent on the test machine

Currently, uplift really is in a very very early stage, and can only barely do the first part.
The second (and more interesting part) is still to be done.  However, some of the functionality
in the various namespaces are working, however it is not yet hooked into a messaging system.

The goal here is to have uplift contain the low-level libraries which can be called by various
services.  The two services conceived of so far are a REST based API as well as a simple message
bus using TLS sockets with a binary data transfer (using fressian most likely)

![Image of Message Flow]
(https://github.com/RedHatQE/uplift/tree/master/doc/MasterFlow.png)

Since uplift can send events of interest, the client/server system should be asynchronous.  For
example, with a log monitor, the client should be notified of an event when some text or pattern
is detected from the logs.


## Usage

TODO:

The most important thing for now is to create a user.edn file somewhere on your system and note
the location in the uplift/resources/properties.edn file.  This is where sensitive information
such as passwords or URL's for files can be specified.

## License

Copyright © 2015 FIXME

Distributed under the Apache2 Public License

