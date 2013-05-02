GateSenseAggr
=============

Hacker Days

How to get started
------------------

After cloning the repo, run

    make deps
    make

(If you get a "cannot find pam_appl.h", install libpam0g-dev (for Ubuntu).)

To run the server:

    make rel
    make node-console

then access link:http://localhost:8080/aggrsense[http://localhost:8080/aggrsense].

Good luck! :-)
