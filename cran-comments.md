## Test environments
* local OS X install, R 3.5.1
* ubuntu 16.04.6 (on travis-ci), R 3.6.1
* mac OS X 10.13.6 (on travis-ci), R 3.6.2
* Windows Server 2012 R2 build 9600 (on AppVeyor), R 3.6.2 

## R CMD check results

0 errors | 0 warnings | 0 note


## rhub check

* Windows Server 2008 R2 SP!, R-devel

0 errors | 0 warnings 

One note

- In checking for non-standard things in the check directory: found the following
files/directories: 'flying-Ex_x64.Rout' 'tests_i386' 'tests_x64' 'examples_i386'
'examples_x64' 'flying_i386.Rout'. (I have no idea on how to solve this)

* Ubuntu Linux 16.04 LTS, R-release, GCC

0 errors | 0 warnings

There were 2 notes

- Possibly mis-spelled words in DESCRIPTION (false warning)

- Examples with CPU or elapsed time > 5s (5.058 s exactly)

* Fedora Linux, R-devel, clang, gfortran

0 errors | 0 warnings

There were two notes

- Possibly mis-spelled words in DESCRIPTION (false warning)

- Examples with CPU or elapsed time > 5s (5.002 s exactly)

* Debian Linux, R-devel, GCC ASAN/UBSAN

- PREPERROR- I suspsect this is not a problem with the package.

* Oracle Solaris 10, x86, 32 bit, R-patched (experimental)

- ERROR: * checking package dependencies ... ERROR
Package required but not available: ‘kableExtra’

Package suggested but not available: ‘testthat’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

I suppose this error is not in my control.

## Fixed from v 0.1.1 (Oracle Solaris 10)

* minimum_power_speed.cpp:22:36: warning: ISO C++ says that these are ambiguous, even though the worst conversion for the first is better than the worst conversion for the second:

*  total_mech_pow.cpp:23:55: warning: ISO C++ says that these are ambiguous, even though the worst conversion for the first is better than the worst conversion for the second:

## Fixed from v 0.1.0 comments

* Software names package names and API names in single quotes in DESCRIPTION

* DOI, ISBN, arXiv added to references in DESCPRIPTION
