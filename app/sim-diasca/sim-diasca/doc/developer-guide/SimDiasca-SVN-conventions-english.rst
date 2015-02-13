-------------------------------------
Source Version Control For Sim-Diasca
-------------------------------------


General Information
===================

.. Note::
   Starting from April 2013, we switched from Subversion to Git, so the sections below will have to be updated.


Context
-------

The current internal work on the Sim-Diasca engine is made with Subversion (SVN), as a source version control system.

All Sim-Diasca developers are strongly encouraged to read the `Subversion official documentation <http://svnbook.red-bean.com/nightly/en/svn-book.html>`_ before contributing.


Similarly, applying most usual good practices (like described in `1 <http://subversion.open.collab.net/articles/best-practices.html>`_) is strongly advised.

.. Note:: We switched to versions of SVN which are at least 1.5, to take advantage of the more complete support for merges. Now our SVN server is in version 1.5 or more recent, the corresponding repository has been updated and user permissions have been fixed, thus the only remaining point to ensure is that each SVN client is itself in version 1.5 or more recent (one may use: ``svn --version --quiet`` to check).

Later, the use of GIT-SVN, then pure GIT is planned.



General Conventions
-------------------

All *check-in* operations must be commented, in English, with a long enough message to be explicit (ex: ``Waiting mechanism fixed for stochastic actors waiting for terminating actors.``), without paraphrasing the changes (that can be obtained anyway, thanks to ``svn diff``).

A check-in should involve all the changes scattered over possibly multiple files regarding one complete logical modification, so that one SVN transaction encompasses a relevant thematic change-set.

Major operations, notably the ones affecting a large number of files, should be kept track of as such, with a textual description. This should be the case notably branch creation and tag creation.

Intermediate check-in operations, dealing with code not having reached yet its final stage, are allowed, but the check-ined code must be at least able to compile, in order not to break the build of other users of the same branch.

Tags should be set on a regular basis, and should be regarded more generally as inexpensive operations.



TO-DO
-----

 - discuss the "rebase" frequency, the release or feature branches
 - consider using ``svn hooks``, to be triggered automatically whenever a certain type of SVN operation occurs (ex: mail-based check-in notification, check-in rejected if its message does not respect constraints like minimum length, etc.)
