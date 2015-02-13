:raw-latex:`\pagebreak`

HOW-TO: Guide For The Most Usual SVN Operations
===============================================


Importing a SVN Module Initially
--------------------------------

Each Sim-Diasca module must be placed in a specific subdirectory, like::

  $ svn mkdir svn+ssh://<user>@<server>/<path>/<location>/<module_name>

For example::

  $ svn mkdir svn+ssh://armstrong@foo.org/home/svn/bar/trunk/my_module




Directory names are preferentially in lowercase only (ex: ``test-cases``), except specific acronyms (ex: ``NATO``).

The initial import is to be performed by copying the sources in a working copy containing a check-out of that module, and adding them, with ``svn add`` (which is recursive by default, unless the option ``-N`` is used).



Retrieving Content From SVN
---------------------------

A *check-out* has to be performed, from the specified location (which can be the trunk, a branch or a tag)::

  $ svn co svn+ssh://<user>@<server>/<path>/<location>/<module_name>

For example::

  $ svn co svn+ssh://armstrong@foo.org/home/svn/bar/tags/my_module-release-1.2.3/baz

.. comment
  $ svn co svn+ssh://armstrong@cln46mr.der.edf.fr/home/svn/Sim-Diasca/<location>/<module_name>





.. comment Les sources obtenues, se référer à la section `Se procurer Erlang`_ pour générer le simulateur.

.. comment Les sources obtenues, se référer à la section `SimIsInstall <http://cln46mr.der.edf.fr:8000/Sim-Diasca/wiki/SimIsInstall>`_ pour générer le simulateur.




Managing SVN Branches
---------------------

Creating a Branch
.................


 #. select the origin of that branch, i.e. the place from which the initial content of the branch should be copied (ex: ``trunk``, or another branch)

 #. check to the origin is up-to-date (``svn status`` / ``svn ci``)

 #. find a proper name for that branch (ex: based on a feature rather than on a developer name)

 #. create the branch: ``svn copy svn+ssh://<user>@<server>/<path>/<location> svn+ssh://<user>@<server>/<path>/branches/<branch name> -m "<comment>"``

 #. document it


.. Note:: Naming a branch according to a developer is generally a mistake, as the branch will be unusable once having been merged back to the trunk, whereas the developer could be still usable.



Merging of a Branch Into Another One
....................................

The goal here is to merge the content of a branch A into a branch B, so that B can benefit from the content of A.


.. Note::

  If ever A is a branch already having inherited, directly or not, from B, the operation must not be a simple *merge*, but a *merge-reintegrate*.

  Indeed if A inherits from B and is then reintegrated into B thanks to a simple merge, then the changes already operated on B before A inherited from it would be applied twice, resulting in an incorrect merge.



For all these merges, both the source and the target branches  must be up-to-date (all check-in operations performed, server synchronized).

Conflicts may occur when merging. As long as they will not be declared resolved and that a check-in is not performed, a merge can be abandoned, without impacting the server.

If wanting then to restore the previous state of the working copy, a recursive revert can be done (``svn revert .``).

Although all branches are managed identically by SVN, the following usual conventions are to be respected:

 - all vendor branches should be created in the top-level ``vendor`` directory (ex: ``vendor/common/0.1``)

 - all other branches should be created in the top-level ``branches`` directory (ex: ``branches/sim-diasca-2.0``)

 - names (all lowercases, words separated by a dash) should describe clearly the changes to be operated on that branch


Similarly, all tags should be created in the top-level ``tags`` directory (ex: ``tags/sim-diasca-release-2.0.6``).


Simple Merge
____________

Three parameters are generally involved:

 #. the server location of the origin branch (A)
 #. the server location of the destination branch (B)
 #. the local working copy on which differences will be applied

The goal is to update B from A.

Once done, both branches can still be modified.

It is possible and generally recommended to repeat this operation regularly (at the pace deemed the most convenient development-wise), in order to avoid that B becomes too different from A over time. Then a future reintegration of B into A would be considerably easier.


Merge-Reintegrate
_________________

This operation, called also *rebase*, consists on reintegrating a branch B into a branch A, whereas B had been created from A (directly or not, i.e. with intermediate branch(es) or not).

If a simple merge was performed, then the modifications inherited from A in B would be applied again when reintegrating in A, which would lead to incorrect merges.

To do so, the source control system has to keep track of which changes have been applied to which elements, which was not supported by SVN versions prior to the 1.5 one.

A side-effect of this newer SVN feature is that the the reintegrated branch (here, B) cannot be used anymore then. This is why branches should not be named according to user names, as the first reintegration will disallow the use of this name afterwards. So branches should preferably be named according to features or topics instead (ex: ``preparation-of-sim-diasca-2.0``).



Managing the SVN Tags
---------------------

Once the developments for a significant milestone are finished, a SVN tag should be set, in order to be able later to restore the code in that version.

A SVN tag named ``foo-release-x.y.z`` can be set (here on the current trunk) with::

	svn copy svn+ssh://<user>@<server>/<path>/<location> svn+ssh://<user>@<server>/<path>/tags/foo-release-x.y.z -m "<comment>"

In addition to these ``foo-release-x.y.z`` tags, which correspond to a major functional evolution of the software, one can define tags like ``milestone-yyyymmdd`` (ex: ``foo-milestone-20080924``), which may correspond to a project deadline. One can get inspiration from the already set tags.

The corresponding *check-out* for a tag is for example::

	svn co svn+ssh://<user>@<server>/<path>/tags/foo-release-x.y.z


Therefore developments are made in a branch whose name define only the major and minor version numbers (ex: ``branches/foo-2.0``), from which all the ``2.0.x`` releases will be generated, resulting in as many tags, such as ``tags/foo-release-2.0.1``.



Removing the Need Of Typing One's Password Endlessly
----------------------------------------------------

The SVN repository is accessed through SSH, and by default the SVN client will request the SSH password to be typed each time a remote operation is needed (ex: when committing versions), which becomes quickly annoying.

A solution is to generate a pair of public/private pair thanks to SSH, and to copy the *public* key on the SVN server.

Keys can be generated by replacing, in the next command, ``armstrong`` by your actual username on the SVN server::

	$ ssh-keygen -t rsa
	Generating public/private rsa key pair.
	Enter file in which to save the key (/home/armstrong/.ssh/id_rsa):
	Enter passphrase (empty for no passphrase):
	Enter same passphrase again:
	Your identification has been saved in /home/armstrong/.ssh/id_rsa.
	Your public key has been saved in /home/armstrong/.ssh/id_rsa.pub.
	The key fingerprint is: XX:XX:XX... armstrong@my_host.org


Under GNU/Linux this copy is preferably [#]_ done thanks to::

	$ ssh-copy-id -i ~/.ssh/id_rsa.pub armstrong@my_host.org


.. [#] To ensure permissions are correctly set.


There is no ``ssh-copy-id`` tool on Windows, thus one has to add manually the content of the ``~/.ssh/id_rsa.pub`` file to the authorised keys of your SVN server (ex: in the ``armstrong@my_host.org:.ssh/authorized_keys`` file).

On both platforms one can then check that a remote SSH login can be performed without a password, with::

	$ ssh armstrong@my_host.org



Comparing the Current Version of an Element With Its Previous One
-----------------------------------------------------------------

Run from a working copy::

   svn diff -r BASE:PREV My_File
