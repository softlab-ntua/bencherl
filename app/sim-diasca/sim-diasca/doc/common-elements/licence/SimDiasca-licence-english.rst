:raw-latex:`\pagebreak`


Sim-Diasca Licence
==================



Licensing Terms
---------------

Sim-Diasca is released under the LGPL licence (*GNU Lesser General Public License*).

The full text of this licence is available in the ``GPL-licence.txt`` and ``LGPL-licence.txt`` files (since the LGPL is based on the GPL), and both files are included in each Sim-Diasca release.

See also the `LGPL <http://www.gnu.org/licenses/lgpl-3.0.html>`_ official homepage.

The advices listed in `How to use GNU licenses for your own software <http://www.gnu.org/licenses/gpl-howto.html>`_ have been applied, as discussed in the next section.





Preparing The Open-Source Licensing of Sim-Diasca
---------------------------------------------


Licence Files
.............

Two files have been added to the Sim-Diasca source distribution:

 - ``LGPL-licence.txt``: the LGPL licence, defined on top of the GPL one
 - ``GPL-licence.txt``: the GPL licence, base of the LGPL licence



File Header
...........

Each source file should include:

 - a copyright notice
 - a statement of copying permission, saying that the program is distributed under the terms of the GNU Lesser General Public License


The copyright notices change every year (ex: ``2008-2010`` is to become ``2008-2011``). Updating it is as simple as starting from a fully committed tree, in the ``sim-diasca`` directory, and running::

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca/branches/sim-diasca-2.0/mock-simulators "2008-2011 EDF R\&D" "2008-2012 EDF R\&D"

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca/branches/sim-diasca-2.0/sim-diasca "2003-2011 EDF R\&D" "2003-2012 EDF R\&D"


.. Comments Also:

  Simplest:

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca/vendor "2003-2011 Olivier Boudeville" "2003-2012 Olivier Boudeville"

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca/branches/sim-diasca-2.0/mock-simulators "2008-2010 EDF R\&D" "2008-2011 EDF R\&D"



  - otherwise -

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca "2003-2010 Olivier Boudeville" "2003-2011 Olivier Boudeville"

  update-copyright-notices.sh Erlang $HOME/Projects/Sim-Diasca/sources/Sim-Diasca/vendor/wooper/1.0/ "2003-2010 Olivier Boudeville" "2003-2011 Olivier Boudeville"

  update-copyright-notices.sh Erlang /home/boudevil/Projects/Sim-Diasca/sources/Sim-Diasca/vendor/traces/0.3/ "2003-2010 Olivier Boudeville" "2003-2011 Olivier Boudeville"
Then once having checked that the operation succeeded, a check-in should be performed immediatly.



For An Erlang file
__________________


For either a ``.hrl`` or a ``.erl`` file, current version of the header is::

  % Copyright (C) 2008-2010 EDF R&D

  % This file is part of Sim-Diasca.

  % Sim-Diasca is free software: you can redistribute it and/or modify
  % it under the terms of the GNU Lesser General Public License as
  % published by the Free Software Foundation, either version 3 of
  % the License, or (at your option) any later version.

  % Sim-Diasca is distributed in the hope that it will be useful,
  % but WITHOUT ANY WARRANTY; without even the implied warranty of
  % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  % GNU Lesser General Public License for more details.

  % You should have received a copy of the GNU Lesser General Public
  % License along with Sim-Diasca.
  % If not, see <http://www.gnu.org/licenses/>.

  % Author: Olivier Boudeville (olivier.boudeville@edf.fr)


Previous version was::

  % Copyright (C) 2008,2009 EDF R&D

  % This file is part of Sim-MS.

  % Sim-MS is free software: you can redistribute it and/or modify
  % it under the terms of the GNU Lesser General Public License as
  % published by the Free Software Foundation, either version 3 of
  % the License, or (at your option) any later version.

  % Sim-MS is distributed in the hope that it will be useful,
  % but WITHOUT ANY WARRANTY; without even the implied warranty of
  % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  % GNU Lesser General Public License for more details.

  % You should have received a copy of the GNU Lesser General Public
  % License along with Sim-MS.
  % If not, see <http://www.gnu.org/licenses/>.

  % Author: Olivier Boudeville (olivier.boudeville@edf.fr)


Use for that the ``licence-header-erlang.txt`` header file.



All-Purpose Text Header
_______________________

This is::

  Copyright (C) 2008-2010 EDF R&D

  This file is part of Sim-Diasca.

  Sim-Diasca is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of
  the License, or (at your option) any later version.

  Sim-Diasca is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Sim-Diasca.
  If not, see <http://www.gnu.org/licenses/>.

  Author: Olivier Boudeville (olivier.boudeville@edf.fr)


Previous version was::

  Copyright (C) 2008,2009 EDF R&D

  This file is part of Sim-MS.

  Sim-MS is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of
  the License, or (at your option) any later version.

  Sim-MS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Sim-MS.
  If not, see <http://www.gnu.org/licenses/>.

  Author: Olivier Boudeville (olivier.boudeville@edf.fr)



Images
......

 - `LGPL logo <http://www.gnu.org/graphics/license-logos.html>`_
 - `EDF logo <http://identite-groupe.edf.com/media_indis_logo_4.php>`_
