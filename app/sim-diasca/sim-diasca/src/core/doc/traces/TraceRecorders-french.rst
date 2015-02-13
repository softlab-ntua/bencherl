.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _enregistreur de trace:
.. _enregistreurs de trace:

.. _enregistreur:
.. _enregistreurs:



Enregistreurs de traces (``TraceRecorder``)
===========================================


Rôle
----

Les enregistreurs de traces assurent le stockage persistant des traces, sous forme de simple fichier texte ou d'une base de données requêtable, potentiellement temps réel et répartie.


Implémentation
--------------

A l'heure actuelle les traces sont directement enregistrées dans un simple fichier par l'`agrégateur`_ de traces utilisé.

A terme il s'agira de les enregistrer dans une base de données (ex: ``Mnesia``), au moyen d'un agrégateur de traces évolué.
