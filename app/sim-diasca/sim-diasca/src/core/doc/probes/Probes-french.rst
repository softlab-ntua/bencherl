.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _sonde:
.. _sondes:


Modélisation des sondes applicatives (``Probe``)
================================================


Présentation
------------

Ces modèles permettent de collecter des données en cours de simulation, et d'en obtenir une vue graphique.


Modélisation
------------


Sondes génériques
.................

Une sonde générique peuvent être configurée dynamiquement pour collecter et générer une visualisation graphique de données quelconques, y compris en terme de nombre de courbes suivies.

Leur sortie graphique est du type :

:raw-html:`<img src="Generic_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{Generic_probe_example.png}`


La sonde générique est définie dans class_Probe.erl_. Elle est testée avec class_Probe_test.erl_.




Sondes spécialisées
...................

.. _sonde de fiabilité:
.. _sondes de fiabilité:


Sondes de fiabilité
___________________


Une fois associée à un `équipement`_, une sonde de fiabilité enregistre à chaque pas de temps l'état de fonctionnement de l'équipement (nominal ou en panne), et peut en générer la chronologie sous la forme suivante :

:raw-html:`<img src="Reliability_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{Reliability_probe_example.png}`


La sonde de fiabilité est définie dans ``class_ReliabilityProbe.hrl`` et ``class_ReliabilityProbe.erl``.
