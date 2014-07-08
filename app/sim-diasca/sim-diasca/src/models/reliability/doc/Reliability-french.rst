.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. contents::
	:depth: 2
	:local:


.. _fiabilité:


Fiabilité des équipements
=========================


Présentation
------------

La fiabilité dans le simulateur est modélisée sous la forme de pannes (totales) et de réparation (totales aussi) affectant un équipement.

Ces pannes et ces réparations suivent des lois probabilistes déterminées par différents modèles de pannes et différents modèles de réparation.

Les notions suivantes sont utilisées :

 - ``MTTF`` signifie *Mean Time To Failure*, la durée moyenne avant qu'un équipement initialement fonctionnel tombe en panne

 - ``MTTR`` signifie *Mean Time To Repair*, la durée moyenne avant qu'un équipement initialement en panne soit réparé et devienne à nouveau fonctionnel

 - ``MTBF`` signifie *Mean Time Between Failures*, la durée moyenne entre deux pannes successives du système : ``MTBF = MTTF + MTTR``.



:raw-latex:`\pagebreak`

.. include:: FailureModels-french.rst


:raw-latex:`\pagebreak`

.. include:: RepairModels-french.rst
