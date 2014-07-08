.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _modèle de réparation:
.. _modèles de réparation:


Modélisation des réparations d'équipement (``RepairModel``)
===========================================================


Présentation
------------


Le modèle de réparation générique permet de déterminer les modalités des réparations opérées sur le système, c'est-à-dire les durées de réparation des équipements tombés préalablement en panne.


:raw-html:`<img src="repair-models.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{repair-models.png}`



Les spécialisations du modèle de réparation générique sont :

 - loi uniforme (instants de réparation équiprobables, bruit blanc) : class_UniformRepairModel.erl_

 - loi gaussienne, la plus fidèle aux réparations rencontrées sur le terrain dans le cas général : class_GaussianRepairModel.erl_




Relations avec les autres modèles
.................................


Ces modèles sont amenés à interagir avec :

 - les `équipements`_, qui, une fois affectés par une panne, peuvent être réparés



Implémentation
--------------

Le modèle de réparation générique est implémenté dans class_RepairModel.erl_.

Les classes spécialisées qui en héritent sont class_UniformRepairModel.erl_ et class_GaussianRepairModel.erl_.
