.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _modèle de panne:
.. _modèles de panne:



Modélisation des pannes d'équipement (``FailureModel``)
=======================================================


Présentation
------------

Le modèle de panne générique permet de déterminer les modalités des pannes affectant le système, c'est-à-dire notamment les instants de survenue de pannes d'équipements préalablement fonctionnels.


:raw-html:`<img src="failure-models.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{failure-models.png}`


Les spécialisations du modèle de panne générique sont :

 - loi exponentielle, la plus fidèle aux pannes rencontrées sur le terrain dans le cas général : class_ExponentialFailureModel.erl_

 - loi gaussienne : class_GaussianFailureModel.erl_



Relations avec les autres modèles
.................................


Ces modèles sont amenés à interagir avec :

 - les `équipements`_, qui sont affectés par les pannes



Implémentation
--------------

Le modèle de panne générique est implémenté dans class_FailureModel.erl_.

Les classes spécialisées qui en héritent sont class_ExponentialFailureModel.erl_ et class_GaussianFailureModel.erl_.
