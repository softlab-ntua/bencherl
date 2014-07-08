.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _équipement:
.. _équipements:



Modélisation des équipements (``Equipment``)
============================================


Présentation
------------


 Un équipement est un `acteur`_ spécialisé, susceptible de tomber en panne et d'être réparé.

Pour cela il fait appel à un `modèle de panne`_ et à un `modèle de réparation`_.




Modélisation
------------



Relations avec les autres modèles
.................................


Ce modèle est amené à interagir avec :

 - les `modèles de panne`_, notamment ceux fondés sur une distribution exponentielle (cf modélisation du MTTF)

 - les `modèles de réparation`_, notamment ceux fondés sur une distribution gaussienne (cf modélisation du MTTR)

 - les `sondes de fiabilité`_ : tout équipement peut avoir son état de fonctionnement suivi par une telle sonde



Implémentation
--------------

La classe ``Equipment``, dont le modèle générique est implémenté dans class_Equipment.erl_, redéfinit automatiquement la méthode ``act/1`` héritée de la classe ``Actor``.

Chaque spécialisation de la classe ``Equipment`` doit en retour définir deux méthodes, ``actNominal/1`` et ``actInDysfunction/1``, pour spécifier le comportement de l'équipement respectivement en régime nominal (i.e. sans être en panne) et en régime défaillant (i.e. en étant en panne).

Si le comportement effectif est délégué à ces deux méthodes, la terminaison de pas de temps reste à la charge de la méthode ``act/1``, car c'est elle qui appellera, en interne à l'instance, celle de ces deux méthodes qui sera appropriée en fonction de l'état de l'objet, de manière synchrone (bloquante et sans échange de message).

Les transitions (changements d'état de l'équipement en terme de panne) sont gérées de manière transparente par la classe-mère ``Equipment`` . En pratique elles sont déclenchées par des appels de méthodes synchronisées, entre l'équipement générique et les modèles de panne et de réparation qui lui sont associés.

L'état d'un équipement est notamment défini par les attributs suivants :

 - ``current_failure_state = nominal | dysfunction``, selon qu'il fonctionne correctement ou non

 - ``next_failure_tick = Integer | undefined``, égal à la valeur du pas de temps pour la prochaine panne (si cette valeur est disponible)

 - ``next_repair_tick = Integer | undefined``, égal à la valeur du pas de temps pour la prochaine réparation (si cette valeur est disponible)




Voir aussi
----------

 - class_TestEquipment.erl_ : équipement de test, et source d'inspiration pour définir son propre équipement spécialisé

 - equipment_integration_test.erl_ : test d'intégration entre des équipements, des modèles de panne et de réparation, et des sondes de fiabilité
