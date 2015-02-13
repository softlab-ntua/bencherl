.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _gestionnaire de temps:
.. _gestionnaires de temps:

.. _ordonnanceur:
.. _ordonnanceurs:



Modélisation des gestionnaires de temps (``TimeManager``)
=========================================================


Objectif
--------

Le gestionnaire de temps synchronise finement chaque acteur de la simulation, afin de maintenir causalité et reproductibilité des simulations dans un contexte massivement parallèle et distribué.



Fonctionnement
--------------

Le temps de la simulation (temps ``virtuel``) est complètement décorrélé du temps réel (perçu par l'utilisateur). Le temps virtuel est discret et progresse à pas constant (``simulation tick``). La durée - exprimée en temps virtuel - de chaque pas peut être choisie librement, sa valeur par défaut est ``TickDuration = 20 ms`` (50 Hz, la fréquence du secteur).

Le temps courant d'un acteur donné s'obtient avec la fonction ``class_Actor:get_current_tick/1``.

Pour convertir un nombre de pas de temps en durée simulée, utiliser ``class_TimeManager:convertTicksToSeconds/{1,2``}. La conversion inverse s'obtient avec ``class_TimeManager:convertSecondsToTicks/{1,2}``.


Implémentation
--------------

Le gestionnaire de temps est implémenté dans la classe définie par class_TimeManager.hrl_ et class_TimeManager.erl_.

Il est testé notamment dans :

 - ``class_TimeManager_interactive_test.erl`` pour le mode interactif et ``class_TimeManager_batch_test.erl`` pour le mode non-interactif (tests unitaires)

 - ``timeManagerAndActorPair_test.erl`` (test d'intégration avec les acteurs de la simulation)



Voir aussi :

 - les `acteurs`_ de la simulation, cadencés par le gestionnaire de temps

 - le `gestionnaire de variables stochastiques`_, devant fonctionner en synergie avec le gestionnaire de temps
