.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _acteur:
.. _acteurs:



Modélisation des acteurs de la simulation (``Actor``)
=====================================================


Présentation
------------

La notion d'*acteur de la simulation* (ou plus simplement d'*acteur*) est la brique de base (la classe-mère de plus haut niveau) de toute simulation : tout élément simulé est un acteur (directement ou indirectement, via spécialisation par héritage), et chaque acteur est synchronisé finement par le `gestionnaire de temps`_ (**TimeManager**) afin de préserver la causalité et la reproductibilité des comportement d'acteurs.


Synchronisation & échanges de messages entre acteurs
----------------------------------------------------


Ordonnancement des requêtes
...........................

Le fait que la prise en compte des messages reçus s'effectue juste après la frontière de pas de temps implique que si A envoie une requête à B à T (nécessitant une réponse), il n'aura sa réponse qu'à T+2 au lieu de T+1 (mais dans tous les cas il ne pourra pas réagir avant T+2, comme prévisible).


Implémentation
--------------


Terminaison d'un pas de temps
.............................

A la fin de son pas, chaque acteur, qu'il ait envoyé un message inter-acteurs ou non, doit appeler ``class_Actor:manage_end_of_tick/1``. Cette fonction gère complètement la terminaison du pas courant : attente asynchrone des accusés de réception des éventuels messages inter-acteurs envoyés, envoi de la notification au gestionnaire de temps, préparation du pas suivant, etc.

Chaque clause de la méthode ``act/1`` (probablement surchargée) finit donc généralement par : ``?wooper_return_state_only( class_Actor:manage_end_of_tick(LatestState) )``



.. comment

   Chaque méthode d'acteur (*Actor Method*) doit positionner son attribut ``termination_mode``

   % This method must set its termination_mode attribute to 'terminated' if
   % it determines the actor has to disappear at next tick (in this case this
   % actor should notify the actors that may send it messages, as otherwise they
   % would freeze, since never receiving acknowlegment for their message to this
   % actor).
   % If this method does not send any actor message, then it must call
   % send_end_of_tick_notification just before returning.
   % If this method sends at least one actor message, then it must not send any
   % end-of-tick notification here, as it will be sent automatically as soon as
   % all sent messages will have been acknowledged by their recipients.



Les acteurs sont implémentés par la classe définie dans ``class_Actor.hrl`` et ``class_Actor.erl``.

Voir aussi:

 - le `gestionnaire de temps`_, qui cadence tous les acteurs, et rien qu'eux

 - l'`acteur stochastique`_, un acteur spécialisé dont le modèle fait usage de variables stochastiques

 - l'`émetteur de traces`_, classe dont dérive chaque acteur
