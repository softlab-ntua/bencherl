.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _supervision de trace:

.. _superviseur de trace:
.. _superviseurs de trace:

.. _superviseur:
.. _superviseurs:




Superviseurs de traces (``TraceSupervisor``)
============================================


Rôle
----

Un superviseur de traces permet de consulter de la manière la plus conviviale possible des traces de simulation, soit en cours d'exécution (les traces sont lues au fil de l'eau) soit de manière post-mortem (étude du déroulement d'une simulation après qu'elle soit achevée).


Outils
------

Plusieurs outils peuvent être employés pour exploiter, y compris en mode interactif, un fichier de traces (supervision de traces), du simple ``tail -f`` jusqu'à des logiciels plus avancés comme `LogMX <http://www.logmx.com>`_.

Pour certains outils de supervision de traces, une traduction spécialisée du format universel de trace vers un format reconnu par l'outil est nécessaire.
Pour chacun de ces formats-cibles, tous les champs du format universel sont listés, en face desquels sont précisés leurs homologues dans le format-cible.


``tail -f``
...........

Les traces sont affichées de manière brute, sans formatage, au fil de leur ajout dans le fichier de trace, issu de l'agrégation.

LogMX
.....

Il est possible de définir directement le format attendu par l'outil pour les traces à partir de son interface graphique, dans le menu ``Tools -> Options -> Parsers`` en cliquant sur la croix verte et en choisissant une syntaxe Log4j pour décrire le format d'entrée. Cette syntaxe est décrite `ici <http://logging.apache.org/log4j/docs/api/org/apache/log4j/PatternLayout.html>`_ et `là <http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html>`_, mais elle ne semble que partiellement reconnue par l'outil aujourd'hui.


Syntaxe Log4J
_____________

Les *tags* reconnus peuvent être définis à l'aide de la syntaxe `Log4J <http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html>`_:

 - identifiant technique : (tag correspondant : ``name of the thread that generated the logging event``, ``t``)

 - nom :  (tag correspondant : ``file name where the logging request was issued``, ``F``)

 - catégorisation de l'émetteur : (tag correspondant : ``fully qualified class name of the caller``, ``C``)

 - horodatage :

  - temps de la simulation (tag correspondant : ``date of the logging event``, ``r``)

  - temps utilisateur (tag correspondant : ``date of the logging event``, ``d``)

 - catégorisation du message : (tag correspondant : ``category of the logging event``, ``c``)

 - priorité : (tag correspondant : ``priority of the logging event``, ``p``)

 - corps du message :  (tag correspondant : ``application supplied message``, ``m``)


La correspondance, décrite sous la forme d'expression régulière Log4j, est : ``%t|%F|%C|%r|%d|%c|%p|%m``.


Malheureusement non seulement cette expression régulière n'est pas reconnue, mais en plus il semble impossible aux parseurs LogMX de gérer d'autres informations que :

 - un horodatage (``date``)

 - un niveau (``level``)

 - une "tâche" (``thread``)

 - un émetteur (``emitter``)

 - un message (``message``)


Parseur Java spécifique
_______________________

Suite aux limites du support par LogMX de Log4J et du nombre de champs supportés, les informations relatives à une trace sont extraites au moyen d'un parseur spécifique, écrit en Java, créé pour reconnaître le format de trace présenté.

La correspondance de repli (champs LogMX vers trace) est:

 - ``date`` : horodatage de la simulation

 - ``level`` : priorité du message

 - ``thread`` : identifiant technique

 - ``emitter`` : catégorisation du type de l'émetteur (ex : ``Actor.Equipment``) augmentée du nom de l'émetteur (ex : ``MonEquipement``). Cela donne : ``Actor.Equipment.MonEquipement``

 - ``message`` : texte du message de la trace


Cela implique que les informations suivantes, relatives à une trace, ne seront pas reconnues spécifiquement par LogMX :

 - catégorisation du type du trace

 - horodatage en temps réel de la trace

 - localisation de l'émetteur


Ces informations sont néanmoins replacées dans le corps du message, en début de texte, chacune entourée de crochets.

Les sources du parseur Sim-Diasca correspondant sont dans ``CeylanTraceParser.java``.

Il est nécessaire de configurer LogMX pour que les niveaux de priorité autres que 1 et 2 soient reconnus. Pour cela il suffit de paramétrer les correspondances, dans ``Tools->Levels Indirections`` :

:raw-html:`<img src="logmx-levels.png"></img>`
:raw-latex:`\includegraphics[scale=1]{logmx-levels.png}`

L'appellation des niveaux native à LogMX est identique au format universel, sauf que les informations typées ``Trace`` utilisent le niveau LogMX ``FINE``. Cette correspondance s'obtient via le menu ``Tools->Levels``.

Au final la console de supervision se présente ainsi:


:raw-html:`<img src="logmx-interface.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{logmx-interface.png}`

L'exemple de traces utilisé est disponible sous ``TraceSample.txt``.

Pour éviter de reconstruire le parseur Sim-Diasca correspondant (nécessite un JDK récent et ant), les ``bytecodes`` précompilés (avec le ``JDK 1.6.0_05``) sont disponibles en téléchargement: `CeylanTraceParser.class``.


Les fichiers de configuration de l'outil sont à placer sous le répertoire ``LogMX_vx.y.z/config``. Ce sont les fichiers suivants :

 - ``logmx.properties``

 - ``managers.properties``

 - ``parsers.properties``




L'outil LogMX a été intégré au simulateur Sim-Diasca en tant que superviseur de traces directement pilotable (lancement, arrêt, attente de la fin de consultation des traces, transmission du fichier de traces générés par l'agrégateur de traces, etc.) via Erlang et, par exemple, depuis les tests.



Implémentation
--------------

L'implémentation correspondante du superviseur de traces fondé sur LogMX est dans ``class_TraceSupervisor.hrl`` et ``class_TraceSupervisor.erl``.


Voir aussi :

 - `Agrégateur`_ de traces, destinataire des traces envoyées

 - Synthèse sur les `traces`_ Sim-Diasca
