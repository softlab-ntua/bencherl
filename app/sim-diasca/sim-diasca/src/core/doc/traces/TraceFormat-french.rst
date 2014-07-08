.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _format de traces:
.. _émetteurs de trace:
.. _traces:


Traces logicielles des simulateurs de SI
========================================


Objectif poursuivi
------------------

Adopter un format de traces (de logs) permet de découpler l'exécution d'une simulation de l'interprétation de son déroulement (*post-processing* type analyse statistique).

Si en plus le format choisi est pensé pour être "universel", pour ne pas dépendre d'un générateur particulier de ces traces ou d'un cas métier, alors plusieurs simulateurs peuvent l'utiliser et partager toute la chaîne en aval de la génération des traces.

Notons que les traces permettent de suivre une exécution, sans pour autant constituer nécessairement la seule forme de résultat de la simulation ni avoir vocation à traduire tous les événements survenus : l'historisation d'événéments de simulation est un sujet plus vaste et qui ne s'incarne pas nécessairement sous forme de traces.

Le découpage des prérogatives en terme de gestion répartie des traces est le suivant :

 - `émetteurs`_ de traces : tout objet simulé, mais aussi tout composant de service technique, peut être amené à enregistrer des traces relatives à son fonctionnement

 - `agrégateurs`_ de traces : s'ils peuvent être en nombre quelconque (par type d'objet, par machine, etc.), la solution la plus usuelle est de n'utiliser qu'un seul agrégateur, qui centralise toutes les traces de tous les émetteurs distribués. Ainsi, analyse d'ensemble et recherche de corrélation sont possibles

 - `superviseurs`_ de traces : ils permettent d'effectuer un suivi en temps réel de la vie du système, via ses traces. Plusieurs outils sont disponibles pour cela. Le suivi peut s'effectuer à distance (sur une autre machine que les calculateurs impliqués) et/ou de manière *post-mortem*, une fois la simulation finie

 - `enregistreurs`_ de traces : ils assurent le stockage persistant des traces, sous forme de simple fichier texte ou d'une base de données requêtable, potentiellement temps réel et répartie

 - `interpréteurs`_ de traces : ils facilitent le dépouillement des résultats par l'extraction et le traitement des traces enregistrées. Cela peut se faire au moyen de requêtes sur une base de traces dont les réponses sont traitées par un module de visualisation de résultats



Description des informations détaillées dans une trace
------------------------------------------------------


Dans le format de trace "pivot", il serait intéressant de faire figurer les informations suivantes :

 - informations portant sur l'*émetteur* du message :

  - **identifiant technique** de l'émetteur, généralement spécifique à une plate-forme de simulation. Par exemple avec Erlang ce serait un numéro de processus (PID) du type ``<0.31.0>``

  - **nom** de l'émetteur, unique dans le contexte d'une simulation donnée, construit pour servir de niveau d'indirection par rapport à un identifiant technique de type identifiant numérique d'un agent ou PID d'un processus dans une plate-forme donnée. Format : suite de caractères, alphanumériques ou dans ``-_+=.#&@``. Par exemple : ``MyObject-17``. Dans le contexte d'une simulation donnée, une bijection existe entre les identifiants techniques et les noms. En revanche, pour un même cas de simulation exécuté deux fois, si les noms doivent être stables (pour assurer la reproductibilité), il n'en est pas nécessairement de même pour les identifiants techniques

  - **catégorisation de l'émetteur**, selon une hiérarchie de catégories, précisée ci-dessous. Par exemple : ``Actor.Equipment.MyObject.SpecificObject``, ou ``Service.Scheduler``. Si un émetteur n'a pas défini sa catégorisation, elle sera fixée à ``NotCategorized``

  - **localisation de l'émetteur**, via un nom de noeud logique et/ou un nom de machine. Par exemple : ``sim_diasca_test@myhost.org``. Si aucun nom pertinent n'est trouvé (ex : la machine n'a pas de nom réseau), ``localhost`` est utilisé

 - informations portant sur le *message lui-même* :

  - **horodatage** du message en temps de :

   - la simulation, sous la forme d'un (grand) entier positif, de ``none`` si la simulation n'est pas en cours ou de ``unknown`` si l'émetteur n'a pas précisé le pas de temps (que la simulation soit en cours ou non). Par exemple, ``3168318240218``

   - l'utilisateur, sous la forme ``dd/mm/yyyy hh:MM:ss``. Par exemple, ``08/04/2008 04:41:24``

  - **catégorisation du message**, selon une hiérarchie de catégories, précisée ci-dessous. Par exemple : ``Simulation.Topic.Event`` ou ``System.Start``

  - niveau de **priorité** de la trace (importance, criticité, sévérité, etc.), nombre entier strictement positif. Les messages les plus prioritaires ont les niveaux de priorité les plus faibles. Aux premiers niveaux de priorité sont associés des noms :

+------------------------+------------+
| Niveau de priorité     | Nom        |
+========================+============+
| 1                      | ``Fatal``  |
+------------------------+------------+
| 2                      | ``Error``  |
+------------------------+------------+
| 3                      | ``Warning``|
+------------------------+------------+
| 4                      | ``Info``   |
+------------------------+------------+
| 5                      | ``Trace``  |
+------------------------+------------+
| 6                      | ``Debug``  |
+------------------------+------------+

  - **corps du message** lui-même, sous forme d'une liste de caractères, de longueur variable (potentiellement nulle, potentiellement comprenant des retours à la ligne)

Il serait envisageable que, selon son type, une trace comporte des attributs supplémentaires. Toutefois dans ce cas on se placerait plutôt alors dans un système d'historisation d'événements, tel qu'évoqué en introduction.



Catégorisation des émetteurs
----------------------------

Un émetteur de trace est identifié par une liste de rubriques, de la plus générale à la plus précise, séparées par un point.

Ces rubriques sont classées de manière arborescente, par exemple selon le début de hiérarchie suivant :

 - Actor

  - StochasticActor

   - FailureModel

   - RepairModel

  - Equipment

 - SimulationService

  - TimeManager

  - RandomManager

  - SimulationManager
  - ScenarioManager

  - TraceService

   - TraceEmitter

   - TraceAggregator

   - TraceMonitor

   - TraceRecorder

   - TraceInterpreter

 - Probe

 - NotCategorized, si aucune catégorie n'a été stipulée

Ainsi un objet de type ``MyType``, de modèle ``MyDesign`` et de marque ``MyTrademark`` peut être classé dans la rubrique ``Actor.Equipment.MyType.MyTrademark.MyDesign``.



Catégorisation des messages
---------------------------

Les catégories d'information peuvent être classées selon le début de hiérarchie suivant :

 - ``System`` : traces -techniques- du simulateur, comme le franchissement d'un pas de temps

  - ``Management``

   - ``SimulationStart``

   - ``SimulationResume``

   - ``SimulationContinue``

   - ``SimulationStop``

   - ``SimulationSave``

   - ``SimulationLoad``

  - ``Time`` : pour les messages en lien avec la gestion du temps simulé, l'ordonnancement :


   - ``BeginningOfTick``
   - ``EndOfTick``

  - ``Random`` : en lien avec la gestion du hasard

  - ``Lifecycle`` : en lien avec la création, l'initialisation, la migration, la suppression d'instances

 - ``Simulation`` : traces "métier", portant sur les éléments simulés, comme le changement d'état d'un objet métier

  - ``Discovery`` : pour les messages en lien avec la découverte d'équipements

  - ``Reading`` : en lien avec la collecte de données

  - ``Update`` : en lien avec la mise à jour du système

  - ``Failure`` : en lien avec la défaillance d'équipements

  - ``Repair`` : en lien avec la réparation d'équipements

  - ``State`` : en lien avec la gestion de l'état des acteurs simulés

  - ``Communication`` : en lien avec la communication entre acteurs

  - ``Uncategorized`` : pour les messages pour lesquels aucune catégorie n'a été mentionnée


Ainsi un message portant sur la panne des équipements pourrait être catégorisé via ``Simulation.Repair``.


Format "universel" d'une trace
------------------------------

Ce format, défini par Sim-Diasca, se veut générique, et s'appliquer à tout type de simulation, d'où l'appellation *universel*.

Chaque trace est stockée dans une seule ligne de texte logique, de longueur arbitraire mais sans caractère de retour à la ligne.

Le séparateur de champ retenu est *pipe* (``|``). Il ne doit pas figurer dans le corps du message.

Le format correspondant est :

  ``identifiant technique|nom|catégorisation de l'émetteur|horodatage simulation|horodatage utilisateur|localisation|catégorisation du message|priorité|corps du message``


Par exemple :

  ``<0.31.0>|MyObject-17|Actor.Equipment.MyObject.SpecificObject|3168318240218|08/04/2008 04:41:24|sim_diasca_test@myhost.org|Simulation.Discovery.LostConnection|2|No answer from device``


Voir aussi :

 - `émetteurs`_ de traces
 - `agrégateurs`_ de traces
 - `superviseurs`_ de traces
 - `enregistreurs`_ de traces
 - `interpréteurs`_ de traces


Le système de traces peut être testé en isolation en exécutant ``traceManagement_test.erl``.
