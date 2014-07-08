.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _gestionnaire de hasard:
.. _gestionnaires de hasard:

.. _gestionnaire de variables stochastiques:
.. _gestionnaires de variables stochastiques:




Modélisation des gestionnaires de variables stochastiques (``RandomManager``)
=============================================================================


Objectif
--------

Beaucoup d'éléments simulés ont un comportement régi par une ou plusieurs variables stochastiques, obéissant à diverses distributions (densités de probabilité).

L'objectif du gestionnaire de variables stochastiques (appelé plus simplement ``gestionnaire de hasard``) est de fournir d'emblée dans Sim-Diasca les lois de probabilité les plus courantes :

 - loi uniforme ("bruit blanc")

 - loi exponentielle

 - loi gaussienne (i.e. loi normale)


tout en préservant certaines propriétés de la simulation, comme la reproductibilité, y compris dans un contexte distribué.


Fonctionnement
--------------

Les générateurs aléatoires utilisés ont généralement un état, qui est caché, et initialisé par défaut si aucune "graine" (``seed``) n'est définie. Dès lors la suite de valeurs qu'ils produisent est entièrement reproductible d'un lancement à l'autre.

Si plusieurs processus Erlang faisaient appel à ces primitives du système sans qu'un ordre reproductible des requêtes soit assuré, chaque appel changerait l'état du générateur pour les appels suivants, et la simulation ne serait pas reproductible car par défaut l'ordonnancement de processus Erlang dans un pas de temps ne l'est pas (il dépend notamment des autres activités de la machine).

Une solution serait de faire en sorte que chaque processus Erlang embarque son propre générateur. Alors le système serait alors reproductible, mais tous les générateurs produiraient la même suite pseudo-aléatoire, ce qui n'est pas souhaitable (par exemple tous les compteurs tomberaient en panne à la même date).

La solution est d'avoir un acteur de la simulation (il est donc synchronisé) qui détient le générateur partagé. La synchronisation assure la reproductibilité, et le fait que de multiples modèles accèdent à ce générateur assure qu'ils n'utiliseront pas des suites aléatoires identiques : chaque nombre demandé fera avancer d'un cran dans la suite. Donc tout consommateur de hasard doit faire usage d'un gestionnaire de hasard.

Cette approche recèle un défaut toutefois : un acteur ayant besoin d'une valeur aléatoire doit faire une requête synchronisée à un générateur, ce qui induit une latence de deux pas de temps (le temps que l'acteur soit prêt à exploiter la valeur retournée par le générateur). Cette latence perturbe de manière indésirable les modèles, et les acteurs simulés "consommant" beaucoup de valeurs aléatoires seraient incorrectement dilués dans le temps simulé.

Le moteur de simulation actuel intègre donc, au niveau de chaque `acteur stochastique`_ (i.e. consommateur de valeurs aléatoires) un mécanisme générique, transparent, lui permettant de maintenir un cache de valeurs aléatoires, et de le renouveler automatiquement auprès du générateur correspondant dès lors qu'il atteint un seuil minimal. Le système est relativement évolué car il permet à tout acteur stochastique de gérer en parallèle autant de suites aléatoires (en nombre et en distribution: gaussienne, uniforme, etc.) que voulu.

Notons qu'en général un gestionnaire de hasard (``RandomManager``) est lancé comme un singleton auquel tous les acteurs stochastiques accèdent. Ce n'est pas impératif : par exemple chaque modèle de panne ou de réparation embarque son gestionnaire de hasard privé, car ils sont assurés d'être sollicités un grand nombre de fois par un grand nombre d'acteurs.



Générateur uniforme
...................


Une `sonde`_ posée en sortie d'une distribution uniforme générée donne le résultat suivant :

:raw-html:`<img src="RandomManager-Uniform_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Uniform_probe.png}`

Les tirages aléatoires dans la plage de valeurs demandée sont équiprobables.
Il est possible d'activer un générateur de bruit blanc de qualité supérieure (conçu pour la cryptographie).


Générateur exponentiel
......................

Cette distribution, définie par un paramètre unique, ``lambda``, donne les résultats suivants :

:raw-html:`<img src="RandomManager-Exponential_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Exponential_probe.png}`

Elle est générée directement par le simulateur, à partir de la source de bruit blanc précédente.



Générateur gaussien
...................

Deux paramètres caractérisent la loi gaussienne (ou loi normale) : ``mu``, la valeur moyenne, et ``sigma``, la variance.

Cela donne la courbe suivante :

:raw-html:`<img src="RandomManager-Gaussian_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Gaussian_probe.png}`




Implémentation
--------------


Le gestionnaire de variables stochastiques est implémenté dans la classe définie par class_RandomManager.hrl_ et class_RandomManager.erl_.

Il est testé notamment dans :

 - class_RandomManager_test.erl_ (test unitaire)

 - randomManagerAndStochasticActorPair_test.erl_ (test d'intégration avec les acteurs stochastiques)


Ses différentes lois élémentaires fournies d'emblée (``Sim-Diasca built-ins``) sont testées spécifiquement dans :

 - class_RandomManager_Uniform_test.erl_

 - class_RandomManager_Exponential_test.erl_

 - class_RandomManager_Gaussian_test.erl_


Voir aussi :

 - le `gestionnaire de temps`_, qui cadence tous les acteurs, y compris le ou les gestionnaires de variables stochastiques

 - l'`acteur stochastique`_, un acteur spécialisé dont le modèle fait usage de variables stochastiques et qui intègre un mécanisme de gestion automatique de ces variables
