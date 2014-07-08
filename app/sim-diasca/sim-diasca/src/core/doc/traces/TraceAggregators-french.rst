.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _agrégateur de trace:
.. _agrégateurs de trace:

.. _agrégateur:
.. _agrégateurs:



Agrégateurs de traces (``TraceAggregator``)
===========================================


Rôle
----

La fonction d'un agrégateur de traces est de centraliser des traces en provenance d'un ensemble d'émetteurs de traces, potentiellement localisés sur des calculateurs distincts.

Si les agrégateurs peuvent être en nombre quelconque (par type d'objet, par machine, etc.), la solution la plus usuelle est de n'utiliser qu'un seul agrégateur, qui centralise l'intégralité des traces de tous les émetteurs distribués. Ainsi, l'analyse d'ensemble et la recherche de corrélation sont possibles.



Implémentation
--------------

Un agrégateur générique est défini via la classe ``TraceAggregator``, implémentée dans ``class_TraceAggregator.hrl`` et ``class_TraceAggregator.erl``.

Cet agrégateur est naturellement réparti (il agrège toutes les traces envoyées de manière concurrente et distribuées sur plusieurs machines). Il reflète la temporalité du système dans une logique d'engagement de moyen (*best effort*) avec pour granularité, quand les émetteurs sont des acteurs (``Actor``), un pas de temps : des messages envoyés lors de pas de temps distincts seront nécessairement classés dans le bon ordre, mais à l'intérieur d'un pas de temps aucun ordre total n'est garanti.


Voir aussi :

 - `Emetteur`_ de traces, source des traces agrégées

 - Synthèse sur les `traces`_ Sim-Diasca
