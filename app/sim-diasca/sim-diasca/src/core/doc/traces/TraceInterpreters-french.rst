.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _interpréteur de trace:
.. _interpréteurs de trace:

.. _interpréteur:
.. _interpréteurs:



Interpréteurs de traces (``TraceInterpreter``)
==============================================


Rôle
----

Les interpréteurs de traces facilitent le dépouillement des résultats par l'extraction et le traitement des traces enregistrées.

Cela peut se faire au moyen de requêtes sur une base de traces, dont les réponses sont traitées par un module de corrélation et de visualisation de résultats.


Implémentation & Outils
-----------------------

Un premier niveau d'interprétation peut être obtenu au moyen de certains outils de `supervision de trace`_. Ainsi les vues synthétiques de LogMX et ses corrélations temporelles élémentaires fournissent des indicateurs utiles.

Pour une exploitation moins limitée des traces, il sera vraisemblablement nécessaire de se fonder d'une part sur un `enregistreur` de trace évolué (ex: permettant un requêtage type base de données), d'autre part sur un module de traitement et de visualisation des traces extraites qui serait à embarquer dans l'interpréteur de traces évoqué ici. Ces développements restent à faire.
