.. _lien télécom:
.. _liens télécom:

.. _lien de communication:
.. _liens de communication:


Modélisation des liens de communication (``DataLink``)
======================================================


Présentation
------------

En terme de liens de communication, de nombreuses variations sont possibles :

  - connexion permanente ou non

  - monodirectionnelle ou bidirectionnelle (alternée *half-duplex* ou simultanée *full-duplex*)

  - facteur d'atténuation (en décibel par mètre)

  - longueur

  - tarification

  - bande-passante maximale effective (débit-crête)

  - latence (durée de transit d'une donnée isolée)

  - modèle de bruits (interférence)

  - gestion de la congestion

  - caractère synchrone (fondé sur une horloge) ou asynchrone


Modélisation
------------

+-----------------------------+-----------------------------------+---------------------+---------------------+
| Nom de l'attribut           | Signification                     | Unités              | Valeurs typiques    |
+=============================+===================================+=====================+=====================+
| ``attenuation_coefficient`` | Atténuation linéique du signal    | dB/m                | 0..90dB             |
+-----------------------------+-----------------------------------+---------------------+---------------------+
| ``length``                  | Longueur du lien                  | m                   | 0.1m..20km          |
+-----------------------------+-----------------------------------+---------------------+---------------------+
| ``data_type``               | Type des informations véhiculées, | N/A                 | ``numerical``,      |
|                             | numériques ou analogiques         |                     | ``analogical``      |
+-----------------------------+-----------------------------------+---------------------+---------------------+



Atténuation
...........


Il s'agit de la perte de puissance du signal en décibels (dB) qui est fonction de la distance entre les interlocuteurs et de la qualité du lien, et qui est considérée ici comme exponentielle [#]_.

Chaque lien est supposé décrit par une atténuation linéique (``attenuation_factor``), caractéristique de ce lien. Par exemple, pour une communication filaire, ce facteur est fonction de la section du câble, de son matériau, de sa forme, de la fréquence des signaux, etc [#]_.

.. [#] Source : article de Wikipedia sur l'`atténuation <http://en.wikipedia.org/wiki/Attenuation>`_


Cette atténuation linéique est supposée constante (lien homogène), si bien que l'atténuation effective du lien est égale à ``A = attenuation_factor * length``, avec ``length`` : longueur de ce lien.

.. [#] Voir aussi : article de Wikipedia sur les `coefficient d'atténuation <http://en.wikipedia.org/wiki/Attenuation_coefficient#Linear_Attenuation_Coefficient>`_, notamment linéiques.


Il est alors possible de modéliser un réseau complet à partir d'un nombre quelconque de ces liens télécom, agencés selon une topologie donnée, pour évaluer le bilan de liaison [#]_.

.. [#] Le bilan de liaison est un calcul par étapes permettant de déterminer la qualité d'une liaison. Les détails varient selon la nature du média, hertzien, ligne, fibre optique, et le type de signaux et de modulation, mais le principe est le même. C'est le calcul global qui relie tous les domaines : radioélectricité, traitement du signal, protocoles, etc. Source : article de Wikipedia `du même nom <http://fr.wikipedia.org/wiki/Bilan_de_liaison>`_.



Diaphonie
.........

Elle n'est pas modélisée actuellement. Il serait envisageable néanmoins de la prendre en compte en ouvrant la possibilité de définir un couplage potentiel entre plusieurs liaisons, a minima via un coefficient quantifiant la proportion, en puissance, de signal transitant d'un lien à l'autre.


Implémentation
--------------

Elle réside principalement dans class_DataLink.hrl_ et class_DataLink.erl_. Seule une implémentation minimaliste existe, elle sera développée plus précisément en fonction des liens - notamment WAN - qui seront à simuler.
