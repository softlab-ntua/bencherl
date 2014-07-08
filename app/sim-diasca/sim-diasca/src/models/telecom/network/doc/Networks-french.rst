.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _réseau de communication:
.. _réseaux de communication:



Modélisation générique de réseaux télécom (``Network``)
=======================================================


.. contents:: Table des matières
	:local:


Présentation
------------

Un réseau télécom générique (indépendamment de la nature de ses communications) est ici modélisé sous la forme d'un graphe dirigé potentiellement cyclique, dont :

  - les sommets sont les équipements communicants en état de communiquer
  - les arêtes représentent le fait que deux équipements peuvent communiquer directement (sans intermédiaire)

Ce graphe peut lui-même être le produit de l'évaluation d'un graphe plus complet, qui modélise :

  - la puissance d'émission des équipements et leur sensibilité de réception
  - la longueur et l'atténuation linéique des liens de communication les liant

Plus précisément, dans un tel réseau, trois types d'éléments de plus haut niveau sont définis :

 - le type *interconnexion* (``Interconnection``)

 - le type *lien* (``DataLink``)

 - le type *équipement communicant* (``class NetworkDevice``)



Avancement
----------

Le modèle de communication mis en place pour le CPL préfigure le modèle de réseau complet, qui pourrait être en place à l'occasion du développement du modèle avancé fondé sur les calculs d'atténuation du signal.
