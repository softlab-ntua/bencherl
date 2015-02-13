.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _équipement communicant:
.. _équipements communicants:


Modélisation des équipements communicants (``CommunicatingDevice``)
===================================================================


Présentation
------------

Ce modèle décrit tout type d'équipements capables de communiquer, en émission et/ou réception, via des `liens de communication`_.

Ces équipements sont appelés en général à faire partie d'un `réseau de communication`_.


Modélisation
------------


+-----------------------------+-----------------------------------+---------------------+---------------------+
| Nom de l'attribut           | Signification                     | Unités              | Valeurs typiques    |
+=============================+===================================+=====================+=====================+
| ``emitting_power``          | Puissance d'émission              | dB                  | 0..90dB             |
+-----------------------------+-----------------------------------+---------------------+---------------------+
| ``receiver_sensibility``    | Sensibilité de réception          | dB                  | 0..90dB             |
+-----------------------------+-----------------------------------+---------------------+---------------------+


Implémentation
--------------

La classe correspondante est implémentée dans class_CommunicatingDevice.erl_.
