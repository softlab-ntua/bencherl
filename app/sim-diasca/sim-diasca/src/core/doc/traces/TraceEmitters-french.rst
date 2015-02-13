.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _émetteur de trace:
.. _émetteurs de trace:

.. _émetteur de traces:
.. _émetteurs de traces:

.. _Emetteur:
.. _émetteur:
.. _émetteurs:



Emetteurs de traces (``TraceEmitter``)
======================================


Rôle
----

Tout émetteur de traces a la faculté d'envoyer des messages qui respectent le `format de traces`_ Sim-Diasca à destination d'un `agrégateur`_ de traces.



Implémentation
--------------

Un émetteur générique est défini via la classe ``TraceEmitter``, implémentée dans ``class_TraceEmitter.hrl`` et ``class_TraceEmitter.erl``.

Pour que les instances d'une classes puissent émettre des traces, il suffit d'utiliser l'héritage (multiple) permis par WOOPER :

  - inclure le fichier d'entête adéquat : ``-include("class_TraceEmitter.hrl").``

  - déclarer cette classe comme l'une des classes-mères de la classe considérée : par exemple, ``-define(wooper_superclasses,[class_TraceEmitter]).``


Dès lors différents types de traces peuvent être émis par les instances de cette classe, au moyen des macros suivantes, correspondant au niveau de priorité défini dans le format universel : ``fatal, error, warning, info, trace, debug``.

Par exemple, l'envoi d'un message d'information se ferait ainsi : ``?info([ "Hello world!" ])``.

L'avantage de ce système est que les traces sont automatiquement centralisées (même dans un contexte distribué) et que leur émission est débrayable [#]_.

.. [#] La désactivation s'obtient en commentant dans ``class_TraceEmitter.hrl`` la ligne ``-define(TracingActivated,).`` : tous les modules précompilés avec cette version de la déclaration deviennent muets en terme de trace, et ceci sans qu'aucune pénalité en terme de performance ne subsiste.


Les macros évoquées (``fatal, error, warning, info, trace, debug``) utilisent implicitement une variable d'état nommée ``State``, dont les attributs contiennent différentes informations nécessaires à la création de la trace.

Cette variable est généralement partout disponible et utilisable (des méthodes jusqu'au destructeur), sauf dans le cas du *constructeur* : d'une part il est nécessaire que les classes-mères aient créé les bases de l'objet courant pour qu'il soit utilisable (i.e. avant d'envoyer un message il faut que le constructeur de ``TraceEmitter`` ait initialisé l'état de l'objet de manière appropriée), d'autre part il est préférable que la catégorisation de l'objet en terme de classe ait été mise à jour.

En effet, juste après la construction de l'état par les classes-mères, la catégorisation reflète leur classe et non la classe-fille qui est en cours d'instanciation. Il faut donc que cette classe-fille mette à jour la catégorisation pour que les messages de traces qu'elle enverra par la suite la respecte.

Exemple de constructeur d'une classe ``class_MyClass`` directement dérivée de ``class_TraceEmitter`` ::

  construct(State,MyName) ->

	  % Impossible d'envoyer un message de trace pour l'instant :
	  % la classe-mère correspondante n'a pas été construite.
	  TraceState = class_TraceEmitter:construct( State, MyName ),

	  % Là on peut envoyer des messages de trace, en précisant
	  % un état qui le permet (TraceState) plutôt que l'état
	  % initial State.
	  % Du coup à la place de "?trace([  "Creating a new actor." ]),"
	  % on utilise :
	  ?send_trace([ TraceState, "Creating a new actor." ]),

	  % Seul problème : la catégorisation de l'émetteur est restée
	  % à celle de la de la classe-mère, TraceEmitter.
	  % Il est préférable de la mettre à jour dès que possible :
	  CategorizedState = ?setAttribute( TraceState,
	  	trace_categorization, "MyClass" ),
	  ?send_trace([ CategorizedState,
	  	"This trace is correctly categorized." ]),

	  % Fin du constructeur:
	  CategorizedState.


Pour un exemple d'utilisation complet, consulter ``class_TestTraceEmitter.erl``, pour la classe émittrice de traces, et ``class_TraceEmitter_test.erl`` pour son test effectif.


Voir aussi :

 - `Agrégateur`_ de traces, destinataire des traces envoyées
 - Synthèse sur les `traces`_ Sim-Diasca
