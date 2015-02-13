Now:

 - all actors have an additional first parameter, the AAI (*Actor Abstract Identifier*), transparently assigned by the load balancer to the actor at its creation (it is just an integer, starting at 1 and incremented by the load balancer at each actor creation); the AAI is an absolute identifier necessary for reproducibility, knowing that, on a distributed context, technical identifiers like PID may change according to the computing infrastructure being used (ex: if adding a computing node between two executions of the same simulation), which would in turn result in different message reorderings

 - at each tick being processed (knowing that idle ticks are automatically skipped by the time manager chains), all actors may be scheduled to perform a triggered behaviour (due to the receiving of at least one actor message on the previous tick), or to act spontaneously, or both (in that order), or none (in that case the actor will not even receive a tick notification)

 - for increased clarity, the ``act/1`` oneway is now the ``actSpontaneous/1`` oneway

 - during the various possible behaviours, triggered or spontaneous, as before actor messages can be sent and the state can be updated, but now, additionally, the next future spontaneous action can be defined as well, by setting the ``future_action`` attribute appropriately, either to ``passive`` (if the actor is only next to be triggered by incoming actor messages), or to the tick offset at which this actor should be spontaneously scheduled next; note that at each scheduled tick this attribute is reset to ``undefined``, which, if not changed in the course of the actor action, is interpreted as ``passive``; then any method triggered by an actor message is free to change in turn this attribute, before any scheduled spontaneous action takes place and is given a last opportunity in this tick to change the course of this actor's next future action; finally, an actor which determines it should terminate should call its ``triggerTermination/1`` oneway instead (which will take care, among other processings, to set accordingly its ``future_action`` attribute)

 - no more need to call the ``class_Actor:manage_end_of_tick`` function at the end of the spontaneous behaviour, only an updated state must be returned


Refer to the ``scheduling/class_TestActor.erl`` and ``random/class_TestStochasticActor.erl`` classes for full examples of these newer conventions. The corresponding test cases can be found in ``distributed_simulation_test.erl``, and be run thanks to ``make distributed_simulation_run``, as usual.
