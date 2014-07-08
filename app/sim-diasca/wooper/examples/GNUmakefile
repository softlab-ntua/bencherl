WOOPER_TOP = ..


include $(WOOPER_TOP)/GNUmakesettings.inc


# Helper modules (ex: basic_utils.beam, wooper_class_manager.beam, etc.) are not
# listed here as dependencies, their possible lacking will be detected at
# run-time.


class_Creature_test: class_Creature.beam


class_Mammal_test: class_Mammal.beam class_Creature.beam

class_Reptile_test: class_Reptile.beam class_Creature.beam


class_ViviparousBeing_test: class_ViviparousBeing.beam

class_OvoviviparousBeing_test: class_OvoviviparousBeing.beam


class_Cat_test: class_Cat.beam class_Mammal_test class_ViviparousBeing_test

class_Platypus_test: \
	class_Platypus.beam           \
	class_ViviparousBeing_test    \
	class_OvoviviparousBeing_test

