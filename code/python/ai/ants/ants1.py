#!/usr/bin/python

# Ants1 is the first version of a very simple swarm AI system by me. Ants can either
# produce food, build/maintain tunnels, or be idle. The numbers of ants doing each
# task changes each turn due to the current conditions present in the anthill.

import random, time
random.seed()

# Ant counts are stored as float, to ease calculations, but are displayed as int. This
# can cause rounding errors in the displays every turn. Oh well.
ants = float(random.randint(100,1000))
foodants = float(ants * (random.randint(0,100) / 100.0))
buildants = float(ants - foodants)
idleants = 0.0
food = 5000.0
tunnels = 1.0
turns = 0

while True:
	print "Turn", turns
	print "Ants:", int(ants), "-", int(foodants), int(buildants), int(idleants)
	print "Food:", int(food)
	print "Tunnels:", int(tunnels)
	print

	# If there are more ants than food, a percentage of ants (relative to the food difference)
	# will die.
	if ants > food:
		antspercent = random.randint(0, int(ants - food)) / ants
		ants = ants * (antspercent / 100.0)
		foodants = foodants * antspercent
		buildants = buildants * antspercent

	# Food is eaten by ants, food is produced by ants, and tunnels are built by ants.
	# Tunnels increase food production, as ants can easier access food locations.
	food -= ants
	food += foodants * random.randint(1, int(tunnels))
	tunnels += random.randint(0, int(buildants))

	# If more tunnels than builders, they can collapse.
	if tunnels > buildants:
		tunnels -= random.randint(0, int(tunnels - buildants))

	# If there are more ants than food, a percentage of ants will stop building and produce food,
	# and vice versa.
	fooddiff = food - ants
	if fooddiff <= 0:
		antschange = random.randint(0, int((fooddiff * -1) / ants))
		foodants = foodants * (1 + antschange / 100.0)
		buildants = ants - foodants - idleants
	else:
		antschange = random.randint(0, int(fooddiff / ants))
		foodants = foodants * (antschange / 100.0)
		ants += random.randint(0, 50)
		idleants = ants - foodants - buildants - idleants

	# If there are more than 1000 extra units of food, up to 25% of food producers will
	# become idle, and if not, up to 25% of idle ants will produce food.
	if fooddiff > 1000:
		foodants = foodants * (1 - random.randint(0, 25) / 100.0)
	else:
		foodants = foodants + idleants * (1 - random.randint(0, 25) / 100.0)

	# If there are more builders than tunnels, up to 25% of builders will
	# become idle, and if not, up to 25% of idle ants will become builders.
	if buildants > tunnels:
		buildants = buildants * (1 - random.randint(0, 25) / 100.0)
	else:
		buildants = buildants + idleants * (1 - random.randint(0, 25) / 100.0)

	idleants = ants - foodants - buildants
	turns += 1
	time.sleep(10)
