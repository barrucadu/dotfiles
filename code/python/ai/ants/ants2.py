#!/usr/bin/python

# Ants2 is the second version of a very simple swarm AI system by me. Ants can either
# produce food, build/maintain tunnels, or be idle. The numbers of ants doing each
# task changes each turn due to the current conditions present in the anthill.
# This version makes use of classes for the nest, ants and tunnels, so tunnels can have
# individual characteristics (danger), and ants can have individual knowledge.
# This version is rather slow. Be warned.

import random
random.seed()

class Nest:
	# Ant counts are stored as float, to ease calculations, but are displayed as int. This can cause rounding errors in the displays every turn. Oh well.
	ants = []
	tunnels = []
	food = 5000.0
	turns = 0
	died = 0

	def __init__(self):
		for i in range(0, random.randrange(100, 1000)): self.ants.append(Ant())
		for i in range(0, random.randrange(1, 10)): self.tunnels.append(Tunnel())
		self.tunnels[0].dangerous = False
		self.tunnels[0].danger = 0
		self.turn()

	def turn(self):
		while True:
			for i in self.tunnels:
				turn = i.turn()
				if turn == False: self.tunnels.remove(i)
			for i in self.ants:
				turn = i.turn(self)
				if turn == False: self.ants.remove(i)

			print "Day", self.turns
			print "Ants:", len(self.ants), "-", self.num("food"), self.num("build"), self.num("idle")
			print "Food:", self.food
			print "Tunnels:", len(self.tunnels)
			print "Ants killed by tunnels:", self.died
			print ""
			
			self.died = 0
			self.turns += 1
		
	def num(self, job):
		count = 0
		for i in self.ants:
			if i.job == job: count += 1
		return count

class Ant:
	safetunnels = []
	dangeroustunnels = []
	job = ""
	age = 0
	starve = 0

	def __init__(self):
		job = random.randrange(0, 100)
		if job < 33: self.job = "food"
		elif job < 66: self.job = "build"
		else: self.job = "idle"

	def turn(self, nest):
		if nest.food == 0:
			# If there is no food, the ant has a 15% chance of dying. No ant can survive more than three turns without food.
			self.starve += 1
			if self.starve == 3 or random.randrange(0, 100) < 15: return False
		else: self.eat(nest)
		
		if self.job == "food":
			# Food ants will harvest food, warn other ants about dangerous tunnels (if they survive), and possibly become idle if there is a surplus of food.
			if random.randrange(0, 100) < 75 and len(self.safetunnels) > 0:
				# There is a 75% chance of using a known safe tunnel
				nest.food += random.randrange(1, 11)
			else:
				# Pick a random tunnel that is not a known dangerous tunnel
				choices = []
				for i in nest.tunnels:
					if self.dangeroustunnels.count(i) == 0: choices.append(i)
				choice = choices[random.randrange(0, len(choices))]
				
				# See if it is safe, and if not did the ant survive to bring food and warnings?
				danger = choice.passthrough()
				if danger == True and random.randrange(0, 100) <= choice.danger:
					nest.died += 1
					return False
				elif danger == True:
					if self.safetunnels.count(choice) == 1: self.safetunnels.remove(choice)
					self.dangeroustunnels.append(choice)
					nest.food += random.randrange(1, 6)
				else:
					self.safetunnels.append(choice)
					nest.food += random.randrange(1, 11)
		elif self.job == "build":
			# Builder ants will build new tunnels if there are any food ants, and maintain up to three tunnels a turn (one if building), and possibly become idle if there is a surplus of builders.
			maintain = 3
			if random.randrange(0, 100) < 66:
				# There is a 66% chance of building a new tunnel
				newtunnel = Tunnel()
				nest.tunnels.append(newtunnel)
				maintain = 1

			for i in range(0, maintain):
				if random.randrange(0, 100) < 75 and len(self.safetunnels) > 0:
					# There is a 75% chance of maintaining a known safe tunnel
					choice = self.safetunnels[random.randrange(0, len(self.safetunnels))]
					choice.maintain()
				else:
					# Pick a random tunnel that is not a known dangerous tunnel
					choices = []
					for j in nest.tunnels:
						if self.dangeroustunnels.count(j) == 0: choices.append(j)
					choice = choices[random.randrange(0, len(choices))]
							
					# See if it is safe, and if not did the ant survive to bring warnings?
					danger = choice.passthrough()
					if danger == True and random.randrange(0, 100) <= choice.danger:
						nest.died += 1
						return False
					elif danger == True:
						if self.safetunnels.count(choice) == 1: self.safetunnels.remove(choice)
						self.dangeroustunnels.append(choice)
					else:
						self.safetunnels.append(choice)
						choice.maintain()
		else:
			# Idle ants are likely to switch to food or building if there is a deficit of either
			changeover = False
			if nest.food - len(nest.ants) < 1000:
				# Food difference is less than 1000, and food could start to run out soon! 33% chance of becmming a food ant.
				if random.randrange(0, 100) < 33:
					changeover = True
					job = "food"
			elif len(nest.tunnels) > nest.num("build") * 2 and changeover == False:
				# There are more tunnels than twice the number of builders, things could start to collapse soon. 33% chance of becoming a builder ant.
				if random.randrange(0, 100) < 33:
					changeover = True
					job = "build"

		# An ant will impart it's knowledge of safe and dangerous tunnels to five other ants every turn.
		for i in range(0, 5):
			choice = nest.ants[random.randrange(0, len(nest.ants))]
			choice.communicate(self.safetunnels, self.dangeroustunnels)
		
		# Every ant older than 10 turns has a 75% chance of dying.
		self.age += 1
		if self.age >= 10 and random.randrange(0, 100) > 25: return False

		# Do not die!
		return True

	def eat(self, nest):
		# Hooray, we can eat! Consume one unit of food, and stop starving to death
		nest.food -= 1
		self.starve = 0

	def communicate(self, safe, dangerous):
		# An ant will tell another about any safe tunnels (or apparently safe tunnels) it knows about.
		for i in safe:
			if self.safetunnels.count(i) == 0 and self.dangeroustunnels.count(i) == 0: self.safetunnels.append(i)
		# It will also tell anyther about any dangerous tunnels it knows about.
		for i in dangerous:
			if self.dangeroustunnels.count(i) == 0:
				self.dangeroustunnels.append(i)
				if self.safetunnels.count(i) == 1: self.safetunnels.remove(i)
		

class Tunnel:
	dangerous = False
	danger = 0
	maintained = 0
	usage = 0

	def __init__(self):
		# It is up to chance whether a tunnel is dangerous or not, 33%. How dangerous, however, is a more variable chance ranging from anywhere between 1 and 100%
		danger = random.randrange(0, 100)
		if danger < 33:
			self.dangerous = True
			self.danger = random.randrange(0, 100)

	def maintain(self):
		self.maintained = 0

	def turn(self):
		self.usage = 0
		self.maintained += 1
		
		# If a tunnel has been unmaintained for two turns, there is a 75% chance of it collapsing.
		if self.maintained == 2 and random.randrange(0, 100) > 25: return False

		# Do not collapse!
		return True

	def passthrough(self):
		self.usage += 1
		if self.dangerous == True and random.randrange(0, 100) <= self.danger: return True
		else: return False

nest = Nest()
