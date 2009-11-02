#!/usr/bin/env python3

import random

class Network:
    neurons = [] # This is a 2D list in the form [layer][neuron] = threshold
    dends   = [] # This is a 2D list in the form [layer][neuron] = [later2, neuron2, weight]
    data    = [] # This is a 2D list in the form [layer][neuron] = value

    def __init__(self, neurons, dends):
        self.neurons = neurons
        self.dends   = dends
        self.data    = []
        
        for i in range(0, len(self.neurons)):
            self.data.append([])

            for j in range(0, len(self.neurons[i])):
                self.data[i].append(0)
        
    def run(self, indata):
        i = 0
        for datum in indata:
            self.data[0][i] = datum
            i += 1

        for i in range(1, len(self.data)):
            for j in range(0, len(self.data[i])):
                self.data[i][j] = 0

        for layer in range(0, len(self.neurons)):
            for neuron in range(0, len(self.neurons[layer])):
                if self.data[layer][neuron] > self.neurons[layer][neuron]:
                    if not self.dends[layer] == []:
                        for dend in self.dends[layer][neuron]:
                            layer2  = dend[0]
                            neuron2 = dend[1]
                            weight  = dend[2]
                            
                            self.data[layer2][neuron2] += self.data[layer][neuron] * weight
                            
                            
        return self.data[len(self.neurons) - 1]

class GA:
    sampledata = [] # This is a list in the form [i] = [data, answer]
    population = [] # This is the current population of networks
    count      = 0  # This is the number of networks for each generation
    neurons    = [] # This is the number of layers/neurons for the network to have
    limit      = 0  # The number of generations to calculate
    error      = 0  # The maximum difference allowed on results
    generation = 0  # The generation number.

    def __init__(self, sampledata, count=10000, limit=10000, neurons=[2, 1], error=0.1):
        self.sampledata = sampledata
        self.count      = count
        self.limit      = limit
        self.error      = error        
        self.neurons    = neurons
        
        for i in range(0, count):
            self.population.append(None)

    def populate(self):
        for i in range(0, self.count):
            neurons = []
            dends   = []
            l       = 0
            for layer in self.neurons:
                neurons.append([])
                dends.append([])
                n = 0
            
                for j in range(0, layer):
                    neurons[len(neurons) - 1].append(random.random() * random.randint(-10, 10))
                    
                    if not l == len(self.neurons) - 1:
                        dends[len(dends) - 1].append([])
                        for k in range(0, self.neurons[l + 1]):
                            dends[len(dends) - 1][n].append([l + 1, k, random.random() * random.randint(-10, 10)])
                    n += 1
                    
                l += 1
                    
            self.population[i] = Network(neurons, dends)

    def run(self, verbose=0):
        if verbose != 0:
            print("Running generation", self.generation)
            
        results = []
        for i in range(0, self.count):
            if verbose == 2:
                print("    Running network", i)
                
            results.append([])
            for sample in self.sampledata:
                results[i].append(self.population[i].run(sample[0]))
                
        self.generation += 1
        return results

    def fittest(self, results, error=False):
        fittest = [None, None]
        if not error:
            error = self.error

        for i in range(0, len(results)):
            fit    = False
            errors = []
            for j in range(0, len(self.sampledata)):
                for k in range(0, len(self.sampledata[j][1])):
                    errors.append(abs(results[i][j][k] - self.sampledata[j][1][k]))
                    if errors[j] < error:
                        fit = True
                    
            if fit:
                if fittest[0] == None:
                    fittest[0] = [i, errors]
                elif fittest[1] == None:
                    fittest[1] = [i, errors]
                else:
                    points  = [0, 0]
                    for i in range(0, len(errors)):
                        if errors[i] < fittest[0][1][i]:
                            points[0] += 1
                        if errors[i] < fittest[1][1][i]:
                            points[1] += 1
                            
                    if points[0] > 2 or points[1] > 2:
                        if points[0] > points[1]:
                            fittest[0] = [i, errors]
                        else:
                            fittest[1] = [i, errors]
                            
        if None in fittest:
            return self.fittest(results, error * 2)
        else:
            return [self.population[fittest[0][0]],
                    self.population[fittest[1][0]]]
    
    def breed(self, partners):
        for i in range(0, self.count):
            neurons = partners[0].neurons
            dends   = partners[0].dends

            for j in range(0, len(neurons)):
                for k in range(0, len(neurons[j])):
                    if random.random() < 0.5:
                        neurons[j][k] = (neurons[j][k] + partners[1].neurons[j][k]) / 2
                    else:
                        neurons[j][k] = (neurons[j][k] - partners[1].neurons[j][k]) / 2
                        
                    if random.random() > 0.5:
                        neurons[j][k] = neurons[j][k] + random.random() * random.randint(-5, 5)
                    else:
                        neurons[j][k] = neurons[j][k] - random.random() * random.randint(-5, 5)

            for j in range(0, len(dends)):
                for k in range(0, len(dends[j])):
                    for l in range(0, len(dends[j][k])):
                        if random.random() < 0.5:
                            dends[j][k][l][2] = (dends[j][k][l][2] + partners[1].dends[j][k][l][2]) / 2
                        else:
                            dends[j][k][l][2] = (dends[j][k][l][2] - partners[1].dends[j][k][l][2]) / 2
                            
                        if random.random() > 0.5:
                            dends[j][k][l][2] = dends[j][k][l][2] + random.random() * random.randint(-5, 5)
                        else:
                            dends[j][k][l][2] = dends[j][k][l][2] - random.random() * random.randint(-5, 5)
            
            self.population[i] = Network(neurons, dends)

ga = GA([[[0, 0], [0]],
         [[1, 0], [1]],
         [[0, 1], [1]],
         [[1, 1], [1]]], 100)
ga.populate()
#for i in range(0, 1):
#    results = ga.run(2)
#    print("    Breeding generation", ga.generation)
#    fittest = ga.fittest(results)
#    ga.breed(fittest)
ga.run()
