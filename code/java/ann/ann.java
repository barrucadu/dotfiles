/*
 * This is a neural network, a collection of layers.
 */
class ann
{
	private layer[] layers; // This is the collection of layers which make up the network.

	/* This adds a series of layers to the network */
	public void build(int count)
	{
		for(int i = 0; i < count; i += 1)
			{
				layer l = new layer;
				this.layers.add(l);
			}
	}

	/* This adds neurones to a layer */
	public void populate(int layer, int[] thresholds)
	{
		layer l = this.layers[layer];
		for(int t : thresholds)
			{
				l.addneuron(t);
			}
	}

	public void run(boolean[] inputs)
	{
		boolean firstrun = true;
		for(layer l : this.layers)
			{
				if(firstrun == true)
					{
						inputs = l.runinput(inputs);
						firstrun = false;
					}
				else
					{
						inputs = l.run(inputs);
					}
			}
		for(boolean b : inputs)
			{
				if(b == true)
					{
						System.out.println("1");
					}
				else
					{
						System.out.println("0");
					}
			}
	}
}

/*
 * This is a layer, a group of neurones.
 */
class layer
{
	private neuron[] neurones; // This is the neurones in the layer.

	/* This adds a neuron to the layer, with the given threshold */
	public void addneuron(int threshold)
	{
		neuron m = new neuron;
		m.threshold = threshold;
		this.neurones.add(m);
	}

	/* This sends a series of given inputs to the neurones in the layer. It returns an array of outputs. */
	public boolean[] run(boolean[] inputs)
	{
		boolean[] outputs;
		for(neuron n : this.neurones)
			{
				for(boolean b : inputs)
					{
						n.speak(b)
					}
				outputs.add(n.fire);
			}
		return outputs;
	}

	/* This treats the layer as an input layer. It sends each successive given input to each successive neuron, it then returns an array of outputs. */
	public boolean[] runinput(boolean[] inputs)
	{
		boolean[] outputs;
		for(neuron n : this.neurones)
			{
				n.speak(inputs[i]);
				outputs.add(n.fire);
			}
		return outputs;
	}
}

/*
 * This is a neuron, the most basic element of a neural network.
 */
class neuron
{
	private int threshold; // This is the minimum number of truth values needed to fire.
	private int input;     // This is the total number of truth values.

	/* This adds one to the input number is true, the input is the output of another neuron. */
	public void speak(boolean value)
	{
		if(value == true)
			{
				this.input += 1
			}
	}

	/* This returns true if the input is greater than the threshold, false otherwise. It then resets the input. */
	public boolean fire()
	{
		if(this.input >= this.threshold)
			{
				this.input = 0;
				return true;
			}
		else
			{
				this.input = 0;
				return false;
			}
	}
}