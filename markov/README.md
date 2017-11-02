## Very simple markov word chain.

The Markov chain is trained using a dataset of text, where each word has a certain probability of being followed by any other. The generator then starts with a random word, and randomly selects a word to follow it based on the trained probability. This process is iterated until a N-word sentence is created.

### constructor(content)
- `content` - the dataset you wish to use for the chain.

### process(start, length)
- `start` - the word with which the algorithm should start.
- `length` - the length of the generated sentence.
