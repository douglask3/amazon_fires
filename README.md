# Unusual fire seasons in a changing climate - A Bayesian approach

This repo contains the Bayesian Inference modelling framework for assessing the contributing of meteological condtions to 2019 Amazonia fires. We'll hopefully have a paper, pre-print out soon. For now, more info can be found on [this poster](https://docs.google.com/drawings/d/16ynwXFCWGg7WFqMO5baoK1NINlqTCPjIZQ7FzDFQKd4/edit?usp=sharing).

### Making your local copy of all this
As with all github stuff, you probably would like to clone this to locally. If your planning on making changes, make a fork for now (top right button) and clone that instead. If you fancy working together on some framework development, drop me an email.
```
git clone https://github.com/douglask3/fireMIPbenchmarking.git
cd fireMIPbenchmarking
git fetch 
git checkout origin/EGU2020
```

### Setup 
Optimization and sampling is run in Jupyter notebooks [here](optimise_run_model). If new to Jupyter, I find the easiest way of setting all this up is through a Conda enviroment. [Instillation instrcutions here](https://docs.anaconda.com/anaconda/install/) depend on your OS. 

Along with all Conda, you'll need a few python packages beyond the everyday stuff (numpy, pandas etc). Optimization uses [pymc3](https://anaconda.org/conda-forge/pymc3):

```
conda install -c conda-forge pymc3
```

Some of the visualisations use matplotlib and iris:

```
conda install -c conda-forge iris
conda install -c conda-forge matplotlib
```

### Data
You'll be able grab driving data and model postior sampled output from [here](https://doi.org/10.5281/zenodo.3588441). While the main papers in review, you'll have to make a request. But it will be public at some point soon. Otherwise, feel free to [email me](mailto:douglas.i.kelley@gmail.com) directly. 


