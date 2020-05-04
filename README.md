# Unusual fire seasons in a changing climate - A Bayesian approach

This repo contains the Bayesian Inference modelling framework for assessing the contributing of meteological condtions to 2019 Amazonia fires. We'll hopefully have a paper, pre-print out soon. For now, more info can be found on [this poster](https://docs.google.com/drawings/d/16ynwXFCWGg7WFqMO5baoK1NINlqTCPjIZQ7FzDFQKd4/edit?usp=sharing). 

## Setup 

### Making your local copy of all this
As with all github stuff, you probably would like to clone this to locally. If your planning on making changes, make a fork for now (top right button) and clone that instead. If you fancy working together on some framework development, drop me an [email](mailto:confirebayes@gmail.com).
```
git clone https://github.com/douglask3/fireMIPbenchmarking.git
cd fireMIPbenchmarking
git fetch 
git checkout origin/EGU2020
```

### Installing stuff
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
You'll be able grab driving data and model postior sampled output from [here](https://doi.org/10.5281/zenodo.3588441). While the main papers in review, you'll have to make a request. But it will be public at some point soon. Otherwise, feel free to [email me](mailto:confirebayes@gmail.com) directly. This should be placed into the [data](data/) directory of your local repo.

### Getting going
The notebooks you need are in [optimise_run_model](optimise_run_model/), and are split into three parts:
1. [prepare_data.ipynb](optimise_run_model/prepare_data.ipynb) You only need to run this if you wish to indtroduce new data. converts netcdf files into a csv file ready for optimization. You can run bayesian optimization using netcdf files, but given the model is (at the moment) time-independant, and given the amount of data we need, csv is faster.

2. [bayesian_inference.ipynb](optimise_run_model/bayesian_inference.ipynb) performs the optimization magic on the data (you may have) processed in [prepare_data.ipynb](optimise_run_model/prepare_data.ipynb).

3. [make_model_output.ipynb](optimise_run_model/make_model_output.ipynb) then samples the postior of the model based on your optimization. 

To run them:
```
cd optimise_run_model
jupyter notebook
```

will do the trick.

The notebooks do a load of funky things.... one of which is explain all the funky things they do. So look inside the notebooks for more ino.

### Known issues and troubleshoot
This will probably get expanded a lot when people start using this, and I'll probably set up some issue tickets at some point. For now...:

1. The model is actually written out twice. Once using theano in [bayesian_inference.ipynb](optimise_run_model/bayesian_inference.ipynb), and again using numpy and iris in [make_model_output.ipynb](optimise_run_model/make_model_output.ipynb). You may think this is clunky and not very elegant... and you'd be right! The problem is that one they need writing with repsective packages for their repectove uses, and make_model_output.ipynb also contains output relevent to Kelley et al. 2019 that would drive down effienceny of the bayesian optimization. However, with a bit of effort and knowhow, this bit of work flow could,er, flow much better. If someone want to imporve this, feel free to volenteer (and [email](mailto:mailto:confirebayes@gmail.com) first ofcourse!

