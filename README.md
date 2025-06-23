# climgan
Python package for training a GAN PRISM emulator, using the stochasticv Wasserstein Generative Adversarial Network as described in https://journals.ametsoc.org/view/journals/aies/4/2/AIES-D-24-0044.1.xml

# Installation
A working version of CUDA with Pytorch is required for this repo. 

1. Create a python virtual environment and activate it:

    ```python3 -m venv myvenv```

    ```source myvenv/bin/activate```

2. Install requirements
    ```pip install -r requirements.txt```

3. Install climgan (-e is essential if you want to customize the code)
```pip install -e /path/to/cloned/climgan/```
