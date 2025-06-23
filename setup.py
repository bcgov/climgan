import sys
from setuptools import setup, find_packages


setup(
    name="climgan",
    description="Prism Emulation with Wasserstein GANs",
    keywords="AI deep learning generative super resolution downscaling",
    packages=find_packages(),
    version="0.1",
    author="Kiri Daust, Nic Annau",
    author_email="kiridaust@uvic.ca",
    zip_safe=True,
    scripts=[
        "climgan/GAN/train.py",
        "climgan/GAN/dataloader.py",
        "climgan/GAN/losses.py",
        "climgan/config/config.py",
        "climgan/config/hyperparams.py",
        "climgan/networks/critic_covariates.py",
        "climgan/networks/prism_generator.py",
        "climgan/mlflow_tools/gen_grid_plots.py",
        "climgan/mlflow_tools/mlflow_epoch.py"
    ],
    install_requires=["numpy", "dask", "torch", "xarray", "scikit-learn"],
    extras_require = {"dask": "distributed"},
    package_dir={"climgan": "climgan"},
    package_data={"climgan": ["data/*", "climgan/"]},
    classifiers="""
        Intended Audience :: Science/Research
        License :: GNU General Public License v3 (GPLv3)
        Operating System :: OS Independent
        Hardware :: Requires CUDA GPU
        Programming Language :: Python :: 3.10
        Topic :: Scientific/Engineering
        Topic :: Software Development :: Libraries :: Python Modules""".split(
                "\n"
    ),
)
