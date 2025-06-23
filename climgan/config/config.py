from climgan.helpers.wrf_times import datetime_wrf_period

import os
from datetime import datetime

import torch

EXPERIMENT_PATH = '/media/data/mlflow_exp'
device = torch.device("cuda:0")
GENERATOR_PATH = "/home/kiridaust/PRISM_Data/dec/gen1/"
