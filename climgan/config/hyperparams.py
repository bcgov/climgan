# Defines the hyperparameter and constants configurations
from climgan.GAN.losses import (
    content_loss,
    wass_loss,
    crps_loss
)

import torch.nn as nn
import torch
import os


# Hyper params
gp_lambda = 10
critic_iterations = 5
batch_size = 49
gamma = 0.01
content_lambda = 20
#variance_lambda = 8
ncomp = 75
lr = 0.00025

# Run configuration parameters
epochs = 151
print_every = 10
save_every = 100
use_cuda = True

# Frequency separation parameters
filter_size = 7
padding = filter_size // 2
low = nn.AvgPool2d(filter_size, stride=1, padding=0)
rf = nn.ReplicationPad2d(padding)


metrics_to_calculate = {
    "MAE": content_loss,
    "Wass": wass_loss
}
