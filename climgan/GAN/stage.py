# Begin - load the data and initiate training
# Defines the hyperparameter and constants configurationsimport gc
from climgan.networks.prism_generator import Generator
from climgan.networks.critic import Critic
from climgan.GAN.dataloader import NetCDFSR
import climgan.mlflow_tools.mlflow_utils as mlf 
import climgan.config.hyperparams as hp
from climgan.config import config
#from climgan.GAN.BourgainEmbed import BourgainSampler
#from climgan.helpers.gen_experiment_datasets import generate_train_test_coarse_fine, load_preprocessed
from xarray.core import dataset
from xarray.core.dataset import Dataset
import xarray as xr
import numpy as np
import torch
from mlflow.tracking import MlflowClient

highres_in = True
toydata = False
rotation = False
data_folder = "/users/kdaust/FFEC/climgan/data/"

def load_preprocessed():
    coarse_train = torch.load(data_folder + "era5_tiles_3001.pt")[:3000,...].unsqueeze(1)
    coarse_test = torch.load(data_folder + "era5_tiles_3001.pt")[:3000,...].unsqueeze(1)
    fine_train = torch.load(data_folder + "prism_tiles_3001.pt")[:3000,...].unsqueeze(1)
    fine_test = torch.load(data_folder + "prism_tiles_3001.pt")[:3000,...].unsqueeze(1)
    hrcov_train = torch.load(data_folder + "dem_tiles_3001.pt")[:3000,...].unsqueeze(1)
    hrcov_test = torch.load(data_folder + "dem_tiles_3001.pt")[:3000,...].unsqueeze(1)
    return coarse_train, fine_train, coarse_test, fine_test, hrcov_train, hrcov_test



assert torch.cuda.is_available(), "CUDA not available"
torch.cuda.empty_cache()
# Load dataset
coarse_train, fine_train, coarse_test, fine_test, hrcov_train, hrcov_test = load_preprocessed()

class StageData:
    def __init__(self, ):

        #print("Min Value: ", torch.min(fine_train))
        print("Coarse data shape: ", coarse_train.shape)
        print("Prism data shape: ", fine_train.shape)
        print("HRCOV data shape: ", hrcov_train.shape)

        self.fine_dim_n = fine_train.shape[-1]
        self.n_predictands = fine_train.shape[1]
        self.coarse_dim_n = coarse_train.shape[-1]
        self.n_covariates = coarse_train.shape[1]##adding invarient
        self.n_hrcov = hrcov_train.shape[1]
        

        print("Network dimensions: ")
        print("Fine: ", self.fine_dim_n, "x", self.n_predictands)
        print("Coarse: ", self.coarse_dim_n, "x", self.n_covariates)
        self.critic = Critic(self.coarse_dim_n, self.fine_dim_n, self.n_predictands).to(config.device)
        self.generator = Generator(self.coarse_dim_n, self.fine_dim_n, self.n_covariates, self.n_hrcov, self.n_predictands).to(config.device)

        # Define optimizers
        self.G_optimizer = torch.optim.Adam(self.generator.parameters(), hp.lr, betas=(0.9, 0.99))
        self.C_optimizer = torch.optim.Adam(self.critic.parameters(), hp.lr, betas=(0.9, 0.99))

        # Set up the run
        # Define the mlflow experiment drectories
        self.mlclient = MlflowClient(tracking_uri=config.EXPERIMENT_PATH)
        self.exp_id = mlf.define_experiment(self.mlclient)
        self.tag = mlf.write_tags()

        # Definte the dataset objects
        self.dataset = NetCDFSR(coarse_train, fine_train, hrcov_train, device=config.device)
        self.testdataset = NetCDFSR(coarse_test, fine_test, hrcov_test, device = config.device)
        
        self.dataloader = torch.utils.data.DataLoader(
            dataset=self.dataset, batch_size=hp.batch_size, shuffle=True
        )
        self.testdataloader = torch.utils.data.DataLoader(
            dataset=self.testdataset, batch_size=hp.batch_size, shuffle=True
        )
