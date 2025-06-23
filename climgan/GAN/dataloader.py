import torch
from torch.utils.data import Dataset
import xarray as xr
import numpy as np

class NetCDFSR(Dataset):
    """Data loader from torch.Tensors"""
    def __init__(
        self,
        coarse: torch.Tensor,
        fine: torch.Tensor,
        hrcov: torch.Tensor,
        device: torch.device,
    ) -> torch.Tensor:
        """
        Initializes the dataset.
        Returns:
            torch.Tensor: The dataset batches.
        """
        self.fine = fine
        self.coarse = coarse
        self.hrcov = hrcov
        self.device = device

    def __len__(self):
        return self.fine.size(0)

    def __getitem__(self, idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()
            
        fine_ = self.fine[idx,...].to(self.device)
        coarse_ = self.coarse[idx,...].to(self.device)
        hrcov_ = self.hrcov[idx,...].to(self.device)
        return coarse_, fine_, hrcov_
