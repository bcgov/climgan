
from climgan.GAN.stage import StageData
import climgan.config.hyperparams as hp
from climgan.config import config
from climgan.GAN.wasserstein import WassersteinGAN
from climgan.mlflow_tools.mlflow_utils import log_hyperparams

import mlflow
import torch
import subprocess
import sys
#sys.stderr = open('errors.txt','w')

def train():
    s = StageData()
    torch.cuda.empty_cache()
    trainer = WassersteinGAN(
        s.generator,
        s.critic,
        s.G_optimizer,
        s.C_optimizer
    )

    mlflow.set_tracking_uri(config.EXPERIMENT_PATH)
    print("Tracking URI: ", mlflow.get_tracking_uri())

    with mlflow.start_run(experiment_id = s.exp_id, run_name = s.tag) as run:
        # mlflow.set_tag(run.info.run_id, s.tag)
        log_hyperparams()
        trainer.train(
            s.dataloader, 
            s.testdataloader,
        ) 

    torch.cuda.empty_cache()

if __name__ == "__main__":
    train()
