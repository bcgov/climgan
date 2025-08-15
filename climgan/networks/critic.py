# Borrowed from:
# https://github.com/Lornatang/SRGAN-PyTorch/
# Adopted to ESRGAN: https://arxiv.org/abs/1809.00219

import torch
import torch.nn as nn


class Critic(nn.Module):
    r"""The main architecture of the discriminator. Similar to VGG structure."""

    def __init__(self, coarse_dim, fine_dim, nc):
        super(Critic, self).__init__()
        self.coarse_dim = coarse_dim
        self.fine_dim = fine_dim
        self.nc = nc

        self.features = nn.Sequential(
            nn.Conv2d(
                self.nc, self.coarse_dim, kernel_size=3, stride=1, padding=1
            ),  # input is 96x96
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                #int(self.coarse_dim*4),
                self.coarse_dim,
                self.coarse_dim,
                kernel_size=3,
                stride=2,
                padding=1,
                bias=False,
            ),  # state size. 48
            #nn.LayerNorm([self.coarse_dim, 64, 64]),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                self.coarse_dim,
                2 * self.coarse_dim,
                kernel_size=3,
                stride=1,
                padding=1,
                bias=False,
            ),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                2 * self.coarse_dim,
                2 * self.coarse_dim,
                kernel_size=3,
                stride=2,
                padding=1,
                bias=False,
            ),  # state size. 24
            #nn.LayerNorm([2 * self.coarse_dim, 32, 32]),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                2 * self.coarse_dim,
                4 * self.coarse_dim,
                kernel_size=3,
                stride=1,
                padding=1,
                bias=False,
            ),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                4 * self.coarse_dim,
                4 * self.coarse_dim,
                kernel_size=3,
                stride=2,
                padding=1,
                bias=False,
            ),  # state size. 12
            #nn.LayerNorm([4 * self.coarse_dim, 16, 16]),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                4 * self.coarse_dim,
                8 * self.coarse_dim,
                kernel_size=3,
                stride=1,
                padding=1,
                bias=False,
            ),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Conv2d(
                8 * self.coarse_dim,
                8 * self.coarse_dim,
                kernel_size=3,
                stride=2,
                padding=1,
                bias=False,
            ),  # state size.  6 x 6
            #nn.LayerNorm([8 * self.coarse_dim, 8, 8]),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            # nn.Conv2d(8*self.coarse_dim, 16*self.coarse_dim, kernel_size=3, stride=1, padding=1, bias=False),  # state size. (512) x 6 x 6
            # nn.LeakyReLU(negative_slope=0.2, inplace=True),
            # nn.Conv2d(16*self.coarse_dim, 16*self.coarse_dim, kernel_size=3, stride=2, padding=1, bias=False),  # state size. (1024) x 3 x 3
            # nn.LeakyReLU(negative_slope=0.2, inplace=True)
        )

        self.classifier = nn.Sequential(
            nn.Linear(int((6*6)*(8 * self.coarse_dim)), 100),
            # nn.Linear(32*24 * self.coarse_dim, 100),
            #nn.Linear(int(221184), 100),
            nn.LeakyReLU(negative_slope=0.2, inplace=True),
            nn.Linear(100, 1),
        )

    def forward(self, input: torch.Tensor, cov_hr: torch.Tensor, cov_lr: torch.Tensor) -> torch.Tensor:
        out = self.features(input)
        out = torch.flatten(out, 1)
        out = self.classifier(out)
        return out
