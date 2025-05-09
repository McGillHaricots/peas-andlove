import os, sys
import pandas as pd
import io
import numpy as np
import math
from typing import Optional
from dataclasses import dataclass
from math import sqrt
import csv

from sklearn import linear_model
from sklearn.model_selection import KFold, train_test_split
from sklearn.model_selection import cross_val_predict
from sklearn.linear_model import Ridge
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import StandardScaler


import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.nn import TransformerEncoder, TransformerEncoderLayer
from torch.nn.modules.transformer import MultiheadAttention, _get_activation_fn
from torch.utils.data import Dataset, DataLoader, TensorDataset
from torchvision import transforms
import torch.nn.functional as F
from torch.utils.data import TensorDataset, DataLoader


import itertools
import torch
from sklearn.model_selection import KFold
from scipy.stats import pearsonr
from concurrent.futures import ProcessPoolExecutor


