## To the best of my current knowledge, these classes are not needed in the main pipeline anymore


import os
import random
from dataclasses import dataclass
from typing import Any, List, Dict, Optional, Union, Tuple

import cv2
import torch
import requests
import numpy as np
from PIL import Image
import plotly.express as px
import matplotlib.pyplot as plt
import plotly.graph_objects as go
from transformers import AutoModelForMaskGeneration, AutoProcessor, pipeline

@dataclass
class BoundingBox:
    xmin: int
    ymin: int
    xmax: int
    ymax: int

    @property
    def xyxy(self) -> List[float]:
        return [self.xmin, self.ymin, self.xmax, self.ymax]
    @property
    
    def center(self) -> Dict[str, int]:
        return {'x': int((self.xmin + self.xmax) / 2), 'y': int((self.ymin + self.ymax) / 2)}

@dataclass
class DetectionResult:
    score: float
    label: str
    box: BoundingBox
    center: List[int]
    mask: Optional[np.array] = None

    @classmethod
    def from_dict(cls, detection_dict: Dict) -> 'DetectionResult':
        box = BoundingBox(xmin=detection_dict['box']['xmin'],
                          ymin=detection_dict['box']['ymin'],
                          xmax=detection_dict['box']['xmax'],
                          ymax=detection_dict['box']['ymax'])
        return cls(score=detection_dict['score'],
                   label=detection_dict['label'],
                   box=box,
                   center=box.center)

class Centers:
    def __init__(self):
        self.current = []
        self.prev = []