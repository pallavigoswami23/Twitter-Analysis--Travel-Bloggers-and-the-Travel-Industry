import numpy as np
import numpy.linalg as la
import matplotlib as mp
import matplotlib.pyplot as plt
from PIL import Image
from urllib.request import urlopen


LIFE = plt.imread("", format="jpg")

def photoshop_detector(img):
    rows, cols, colors = img.shape
    flattened = img.reshape((-1, colors)) # flatten rows and cols into a single dimension
    # PCA is just the SVD of the covariance matrix
    U, S, V = la.svd(np.cov(flattened.T))

    PC2 = U[:,1] # 2nd principle component
    projected = np.dot(flattened,PC2)
    return np.reshape( projected, (rows,cols)) # unflatten

plt.imshow(photoshop_detector(LIFE), cmap = mp.cm.Greys_r)
plt.show()
