import numpy as np
import scipy.io
import cPickle

X = np.load("MaskedData.npz")['X'][()]
y = np.load("MaskedData.npz")['y'][()]

# X.mtx
with open("X.mtx", "wb") as f:
    scipy.io.mmwrite(target=f, a=X, field="integer")

# y.txt
with open("y.txt", "wb") as text_file:
    for label in y:
        text_file.write(str(label) + "\n")

# vocab.txt
with open("vocab.pkl", "rb") as f:
    vocab = cPickle.load(f)

with open("vocab.txt", "wb") as text_file:
    for word in vocab:
        text_file.write(word + "\n")

# MaskedDataRawFixed.csv
raw_file = open("MaskedDataRaw.csv")
with open("MaskedDataRawFixed.csv", "wb") as new_csv_file:
    for tweet in raw_file.readlines():
        new_csv_file.write(tweet)
