import numpy as np
import scipy.io
import cPickle
import re

X = np.load("MaskedData.npz")['X'][()]
y = np.load("MaskedData.npz")['y'][()]

with open("vocab.pkl", "rb") as f:
    vocab = cPickle.load(f)

# X.mtx
with open("X.mtx", "wb") as f:
    scipy.io.mmwrite(target=f, a=X, field="integer")

# y.txt
with open("y.txt", "wb") as text_file:
    for label in y:
        text_file.write(str(label) + "\n")

# vocab.txt
with open("vocab.txt", "wb") as text_file:
    for word in vocab:
        text_file.write(word + "\n")

raw_file = open("MaskedDataRaw.csv")
lines = raw_file.readlines()
lines[0] = lines[0][3:len(lines[0])]
new_lines = []
for line in lines:
    n = len(line)
    new_line = line[0:n - 2] + line[n - 1]
    new_lines.append(new_line)

# Find lines that have an unmatched double quotation
pattern = "^[^\"]*\"[^\"]*$"
index = 0
for line in new_lines:
    index += 1
    match = re.search(pattern, line)
    if match:
        print(index, line)

# Lines that have an unmatched double quotation
# new_lines[92447] = '886579,1,Kaggle,I wish I could write a paper entitled " Why Harry Potter is Awesome.\n'
# new_lines[172290] = '568993,1,Kaggle,Also: " Sexy Harry Potter.\n'
# new_lines[188332] = '205699,1,Sentiment140,\' Barack Obama shows his funny side " &gt;&gt; http://tr.im/l0gY !! Great speech..\n'
# new_lines[192360] = '5182,0,Kaggle," DA VINCI CODE SUCKS.\n'
# new_lines[479304] = '535893,1,Kaggle," I love you Harry Potter!\n'
# new_lines[558994] = '4290,0,Kaggle," Mission Impossible 3 sucks like a Theatan energy field!\n'
# new_lines[589419] = '5183,0,Kaggle," I hate Harry Potter.\n'
# new_lines[662063] = '885008,1,Kaggle,I liked the first " Mission Impossible.\n'
# new_lines[666612] = '1045840,0,Kaggle,MI: 2 was generally agreed to be a decent action movie but a terrible fit for the name " Mission Impossible.\n'
# new_lines[678159] = '839475,1,Kaggle,I love " The Da Vinci Code!\n'
# new_lines[707021] = '535891,1,Kaggle," Brokeback Mountain was an awesome movie. < 33..\n'
# new_lines[721208] = '535890,0,Kaggle," Brokeback Mountain sucked!\n'
# new_lines[1253873] = '8837,0,Kaggle," brokeback mountain was terrible.\n'
# new_lines[1535511] = '884977,1,Sentiment140,I like this guy : \' Barack Obama shows his funny side " &gt;&gt; http://tr.im/l0gY !!\n'
# new_lines[1577785] = '1540070,0,Kaggle,Stupid talk shows talking about " real life Brokeback Mountain stories.\n'

# Lines that do not have an unmatched double quotation
new_lines[92447] = '886579,1,Kaggle,I wish I could write a paper entitled Why Harry Potter is Awesome.\n'
new_lines[172290] = '568993,1,Kaggle,Also:  Sexy Harry Potter.\n'
new_lines[188332] = '205699,1,Sentiment140,\' Barack Obama shows his funny side  &gt;&gt; http://tr.im/l0gY !! Great speech..\n'
new_lines[192360] = '5182,0,Kaggle, DA VINCI CODE SUCKS.\n'
new_lines[479304] = '535893,1,Kaggle, I love you Harry Potter!\n'
new_lines[558994] = '4290,0,Kaggle, Mission Impossible 3 sucks like a Theatan energy field!\n'
new_lines[589419] = '5183,0,Kaggle, I hate Harry Potter.\n'
new_lines[662063] = '885008,1,Kaggle,I liked the first  Mission Impossible.\n'
new_lines[666612] = '1045840,0,Kaggle,MI: 2 was generally agreed to be a decent action movie but a terrible fit for the name  Mission Impossible.\n'
new_lines[678159] = '839475,1,Kaggle,I love  The Da Vinci Code!\n'
new_lines[707021] = '535891,1,Kaggle, Brokeback Mountain was an awesome movie. < 33..\n'
new_lines[721208] = '535890,0,Kaggle, Brokeback Mountain sucked!\n'
new_lines[1253873] = '8837,0,Kaggle, brokeback mountain was terrible.\n'
new_lines[1535511] = '884977,1,Sentiment140,I like this guy : \' Barack Obama shows his funny side  &gt;&gt; http://tr.im/l0gY !!\n'
new_lines[1577785] = '1540070,0,Kaggle,Stupid talk shows talking about  real life Brokeback Mountain stories.\n'

# MaskedDataRawFixed.csv
with open("MaskedDataRawFixed.csv", "wb") as new_csv_file:
    for line in new_lines:
        new_csv_file.write(line)
