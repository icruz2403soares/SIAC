
import numpy as np
import pandas as pd

from sklearn.datasets import make_circles

np.random.seed(0)

X, Y = make_circles(

    n_samples = (2000, 400), 
    
    noise = 0.145, factor = 0.55

)

Dados = pd.DataFrame(X, columns = ['A', 'B'])

Dados['Y'] = Y

Dados.to_csv('Teste\\Dados\\Primeiro.csv', index = False)

X, Y = make_circles(

    n_samples = (2000, 200), 
    
    noise = 0.145, factor = 0.55

)

Dados = pd.DataFrame(X, columns = ['A', 'B'])

Dados['Y'] = Y

Dados.to_csv('Teste\\Dados\\Segundo.csv', index = False)

X, Y = make_circles(

    n_samples = (2000, 100), 
    
    noise = 0.145, factor = 0.55

)

Dados = pd.DataFrame(X, columns = ['A', 'B'])

Dados['Y'] = Y

Dados.to_csv('Teste\\Dados\\Terceiro.csv', index = False)
