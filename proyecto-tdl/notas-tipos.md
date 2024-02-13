|- : consecuencia logica

|- e : T 
'En consecuencia e tiene el tipo T'

### DEF
Typing environment (Env, por ahora) : funcion lookup que mapea variables con sus tipos.
Por lo tanto, 

Env(t) = T
----------
Env |- t : T

### Ejemplo de regla de inferencia

Env |- e1 : int         Env |- e2 : int
----------------------------------------
        Env |- e1 + e2 : int
