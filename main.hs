-- Definindo tipos de dados para tornar o código mais legível
type Estado = (Coordenadas, Bool)       -- (localização, ligado/desligado)
type Movimento = (Int, Int, Int)        -- Movimento nas coordenadas (x, y, z)
type NaveID = Int                       -- Identificador da nave
type Nave = ([Movimento], NaveID)       -- (lista de movimentos, ID da nave)

-- 1. Função para atualizar o estado da nave com base na ação ligar/desligar
atualiza_acao :: Bool -> Estado -> Estado
atualiza_acao acao (localizacao, _) = (localizacao, acao)

-- 2. Função para mover a nave dada uma tripla de movimento e seu estado atual
move :: Movimento -> Estado -> Estado
move (dx, dy, dz) ((x, y, z), True)  = ((x + dx, y + dy, z + dz), True)
move (dx, dy, dz) ((x, y, z), False) = ((x, y, z), False)


-- 3. Função para aplicar uma lista de movimentos em sequência sobre o estado da nave
move_lista :: [Movimento] -> Estado -> Estado
move_lista [] estado = estado
move_lista (mov:movs) estado = move_lista movs (move mov estado)

-- 4. Função para aplicar movimentos a várias naves e retornar seus estados finais
move_varios :: [Nave] -> [Estado] -> [(Estado, NaveID)]
move_varios [] _ = []
move_varios ((movs, id):naves) (estado:estados) =
    let estadoFinal = move_lista movs estado
    in (estadoFinal, id) : move_varios naves estados

-- 5. Função para verificar se uma nave colide com outras (compartilha coordenadas)
verifica_embates :: Estado -> [Estado] -> Bool
verifica_embates (localizacao, _) [] = False 
verifica_embates (localizacao, _) ((loc, _):resto)
  | loc == localizacao = True 
  | caso_contrario = verifica_embates (localizacao, _) resto

-- 6. Função para mover várias naves, evitando embates
move_varios_sem_embates :: [Nave] -> [Estado] -> [(Estado, NaveID)]
move_varios_sem_embates [] _ = []
move_varios_sem_embates ((movs, id):naves) estados =
    let estadoFinal = move_lista movs (head estados)
        colide = verifica_embates estadoFinal (tail estados)
    in if colide
       then (head estados, id) : move_varios_sem_embates naves (tail estados) -- Se embater, a nave não se move
       else (estadoFinal, id) : move_varios_sem_embates naves (tail estados) -- Se não embater, a nave move-se


