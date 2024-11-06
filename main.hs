-- Definindo os tipos
type Coordenada = (Int, Int, Int)
type Estado = (Coordenada, Bool)
type Movimento = (Int, Int, Int)
type ID = Int

-- Funções definidas anteriormente
atualiza_acao :: Bool -> Estado -> Estado
atualiza_acao acao (posicao, _) = (posicao, acao)

move :: Movimento -> Estado -> Estado
move (dx, dy, dz) ((x, y, z), ligado) 
    | ligado    = ((x + dx, y + dy, z + dz), ligado)
    | otherwise = ((x, y, z), ligado)

move_lista :: [Movimento] -> Estado -> Estado
move_lista [] estado = estado
move_lista (m:ms) estado = move_lista ms (move m estado)

move_varios :: [([Movimento], ID)] -> [Estado] -> [(Estado, ID)]
move_varios [] [] = []
move_varios ((movs, id):xs) (estado:estados) = 
    (move_lista movs estado, id) : move_varios xs estados

verifica_embates :: Estado -> [Estado] -> Bool
verifica_embates (posicao, _) estados = any (\(pos, _) -> pos == posicao) estados

move_varios_sem_embate :: [([Movimento], ID)] -> [Estado] -> [(Estado, ID)]
move_varios_sem_embate [] _ = []
move_varios_sem_embate _ [] = []
move_varios_sem_embate ((movs, id):xs) (estado:estados) = 
    let novoEstado = move_lista movs estado
    in if verifica_embates novoEstado estados 
       then (estado, id) : move_varios_sem_embate xs estados
       else (novoEstado, id) : move_varios_sem_embate xs (novoEstado:estados)

-- Função principal para executar o programa
main :: IO ()
main = do
    let nave1 = ((0, 0, 0), False)
    let nave2 = ((10, 10, 10), True)
    
    let nave1Ligada = atualiza_acao True nave1
    
    let movimentosNave1 = [(1, 1, 1), (0, 0, 1)]
    let movimentosNave2 = [(0, -1, -1), (-1, 0, 0)]
    
    let estadosIniciais = [nave1Ligada, nave2]
    
    print estadosIniciais

    let navesComMovimentos = [(movimentosNave1, 1), (movimentosNave2, 2)]
    let estadosFinaisSemColisao = move_varios_sem_embate navesComMovimentos estadosIniciais
    
    print estadosFinaisSemColisao
