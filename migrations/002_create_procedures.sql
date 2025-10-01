-- Script para criar as Stored Procedures

-- Procedure para inserir uma nova pesagem e atualizar o peso médio geral do lote
CREATE OR REPLACE PROCEDURE SP_INSERIR_PESAGEM (
    p_id_lote IN NUMBER,
    p_data_pesagem IN DATE,
    p_peso_medio IN NUMBER,
    p_quantidade_pesada IN NUMBER
)
IS
    v_qtd_inicial NUMBER;
BEGIN
    -- Valida se a quantidade pesada não excede a quantidade inicial
    SELECT QUANTIDADE_INICIAL INTO v_qtd_inicial FROM TAB_LOTE_AVES WHERE ID_LOTE = p_id_lote;
    IF p_quantidade_pesada > v_qtd_inicial THEN
        RAISE_APPLICATION_ERROR(-20001, 'A quantidade pesada (' || p_quantidade_pesada || ') não pode ultrapassar a quantidade inicial do lote (' || v_qtd_inicial || ').');
    END IF;

    -- Insere o registro de pesagem
    INSERT INTO TAB_PESAGEM (ID_PESAGEM, ID_LOTE_FK, DATA_PESAGEM, PESO_MEDIO, QUANTIDADE_PESADA)
    VALUES (SEQ_PESAGEM.NEXTVAL, p_id_lote, p_data_pesagem, p_peso_medio, p_quantidade_pesada);

    -- Atualiza o peso médio geral do lote (calculando a média ponderada de todas as pesagens)
    UPDATE TAB_LOTE_AVES
    SET PESO_MEDIO_GERAL = (
        SELECT SUM(PESO_MEDIO * QUANTIDADE_PESADA) / SUM(QUANTIDADE_PESADA)
        FROM TAB_PESAGEM
        WHERE ID_LOTE_FK = p_id_lote
    )
    WHERE ID_LOTE = p_id_lote;

    COMMIT;
EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END SP_INSERIR_PESAGEM;
/

-- Procedure para inserir uma nova mortalidade e retornar o percentual acumulado
CREATE OR REPLACE PROCEDURE SP_INSERIR_MORTALIDADE (
    p_id_lote IN NUMBER,
    p_data_mortalidade IN DATE,
    p_quantidade_morta IN NUMBER,
    p_observacao IN VARCHAR2,
    p_percentual_mortalidade_out OUT NUMBER
)
IS
    v_qtd_inicial NUMBER;
    v_total_mortas_anterior NUMBER;
    v_total_mortas_atual NUMBER;
BEGIN
    -- Busca a quantidade inicial e o total de mortes já registradas
    SELECT QUANTIDADE_INICIAL INTO v_qtd_inicial FROM TAB_LOTE_AVES WHERE ID_LOTE = p_id_lote;
    SELECT NVL(SUM(QUANTIDADE_MORTA), 0) INTO v_total_mortas_anterior FROM TAB_MORTALIDADE WHERE ID_LOTE_FK = p_id_lote;

    v_total_mortas_atual := v_total_mortas_anterior + p_quantidade_morta;

    -- Valida se a mortalidade acumulada não ultrapassa a quantidade inicial
    IF v_total_mortas_atual > v_qtd_inicial THEN
        RAISE_APPLICATION_ERROR(-20002, 'A soma das mortalidades (' || v_total_mortas_atual || ') não pode ultrapassar a quantidade inicial do lote (' || v_qtd_inicial || ').');
    END IF;

    -- Insere o registro de mortalidade
    INSERT INTO TAB_MORTALIDADE (ID_MORTALIDADE, ID_LOTE_FK, DATA_MORTALIDADE, QUANTIDADE_MORTA, OBSERVACAO)
    VALUES (SEQ_MORTALIDADE.NEXTVAL, p_id_lote, p_data_mortalidade, p_quantidade_morta, p_observacao);

    -- Calcula e retorna o percentual de mortalidade acumulada
    p_percentual_mortalidade_out := (v_total_mortas_atual / v_qtd_inicial) * 100;

    COMMIT;
EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END SP_INSERIR_MORTALIDADE;
/
