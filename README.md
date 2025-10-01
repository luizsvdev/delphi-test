# Avaliação Prática - Desenvolvedor Lazarus

Este projeto é uma solução para a avaliação prática de desenvolvimento, focada no controle de pesagem e mortalidade de aves em um sistema de gestão de granjas.

## Tecnologias Utilizadas
* **Linguagem:** Object Pascal (Lazarus)
* **Banco de Dados:** Oracle (com PL/SQL)
* **Componentes de Dados:** ZeosLib

## Pré-requisitos
1.  **Lazarus IDE:** Certifique-se de ter o Lazarus instalado.
2.  **ZeosLib:** É **essencial** ter os componentes do ZeosLib instalados na sua IDE Lazarus. Você pode fazer isso pelo menu `Pacote > Instalar/Desinstalar Pacotes`.
3.  **Oracle Client:** O cliente Oracle (Instant Client, por exemplo) deve estar instalado e acessível no `PATH` do sistema para que o ZeosLib possa se comunicar com o banco.

## Instruções de Configuração

### 1. Banco de Dados Oracle

1.  Certifique-se de que você tem acesso a um esquema (usuário) em um banco de dados Oracle.
2.  Execute os scripts da pasta `migrations/` na ordem correta:
    * Primeiro, execute `001_create_tables.sql`.
    * Em seguida, execute `002_create_procedures.sql`.

### 2. Configuração do Projeto Lazarus

1.  Abra o arquivo `src/PoultryManager.lpi` no Lazarus.
2.  No editor, abra o DataModule (`udatamodule.pas`).
3.  Selecione o componente `ZConnection1`.
4.  No `Inspetor de Objetos`, preencha as propriedades de conexão com os dados do seu banco:
    * `Database`
    * `HostName`
    * `Password`
    * `Port`
    * `Protocol` (deve ser `oracle`)
    * `User`
5.  Salve as alterações, compile (`Shift+F9`) e execute (`F9`) o projeto.
