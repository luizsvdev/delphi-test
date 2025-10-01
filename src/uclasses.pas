unit uclasses;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TLote = class
  private
    FID: Integer;
    FDescricao: string;
    FDataEntrada: TDateTime;
    FQuantidadeInicial: Integer;
  public
    property ID: Integer read FID write FID;
    property Descricao: string read FDescricao write FDescricao;
    property DataEntrada: TDateTime read FDataEntrada write FDataEntrada;
    property QuantidadeInicial: Integer read FQuantidadeInicial write FQuantidadeInicial;
  end;

  TPesagem = class
  private
    FID: Integer;
    FIDLote: Integer;
    FDataPesagem: TDateTime;
    FPesoMedio: Currency;
    FQuantidadePesada: Integer;
  public
    property ID: Integer read FID write FID;
    property IDLote: Integer read FIDLote write FIDLote;
    property DataPesagem: TDateTime read FDataPesagem write FDataPesagem;
    property PesoMedio: Currency read FPesoMedio write FPesoMedio;
    property QuantidadePesada: Integer read FQuantidadePesada write FQuantidadePesada;
  end;

  TMortalidade = class
  private
    FID: Integer;
    FIDLote: Integer;
    FDataMortalidade: TDateTime;
    FQuantidadeMorta: Integer;
    FObservacao: string;
  public
    property ID: Integer read FID write FID;
    property IDLote: Integer read FIDLote write FIDLote;
    property DataMortalidade: TDateTime read FDataMortalidade write FDataMortalidade;
    property QuantidadeMorta: Integer read FQuantidadeMorta write FQuantidadeMorta;
    property Observacao: string read FObservacao write FObservacao;
  end;

implementation

end.
