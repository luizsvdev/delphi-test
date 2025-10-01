unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, StdCtrls,
  ExtCtrls, DBCtrls, DB, ComCtrls, ZDataSet;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnNovaMortalidade: TButton;
    btnNovaPesagem: TButton;
    dbgLotes: TDBGrid;
    dbgMortalidade: TDBGrid;
    dbgPesagens: TDBGrid;
    dbedtDescricao: TDBEdit;
    dbedtQtdInicial: TDBEdit;
    GroupBox1: TGroupBox;
    lblDescricao: TLabel;
    lblQtdInicial: TLabel;
    lblSaude: TLabel;
    PageControl: TPageControl;
    pnlSaude: TPanel;
    TabMortalidade: TTabSheet;
    TabPesagens: TTabSheet;
    procedure btnNovaMortalidadeClick(Sender: TObject);
    procedure btnNovaPesagemClick(Sender: TObject);
    procedure dbedtDescricaoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure qryLotesAfterScroll(DataSet: TDataSet);
  private
    procedure AtualizarDetalhesLote;
    procedure AtualizarIndicadorSaude;
    function ObterMortalidadeAcumulada(ALoteID: Integer): Double;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses udatamodule;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  try
    dmData.ZConnection1.Connect;
    dmData.qryLotes.Open;

    AtualizarDetalhesLote;
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao conectar ao banco de dados: ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TfrmMain.qryLotesAfterScroll(DataSet: TDataSet);
begin
  AtualizarDetalhesLote;
end;

procedure TfrmMain.AtualizarDetalhesLote;
var
  LoteID: Integer;
begin
  if not dmData.qryLotes.IsEmpty then
  begin
    LoteID := dmData.qryLotes.FieldByName('ID_LOTE').AsInteger;

    dmData.qryPesagens.Close;
    dmData.qryPesagens.ParamByName('ID_LOTE').AsInteger := LoteID;
    dmData.qryPesagens.Open;

    dmData.qryMortalidade.Close;
    dmData.qryMortalidade.ParamByName('ID_LOTE').AsInteger := LoteID;
    dmData.qryMortalidade.Open;

    AtualizarIndicadorSaude;
  end
  else
  begin
    dmData.qryPesagens.Close;
    dmData.qryMortalidade.Close;
    lblSaude.Caption := 'SaÃºde do Lote';
    pnlSaude.Color := clBtnFace;
  end;
end;

function TfrmMain.ObterMortalidadeAcumulada(ALoteID: Integer): Double;
var
  TotalMortas, QtdInicial: Integer;
  qrySoma: TZQuery;
begin
  Result := 0;
  QtdInicial := dmData.qryLotes.FieldByName('QUANTIDADE_INICIAL').AsInteger;

  if QtdInicial > 0 then
  begin
    qrySoma := TZQuery.Create(nil);
    try
      qrySoma.Connection := dmData.ZConnection1;
      qrySoma.SQL.Text := 'SELECT SUM(QUANTIDADE_MORTA) FROM TAB_MORTALIDADE WHERE ID_LOTE_FK = :ID';
      qrySoma.ParamByName('ID').AsInteger := ALoteID;
      qrySoma.Open;
      if not qrySoma.Fields[0].IsNull then
      begin
        TotalMortas := qrySoma.Fields[0].AsInteger;
        Result := (TotalMortas / QtdInicial) * 100;
      end;
    finally
      qrySoma.Free;
    end;
  end;
end;

procedure TfrmMain.AtualizarIndicadorSaude;
var
  PercentualMortalidade: Double;
  LoteID: Integer;
begin
  if dmData.qryLotes.IsEmpty then Exit;

  LoteID := dmData.qryLotes.FieldByName('ID_LOTE').AsInteger;
  PercentualMortalidade := ObterMortalidadeAcumulada(LoteID);

  lblSaude.Caption := 'SaÃºde do Lote (' + FormatFloat('0.00', PercentualMortalidade) + '% de mortalidade)';

  if PercentualMortalidade > 10 then
    pnlSaude.Color := clRed
  else if PercentualMortalidade >= 5 then
    pnlSaude.Color := clYellow
  else
    pnlSaude.Color := clGreen;
end;


procedure TfrmMain.btnNovaPesagemClick(Sender: TObject);
var
  Data: TDateTime;
  PesoMedioStr, QuantidadeStr: string;
  PesoMedio: Currency;
  Quantidade: Integer;
begin
  if dmData.qryLotes.IsEmpty then
  begin
    ShowMessage('Selecione um lote primeiro.');
    Exit;
  end;

  PesoMedioStr := '1.85';
  QuantidadeStr := '100';

  if InputQuery('Nova Pesagem', 'Peso Médio (kg):', PesoMedioStr) then
    if InputQuery('Nova Pesagem', 'Quantidade de Aves Pesadas:', QuantidadeStr) then
    begin
      // usa PesoMedioStr e QuantidadeStr
    end;


  try
    Data := Now;
    PesoMedio := StrToFloat(PesoMedioStr);
    Quantidade := StrToInt(QuantidadeStr);

    if Quantidade > dmData.qryLotes.FieldByName('QUANTIDADE_INICIAL').AsInteger then
    begin
      ShowMessage('A quantidade pesada nÃ£o pode ultrapassar a quantidade inicial do lote.');
      Exit;
    end;

    with dmData.spInserirPesagem do
    begin
      ParamByName('p_id_lote').AsInteger := dmData.qryLotes.FieldByName('ID_LOTE').AsInteger;
      ParamByName('p_data_pesagem').AsDateTime := Data;
      ParamByName('p_peso_medio').AsFloat := PesoMedio;
      ParamByName('p_quantidade_pesada').AsInteger := Quantidade;
      ExecProc;
    end;

    ShowMessage('Pesagem registrada com sucesso!');
    dmData.qryPesagens.Refresh;
    dmData.qryLotes.Refresh;
  except
    on E: Exception do
      ShowMessage('Erro ao registrar pesagem: ' + E.Message);
  end;
end;

procedure TfrmMain.dbedtDescricaoChange(Sender: TObject);
begin

end;


procedure TfrmMain.btnNovaMortalidadeClick(Sender: TObject);
var
  Data: TDateTime;
  QuantidadeStr, Observacao: string;
  Quantidade: Integer;
  PercentualMortalidade: Double;
begin
  if dmData.qryLotes.IsEmpty then
  begin
    ShowMessage('Selecione um lote primeiro.');
    Exit;
  end;

  QuantidadeStr := '10';
  Observacao := '';
  if not InputQuery('Nova Mortalidade', 'Quantidade de Aves Mortas:', QuantidadeStr) then
    Exit;

  if not InputQuery('Nova Mortalidade', 'Observação:', Observacao) then
    Exit;

  // se chegou aqui, as duas variáveis foram preenchidas


  try
    Data := Now;
    Quantidade := StrToInt(QuantidadeStr);

    with dmData.spInserirMortalidade do
    begin
      ParamByName('p_id_lote').AsInteger := dmData.qryLotes.FieldByName('ID_LOTE').AsInteger;
      ParamByName('p_data_mortalidade').AsDateTime := Data;
      ParamByName('p_quantidade_morta').AsInteger := Quantidade;
      ParamByName('p_observacao').AsString := Observacao;
      ExecProc;
      PercentualMortalidade := ParamByName('p_percentual_mortalidade_out').AsFloat;
    end;

    ShowMessage('Mortalidade registrada com sucesso!');
    dmData.qryMortalidade.Refresh;
    AtualizarIndicadorSaude;

  except
    on E: Exception do
      ShowMessage('Erro ao registrar mortalidade: ' + E.Message);
  end;
end;

end.
