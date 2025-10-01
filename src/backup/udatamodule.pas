unit udatamodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ZConnection, ZDataset, ZStoredProcedure, DB;

type

  { TdmData }

  TdmData = class(TDataModule)
    dsLotes: TDataSource;
    dsMortalidade: TDataSource;
    dsPesagens: TDataSource;
    qryLotes: TZQuery;
    qryMortalidade: TZQuery;
    qryPesagens: TZQuery;
    spInserirMortalidade: TZStoredProc;
    spInserirPesagem: TZStoredProc;
    ZConnection1: TZConnection;
    procedure DataModuleCreate(Sender: TObject);
    procedure ZConnection1AfterConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmData: TdmData;

implementation

{$R *.lfm}

{ TdmData }

procedure TdmData.ZConnection1AfterConnect(Sender: TObject);
begin

end;

procedure TdmData.DataModuleCreate(Sender: TObject);
begin

end;

end.
