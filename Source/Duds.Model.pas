unit Duds.Model;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Duds.Common.Classes;

type
  TDudsModel = class(TObject)
  private
    FFiles: TDictionary<String, String>;
    FDelphiFiles: TObjectDictionary<String, TDelphiFile>;
  public
    constructor Create;
    destructor Destroy; override;

    property Files: TDictionary<String, String> read FFiles;
    property DelphiFiles: TObjectDictionary<String, TDelphiFile> read FDelphiFiles;
  end;

implementation

{ TDudsModel }

constructor TDudsModel.Create;
begin
  inherited Create;
  FFiles       := TDictionary<String, String>.Create;
  FDelphiFiles := TObjectDictionary<String, TDelphiFile>.Create([doOwnsValues]);
end;

destructor TDudsModel.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FDelphiFiles);
  inherited;
end;

end.
