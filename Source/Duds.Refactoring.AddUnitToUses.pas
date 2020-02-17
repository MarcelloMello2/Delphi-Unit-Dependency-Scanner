unit Duds.Refactoring.AddUnitToUses;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  System.DateUtils,
  System.Generics.Collections, System.IOUtils,

  Duds.Common.Types,
  Duds.Common.Strings,
  Duds.Common.Classes,
  Duds.Common.Files,
  Duds.Common.Language,
  Duds.Common.Log,
  Duds.Common.Interfaces,
  Duds.Model,
  Duds.Common.Refactoring;

type
  TAddUnitToUsesRefactoring = class(TObject)
  private
    FModel: TDudsModel;
    procedure AddUnitToUsesInFile(DummyRun: Boolean; DelphiFile: TDelphiFile;
      InsertAtUsesUnitName, NewUnitName: String);

  public
    procedure AddUnitToUses(DummyRun: Boolean; DelphiFile: TDelphiFile; NewUnitName: String);

    property Model: TDudsModel read FModel write FModel;

  end;

implementation

{ TAddUnitToUsesRefactoring }

procedure TAddUnitToUsesRefactoring.AddUnitToUses(DummyRun: Boolean; DelphiFile: TDelphiFile; NewUnitName: String);
var
  ReferencedUnitName: string;
  UpdatedCount, AlreadyReferencedCount: Integer;
  CurrentDelphiFile: TDelphiFile;
begin
  UpdatedCount := 0;
  AlreadyReferencedCount := 0;

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISISADUMMYRUN, LogWarning);

  ReferencedUnitName := DelphiFile.UnitInfo.DelphiUnitName;

  TDudsLogger.GetInstance.Log('Adding unit <%s> to all uses clauses that already reference unit <%s>.',
    [NewUnitName, ReferencedUnitName]);

  // Walk through all units
  for CurrentDelphiFile in FModel.DelphiFileList do
  begin
    // Only update .pas files, not project files
    if CurrentDelphiFile.UnitInfo.DelphiFileType = ftPAS then
    begin
      if FModel.IsUnitUsed(ReferencedUnitName, CurrentDelphiFile) then
        if FModel.IsUnitUsed(NewUnitName, CurrentDelphiFile) then
        begin
          TDudsLogger.GetInstance.Log('Unit <%s> already references unit <%s>. No update needed.',
            [CurrentDelphiFile.UnitInfo.DelphiUnitName, NewUnitName]);
          Inc(AlreadyReferencedCount);
        end
        else
        begin
          TDudsLogger.GetInstance.Log('Adding unit <%s> to uses clause of unit <%s>.',
            [NewUnitName, CurrentDelphiFile.UnitInfo.DelphiUnitName]);

          AddUnitToUsesInFile(DummyRun, CurrentDelphiFile, ReferencedUnitName, NewUnitName);

          Inc(UpdatedCount);
        end;
    end;
  end;

  if AlreadyReferencedCount > 0 then
    TDudsLogger.GetInstance.Log(StrNumberOfAlreadyReferencingUnits, [AlreadyReferencedCount]);
  TDudsLogger.GetInstance.Log(StrFinishedUpdated, [UpdatedCount]);

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISWASADUMMYRUN, LogWarning);
end;

procedure TAddUnitToUsesRefactoring.AddUnitToUsesInFile(DummyRun: Boolean; DelphiFile: TDelphiFile; InsertAtUsesUnitName: string;
  NewUnitName: String);
var
  InsertAtUnit: IUsedUnitInfo;
  usedUnit: IUsedUnitInfo;
  ReplacementText: string;
begin
  InsertAtUnit := nil;
  for usedUnit in DelphiFile.UnitInfo.UsedUnits do
    if usedUnit.DelphiUnitName.Equals(InsertAtUsesUnitName) then
       InsertAtUnit := usedUnit;

  if InsertAtUnit = nil then
     raise Exception.Create('internal error - referenced unit not found in uses clause');


  ReplacementText := NewUnitName + ', ' + InsertAtUsesUnitName;

  SafeReplaceTextInFile(DummyRun, DelphiFile.UnitInfo.Filename, InsertAtUsesUnitName, ReplacementText, InsertAtUnit.Position);

  //TODO: Insert a "used unit"
  //TODO: Update offsets
  //TODO: update GUI trees
end;

end.
