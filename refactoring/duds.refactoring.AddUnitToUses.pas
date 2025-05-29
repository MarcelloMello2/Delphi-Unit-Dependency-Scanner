unit duds.refactoring.AddUnitToUses;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  System.DateUtils,
  System.Generics.Collections, System.IOUtils,

  duds.common.Types,
  duds.common.Strings,
  duds.common.Classes,
  duds.common.Files,
  duds.common.Language,
  duds.common.Log,
  duds.common.Interfaces,
  duds.common.Refactoring,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.analyzer.model;

type
  TAddUnitToUsesRefactoring = class(TObject)
  private
    FModel: TDudsModel;
    procedure AddUnitToUsesInFile(DummyRun: Boolean; DelphiFile: TDelphiFile;
      InsertAtUsesUnitName, NewUnitName: String);

  public
    procedure AddUnitToUses(DummyRun: Boolean; DelphiFile: TDelphiFile; NewUnitName: String; aRestrictToUnitNames: TStrings);

    property Model: TDudsModel read FModel write FModel;

  end;

implementation

{ TAddUnitToUsesRefactoring }

procedure TAddUnitToUsesRefactoring.AddUnitToUses(DummyRun: Boolean; DelphiFile: TDelphiFile; NewUnitName: String; aRestrictToUnitNames: TStrings);
var
  ReferencedUnitName: string;
  UpdatedCount, AlreadyReferencedCount: Integer;
  CurrentDelphiFile: TDelphiFile;
  NewUnitDelphiFile: TDelphiFile;
  AddOnlyInUnits:    TStringList;
  i:                 Integer;
  aUnit:             string;
begin
  UpdatedCount := 0;
  AlreadyReferencedCount := 0;

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISISADUMMYRUN, LogWarning);

  ReferencedUnitName := DelphiFile.UnitInfo.DelphiUnitName;

  TDudsLogger.GetInstance.Log('Adding unit <%s> to all uses clauses that already reference unit <%s>.',
    [NewUnitName, ReferencedUnitName]);

  // Add the new unit to the model
  NewUnitDelphiFile := FModel.FindParsedDelphiUnit(NewUnitName, nil);
  if not Assigned(NewUnitDelphiFile) then
  begin
    if not DummyRun then
    begin
      NewUnitDelphiFile := FModel.CreateDelphiFile(NewUnitName, false);
      NewUnitDelphiFile.UnitInfo                := TUnitInfo.Create;
      NewUnitDelphiFile.UnitInfo.DelphiUnitName := NewUnitName;
    end;
    TDudsLogger.GetInstance.Log('Unit <%s> was not part of the dependency model, adding it to the model now.', [NewUnitName]);
  end;

  AddOnlyInUnits := TStringList.Create;
  try
    // Operation restricted to list of unit names? => normalize list first
    for i := 0 to aRestrictToUnitNames.Count -1 do
    begin
      aUnit := ChangeFileExt(ExtractFileName(aRestrictToUnitNames[i]), '');
      aUnit := aUnit.Trim.ToLower;
      if not aUnit.IsEmpty then
        if AddOnlyInUnits.IndexOf(aUnit) = -1 then
          AddOnlyInUnits.Add(aUnit);
    end;

    // Walk through all units
    for CurrentDelphiFile in FModel.DelphiFileList do
    begin
      // Only update .pas files, not project files
      if CurrentDelphiFile.UnitInfo.DelphiFileType = ftPAS then
      begin
        if FModel.IsUnitUsed(ReferencedUnitName, CurrentDelphiFile) then
          if (AddOnlyInUnits.Count = 0) or (AddOnlyInUnits.IndexOf(CurrentDelphiFile.UnitInfo.DelphiUnitName.ToLower) > -1)  then
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
  finally
    AddOnlyInUnits.Free;
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
  InsertBeforeUsedUnit, NewUsedUnit: IUsedUnitInfo;
  InsertBeforeUsedUnitIndex: Integer;
  UsedUnit: IUsedUnitInfo;
  ReplacementText: string;
  PositionOffset: Integer;
  IsUsesAfterInserted: Boolean;
  FollowingUsedUnitInfo: IUsedUnitInfo;
  InsertedUnitDelphiFile: TDelphiFile;
begin
  InsertBeforeUsedUnit := nil;
  for UsedUnit in DelphiFile.UnitInfo.UsedUnits do
    if UsedUnit.DelphiUnitName.ToLower.Equals(InsertAtUsesUnitName.ToLower) then
       InsertBeforeUsedUnit := UsedUnit;

  if InsertBeforeUsedUnit = nil then
     raise Exception.Create('internal error - referenced unit not found in uses clause');


  ReplacementText := NewUnitName + ', ' + InsertAtUsesUnitName;
  PositionOffset  := Length(ReplacementText) - Length(InsertAtUsesUnitName);

  // step 1: Replace text "UnitOld" with "unitNew, UnitOld"
  SafeReplaceTextInFile(DummyRun, DelphiFile.UnitInfo.Filename, InsertAtUsesUnitName, ReplacementText, InsertBeforeUsedUnit.Position);

  if not DummyRun then
  begin
    // step 2: "Insert" the new "used unit" into the usedUnits list
    NewUsedUnit := TUsedUnitInfo.Create;

    NewUsedUnit.Filename       := '';
    NewUsedUnit.DelphiUnitName := NewUnitName;
    NewUsedUnit.Position       := InsertBeforeUsedUnit.Position;  // takes over position from the "insert before unit"
    NewUsedUnit.InFilePosition := 0;
    NewUsedUnit.UsesType       := InsertBeforeUsedUnit.UsesType;  // same position, same uses type...

    InsertBeforeUsedUnitIndex := DelphiFile.UnitInfo.UsedUnits.IndexOf(InsertBeforeUsedUnit);
    DelphiFile.UnitInfo.UsedUnits.Insert(InsertBeforeUsedUnitIndex, NewUsedUnit);

    // increase the used counter of the inserted unit
    InsertedUnitDelphiFile := FModel.FindParsedDelphiUnit(NewUnitName, nil);
    if not Assigned(InsertedUnitDelphiFile) then
      raise Exception.Create('this should not happen, the new unit should have been added to the model');
    InsertedUnitDelphiFile.UsedCount := InsertedUnitDelphiFile.UsedCount + 1;

    // step 3: Update positions offsets of existing uses
    IsUsesAfterInserted := false;
    for FollowingUsedUnitInfo in DelphiFile.UnitInfo.UsedUnits do
    begin
      if FollowingUsedUnitInfo = InsertBeforeUsedUnit then
        IsUsesAfterInserted := true;
      if IsUsesAfterInserted then
        FollowingUsedUnitInfo.UpdatePosition(PositionOffset);
    end;
  end;

  // After this action, the GUI Trees has to be rebuild because the model changed! See GUI for that...
end;

end.
