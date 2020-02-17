unit Duds.Refactoring.RenameUnit;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  System.DateUtils,
  System.Generics.Collections, System.IOUtils, System.RegularExpressions,

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
  TDudsRenameRefacotring = class(TObject)
  private
    FModel: TDudsModel;
  public
    procedure RenameDelphiFile(const aClearLog: Boolean; const SearchStringOrOldUnitName, ReplaceStringOrNewUnitName: String;
      const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment, LowerCaseExtension: Boolean);

    property Model: TDudsModel read FModel write FModel;

  end;

implementation

procedure TDudsRenameRefacotring.RenameDelphiFile(const aClearLog: Boolean; const SearchStringOrOldUnitName, ReplaceStringOrNewUnitName: String;
  const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment, LowerCaseExtension: Boolean);

  function IsUnitNameMatch(const UnitName: String): Boolean;
  begin
    if ExactMatch then
      Result := SameText(SearchStringOrOldUnitName, UnitName)
    else
      Result := TRegEx.IsMatch(UnitName, SearchStringOrOldUnitName);
  end;

  function SearchAndReplaceUnitName(const UnitName: String): String;
  begin
    if IsUnitNameMatch(UnitName) then
    begin
      if ExactMatch then
        Result := StringReplace(UnitName, SearchStringOrOldUnitName, ReplaceStringOrNewUnitName, [rfIgnoreCase])
      else
        Result := TRegEx.Replace(UnitName, SearchStringOrOldUnitName, ReplaceStringOrNewUnitName);
    end
    else
      raise Exception.Create('Why here? This function should only be called for renaming a unitname...')
  end;

  function RenameMatchingFiles(const OldFilename, NewUnitName: String; const IsHistory: Boolean;
    LowerCaseExtension: Boolean): Boolean;
  var
    ScanFilenames: TObjectList<TFileInfo>;
    i: Integer;
    Filter: String;
    NewFilename, LogMsg: String;
    NewFileExtension: String;
  begin
    Result := FALSE;

    if IsHistory then
      Filter := ExtractFileName(OldFilename) + '.*'
    else
      Filter := ExtractFilenameNoExt(OldFilename) + '.*';

    ScanFilenames := ScanFiles(ExtractFileDir(OldFilename), Filter, FALSE, FALSE, TRUE);
    try
      for i := 0 to pred(ScanFilenames.Count) do
      begin
        if (not ScanFilenames[i].IsDir) and
          (SameText(ExtractFilenameNoExt(OldFilename), ExtractFilenameNoExt(ScanFilenames[i].Filename))) then
        begin
          NewFileExtension := ExtractFileExt(ScanFilenames[i].Filename);
          if LowerCaseExtension then
            NewFileExtension := LowerCase(NewFileExtension);
          NewFilename := IncludeTrailingPathDelimiter(ExtractFileDir(ScanFilenames[i].Filename)) + NewUnitName +
            NewFileExtension;

          if DummyRun then
            Result := TRUE
          else
            Result := RenameFile(ScanFilenames[i].Filename, NewFilename);

          LogMsg := StrFileSRenamedTo;

          if IsHistory then
            LogMsg := StrHistory + LogMsg;

          TDudsLogger.GetInstance.Log(LogMsg, [ScanFilenames[i].Filename, NewFilename]);

          if (not IsHistory) and (RenameHistoryFiles) then
            RenameMatchingFiles(IncludeTrailingPathDelimiter(ExtractFileDir(OldFilename)) + '__history\' +
              ExtractFileName(ScanFilenames[i].Filename), NewUnitName + ExtractFileExt(ScanFilenames[i].Filename), TRUE,
              LowerCaseExtension);
        end;
      end;
    finally
      FreeAndNil(ScanFilenames);
    end;
  end;

var
  UpdatedCount: Integer;

  procedure UpdateUsesClauseInDelphiFile(DelphiFile: TDelphiFile; UsedUnitInfo: IUsedUnitInfo);
  var
    PositionOffset: Integer;
    OldUnitName, NewUnitName: String;
    ReplaceSuccessful: Boolean;
    IsUsesAfterRenamedUses: Boolean;
    FollowingUsedUnitInfo: IUsedUnitInfo;
  begin
    OldUnitName    := UsedUnitInfo.DelphiUnitName;
    NewUnitName    := SearchAndReplaceUnitName(OldUnitName);
    PositionOffset := Length(NewUnitName) - Length(OldUnitName);

    // step 1: apply the rename in the file
    ReplaceSuccessful := FALSE;

    // replace text occurence for <OldUnitName in '....\OldUnitName.pas'>
    // ->                                               ^^^^^^^^^^^
    if UsedUnitInfo.InFilePosition > 0 then
    begin
      ReplaceSuccessful := SafeReplaceTextInFile(DummyRun, DelphiFile.UnitInfo.Filename, OldUnitName, NewUnitName, UsedUnitInfo.InFilePosition);
      if ReplaceSuccessful then
        if not DummyRun then
        begin
          UsedUnitInfo.InFilePosition := UsedUnitInfo.InFilePosition + PositionOffset;
          PositionOffset := PositionOffset * 2; // x 2 because there were two text-replaces when
        end;

    end;

    // replace text occurence for <OldUnitName in '....\OldUnitName.pas'>
    // ->                          ^^^^^^^^^^^
    ReplaceSuccessful := SafeReplaceTextInFile(DummyRun, DelphiFile.UnitInfo.Filename, OldUnitName, NewUnitName, UsedUnitInfo.Position)
                         or ReplaceSuccessful;

    if ReplaceSuccessful then
    begin
      TDudsLogger.GetInstance.Log(StrUpdatedUsesClause, [OldUnitName, NewUnitName, DelphiFile.UnitInfo.Filename]);

      if not DummyRun then
      begin
        // step 2: update the recently changed uses in the meta data (start position stays valid...)
        UsedUnitInfo.DelphiUnitName := NewUnitName;

        // step 3: update position of all following uses in the meta data
        IsUsesAfterRenamedUses := false;
        for FollowingUsedUnitInfo in DelphiFile.UnitInfo.UsedUnits do
        begin
          if IsUsesAfterRenamedUses then
            FollowingUsedUnitInfo.UpdatePosition(PositionOffset);
          if FollowingUsedUnitInfo = UsedUnitInfo then
            IsUsesAfterRenamedUses := true;
        end;

      end;

      Inc(UpdatedCount);
    end;
  end;

  function RenameDelphiFile(DelphiFile: TDelphiFile): Boolean;
  var
    NewUnitName, NewUnitFilename, NewUnitFilenameExt, OldUnitNameComment, OldUnitName, OldFilename: String;
    i: Integer;
    PosOffset: Integer;
    UnitInfo: IUnitInfo;
    UsedUnitInfo: IUsedUnitInfo;
  begin
    Result := FALSE;

    UnitInfo := DelphiFile.UnitInfo;

    // Store the name of the old unit
    OldFilename := UnitInfo.Filename;
    OldUnitName := UnitInfo.DelphiUnitName;
    if InsertOldNameComment then
      OldUnitNameComment := ' {renamed from ' + OldUnitName + '.pas}'
    else
      OldUnitNameComment := '';

    // Generate the new unit name and filename
    NewUnitName := SearchAndReplaceUnitName(UnitInfo.DelphiUnitName);
    NewUnitFilenameExt := ExtractFileExt(UnitInfo.Filename);
    if LowerCaseExtension then
      NewUnitFilenameExt := LowerCase(NewUnitFilenameExt);
    NewUnitFilename := IncludeTrailingPathDelimiter(ExtractFileDir(UnitInfo.Filename)) + NewUnitName +
      NewUnitFilenameExt;

    // Update the unit name in the file to be renamed
    if UnitInfo.DelphiUnitNamePosition = 0 then
    begin
      // We have no info about the position of the unit name in the source code.
      // Most likely due to a bug in the tokeniser.
      TDudsLogger.GetInstance.Log(StrUnableToRenameS, [UnitInfo.DelphiUnitName]);
    end
    else

      // Try to rename the file
      if RenameMatchingFiles(UnitInfo.Filename, NewUnitName, FALSE, LowerCaseExtension) then
      begin
        Result := TRUE;

        // If the file was renamed, update the local file info
        if not DummyRun then
          UnitInfo.Filename := NewUnitFilename;

        // Update the unitname clause
        if DummyRun or (SafeReplaceTextInFile(DummyRun, UnitInfo.Filename, UnitInfo.DelphiUnitName,
          NewUnitName + OldUnitNameComment, UnitInfo.DelphiUnitNamePosition)) then
        begin
          TDudsLogger.GetInstance.Log(StrUpdatedDelphiUnitNameIn, [UnitInfo.Filename, OldUnitName, NewUnitName]);

          if not DummyRun then
          begin
            UnitInfo.DelphiUnitName := NewUnitName;

            // Calculate the offset for the units in the uses clauses
            PosOffset := Length(NewUnitName + OldUnitNameComment) - Length(OldUnitName);

            // Update the position of all the used units
            for UsedUnitInfo in UnitInfo.UsedUnits do
              UsedUnitInfo.UpdatePosition(PosOffset);
          end;
        end;
      end;
  end;

var
  // StepNode: PVirtualNode;
  NodeDelphiUnitName: String;
  PreviousUnitName: String;
  DelphiFile: TDelphiFile;
  UsedUnitInfo: IUsedUnitInfo;
begin
  if aClearLog then
    TDudsLogger.GetInstance.Clear;

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISISADUMMYRUN, LogWarning);

  // Clear PreviousUnitNames in all files
  for DelphiFile in FModel.DelphiFileList do
     DelphiFile.UnitInfo.PreviousUnitName := '';

  UpdatedCount := 0;

  // Update all the uses clauses in the files
  for DelphiFile in FModel.DelphiFileList do
  begin

    // Is this the file we are renaming?
    if (DelphiFile.UnitInfo.DelphiFileType = ftPas) and IsUnitNameMatch(DelphiFile.UnitInfo.DelphiUnitName) then
    begin
      PreviousUnitName := DelphiFile.UnitInfo.DelphiUnitName;
      if RenameDelphiFile(DelphiFile) then
        DelphiFile.UnitInfo.PreviousUnitName := PreviousUnitName;
    end;

    // check all uses of the file
    for UsedUnitInfo in DelphiFile.UnitInfo.UsedUnits do
       if IsUnitNameMatch(UsedUnitInfo.DelphiUnitName) then
          UpdateUsesClauseInDelphiFile(DelphiFile, UsedUnitInfo);
  end;

  TDudsLogger.GetInstance.Log(StrFinishedUpdated, [UpdatedCount]);

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISWASADUMMYRUN, LogWarning);
end;

end.
