unit Duds.Refactoring.RenameUnit;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  System.DateUtils,
  System.Generics.Collections, System.IOUtils, System.RegularExpressions,

  Duds.Common.Strings,

  Duds.Common.Classes,
  Duds.Common.Files,
  Duds.Common.Language,
  Duds.Common.Log,
  Duds.Common.Interfaces,
  Duds.Common.Refactoring;

type
  TDudsRenameRefacotring = class(TObject)
  public
    procedure RenameDelphiFile(const aClearLog: Boolean; const SearchString, ReplaceString: String;
      const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment, LowerCaseExtension: Boolean);

  end;

implementation

procedure TDudsRenameRefacotring.RenameDelphiFile(const aClearLog: Boolean; const SearchString, ReplaceString: String;
  const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment, LowerCaseExtension: Boolean);

  function GetDelphiUnitName(Node: PVirtualNode): String;
  begin
    if FNodeObjects[GetID(Node)].DelphiFile.UnitInfo.PreviousUnitName <> '' then
      Result := FNodeObjects[GetID(Node)].DelphiFile.UnitInfo.PreviousUnitName
    else
      Result := FNodeObjects[GetID(Node)].DelphiFile.UnitInfo.DelphiUnitName;
  end;

  function IsUnitNameMatch(const UnitName: String): Boolean;
  begin
    if ExactMatch then
      Result := SameText(SearchString, UnitName)
    else
      Result := TRegEx.IsMatch(UnitName, SearchString);
  end;

  function SearchAndReplaceUnitName(const UnitName: String): String;
  begin
    if IsUnitNameMatch(UnitName) then
    begin
      if ExactMatch then
        Result := StringReplace(UnitName, SearchString, ReplaceString, [rfReplaceAll, rfIgnoreCase])
      else
        Result := TRegEx.Replace(UnitName, SearchString, ReplaceString);
    end
    else
      Result := UnitName;
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

  procedure UpdateUsesClause(UpdateNode: PVirtualNode);
  var
    StepNode: PVirtualNode;
    PosOffset: Integer;
    FileUsingTheUnit: IUnitInfo;
    UsedUnitIndex: Integer;
    UsedUnitInfo: IUsedUnitInfo;
    OldUnitName, NewUnitName: String;
    SomethingChanged: Boolean;
  begin
    OldUnitName := GetDelphiUnitName(UpdateNode);
    NewUnitName := SearchAndReplaceUnitName(OldUnitName);

    FileUsingTheUnit := FNodeObjects[GetID(UpdateNode.Parent)].DelphiFile.UnitInfo;
    // file to update (using the unit to rename)

    UsedUnitIndex := GetNodeIndex(UpdateNode);
    // Index of the unit in the uses list of the file to update
    UsedUnitInfo := FileUsingTheUnit.UsedUnits[UsedUnitIndex];
    // detail info about the used unit we are renaming

    // step 1: apply the rename in the file
    SomethingChanged := FALSE;

    // replace text occurence for <OldUnitName in '....\OldUnitName.pas'>
    // ->                                               ^^^^^^^^^^^
    if UsedUnitInfo.InFilePosition > 0 then
      SomethingChanged := SafeReplaceTextInFile(DummyRun, FileUsingTheUnit.Filename, OldUnitName, NewUnitName,
        UsedUnitInfo.InFilePosition);

    // replace text occurence for <OldUnitName in '....\OldUnitName.pas'>
    // ->                          ^^^^^^^^^^^
    SomethingChanged := SafeReplaceTextInFile(DummyRun, FileUsingTheUnit.Filename, OldUnitName, NewUnitName,
      UsedUnitInfo.Position) or SomethingChanged;

    if SomethingChanged then
    begin
      TDudsLogger.GetInstance.Log(StrUpdatedUsesClause, [OldUnitName, NewUnitName, FileUsingTheUnit.Filename]);

      if not DummyRun then
      begin
        PosOffset := Length(NewUnitName) - Length(OldUnitName);

        // step 2: update the recently changed uses in the meta data
        UsedUnitInfo.DelphiUnitName := NewUnitName;
        if UsedUnitInfo.InFilePosition > 0 then
        begin
          if UsedUnitInfo.Position > 0 then
            UsedUnitInfo.InFilePosition := UsedUnitInfo.InFilePosition + PosOffset;

          PosOffset := PosOffset * 2;
        end;

        // step 3: update position of all following uses in the meta data
        StepNode := UpdateNode.NextSibling;

        while StepNode <> nil do
        begin
          FileUsingTheUnit.UsedUnits[GetNodeIndex(StepNode)].UpdatePosition(PosOffset);

          StepNode := StepNode.NextSibling;
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
            for i := 0 to pred(UnitInfo.UsedUnits.Count) do
              UnitInfo.UsedUnits[i].UpdatePosition(PosOffset);
          end;
        end;
      end;
  end;

var
  StepNode: PVirtualNode;
  NodeDelphiUnitName, UpdateFilename: String;
  PreviousUnitName: String;
begin
  if aClearLog then
    TDudsLogger.GetInstance.Clear;

  // Clear PreviousUnitNames
  StepNode := vtUnitsTree.GetFirst;

  while StepNode <> nil do
  begin
    FNodeObjects[GetID(StepNode)].DelphiFile.UnitInfo.PreviousUnitName := '';

    StepNode := vtUnitsTree.GetNext(StepNode);
  end;

  UpdatedCount := 0;

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISISADUMMYRUN, LogWarning);

  // Update all the uses clauses in the files
  StepNode := vtUnitsTree.GetFirst;

  while StepNode <> nil do
  begin
    NodeDelphiUnitName := GetDelphiUnitName(StepNode);

    // Does this name unit match?
    if IsUnitNameMatch(NodeDelphiUnitName) then
    begin
      // Get the filename for the unit
      UpdateFilename := FNodeObjects[GetID(StepNode)].DelphiFile.UnitInfo.Filename;

      // Update the filename if this is the original file node
      if FNodeObjects[GetID(StepNode)].DelphiFile.BaseNode = StepNode then
      begin
        PreviousUnitName := FNodeObjects[GetID(StepNode)].DelphiFile.UnitInfo.DelphiUnitName;

        if RenameDelphiFile(FNodeObjects[GetID(StepNode)].DelphiFile) then
          FNodeObjects[GetID(StepNode)].DelphiFile.UnitInfo.PreviousUnitName := PreviousUnitName;
      end;

      // Only update the uses clasuses for non root nodes
      if vtUnitsTree.GetNodeLevel(StepNode) > 0 then
        UpdateUsesClause(StepNode);
    end;

    StepNode := vtUnitsTree.GetNext(StepNode);
  end;

  TDudsLogger.GetInstance.Log(StrFinishedUpdated, [UpdatedCount]);

  if DummyRun then
    TDudsLogger.GetInstance.Log(StrTHISWASADUMMYRUN, LogWarning);

end;

end.
