unit Duds.Export.Gephi;

interface

uses
  System.Classes, System.SysUtils,

  Duds.Common.Interfaces,
  Duds.Common.Strings,
  Duds.Common.Classes,
  Duds.Scan.Model;

procedure ExportToGephi(Model: TDudsModel; ExportUnitsNotInPath: Boolean; const Filename: String);

implementation

procedure ExportToGephi(Model: TDudsModel; ExportUnitsNotInPath: Boolean; const Filename: String);
var
  Lines: TStringList;
  CurrentLine: String;
  UsedUnitInfo: IUsedUnitInfo;
  DelphiFile, UsedDelphiFile: TDelphiFile;
begin
  Lines := TStringList.Create;
  try
    for DelphiFile in Model.ParsedDelphiFiles.Values do
    begin
      if DelphiFile.InSearchPath or ExportUnitsNotInPath then
      begin
        // Add current unit name
        CurrentLine := Trim(DelphiFile.UnitInfo.DelphiUnitName);

        // Add references to all used units
        for UsedUnitInfo in DelphiFile.UnitInfo.UsedUnits do
          if Model.ParsedDelphiFiles.TryGetValue(UpperCase(UsedUnitInfo.DelphiUnitName), UsedDelphiFile) then
            if UsedDelphiFile.InSearchPath or ExportUnitsNotInPath then
              AddToken(CurrentLine, Trim(UsedUnitInfo.DelphiUnitName), ';');

        Lines.Add(CurrentLine);
      end;
    end;

    Lines.SaveToFile(Filename);
  finally
    FreeAndNil(Lines);
  end;
end;

end.
