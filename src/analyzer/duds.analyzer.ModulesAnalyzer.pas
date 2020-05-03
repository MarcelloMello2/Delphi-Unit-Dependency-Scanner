unit duds.analyzer.ModulesAnalyzer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils,

  duds.common.Interfaces,
  duds.common.Language,
  duds.common.Types,
  duds.common.Log,
  duds.analyzer.model,
  duds.common.Classes,
  duds.common.modules, duds.analyzer.CustomAnalyzer, duds.common.Utils;

type
  TModulesAnalyzer = class(TCustomAnalyzer)
  protected
    function GetLogAreaName: string; override;

  public
    procedure MapUnitsToModules;
    procedure AnalyzeDependencies;

  end;

implementation

{ TModulesAnalyzer }

function TModulesAnalyzer.GetLogAreaName: string;
begin
  Result := 'Modules Analysis';
end;

procedure TModulesAnalyzer.MapUnitsToModules;
var
  aDelphiFile: TDelphiFile;
  i: integer;
  aModule: TModule;
  UnitsTotal, UnitsMappedToModules: Integer;
begin
  fModel.Modules.ClearModulesAnalysisData;
  fModel.Modules.ReBuildFastSearchList;
  fModel.Modules.CreateUnknownModule;

  UnitsTotal := 0;
  UnitsMappedToModules := 0;
  for i := 0 to pred(fModel.DelphiFileList.Count) do
  begin
    if fCancelled then
      break;

    aDelphiFile := fModel.DelphiFileList[i];
    if (aDelphiFile.UnitInfo.DelphiFileType = ftPAS) or (not aDelphiFile.InSearchPath) then  // files that are not in search path do not get a filetype but should be '.pas'...
    begin
      Inc(UnitsTotal);

      // try to find a matching module
      if fModel.Modules.FindModuleForUnit(aDelphiFile.UnitInfo, aModule) then
        Inc(UnitsMappedToModules)
      else
        aModule := fModel.Modules.UnknownModule;

      aDelphiFile.UnitInfo.Module := aModule;

      // update the modules analysis data
      aModule.AnalysisData.NumberOfFiles := aModule.AnalysisData.NumberOfFiles + 1;
      if not aDelphiFile.InSearchPath then
        aModule.AnalysisData.NumberOfFilesNotInPath := aModule.AnalysisData.NumberOfFilesNotInPath + 1;
      aModule.AnalysisData.LinesOfCode   := aModule.AnalysisData.LinesOfCode   + aDelphiFile.UnitInfo.LinesOfCode;
    end;
  end;

  Log(StrModulesIdentified, [FormatCardinal(UnitsMappedToModules), FormatCardinal(UnitsTotal)], LogInfo);
end;

procedure TModulesAnalyzer.AnalyzeDependencies;
begin
  if fCancelled then
    exit;
   // TODO
end;



end.
